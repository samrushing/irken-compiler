#!/bin/sh
# -*- Mode: sh -*-

# test doom/db/postgres.scm (t_postgres.scm) against a throwaway
#  PostgreSQL cluster, made with initdb in a temporary directory.
#
#   $ doom/test/postgres/run.sh        (from the top of the source tree)
#
# needs the server's programs (initdb, pg_ctl, psql) on $PATH, or in
#  $PG_BIN; and openssl, and python3 (for fake_server.py).  the cluster
#  listens on 127.0.0.1, port $PG_TEST_PORT (default 5433).
#
# everything is run twice: in plain text, and over TLS.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../../.." && pwd)
prog="$here/t_postgres"
port=${PG_TEST_PORT:-5433}
fakeport=$((port + 66))
work=$(mktemp -d)
[ -n "$PG_BIN" ] && PATH="$PG_BIN:$PATH"
status=1

stop () {
    if [ -n "$started" ]; then
        pg_ctl -D "$work/db" -m immediate stop > /dev/null 2>&1
        started=
    fi
    if [ -n "$fake" ]; then
        kill $fake 2>/dev/null
        wait $fake 2>/dev/null
        fake=
    fi
}

cleanup () {
    stop
    rm -rf "$work"
}
trap cleanup EXIT

for p in initdb pg_ctl psql openssl python3; do
    command -v $p > /dev/null || { echo "can't find $p (set PG_BIN?)"; exit 1; }
done

if [ ! -x "$prog" ]; then
    (cd "$top" && IRKENLIB=. self/irken doom/test/postgres/t_postgres.scm -q) || exit 1
fi

openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
        -keyout "$work/key.pem" -out "$work/cert.pem" -days 1 -subj /CN=localhost \
        -addext "subjectAltName=DNS:localhost" 2>/dev/null || exit 1
chmod 600 "$work/key.pem"

initdb -D "$work/db" -A trust -U postgres > "$work/initdb.log" 2>&1 \
    || { cat "$work/initdb.log"; exit 1; }

cat >> "$work/db/postgresql.conf" <<EOF
port = $port
listen_addresses = '127.0.0.1'
unix_socket_directories = '$work'
ssl = on
ssl_cert_file = '$work/cert.pem'
ssl_key_file = '$work/key.pem'
password_encryption = 'scram-sha-256'
EOF

cat > "$work/db/pg_hba.conf" <<EOF
local     all  all                   trust
host      all  trusty   127.0.0.1/32 trust
host      all  clear    127.0.0.1/32 password
host      all  old      127.0.0.1/32 md5
hostssl   all  tlsonly  127.0.0.1/32 scram-sha-256
hostnossl all  tlsonly  127.0.0.1/32 reject
host      all  all      127.0.0.1/32 scram-sha-256
EOF

pg_ctl -D "$work/db" -l "$work/server.log" -w start > /dev/null \
    || { cat "$work/server.log"; exit 1; }
started=1

psql="psql -q -h $work -p $port -U postgres -d postgres"
$psql -c "create user trusty" \
      -c "create user clear password 'clearpw'" \
      -c "create user sam password 'hunter2'" \
      -c "create user tlsonly password 'tlspw'" \
      -c "set password_encryption = 'md5'; create user old password 'oldpw'" \
      -c "create database test owner sam" 2>&1 | grep -v "^WARNING\|^DETAIL\|^HINT" \
    || true

echo "--- plain text"
"$prog" $port
plain=$?
echo "--- TLS"
"$prog" $port tls "$work/cert.pem"
tls=$?

python3 "$here/fake_server.py" $fakeport > "$work/fake.log" 2>&1 &
fake=$!
for i in 1 2 3 4 5; do
    grep -q ready "$work/fake.log" 2>/dev/null && break
    sleep 1
done
echo "--- misbehaving server"
"$prog" $fakeport fake
faked=$?

[ $plain -eq 0 ] && [ $tls -eq 0 ] && [ $faked -eq 0 ] && status=0
exit $status
