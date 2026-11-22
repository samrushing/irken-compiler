#!/bin/sh
# -*- Mode: sh -*-

# test doom/mail/imap.scm (t_imap.scm) against a throwaway dovecot
#  (dovecot.sh), and against fake_server.py.
#
#   $ doom/test/imap/run.sh        (from the top of the source tree)
#
# needs dovecot (2.3) and doveadm on $PATH, openssl, and python3.  the
#  server listens on 127.0.0.1, ports $IMAP_TEST_PORT (default 2143)
#  and the next one up.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../../.." && pwd)
prog="$here/t_imap"
port=${IMAP_TEST_PORT:-2143}
fakeport=$((port + 66))
work=$(mktemp -d)
status=1

stop () {
    if [ -f "$work/dovecot.conf" ]; then
        doveadm -c "$work/dovecot.conf" stop > /dev/null 2>&1
        # (in case doveadm couldn't reach it)
        [ -f "$work/run/master.pid" ] && kill $(cat "$work/run/master.pid") 2>/dev/null
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

for p in dovecot doveadm openssl python3; do
    command -v $p > /dev/null || { echo "can't find $p"; exit 1; }
done

if [ ! -x "$prog" ]; then
    (cd "$top" && IRKENLIB=. self/irken doom/test/imap/t_imap.scm -q) || exit 1
fi

"$here/dovecot.sh" "$work" $port || exit 1
for i in 1 2 3 4 5; do
    grep -q "starting up" "$work/dovecot.log" 2>/dev/null && break
    sleep 1
done

echo "--- dovecot"
"$prog" $port "$work/cert.pem"
real=$?

python3 "$here/fake_server.py" $fakeport > "$work/fake.log" 2>&1 &
fake=$!
for i in 1 2 3 4 5; do
    grep -q ready "$work/fake.log" 2>/dev/null && break
    sleep 1
done
echo "--- misbehaving server"
"$prog" $fakeport fake
faked=$?

[ $real -eq 0 ] && [ $faked -eq 0 ] && status=0
exit $status
