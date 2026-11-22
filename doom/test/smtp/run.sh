#!/bin/sh
# -*- Mode: sh -*-

# test doom/mail/smtp.scm, through demo/mail/send, against the servers
#  in smtpd.py (needs python3 with aiosmtpd, and openssl).
#
#   $ doom/test/smtp/run.sh        (from the top of the source tree)
#
# builds demo/mail/send first if it isn't there.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../../.." && pwd)
send="$top/demo/mail/send"
work=$(mktemp -d)
pass=0
fail=0

# (waiting for it keeps bash, as /bin/sh, from reporting "Terminated".)
stop_server () {
    if [ -n "$server" ]; then
        kill $server 2>/dev/null
        wait $server 2>/dev/null
        server=
    fi
}

cleanup () {
    stop_server
    rm -rf "$work"
}
trap cleanup EXIT

if [ ! -x "$send" ]; then
    (cd "$top" && IRKENLIB=. self/irken demo/mail/send.scm -q) || exit 1
fi

openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
        -keyout "$work/key.pem" -out "$work/cert.pem" -days 1 -subj /CN=localhost \
        -addext "subjectAltName=DNS:localhost" 2>/dev/null || exit 1
openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
        -keyout "$work/other-key.pem" -out "$work/other.pem" -days 1 -subj /CN=other.example 2>/dev/null || exit 1

start_server () {
    stop_server
    rm -rf "$work/mail"
    python3 "$here/smtpd.py" "$work" $1 > "$work/server.log" 2>&1 &
    server=$!
    for i in 1 2 3 4 5 6 7 8 9 10; do
        grep -q ready "$work/server.log" 2>/dev/null && return
        sleep 1
    done
    echo "server didn't start:"; cat "$work/server.log"; exit 1
}

# check <name> <exit code wanted> <text wanted in the output, or ""> <command...>
check () {
    name=$1; want=$2; text=$3; shift 3
    out=$("$@" 2>&1)
    got=$?
    if [ $got -eq $want ] && { [ -z "$text" ] || printf '%s\n' "$out" | grep -qF -- "$text"; }; then
        pass=$((pass + 1))
        echo "ok    $name"
    else
        fail=$((fail + 1))
        echo "FAIL  $name (exit $got, wanted $want)"
        printf '%s\n' "$out" | sed 's/^/        /'
    fi
}

# the newest message the server saved, as "to | options | data", with
#  each CRLF in the data shown as "<CRLF>".
last_mail () {
    python3 -c "
import json, os, sys
d = '$work/mail'
m = json.load(open(os.path.join(d, sorted(os.listdir(d))[-1])))
print(' '.join(m['to']), '|', ' '.join(m['opts']), '|', m['data'].replace('\\r\\n', '<CRLF>'))"
}

msg="$work/msg.txt"
printf 'From: sam@example.com\nTo: fred@example.com\nSubject: test\n\nhello\n.one dot\n..two dots\nbye\n' > "$msg"
root="-root $work/cert.pem"
export SMTP_PASSWORD=hunter2

start_server

check "starttls + auth" 0 "sent to 2 of 2" \
      $send -v -host localhost -port 2587 $root -user sam -from sam@example.com -f "$msg" fred@x wilma@x
check "  message arrived intact" 0 "fred@x wilma@x | SIZE=" last_mail
check "  dot-stuffing undone" 0 "<CRLF>hello<CRLF>.one dot<CRLF>..two dots<CRLF>bye<CRLF>" last_mail
check "implicit tls + auth" 0 "" \
      $send -tls -host localhost -port 2465 $root -user sam -from a@x -f "$msg" fred@x
check "plain, no auth" 0 "" \
      $send -plain -host localhost -port 2525 -from a@x -f "$msg" fred@x
check "8-bit message" 0 "" \
      sh -c "printf 'Subject: caf\\303\\251\\n\\nna\\303\\257ve\\n' | $send -plain -host localhost -port 2525 -from a@x fred@x"
check "  BODY=8BITMIME" 0 "BODY=8BITMIME" last_mail
check "wrong password" 1 "535" \
      env SMTP_PASSWORD=wrong $send -host localhost -port 2587 $root -user sam -from a@x -f "$msg" fred@x
check "no password without tls" 1 "won't send a password" \
      $send -plain -host localhost -port 2525 -user sam -from a@x -f "$msg" fred@x
check "one recipient refused" 1 "refused: nobody@x: 550" \
      $send -plain -host localhost -port 2525 -from a@x -f "$msg" fred@x nobody@x
check "all recipients refused" 1 "RCPT: 550" \
      $send -plain -host localhost -port 2525 -from a@x -f "$msg" nobody@x nobody@y
check "starttls not offered" 1 "doesn't offer STARTTLS" \
      $send -host localhost -port 2525 $root -from a@x -f "$msg" fred@x
check "untrusted certificate" 1 "no trusted root" \
      $send -host localhost -port 2587 -root "$work/other.pem" -from a@x -f "$msg" fred@x
check "no certificates in -root" 1 "no certificates in /dev/null" \
      $send -host localhost -port 2587 -root /dev/null -from a@x -f "$msg" fred@x
check "certificate for another name" 1 "not valid for 127.0.0.1" \
      $send -host 127.0.0.1 -port 2587 $root -from a@x -f "$msg" fred@x
check "CR/LF in an address" 1 "can't send" \
      $send -plain -host localhost -port 2525 -from "$(printf 'a@x>\r\nRCPT TO:<evil@x')" -f "$msg" fred@x
python3 -c "print('Subject: big\n\n' + ('x' * 70 + '\n') * 2000)" > "$work/big.txt"
check "bigger than SIZE" 1 "server limit is 100000" \
      $send -host localhost -port 2587 $root -user sam -from a@x -f "$work/big.txt" fred@x
check "plain text after STARTTLS reply" 1 "data received after the STARTTLS reply" \
      $send -host localhost -port 2600 $root -from a@x -f "$msg" fred@x
check "malformed reply" 1 "bad reply from server" \
      $send -host localhost -port 2600 $root -from a@x -f "$msg" fred@x

start_server login

check "auth LOGIN" 0 "authenticated as sam" \
      $send -v -host localhost -port 2587 $root -user sam -from a@x -f "$msg" fred@x
check "auth LOGIN, wrong password" 1 "535" \
      env SMTP_PASSWORD=wrong $send -tls -host localhost -port 2465 $root -user sam -from a@x -f "$msg" fred@x

echo "$pass passed, $fail failed"
[ $fail -eq 0 ]
