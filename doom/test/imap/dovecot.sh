#!/bin/sh
# -*- Mode: sh -*-

# start a throwaway dovecot in <dir>, as the current user:
#   127.0.0.1:<port>      IMAP, with STARTTLS
#   127.0.0.1:<port>+1    IMAP over TLS (like 993)
# users: sam/hunter2 (with some messages), other/otherpw.
#  plain-text logins are allowed without TLS, for the tests.
#
#   dovecot.sh <dir> <port>
#
# stop it with: doveadm -c <dir>/dovecot.conf stop
#
# needs dovecot (2.3), openssl.  run by run.sh.

dir=$1
port=$2
tlsport=$((port + 1))
me=$(id -un)
mkdir -p "$dir/run" "$dir/mail" || exit 1

openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
        -keyout "$dir/key.pem" -out "$dir/cert.pem" -days 1 -subj /CN=localhost \
        -addext "subjectAltName=DNS:localhost" 2>/dev/null || exit 1

cat > "$dir/passwd" <<PW
sam:{PLAIN}hunter2::::::
other:{PLAIN}otherpw::::::
PW

cat > "$dir/dovecot.conf" <<CONF
base_dir = $dir/run
state_dir = $dir/run
log_path = $dir/dovecot.log
default_internal_user = $me
default_internal_group = $(id -gn)
default_login_user = $me
protocols = imap
listen = 127.0.0.1
ssl = yes
ssl_cert = <$dir/cert.pem
ssl_key = <$dir/key.pem
disable_plaintext_auth = no
auth_mechanisms = plain login
mail_location = maildir:$dir/mail/%u
passdb {
  driver = passwd-file
  args = $dir/passwd
}
userdb {
  driver = static
  args = uid=$me gid=$(id -gn) home=$dir/mail/%u
}
service imap-login {
  inet_listener imap {
    address = 127.0.0.1
    port = $port
  }
  inet_listener imaps {
    address = 127.0.0.1
    port = $tlsport
    ssl = yes
  }
}
service anvil {
  chroot =
}
service imap-login {
  chroot =
}
namespace inbox {
  inbox = yes
  separator = /
  mailbox Drafts {
    special_use = \Drafts
    auto = create
  }
  mailbox Trash {
    special_use = \Trash
    auto = create
  }
}
CONF

dovecot -c "$dir/dovecot.conf" || { cat "$dir/dovecot.log"; exit 1; }
