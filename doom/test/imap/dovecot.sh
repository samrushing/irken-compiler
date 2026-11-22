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
# needs dovecot (2.3 or 2.4), openssl.  run by run.sh.

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

# dovecot 2.4 changed its configuration language: settings renamed,
#  named passdb/userdb blocks, %{user} rather than %u.
grp=$(id -gn)
hard=$(ulimit -H -d)
if [ "$hard" = unlimited ]; then
    vsz=9223372036854775807     # macOS's RLIM_INFINITY, 2^63-1
else
    vsz=$((hard * 1024))
fi
case "$(dovecot --version)" in
2.[0-3].*)
cat > "$dir/dovecot.conf" <<CONF
base_dir = $dir/run
state_dir = $dir/run
log_path = $dir/dovecot.log
default_internal_user = $me
default_internal_group = $(id -gn)
default_login_user = $me
# (no per-process memory limit: macOS refuses setrlimit(RLIMIT_DATA).)
default_vsz_limit = 0
# (a test needs few connections; macOS's default ulimit -n is 256.)
default_client_limit = 50
default_process_limit = 10
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

;;
*)
cat > "$dir/dovecot.conf" <<CONF
dovecot_config_version = 2.4.0
dovecot_storage_version = 2.4.0
base_dir = $dir/run
state_dir = $dir/run
log_path = $dir/dovecot.log
default_internal_user = $me
default_internal_group = $grp
default_login_user = $me
# (the per-process memory limit: our own hard limit.  macOS refused a
#  lower one with EINVAL, and a higher one is only for root; 2.4 won't
#  take 0 for none.)
default_vsz_limit = $vsz
# (a test needs few connections; macOS's default ulimit -n is 256.)
default_client_limit = 50
default_process_limit = 10
protocols {
  imap = yes
}
listen = 127.0.0.1
ssl = yes
ssl_server_cert_file = $dir/cert.pem
ssl_server_key_file = $dir/key.pem
auth_allow_cleartext = yes
auth_mechanisms = plain login
mail_driver = maildir
mail_home = $dir/mail/%{user}
mail_path = $dir/mail/%{user}
passdb passwd-file {
  passwd_file_path = $dir/passwd
}
userdb static {
  fields {
    uid = $me
    gid = $grp
    home = $dir/mail/%{user}
  }
}
service imap-login {
  chroot =
  inet_listener imap {
    port = $port
  }
  inet_listener imaps {
    port = $tlsport
    ssl = yes
  }
}
service anvil {
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
;;
esac

dovecot -c "$dir/dovecot.conf" || { cat "$dir/dovecot.log"; exit 1; }
