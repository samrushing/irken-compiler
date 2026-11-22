# -*- Mode: Python -*-

# SMTP servers for testing doom/mail/smtp.scm (see run.sh).
#
#   python3 smtpd.py <dir> [login]
#
# in <dir>, which must hold cert.pem and key.pem:
#   127.0.0.1:2587  STARTTLS required, AUTH (sam/hunter2), SIZE 100000
#   127.0.0.1:2465  TLS from the start, AUTH
#   127.0.0.1:2525  plain text, no STARTTLS
#   127.0.0.1:2600  a bad server: see bad_server()
# with 'login', AUTH offers only LOGIN, not PLAIN.
#
# each message received is written to <dir>/mail/NNN.json.  addresses
# starting with "nobody@" are refused.
#
# needs aiosmtpd (pip install aiosmtpd).

import json
import os
import socket
import ssl
import sys
import threading
import time

from aiosmtpd.controller import Controller
from aiosmtpd.smtp import AuthResult, LoginPassword

root = sys.argv[1]
login_only = len(sys.argv) > 2 and sys.argv[2] == 'login'
maildir = os.path.join(root, 'mail')
os.makedirs(maildir, exist_ok=True)

class Handler:
    async def handle_RCPT(self, server, session, envelope, address, rcpt_options):
        if address.startswith('nobody@'):
            return '550 5.1.1 no such user'
        envelope.rcpt_tos.append(address)
        return '250 OK'

    async def handle_DATA(self, server, session, envelope):
        n = len(os.listdir(maildir))
        with open(os.path.join(maildir, '%03d.json' % n), 'w') as f:
            json.dump({'from': envelope.mail_from,
                       'to': envelope.rcpt_tos,
                       'opts': envelope.mail_options,
                       'data': envelope.original_content.decode('latin-1')}, f)
        return '250 2.0.0 queued as %03d' % n

def authenticator(server, session, envelope, mechanism, auth_data):
    ok = (isinstance(auth_data, LoginPassword)
          and auth_data.login == b'sam' and auth_data.password == b'hunter2')
    return AuthResult(success=ok, handled=False)

# a server that misbehaves, once per connection, in turn:
#  1. sends a plain-text reply after its STARTTLS reply (an injection
#     attempt: cf. CVE-2011-0411), which the client must refuse.
#  2. sends a malformed reply.
def bad_server():
    l = socket.socket()
    l.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    l.bind(('127.0.0.1', 2600))
    l.listen()
    for mode in ['inject', 'garbage']:
        c, _ = l.accept()
        f = c.makefile('rb')
        c.sendall(b'220-bad.example ESMTP\r\n220 a multi-line greeting\r\n')
        f.readline()
        c.sendall(b'250-bad.example\r\n250 STARTTLS\r\n')
        f.readline()
        if mode == 'inject':
            c.sendall(b'220 go ahead\r\n250-bad.example\r\n250 AUTH PLAIN\r\n')
        else:
            c.sendall(b'2x0 what\r\n')
        time.sleep(0.5)
        c.close()

ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.load_cert_chain(os.path.join(root, 'cert.pem'), os.path.join(root, 'key.pem'))
exclude = ['PLAIN'] if login_only else []

servers = [
    Controller(Handler(), hostname='127.0.0.1', port=2587, tls_context=ctx,
               require_starttls=True, authenticator=authenticator,
               auth_require_tls=True, auth_exclude_mechanism=exclude,
               data_size_limit=100000),
    # (aiosmtpd doesn't count TLS from the start as TLS for AUTH.)
    Controller(Handler(), hostname='127.0.0.1', port=2465, ssl_context=ctx,
               authenticator=authenticator, auth_require_tls=False,
               auth_exclude_mechanism=exclude),
    Controller(Handler(), hostname='127.0.0.1', port=2525),
]
for s in servers:
    s.start()
threading.Thread(target=bad_server, daemon=True).start()
print('ready', flush=True)
while True:
    time.sleep(3600)
