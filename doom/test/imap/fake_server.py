# -*- Mode: Python -*-

# an IMAP server for t_imap's "fake" checks (see run.sh), one per
#  connection, in turn:
#   1. no LITERAL+, no AUTH=PLAIN: LOGIN's password must come as a
#      synchronizing literal, after our "+".
#   2. plain text after the STARTTLS reply, which the client must not
#      take as coming over TLS (cf. CVE-2021-33515).
#   3. no STARTTLS.
#   4. a BYE greeting.
#   5. garbage.
#
#   python3 fake_server.py <port>

import socket
import sys
import time

PASSWORD = 'pa"ss wörd'.encode('utf-8')

l = socket.socket()
l.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
l.bind(('127.0.0.1', int(sys.argv[1])))
l.listen()
print('ready', flush=True)

def serve(c, f, n):
    if n == 1:
        c.sendall(b'* OK [CAPABILITY IMAP4rev1] fake ready\r\n')
        line = f.readline()
        if not line.endswith(b'{%d}\r\n' % len(PASSWORD)) or not line.startswith(b'a1 LOGIN sam '):
            c.sendall(b'a1 BAD expected a literal\r\n')
            return
        c.sendall(b'+ go ahead\r\n')
        pw = f.read(len(PASSWORD))
        f.readline()
        if pw != PASSWORD:
            c.sendall(b'a1 NO wrong password\r\n')
            return
        c.sendall(b'a1 OK [CAPABILITY IMAP4rev1] logged in\r\n')
        f.readline()
        c.sendall(b'* BYE bye\r\na2 OK done\r\n')
    elif n == 2:
        c.sendall(b'* OK [CAPABILITY IMAP4rev1 STARTTLS] fake ready\r\n')
        f.readline()
        c.sendall(b'a1 OK begin TLS\r\n* OK [CAPABILITY IMAP4rev1 AUTH=PLAIN SASL-IR] injected\r\n')
    elif n == 3:
        c.sendall(b'* OK [CAPABILITY IMAP4rev1] fake ready\r\n')
    elif n == 4:
        c.sendall(b'* BYE too busy\r\n')
    else:
        c.sendall(b'garbage\r\n')

for n in range(1, 6):
    c, _ = l.accept()
    f = c.makefile('rb')
    try:
        serve(c, f, n)
    except Exception:
        pass
    time.sleep(0.3)
    c.close()
