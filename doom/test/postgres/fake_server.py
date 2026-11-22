# -*- Mode: Python -*-

# a server that misbehaves at the SSLRequest, for t_postgres (see run.sh):
#  the first connection is told 'N' (no TLS), the second 'S' followed by
#  plain text, which the client must not take as coming over TLS
#  (cf. CVE-2021-23222).
#
#   python3 fake_server.py <port>

import socket
import sys
import time

l = socket.socket()
l.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
l.bind(('127.0.0.1', int(sys.argv[1])))
l.listen()
print('ready', flush=True)
for answer in [b'N', b'S' + b'R\x00\x00\x00\x08\x00\x00\x00\x00']:
    c, _ = l.accept()
    c.recv(8)
    c.sendall(answer)
    time.sleep(0.5)
    c.close()
