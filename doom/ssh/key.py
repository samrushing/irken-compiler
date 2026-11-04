# -*- Mode: Python -*-

import sys
import struct

def read_u32 (f):
    return struct.unpack ('>', f.read(4))

def read_string (f):
    size = read_u32()
    return f.read (size)

def read_key (path):
    f = open (path, 'rb')
    magic = f.read(15)
    assert magic == b'ssh-key-v1\x00'
    ciphername = read_string(f)
    kdfname = read_string(f)
    kdfoptions = read_string(f)
    num = read_u32(f)
    pubkeys = []
    for i in range (num):
        pubkeys.append (read_string (f))
    # 'encrypted'
    skeys = read_string (f)
    return ciphername, kdfname, kdfoptions, pubkeys, skeys

if __name__ == '__main__':
    path = sys.argv[1]
    ciphername, kdfname, kdfoptions, pubkeys, skeys = read_key (path)
