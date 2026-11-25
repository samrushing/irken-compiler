# -*- Mode: Python -*-

# a small fake internet for testing doom/resolver: authoritative
#  servers on 127.0.0.1, server 192.0.2.N listening on port <base>+N
#  (UDP and TCP).  t_resolver maps the addresses the same way.
#
#   python3 fake_world.py <base>
#
# the root (192.0.2.1) delegates test. to 192.0.2.2, which delegates
#  zones that each test something: see WORLD.  needs dnslib.

import socket
import socketserver
import struct
import sys
import threading

from dnslib import DNSRecord, RR, QTYPE, RCODE, DNSHeader, DNSQuestion

BASE = int(sys.argv[1])

def rrs(text):
    return RR.fromZone(text)

SOA_TEST = "test. 3600 IN SOA ns1.nic.test. hostmaster.test. 1 3600 600 86400 300\n"
SOA_EXAMPLE = "example.test. 3600 IN SOA ns1.example.test. hostmaster.example.test. 1 3600 600 86400 300\n"

ROOT = """
. 86400 IN SOA a.root.test. hostmaster.test. 1 1800 900 604800 86400
. 518400 IN NS a.root.test.
test. 172800 IN NS ns1.nic.test.
ns1.nic.test. 172800 IN A 192.0.2.2
"""

TEST = SOA_TEST + """
test. 3600 IN NS ns1.nic.test.
ns1.nic.test. 3600 IN A 192.0.2.2
example.test. 3600 IN NS ns1.example.test.
example.test. 3600 IN NS ns2.example.test.
ns1.example.test. 3600 IN A 192.0.2.3
ns2.example.test. 3600 IN A 192.0.2.4
glueless.test. 3600 IN NS ns.helper.example.test.
lame.test. 3600 IN NS ns-a.lame.test.
lame.test. 3600 IN NS ns-b.lame.test.
ns-a.lame.test. 3600 IN A 192.0.2.6
ns-b.lame.test. 3600 IN A 192.0.2.7
slow.test. 3600 IN NS ns-a.slow.test.
slow.test. 3600 IN NS ns-b.slow.test.
ns-a.slow.test. 3600 IN A 192.0.2.8
ns-b.slow.test. 3600 IN A 192.0.2.9
dead.test. 3600 IN NS ns.dead.test.
ns.dead.test. 3600 IN A 192.0.2.8
cycle1.test. 3600 IN NS ns.cycle2.test.
cycle2.test. 3600 IN NS ns.cycle1.test.
big.test. 3600 IN NS ns.big.test.
ns.big.test. 3600 IN A 192.0.2.10
noedns.test. 3600 IN NS ns.noedns.test.
ns.noedns.test. 3600 IN A 192.0.2.11
poison.test. 3600 IN NS ns.poison.test.
ns.poison.test. 3600 IN A 192.0.2.12
spoof.test. 3600 IN NS ns.spoof.test.
ns.spoof.test. 3600 IN A 192.0.2.13
wrongq.test. 3600 IN NS ns-a.wrongq.test.
wrongq.test. 3600 IN NS ns-b.wrongq.test.
ns-a.wrongq.test. 3600 IN A 192.0.2.14
ns-b.wrongq.test. 3600 IN A 192.0.2.9
nocase.test. 3600 IN NS ns.nocase.test.
ns.nocase.test. 3600 IN A 192.0.2.15
refused.test. 3600 IN NS ns.refused.test.
ns.refused.test. 3600 IN A 192.0.2.16
"""

EXAMPLE = SOA_EXAMPLE + """
example.test. 3600 IN NS ns1.example.test.
example.test. 3600 IN NS ns2.example.test.
ns1.example.test. 3600 IN A 192.0.2.3
ns2.example.test. 3600 IN A 192.0.2.4
www.example.test. 300 IN A 192.0.2.80
www.example.test. 300 IN A 192.0.2.180
alias.example.test. 300 IN CNAME www.example.test.
out.example.test. 300 IN CNAME www.glueless.test.
loop1.example.test. 300 IN CNAME loop2.example.test.
loop2.example.test. 300 IN CNAME loop1.example.test.
txtonly.example.test. 300 IN TXT "just text"
mx.example.test. 300 IN MX 10 mail.example.test.
ns.helper.example.test. 300 IN A 192.0.2.5
unknown.example.test. 300 IN TYPE65280 \\# 4 0a0b0c0d
""" + "".join("c%d.example.test. 300 IN CNAME c%d.example.test.\n" % (i, i + 1) for i in range(1, 12)) + \
    "c12.example.test. 300 IN A 192.0.2.99\n"

def simple(zone, body):
    soa = "%s 3600 IN SOA ns.%s hostmaster.%s 1 3600 600 86400 300\n" % (zone, zone, zone)
    return soa + body

GLUELESS = simple("glueless.test.", "glueless.test. 3600 IN NS ns.helper.example.test.\nwww.glueless.test. 300 IN A 192.0.2.81\n")
LAME_GOOD = simple("lame.test.", "www.lame.test. 300 IN A 192.0.2.82\n")
SLOW_GOOD = simple("slow.test.", "www.slow.test. 300 IN A 192.0.2.83\n")
WRONGQ_GOOD = simple("wrongq.test.", "www.wrongq.test. 300 IN A 192.0.2.86\n")
BIG = simple("big.test.", "www.big.test. 300 IN A 192.0.2.84\n" +
             "".join('www.big.test. 300 IN TXT "%s"\n' % ("%02d" % i * 100) for i in range(20)))
NOEDNS = simple("noedns.test.", "www.noedns.test. 300 IN A 192.0.2.85\n")
POISON = simple("poison.test.", "x.poison.test. 300 IN A 192.0.2.87\n"
                "sub.poison.test. 300 IN NS ns1.example.test.\n")
SPOOF = simple("spoof.test.", "www.spoof.test. 300 IN A 192.0.2.88\n")
NOCASE = simple("nocase.test.", "www.nocase.test. 300 IN A 192.0.2.89\n")

# server number -> (zone origins and records, behaviour)
WORLD = {
    1:  ([".", ROOT], None),
    2:  (["test.", TEST], None),
    3:  (["example.test.", EXAMPLE], None),
    4:  (["example.test.", EXAMPLE], None),
    5:  (["glueless.test.", GLUELESS], None),
    6:  (["lame.test.", ""], "lame"),           # refers lame.test back up to test.
    7:  (["lame.test.", LAME_GOOD], None),
    8:  ([], "silent"),                         # never answers
    9:  (["slow.test.", SLOW_GOOD, "wrongq.test.", WRONGQ_GOOD], None),
    10: (["big.test.", BIG], "truncate"),       # UDP: always TC
    11: (["noedns.test.", NOEDNS], "noedns"),   # FORMERR to an OPT
    12: (["poison.test.", POISON], "poison"),   # extra records it has no business sending
    13: (["spoof.test.", SPOOF], "spoof"),      # a wrong-id reply first
    14: (["wrongq.test.", WRONGQ_GOOD], "wrongq"),   # answers another question
    15: (["nocase.test.", NOCASE], "nocase"),   # doesn't preserve the case of the name
    16: ([], "refused"),
}

def norm(name):
    return str(name).lower().rstrip('.') + '.'

def under(name, zone):
    return zone == '.' or name == zone or name.endswith('.' + zone)

class Server:
    def __init__(self, n, spec, behaviour):
        self.n = n
        self.behaviour = behaviour
        self.zones = {}
        for i in range(0, len(spec), 2):
            self.zones[norm(spec[i])] = rrs(spec[i + 1]) if spec[i + 1] else []

    def answer(self, data, tcp):
        q = DNSRecord.parse(data)
        b = self.behaviour
        if b == 'silent':
            return []
        r = q.reply()
        r.header.ra = 0
        r.header.aa = 0     # (only answers from the zone itself are authoritative)
        if b == 'refused':
            r.header.rcode = RCODE.REFUSED
            return [r.pack()]
        has_opt = any(rr.rtype == QTYPE.OPT for rr in q.ar)
        if b == 'noedns' and has_opt:
            r = DNSRecord(DNSHeader(id=q.header.id, qr=1, rcode=RCODE.FORMERR), q=q.q)
            return [r.pack()]
        if b == 'truncate' and not tcp:
            r.header.tc = 1
            return [r.pack()]
        qname = norm(q.q.qname)
        qtype = q.q.qtype
        if b == 'lame':
            r.add_auth(*rrs("test. 3600 IN NS ns1.nic.test."))
            r.add_ar(*rrs("ns1.nic.test. 3600 IN A 192.0.2.2"))
            return [r.pack()]
        self.fill(r, qname, qtype)
        if b == 'nocase':
            r.q.qname = qname.rstrip('.')
        if b == 'wrongq':
            r = DNSRecord(DNSHeader(id=q.header.id, qr=1, aa=1),
                          q=DNSQuestion("other.wrongq.test.", qtype))
            r.add_answer(*rrs("other.wrongq.test. 300 IN A 6.6.6.6"))
        if b == 'poison':
            r.add_answer(*rrs("www.example.test. 300 IN A 6.6.6.6"))
            r.add_ar(*rrs("ns1.example.test. 300 IN A 6.6.6.6"))
            r.add_ar(*rrs("www.example.test. 300 IN A 6.6.6.6"))
        out = [r.pack()]
        if b == 'spoof' and not tcp:
            fake = DNSRecord(DNSHeader(id=(q.header.id + 1) % 65536, qr=1, aa=1), q=q.q)
            fake.add_answer(*rrs("www.spoof.test. 300 IN A 6.6.6.6"))
            out = [fake.pack()] + out
        return out

    def fill(self, r, qname, qtype):
        zone = None
        for z in self.zones:
            if under(qname, z) and (zone is None or len(z) > len(zone)):
                zone = z
        if zone is None:
            r.header.rcode = RCODE.REFUSED
            return
        records = self.zones[zone]
        # a delegation between the zone and the name?
        cut = None
        for rr in records:
            owner = norm(rr.rname)
            if rr.rtype == QTYPE.NS and owner != zone and under(qname, owner):
                if cut is None or len(owner) > len(cut):
                    cut = owner
        if cut:
            for rr in records:
                if rr.rtype == QTYPE.NS and norm(rr.rname) == cut:
                    r.add_auth(rr)
                    for g in records:
                        if g.rtype == QTYPE.A and norm(g.rname) == norm(rr.rdata):
                            r.add_ar(g)
            return
        r.header.aa = 1
        here = [rr for rr in records if norm(rr.rname) == qname]
        cnames = [rr for rr in here if rr.rtype == QTYPE.CNAME]
        if cnames and qtype != QTYPE.CNAME:
            r.add_answer(*cnames)
            return
        matching = [rr for rr in here if rr.rtype == qtype]
        soa = [rr for rr in records if rr.rtype == QTYPE.SOA]
        if matching:
            r.add_answer(*matching)
        elif here or any(under(norm(rr.rname), qname) for rr in records):
            r.add_auth(*soa)
        else:
            r.header.rcode = RCODE.NXDOMAIN
            r.add_auth(*soa)

servers = {n: Server(n, spec, b) for n, (spec, b) in WORLD.items()}

def make_udp(server):
    class H(socketserver.BaseRequestHandler):
        def handle(self):
            data, sock = self.request
            try:
                for out in server.answer(data, False):
                    sock.sendto(out, self.client_address)
            except Exception as e:
                print('udp', server.n, e, flush=True)
    return H

def make_tcp(server):
    class H(socketserver.BaseRequestHandler):
        def handle(self):
            try:
                n = struct.unpack('>H', self.request.recv(2))[0]
                data = b''
                while len(data) < n:
                    data += self.request.recv(n - len(data))
                for out in server.answer(data, True):
                    self.request.sendall(struct.pack('>H', len(out)) + out)
            except Exception as e:
                print('tcp', server.n, e, flush=True)
    return H

class UDP(socketserver.ThreadingUDPServer):
    allow_reuse_address = True

class TCP(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True

for n, server in servers.items():
    for cls, handler in ((UDP, make_udp(server)), (TCP, make_tcp(server))):
        s = cls(('127.0.0.1', BASE + n), handler)
        threading.Thread(target=s.serve_forever, daemon=True).start()

print('ready', flush=True)
threading.Event().wait()
