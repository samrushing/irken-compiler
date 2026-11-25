# doom/resolver

A recursive DNS resolver for doom. It starts from the root servers, follows
referrals, and caches what it learns. It doesn't use the system's resolver
or anyone else's recursive server.

    (let ((r (dns/resolver (dns/config))))
      (r.resolve "example.com" (dnstype:mx)))
    ;; -> {status answers cnames trace}

`status` is `:answer`, `:nodata`, `:nxdomain` or `(:failed why)`. `trace`
records each server that was asked, what it said, and why the resolver
went on, ignored the reply, or gave up. getaddrinfo() throws that
information away, and a mail server (or anyone diagnosing one) needs it.

## Provenance

The design comes from the resolver Sam Rushing wrote for shrapnel at
IronPort (`coro/dns/cache.py`, https://github.com/ironport/shrapnel). That
resolver was based on Dan Bernstein's dnscache (djbdns), "with full
paranoia enabled", and was used in production mail servers. Its main
features carry over:

- djb's classification of replies: answer, CNAME, NXDOMAIN, NODATA,
  referral, or lame;
- bailiwick checks on glue and referrals, so a server can only tell us
  about names it is responsible for;
- loop, depth and query-count limits, so a hostile or broken zone can't
  make the resolver run forever;
- the resolver doesn't trust anything it didn't ask for.

Things added since shrapnel, for DNS as it is in 2026:

- EDNS0, with a 1232-byte buffer (the DNS Flag Day 2020 value), and a retry
  without it on FORMERR, NOTIMP or BADVERS;
- TCP when a reply is truncated;
- checking that a reply echoes our question;
- optional 0x20 case randomization;
- skipping replies with the wrong id;
- negative caching from the SOA (RFC 2308).

This Irken version was written in November 2026 by Claude (Anthropic's
Claude Opus 5.5), working with Sam, who supplied the original code, the
design, and the testing on real networks.

## Files

    doom/resolver/resolver.scm      the resolver and its cache
    lib/net/dns.scm                 packing and unpacking DNS messages
    doom/timeout.scm                waiting for a socket, with a timeout
    demo/dns/resolve.scm            command line: resolve [-t type] [-v] name
    doom/test/resolver/run.sh       tests against fake_world.py (needs dnslib)

## Not yet

- merging identical queries that are in flight at the same time;
- remembering dead servers;
- root priming;
- IPv6 transport;
- DNSSEC validation (it would roughly double the code; needed for DANE).
