#!/bin/sh
# -*- Mode: sh -*-

# test doom/resolver (t_resolver.scm) against fake_world.py: a small
#  fake internet of misbehaving authoritative servers on 127.0.0.1.
#
#   $ doom/test/resolver/run.sh        (from the top of the source tree)
#
# needs python3 with dnslib (pip install dnslib).  the fake servers use
#  ports $DNS_TEST_BASE+1 to +16 (default 5300), UDP and TCP.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../../.." && pwd)
prog="$here/t_resolver"
base=${DNS_TEST_BASE:-5300}
work=$(mktemp -d)

cleanup () {
    if [ -n "$world" ]; then
        kill $world 2>/dev/null
        wait $world 2>/dev/null
    fi
    rm -rf "$work"
}
trap cleanup EXIT

python3 -c "import dnslib" 2>/dev/null || { echo "needs dnslib: pip install dnslib"; exit 1; }

if [ ! -x "$prog" ]; then
    (cd "$top" && IRKENLIB=. self/irken doom/test/resolver/t_resolver.scm -q) || exit 1
fi

python3 "$here/fake_world.py" $base > "$work/world.log" 2>&1 &
world=$!
for i in 1 2 3 4 5; do
    grep -q ready "$work/world.log" 2>/dev/null && break
    sleep 1
done

"$prog" $base
status=$?
if [ $status -ne 0 ]; then
    echo "--- the fake world's log"
    cat "$work/world.log"
fi
exit $status
