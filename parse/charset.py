# -*- Mode: Python -*-

import operator

special_chars = {'r':'\r', 'n':'\n', 't':'\t'}

is_a = isinstance

def compact_chr (n):
    "helper utility for printing characters"
    ch = chr(n)
    if len(repr(ch)) == 3:
        return ch
    else:
        return repr(ch)[1:-1]

class charset:

    def __init__ (self, set, bits=8):
        self.bits = bits
        self.max_char = 2 ** self.bits
        assert (len(set) == self.max_char)
        self.set = tuple (set)
        self.num_set = reduce (operator.add, set)
        self.repr_cache = None

    def as_string (self):
        return ''.join ([str(x) for x in self.set])

    def as_ranges (self):
        "describe a charset compactly as a set of ranges"
        count = reduce (operator.add, self.set)
        ranges = []
        if count == self.max_char:
            # it's the whole set, aka DOT
            return 0, [(0, 255)]
        elif count > (self.max_char / 2):
            # more set than not set, print as an inverse
            invert = 1
        else:
            invert = 0
        on = invert ^ self.set[0]
        if on:
            start = 0
        for i in range (1,self.max_char + 1):
            if i == self.max_char:
                # pretend there's an 'off' bit at the end
                this_bit_on = 0
            else:
                this_bit_on = invert ^ self.set[i]
            if on:
                # still on?
                if this_bit_on:
                    # yup
                    pass
                else:
                    ranges.append ((start, i-1))
                    on = 0
            else:
                # still off?
                if this_bit_on:
                    # nope
                    start = i
                    on = 1
                else:
                    # yup
                    pass
        return invert, ranges

    def compute_repr (self):
        "render a character set, in the expected manner"
        invert, ranges = self.as_ranges()
        if ranges == [(0,255)]:
            return '.'
        if invert:
            r = ["[^"]
        else:
            r = ["["]
        for start, end in ranges:
            if start == end:
                # one character
                r.append (compact_chr (start))
            elif start + 1 == end:
                # two characters
                r.append (compact_chr (start))
                r.append (compact_chr (end))
            else:
                # an actual 'range'
                r.append ('%s-%s' % (compact_chr (start), compact_chr (end)))
        #count = reduce (operator.add, self.set)
        #r.append ('].%d' % (count,))
        r.append (']')
        return ''.join (r)

    def __repr__ (self):
        if self.repr_cache is None:
            self.repr_cache = self.compute_repr()
        #if self.bits != 8:
        #    return '%d' % (self.bits,) + self.repr_cache
        #else:
        return self.repr_cache

    def __hash__ (self):
        return hash (self.set)

    def __cmp__ (self, other):
        if isinstance (other, charset):
            return cmp (self.set, other.set)
        else:
            return 1

    def __getitem__ (self, index):
        return self.set[index]

    def has (self, ch):
        return self.set[ord(ch)]

    def __add__ (self, other):
        assert isinstance (other, charset)
        new_set = [ (self.set[i] or other.set[i]) for i in range (self.max_char)]
        return make_charset (new_set, self.bits)

    def overlap (self, other):
        "is the union of <self> and <other> non-empty?"
        #W ('overlap (%r, %r)\n' % (self, other))
        if other is self:
            return 1
        elif isinstance (other, charset):
            for i in range (self.max_char):
                if other.set[i] and self.set[i]:
                    return 1
            return 0
        else:
            return 0

    def insensitive (self):
        "return a case-insensitive version of this charset"
        import string
        set = list(self.set[:])
        # trying not to make too many ascii assumptions.
        # as a pre-processing step, an insensitive regexp is
        # downcased - so we need only copy the status of the lowercase
        # letters to the uppercase letters.
        for i in range (len (string.lowercase)):
            ch_lo = ord(string.lowercase[i])
            ch_hi = ord(string.uppercase[i])
            set[ch_hi] = set[ch_lo]
        return make_charset (set, self.bits)

cache = {}

def make_charset (set, bits=8):
    "make a charset, using the cache if possible"
    set = tuple (set)
    if not cache.has_key (set):
        cache[set] = charset (set, bits)
    return cache[set]

def make_single_charset (ch, bits=8):
    "make a charset consisting of a single character"
    assert (type(ch) is type('') and len(ch) == 1)
    set = [0] * (2 ** bits)
    set[ord(ch)] = 1
    return make_charset (set, bits)

def parse_charset (s, pos=1):
    "parse a charset definition (e.g., '[A-Za-z0-9]' or '[^0-9])"
    set = [0] * 256
    invert = 0
    if s[pos:pos+1] == '^':
        # negate
        invert = 1
        pos += 1
    elif s[pos:pos+1] == '-':
        # set the minus character
        set[ord(s[pos:pos+1])] = 1
        pos += 1
    while s[pos] != ']':
        if s[pos] == '-':
            start = ord(s[pos-1])
            end   = ord(s[pos+1])
            assert (start < end)
            for i in range(start,end+1):
                set[i] = 1
            pos += 2
        elif s[pos] == '\\':
            # escape
            ch = s[pos+1]
            ch = special_chars.get (ch, ch)
            set[ord(ch)] = 1
            pos += 2
        else:
            set[ord(s[pos])] = 1
            pos += 1
    if invert:
        return make_charset ([ not x for x in set ]), pos
    else:
        return make_charset (set), pos

DOT = make_charset ([1]*256)
