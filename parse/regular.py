# -*- Mode: Python -*-

import charset

# this is so they print out pretty
class operator:
    def __init__ (self, s):
        self.s = s
    def __repr__ (self):
        return self.s

# operators
CONCAT   = operator ('+')
STAR     = operator ('*')
OR       = operator ('|')
PLUS     = operator ('+')
OPTIONAL = operator ('?')

# token, used by lexer
TOKEN    = operator ('TOKEN')

# (ab|ba)*
# (star (or (concat "a" "b") (concat "b" "a")))

r1 = '(ab|ba)*'
r2 = '(\r\n.\r\n)|(\n.\n)|(\r.\r)'
r3 = 'a+(.a+)+$'

def pprint_regexp (exp):
    if type(exp) == type(''):
        return exp
    else:
        op = exp[0]
        args = map (pprint_regexp, exp[1:])
        if op is CONCAT:
            return ''.join (args)
        elif op is STAR:
            return '(' + args[0] + ')*'
        elif op is OR:
            return '(' + '|'.join (args) + ')'
        else:
            raise ValueError, 'Unknown operator "%s"' % op

def nary_op (l, op):
    "apply binary OP to the <n> expressions of <l>"
    if len(l) == 1:
        return l[0]
    elif len(l) == 2:
        return (op, l[0], l[1])
    else:
        return (op, l[0], nary_op (l[1:], op))

def n_concat (l):
    "CONCAT the <n> expressions of <l> using only the binary operator"
    return nary_op (l, CONCAT)

def n_or (l):
    return nary_op (l, OR)

def parse (s, pos=0):
    """parse a regular expression into tree format."""
    exp, pos = _parse(s,pos)
    return exp

# abc+def
# parse a series of atoms, each to be CONCAT'd together

def _parse (s, pos=0):
    exp = []
    while 1:
        if pos >= len(s):
            return n_concat (exp), pos
        else:
            ch = s[pos]
            if ch == '(':
                sub, pos = _parse (s, pos+1)
                exp.append (sub)
            elif ch == ')':
                return n_concat (exp), pos
            elif ch == '*':
                # apply STAR to the last expression
                exp[-1] = (STAR, exp[-1])
            elif ch == '+':
                # apply PLUS to the last expression
                exp[-1] = (PLUS, exp[-1])
            elif ch == '|':
                # apply OR to the left side vs. whatever follows
                # note: this is more 'greedy' than +,?, or *
                sub, pos = _parse (s, pos+1)
                return (OR, n_concat (exp), sub), pos
            elif ch == '?':
                # apply OPTIONAL to the last expression
                exp[-1] = (OPTIONAL, exp[-1])
            elif ch == '[':
                # charset
                set, pos = charset.parse_charset (s, pos + 1)
                exp.append (set)
            elif ch == '\\':
                # literal character
                pos += 1
                ch = s[pos]
                ch = charset.special_chars.get (ch, ch)
                exp.append (charset.make_single_charset (ch))
            elif ch == '.':
                # dot
                exp.append (charset.DOT)
            else:
                exp.append (charset.make_single_charset (ch))
            pos += 1

if __name__ == '__main__':
    #print parse ('([A-Za-z][A-Za-z0-9_]*')
    #print parse (r'"([^\\\n"]|(\\.))*"')
    augassign = '|'.join (r'\+= -= \*= /= %= &= \|= \^= <<= >>= \*\*= //='.split())
    augassign = ('(' + '|'.join (r'\+ - \* / % & \| \^ << >> \*\* //'.split()) + ')=')
    print parse (augassign)
