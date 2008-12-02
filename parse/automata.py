# -*- Mode: Python -*-

import sys
W = sys.stdout.write
from pprint import pprint as pp

import charset
import regular

is_a = isinstance

class regexp_to_nfa_converter:

    def __init__ (self):
        self.counter = 0
        self.tokens = []

    def next_state (self):
        result = self.counter
        self.counter += 1
        return result

    # ((<from>, (<char>, <to>), ...), <start>, <end>)

    def walk_exp (self, m):
        "walk a tree-regexp creating an NFA"
        if is_a (m, charset.charset):
            s = self.next_state()
            e = self.next_state()
            return [(s, (m, e))], s, e
        else:
            op = m[0]
            if op is regular.CONCAT:
                (a_trans, a_start, a_end) = self.walk_exp (m[1])
                (b_trans, b_start, b_end) = self.walk_exp (m[2])
                # one new transition
                trans =  a_trans + b_trans + [(a_end, (None, b_start))]
                return trans, a_start, b_end
            elif op in (regular.STAR, regular.PLUS):
                e_trans, e_start, e_end = self.walk_exp (m[1])
                new_start = self.next_state()
                new_end   = self.next_state()
                # four new transitions
                trans = e_trans + [
                    (e_end, (None, e_start)),
                    (new_start, (None, e_start)),
                    (e_end, (None, new_end)),
                    ]
                if op is regular.STAR:
                    trans.append ((new_start, (None, new_end)))
                return trans, new_start, new_end
            elif op is regular.OR:
                (a_trans, a_start, a_end) = self.walk_exp (m[1])
                (b_trans, b_start, b_end) = self.walk_exp (m[2])
                new_start = self.next_state()
                new_end   = self.next_state()
                # four new transitions
                trans = a_trans + b_trans + [
                    (new_start, (None, a_start)),
                    (new_start, (None, b_start)),
                    (a_end, (None, new_end)),
                    (b_end, (None, new_end))
                    ]
                return trans, new_start, new_end
            elif op is regular.OPTIONAL:
                e_trans, e_start, e_end = self.walk_exp (m[1])
                # add e_start to e_end (if it ain't already)
                if e_start not in e_end:
                    e_end = e_end + [e_start]
                return e_trans, e_start, e_end
            elif op is regular.TOKEN:
                trans, start, end = self.walk_exp (m[1])
                self.tokens.append ((end, m[2]))
                return trans, start, end
            else:
                raise ValueError, "unknown operator '%s'" % repr(op)

    def coalesce (self, m):
        "group transitions by <from-state>"
        r = [ [] for x in range (self.counter) ]
        tl, start, end = m
        tl.sort()
        for fs, trans in tl:
            r[fs].append (trans)
        return r, start, end

    def go (self, exp):
        return self.coalesce (self.walk_exp (exp))

def regexp_to_nfa (exp):
    parsed = parse_regexp (exp)
    return regexp_to_nfa_converter().go (parsed)

def find_cutpoints (sets):
    "find all the cutpoints (i.e., partition ranges) in <sets>"
    cuts = {0:None, 256:None}
    on = 0
    for set in sets:
        for i in range (0, 256):
            if on:
                if not set.set[i]:
                    on = 0
                    cuts[i] = None
            else:
                if set.set[i]:
                    on = 1
                    cuts[i] = None
    cuts = cuts.keys()
    cuts.sort()
    #W ("cutpoints (%r) => %r\n" % (sets, cuts))
    return cuts

def compact_chr (n):
    "helper utility for printing characters"
    ch = chr(n)
    if len(repr(ch)) == 3:
        return ch
    else:
        return repr(ch)[1:-1]

def make_partition (sets):
    "find the unique disjoint sets that cover <sets>"
    cuts = find_cutpoints (sets)
    result = []
    for i in range (len (cuts)-1):
        # make a charset for this range
        set = [0] * 256
        start = cuts[i]
        end = cuts[i+1]-1
        count = 0
        for j in range (start, end+1):
            set[j] = 1
            count += 1
        if count == 1:
            result.append (charset.make_single_charset(chr(start)))
        else:
            result.append (charset.make_charset (set))
    if (len(result) > 1) and (0xff not in cuts):
        result[0] = result[0] + result[-1]
        result.pop (-1)
    #pp (result)
    return result

class nfa_dfa_converter:

    def __init__ (self, nfa, start, end):
        self.nfa = nfa
        self.start = start
        self.end = end
        self.initial = self.epsilon_closure (start)
        self.states = [self.initial]
        self.dfa = []

    def moves (self, state, symbol):
        "all from <state> with <symbol>"
        r = set()
        for sym, to_state in self.nfa[state]:
            if symbol is None and sym is None:
                r.add (to_state)
            elif symbol and symbol.overlap (sym):
                r.add (to_state)
        return r

    def epsilon_closure (self, state):
        return self._epsilon_closure (state, set ([state]))

    def _epsilon_closure (self, state, visited):
        epsilon_moves = self.moves (state, None)
        adjoined = visited.union (epsilon_moves)
        if len(visited) == len(adjoined):
            return visited
        else:
            r = set()
            for s in adjoined.difference (visited):
                r = r.union (self._epsilon_closure (s, adjoined))
            return r

    def symbol_closure (self, state, symbol):
        one = self.epsilon_closure (state)
        two = set()
        for s in one:
            two = two.union (self.moves (s, symbol))
        r = set()
        for s in two:
            r = r.union (self.epsilon_closure (s))
        return r

    def set_symbol_closure (self, s, symbol):
        r = set()
        for x in s:
            r = r.union (self.symbol_closure (x, symbol))
        return r

    def find_end_states (self, tokens=()):
        "find those members of <state_set> that contain <end_state>"
        state_sets = self.states
        end_state = self.end
        r = []
        actions = []
        for i in range(len(state_sets)):
            s = state_sets[i]
            if end_state in s:
                for token_state, action in tokens:
                    if token_state in s:
                        actions.append ((i, action))
                r.append (i)
        return r, actions

    def add_transition (self, from_index, sym, to_index):
        # can we merge this <sym> with a pre-existing transition?
        for i in range (len (self.dfa)):
            fs2, sym2, ts2 = self.dfa[i]
            if (fs2, ts2) == (from_index, to_index):
                # equivalent, so merge the transition
                self.dfa[i] = fs2, sym2 + sym, ts2
                break
        else:
            # no, this is a new transition
            self.dfa.append ((from_index, sym, to_index))

    def walk (self, index, s):
        # collect all starting non-epsilon moves
        moves = set()
        for state in s:
            for sym, to_state in self.nfa[state]:
                if is_a (sym, charset.charset):
                    moves.add (sym)
        # use <moves> to partition the full character set
        moves = make_partition (moves)
        for sym in moves:
            closure = self.set_symbol_closure (s, sym)
            # have we seen this superstate yet?
            if closure in self.states:
                # yes, try to merge with another transition
                self.add_transition (index, sym, self.states.index (closure))
            else:
                # it's a new superstate
                new_index = len(self.states)
                self.states.append (closure)
                # so walk it
                self.walk (new_index, closure)
                self.dfa.append ((index, sym, new_index))

    def coalesce (self):
        "group transitions by <from-state>"
        # [(0,x,1),(0,y,2),(1,a,3)...] => [[(x,1),(y,2)],[(a,3),...]]
        r = [ [] for x in range (len (self.states)) ]
        for fs, sym, ts in self.dfa:
            r[fs].append ((sym, ts))
        return r

    def go (self, tokens=()):
        self.walk (0, self.initial)
        self.dfa.sort()
        self.finals, self.actions = self.find_end_states (tokens)
        return self.dfa, self.finals, self.actions

def nfa_to_dfa (m, s, e):
    c = nfa_dfa_converter(m, s, e)
    return c.go()
    #return c.minimize()

def regexp_to_dfa (exp):
    m, s, e = regexp_to_nfa (exp)
    return nfa_to_dfa (m, s, e)

if __name__ == '__main__':
    import pprint
    import sys
    while 1:
        sys.stderr.write ('Enter a simple regexp (e.g. "a(b|c)+d*"): ')
        line = sys.stdin.readline()
        if not line:
            sys.stderr.write ('\n')
            break
        else:
            exp = line[:-1]
            parsed = regular.parse (exp)
            sys.stderr.write ('Parsed: %r\n' % (parsed,))
            m, s, e = regexp_to_nfa_converter().go (parsed)
            sys.stderr.write ('NFA:\n')
            pprint.pprint (m)
            sys.stderr.write ('start: %r end: %r\n' % (s, e))
            dfa, finals, actions = nfa_to_dfa (m, s, e)
            sys.stderr.write ('DFA:\n')
            pprint.pprint (dfa)
            sys.stderr.write ('final states: %r\n' % (finals,))
            sys.stderr.write ('-----------------------------\n')
