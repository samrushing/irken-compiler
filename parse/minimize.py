# -*- Mode: Python -*-
#
# removed from automata.py - minimization doesn't do much for a lexer,
#  since you have to keep the final states separate in order to distinguish
#  the different kinds of tokens.
#
# Note: the trick used here seems to work, though.
#  Instead of starting your partition with final and non-final states,
#  divide the final set in a way that retains the distinction between
#  the sub-expressions (wrapped in TOKEN by lexer.py)

    def minimize (self, collapse_finals=True):
        # XXX using minimization for a lexer is probably not worth it.
        n_states = len (self.states)
        W ('self.finals=%r\n' % (self.finals,))
        final = self.finals

        t = {}
        for fs, ch, ts in self.dfa:
            t2 = t.get (fs, {})
            t2[ch] = ts
            t[fs] = t2

        W ('minimize - input dfa:\n')
        pp (t)
        W ('minimize - finals: %r\n' % (final,))

        # make sure final states exist
        for f in final:
            if not t.has_key (f):
                t[f] = {}

        # partition
        non_final = set (t.keys()).difference (final)

        if not non_final or not final:
            W ('warning: dfa is all-final or all-non-final\n')

        if collapse_finals:
            # standard: partition into final and non-final
            part = [final, non_final]
        else:
            # leave each final state distinguished
            # (useful for lexing)
            part = [[x] for x in final] + [non_final]
        last_size = len(part)

        W ('part=%r\n' % (part,))

        # churn
        while 1:

            # make state->partition map
            m = {}
            for i in range(len(part)):
                for s in part[i]:
                    m[s] = i

            W ('----------------------------------------\n')
            W ('part=%r\n' % (part,))

            new_part = []
            for sub in part:
                if len(sub) == 1:
                    # can't refine a one-element partition
                    new_part.append (sub)
                else:
                    refined = {}
                    # <refined> is a map from:
                    #    ((sym0, ts0), (sym1, ts1), ...) => [<fs0>, <fs1>, ...]
                    # where each <ts> is a to-state in the current
                    #   partition.
                    # each element of <refined> defines a new sub-partition
                    #   of the current partition.
                    for fs in sub:
                        key = {}
                        for sym, ts in t[fs].items():
                            ts2 = m[ts]
                            key[(sym, ts2)] = None
                        key = key.keys()
                        key.sort()
                        key = tuple(key)
                        val = refined.get (key, [])
                        val.append (fs)
                        refined[key] = val
                    #W ('sub %r refined to\n' % (sub,))
                    #pp (refined)
                    # now we can extra the unique sub-sets of <sub>
                    # out of <refined>
                    r2 = set()
                    for sub in refined.values():
                        r2.add (tuple(sub))
                    # extend the new partition scheme with this newly
                    # newly refined set.
                    new_part.extend ([list(x) for x in r2])

            #W ('new_part=%r\n' % (part,))

            if len(new_part) == len(part):
                break
            else:
                part = new_part

        # <part> now holds our final partitioning of DFA states.
        # collapse those that are equivalent

        #W ('final partition: %r\n' % (part,))

        # we sort the new partition so that state zero (initial) is in front...
        for p in part:
            p.sort()
        part.sort()

        # make old_state->new_state map
        m = {}
        for i in range(len(part)):
            for s in part[i]:
                m[s] = i

        # translate dfa
        old_dfa = t
        new_dfa = []

        for i in range(len(part)):
            # pick the first of the equiv set
            if len(part[i]):
                equiv = part[i][0]
                old_trans = old_dfa[equiv]
                new_trans = []
                for (sym, ts) in old_trans.items():
                    new_trans.append ((sym, m[ts]))
                new_dfa.append (new_trans)
        # translate final states
        new_finals = set()
        for f in final:
            new_finals.add (m[f])

        # ugh, what a bitch that was!
        return new_dfa, new_finals
