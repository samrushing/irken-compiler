
is_a = isinstance
import Parsing

def make_map (l):
    m = {}
    for i in range (len (l)):
        m[l[i]] = i
    return m

def build_tables (s):
    tokens   = s._tokens.keys()
    nonterms = s._nonterms.keys()
    goto     = s._goto
    action   = s._action
    token_map = make_map (tokens)
    nt_map = make_map (nonterms)
    action2 = []
    for i in range (len (action)):
        d = {}
        for k,v in action[i].iteritems():
            # v is always a one-element list?  GLR-related?
            if is_a (v[0], Parsing.ShiftAction):
                v = -1, v[0].nextState
            elif is_a (v[0], Parsing.ReduceAction):
                p = v[0].production
                v = -2, (len(p.rhs), p.lhs.name)
            else:
                raise ValueError
            d[token_map[k.name]] = v
        action2.append (d)
    goto2 = []
    for i in range (len (goto)):
        d = {}
        for k,v in goto[i].iteritems():
            d[k.name] = v
        goto2.append (d)
    return goto2, action2, token_map, nt_map

def gen_irken (file, tables):
    W = file.write
    goto, actions, tm, ntm = tables
    items = tm.items()
    items.sort (lambda a,b: cmp (a[1],b[1]))
    W ('(define terminals #(\n')
    for item, index in items:
        W ('    \'%s\n' % (item,))
    W ('  ))\n')
    items = ntm.items()
    items.sort (lambda a,b: cmp (a[1],b[1]))
    W ('(define non-terminals #(\n')
    for item, index in items:
        W ('    \'%s\n' % (item,))
    W ('  ))\n')
    W ('(define actions\n')
    W ('  (let ()\n')
    W ('    (define C action-list:cons)\n')
    W ('    (define N action-list:nil)\n')
    W ('    (define S action:shift)\n')
    W ('    (define R action:reduce)\n')
    W ('    #(\n')
    for action in actions:
        l = '(N)'
        for k, v in action.items():
            shift_reduce, n = v
            if shift_reduce == -1:
                #W ('    (:action %d (:shift %d))\n' % (k, n,))
                #W ('    (action:shift %d %d)\n' % (k, n))
                l = '(C %d (S %d) %s)' % (k, n, l)
            else:
                plen, nt = n
                #W ('    (:action %d (:reduce %d %d))\n' % (k, plen, ntm[nt]))
                #W ('    (action:reduce %d %d %d)\n' % (k, plen, ntm[nt]))
                l = '(C %d (R %d %d) %s)' % (k, plen, ntm[nt], l)
        W ('      %s\n' % l)
    W ('   )))\n')
    W ('(define goto\n')
    W ('  (let ()\n')
    W ('    (define C goto-list:cons)\n')
    W ('    (define N goto-list:nil)\n')
    W ('    #(\n')
    for entry in goto:
        l = '(N)'
        items = entry.items()[:]
        items.reverse()
        for k, v in items:
            l = '(C %d %d %s)' % (ntm[k], v, l)
        W ('      %s\n' % l)
    W ('  )))\n')

if __name__ == '__main__':
    import sys
    base = sys.argv[1]
    exec ("import %s" % (base,))
    spec = eval ("%s.spec" % (base,))
    tables = build_tables (spec)
    file = open ('%s.scm' % (base,), 'wb')
    gen_irken (file, tables)
    
    

    
