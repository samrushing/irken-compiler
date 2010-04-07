# -*- Mode: Python -*-

#from pprint import pprint as pp

def build_dependency_graph (root):
    g = {}
    def search (exp, current_fun):
        if exp.is_a ('varref'):
            current_fun.add (exp.params)
        elif exp.is_a ('fix'):
            for i in range (len (exp.names)):
                name = exp.names[i].name
                init = exp.inits[i]
                fun = set()
                g[name] = fun
                search (init, fun)
            search (exp.body, current_fun)
        else:
            for sub in exp.subs:
                search (sub, current_fun)
    g['top'] = set()
    search (root, g['top'])
    #from pprint import pprint as pp
    #pp (g)
    #raw_input()
    return g

def transpose (g):
    gt = {}
    for k in g.keys():
        gt[k] = set()
    for k, vl in g.items():
        for v in vl:
            if gt.has_key (v):
                gt[v].add (k)
            else:
                gt[v] = set ([k])
    return gt

# http://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
#
# Finds the strongly-connected components of the graph.  We need this to find
# out how a pedantic programmer might have grouped a set of functions carefully
# into letrecs, so that we can isolate such groups - otherwise they're all typed
# together as a single letrec.  That causes polymorphic instantiation to fail in
# many cases, because HM disallows polymorphism in recursive functions.  [yes,
# it's hard to explain]

def strongly (g):
    s = []
    visited = set()
    unknown = set()

    def visit0 (u):
        visited.add (u)
        if g.has_key (u):
            for v in g[u]:
                if v not in visited:
                    visit0 (v)
        else:
            unknown.add (u)
        s.append (u)

    # walk the graph forward, pushing finished nodes onto <s>
    for u in g.keys():
        if u not in visited:
            visit0 (u)
        
    gt = transpose (g)
    visited = set()

    def visit1 (u):
        visited.add (u)
        for v in gt[u]:
            if v not in visited:
                visit1 (v)
        r1.add (u)

    # walk backward, popping strongly connected components off <s>
    r0 = []
    while s:
        u = s.pop()
        if u not in visited:
            r1 = set()
            visit1 (u)
            # a strongly-connected component, collect it.
            r0.append (r1)

    # I think this puts the subcomponents in topological order.
    r0.reverse()
    # make a handy map from vertex => component
    map = {}
    for component in r0:
        for v in component:
            map[v] = component
    return r0, map

def partition_fix (exp, scc_graph):
    # partition the functions in this fix into sets of mutually-recursive functions
    vardefs = exp.names
    name_map = {}
    # map of <name> => <index>
    for i in range (len (vardefs)):
        name_map[vardefs[i].name] = [i, False]
    names = [x.name for x in vardefs]
    inits = exp.inits
    n = len (inits)
    leftover = range (n)
    parts = [[]]
    for component in scc_graph:
        if len(parts[-1]):
            parts.append ([])
        for name in component:
            probe = name_map.get (name, None)
            if probe and not probe[1]:
                # index
                parts[-1].append (probe[0])
                # flag it as done
                probe[1] = True
                leftover.remove (probe[0])
    # the leftovers should all be non-functions
    if leftover:
        parts.insert (0, leftover)
    if parts[-1] == []:
        # dangling empty partition
        del parts[-1]
    # within each part, retain original source order
    for part in parts:
        part.sort()
    return parts

def reorder_fix (exp, scc_graph):
    partition = partition_fix (exp, scc_graph)
    n = len(exp.inits)
    names = []
    inits = []
    r = []
    i = 0
    for part in partition:
        r.append ([])
        for j in part:
            names.append (exp.names[j])
            inits.append (exp.inits[j])
            r[-1].append (i)
            i += 1
    # XXX rejigger node data
    exp.names = exp.params = names
    exp.inits = inits
    body = exp.subs[-1]
    exp.subs = inits + [body]
    assert (len(exp.inits) == n)
    return r

