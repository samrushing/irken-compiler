# -*- Mode: Python -*-

# global settings and context for an invocation of the compiler

class context:
    verbose = False
    noinline = False
    annotate = False
    trace = False
    step_solver = False
    typetype = False
    no_range = False
    optimize = False
    force_32 = False

    def __init__ (self):
        self.datatypes = {}
        self.dep_graph = None
        self.scc_graph = None
        self.scc_map = None
        self.var_dict = None
        self.record_types = None
        self.datatypes = {}
        self.cincludes = set()
        self.records2 = {}
        self.labels2 = {}
        # XXX should probably remove this hack since we're not using polymorphic variants this way any more.
        self.variant_labels = {
            # Hack.  since pxll.h and header.c:dump_object() already
            #   know about the 'builtin' cons and nil constructors, we
            #   hard-code them in here.  You'll note that TC_PAIR is
            #   two entries before TC_USEROBJ...
            'cons': -2,
            'nil': -3,
            }
        # don't throw away precious tag space.
        self.nvariant_offset = len(self.variant_labels)
        # constructed literals
        self.constructed = []
        self.symbols = {}

