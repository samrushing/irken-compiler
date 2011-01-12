# -*- Mode: Python -*-

# global settings and context for an invocation of the compiler

class context:
    verbose = False
    noinline = False
    annotate = False
    trace = False
    profile = False
    step_solver = False
    typetype = False
    no_range = False
    optimize = False
    force_32 = False
    print_types = False

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
        self.variant_labels = {}
        # constructed literals
        self.constructed = []
        self.symbols = {}
        self.standard_macros = "lib/derived.scm"
        # number of lines before and after a type error node
        self.type_error_lines = 15
        # all functions seen by the back end
        self.functions = []
