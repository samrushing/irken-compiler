# -*- Mode: Python -*-

import string
import sys
import itypes
W = sys.stderr.write

from pdb import set_trace as trace

is_a = isinstance

class RegisterError:
    pass

class function:
    def __init__ (self, name):
        self.name = name
        self.known_allocs = 0

class c_backend:

    def __init__ (self, out, src, num_regs, context):
        self.out = out
        self.src = src
        self.num_regs = num_regs
        self.indent = 0
        self.context = context
        self.annotate = context.annotate
        self.trace = context.trace
        self.profile = context.profile
        self.toplevel_env = False
        self.current_fun = function ('toplevel')
        self.write_header()
        self.max_free = 0

    def go (self, insns):
        self.emit (insns)
        self.done()

    def write_header (self):
        for header in self.context.cincludes:
            self.out.write ('#include <%s>\n' % (header,))
        header = open ('header.c').read()
        # XXX ugh, this is getting lame.
        tag0 = '// CONSTRUCTED LITERALS //\n'
        stop0 = header.find (tag0) + len (tag0)
        self.out.write (header[:stop0])
        self.emit_constructed_literals()
        if self.profile:
            self.emit_profile_counters()
        tag1 = '// REGISTER_DECLARATIONS //\n'
        stop1 = header.find (tag1) + len (tag1)
        self.out.write (header[stop0:stop1])
        self.out.write (self.register_declarations())
        self.indent = 1
        self.emit_gc_copy_regs (self.num_regs)
        self.out.write (header[stop1:])

    def register_declarations (self):
        return '\n'.join (["  register object * r%d;" % i for i in range (self.num_regs + 1) ])

    def emit_constructed_literals (self):

        # we support three types of non-immediate literals:

        # 1) strings.  identical strings are *not* merged, since
        #      modifying strings is a reasonable choice.
        # 2) symbols.  this emits a string followed by a symbol tuple.
        #      these are collected so each is unique.  any runtime
        #      symbol table should be populated with these first.
        # 3) constructed.  trees of literals made of constructors
        #      (e.g. lists formed with QUOTE), and vectors.  each tree
        #      is rendered into a single C array where the first value
        #      in the array points to the beginning of the top-level
        #      object.

        name = None
        dtm = self.context.datatypes
        l = []

        def dotag (t):
            # XXX this is horrible.  I really want to be able to specify cons/nil symbolically
            #   when possible, in order to get lists handled somewhat automatically (especially
            #   when printing).  But the hairy UOXXX/UIXXX macros can't handle symbolic tags, yet.  FIX ME.
            if is_a (t, int):
                return t
            else:
                # these values will break as soon as the TC_XXX numbers get changed.
                return {'TC_PAIR':-3, 'TC_BOOL':-4, 'TC_SYMBOL':-2}[t]
        
        # now we walk the bitch, appending to a list of pxll_ints.
        def walk (exp):
            if exp.is_a ('primapp'):
                if exp.name.startswith ('%dtcon/'):
                    # constructor
                    ignore, dtname, alt = exp.name.split ('/')
                    dt = dtm[dtname]
                    tag = dt.tags[alt]
                    if dtname == 'symbol':
                        return 'UPTR(%d,1)' % (exp.index)
                    elif dt.uimm.has_key (alt):
                        # a single immediate argument, use it directly.
                        assert (len(exp.args) == 1)
                        return walk (exp.args[0])
                    elif len(exp.args):
                        # constructor with args, a tuple
                        # first - emit all the args
                        args = [ walk (x) for x in exp.args ]
                        # emit the header
                        addr = len (l)
                        l.append ('UOHEAD(%d,%d)' % (len(exp.args), dotag (tag)))
                        l.extend (args)
                        exp.index = -1
                        return 'UPTR(%d,%d)' % (i, addr)
                    elif tag in ('TC_NIL', 'PXLL_TRUE', 'PXLL_FALSE'):
                        # XXX ugh, special case again, fixme
                        return '(pxll_int)%s' % (tag,)
                    else:
                        # constructor with no args, an immediate
                        return 'UITAG(%d)' % (dotag (tag,))
                elif exp.name.startswith ('%vector-literal/'):
                    args = [ walk (x) for x in exp.args ]
                    addr = len (l)
                    l.append ('(%d<<8)|TC_VECTOR' % (len(exp.args),))
                    l.extend (args)
                    return 'UPTR(%d,%d)' % (i, addr)
                else:
                    raise ValueError ("unsupported primapp in constructed literal: %r" % (exp,))
            elif exp.is_a ('literal'):
                # XXX this duplicates smarts in cps.py that need to be moved into this file.
                if exp.ltype == 'int':
                    return (exp.value << 1) | 1
                elif exp.ltype == 'char':
                    if exp.value == 'eof':
                        return 257<<8|0x02
                    else:
                        return ord(exp.value)<<8|0x02
                elif exp.ltype == 'undefined':
                    return 0x0e
                elif exp.ltype == 'string':
                    return 'UPTR0(%d)' % (exp.index)
                else:
                    raise ValueError ("unsupported type in constructed literal: %r" % (exp,))
            elif exp.is_a ('constructed'):
                return 'UPTR(%d)' % (exp.index)
            else:
                raise ValueError ("unsupported type in constructed literal: %r" % (exp,))

        # synthesize an extra constant, a vector of all the symbols.

        # go through the list of top-level constructed literals, and emit them.
        lengths = []
        for i in range (len (self.context.constructed)):
            lit = self.context.constructed[i]
            if lit.is_a ('literal') and lit.ltype == 'string':
                # strings are a special case here because they have a non-uniform structure: the existence of
                #   the uint32_t <length> field means it's hard for us to put a UPTR in the front.
                s = lit.value
                slen = len(s)
                self.write ('pxll_string constructed_%d = { STRING_HEADER(%d), %d, "%s" };' % (i, slen, slen, self.c_string (s)))
            elif lit.is_a ('primapp') and lit.name == '%dtcon/symbol/t':
                # there's a temptation to skip the extra pointer at the front, but that would require additional smarts
                #   in insn_constructed (as already exist for strings).
                # NOTE: this reference to the string object only works because it comes before the symbol in self.context.constructed.
                self.write ('// symbol %r' % (lit.args[0],))
                self.write ('pxll_int constructed_%d[] = {UPTR(%d,1), SYMBOL_HEADER, UPTR0(%d)};' % (i, i, lit.args[0].index))
            else:
                # normal constructors
                l = [None]
                val = walk (lit)
                l[0] = val
                self.write ('pxll_int constructed_%d[] = {%s};' % (i, ', '.join ([str(x) for x in l])))
                lengths.append (len (l))

        #pxll_int constructed_8[] = {UPTR(8,1), (4<<8)|TC_VECTOR, UPTR(1,1), UPTR(3,1), UPTR(5,1), UPTR(7,1)};
        syms = []
        for sym, (index0, index1) in self.context.symbols.iteritems():
            syms.append ('UPTR(%d,1)' % (index1,))
        self.write ('pxll_int pxll_internal_symbols[] = {(%d<<8)|TC_VECTOR, %s};' % (len(syms), ', '.join (syms)))

    def emit_gc_copy_regs (self, nregs):
        # emit functions to copy registers in and out of the heap as gc roots
        self.write ('')
        self.write ('void gc_regs_in (int n) {')
        self.write ('  switch (n) {')
        for i in reversed (range (nregs+1)):
            self.write ('  case %d: heap1[%d] = r%d;' % (i+1, i+3, i))
        self.write ('}}')
        self.write ('void gc_regs_out (int n) {')
        self.write ('  switch (n) {')
        for i in reversed (range (nregs+1)):
            self.write ('  case %d: r%d = heap0[%d];' % (i+1, i, i+3))
        self.write ('}}')

    def emit_profile_counters (self):
        # for now, all we do is count function calls
        self.write ('// PROFILE COUNTERS')
        for fun in self.context.functions:
            label = self.function_label (fun)
            self.write ('pxll_int prof_count_%s = 0;' % label)

    def emit_dump_profile (self):
        for fun in self.context.functions:
            label = self.function_label (fun)
            self.write ('fprintf (stderr, "%%20ld %s\\n", prof_count_%s);' % (label, label))

    def emit (self, insns):
        for insn in insns:
            name = 'insn_%s' % (insn.name,)
            if self.annotate:
                self.write ('// %s' % (insn.print_info(),))
            fun = getattr (self, name, None)
            fun (insn)
            self.max_free = max (len(insn.free_regs), self.max_free)

    def done (self):
        #print 'max_free =', self.max_free
        # close out the vm() function...
        self.indent = 0
        self.write (' Lreturn:')
        if self.profile:
            self.emit_dump_profile()
        self.write (" return (pxll_int) result;\n}")
        if len(self.context.records2):
            # write out the record field lookup function
            self.out.write (
                "static int lookup_field (int tag, int label)\n"
                # "{ fprintf (stderr, \"[%d %d]\", tag, label);"
                "{ switch (tag) {\n"
                )
            for rec, tag in self.context.records2.iteritems():
                self.out.write ("  case %d:\n" % tag)
                self.out.write ("  switch (label) {\n")
                for i in range (len (rec)):
                    label = rec[i]
                    self.out.write ("    case %d: return %d; break;\n" % (self.context.labels2[label], i))
                self.out.write ("  } break;\n")
            self.out.write ("}}\n")
            

    label_counter = 0

    def new_label (self):
        "generate a fresh unused label"
        self.label_counter += 1
        return 'L%d' % (self.label_counter,)

    safe_name_map = {'!':'_bang','*':'_splat','?':'_question','+':'_plus','-':'_'}

    def frob_name (self, name):
        l = []
        for ch in name.lower():
            if self.safe_name_map.has_key (ch):
                r = self.safe_name_map[ch]
            elif ch in 'abcdefghijklmnopqrstuvwxyz_0123456789':
                r = ch
            else:
                r = '_%02x' % (ord (ch),)
            l.append (r)
        r = ''.join (l)
        if r == '_':
            # special case
            r = 'minus'
        return r

    def function_label (self, fun):
        "generate/memoize a unique label for a known function"
        if hasattr (fun, 'label'):
            return fun.label
        else:
            if fun.name:
                #fun.label = 'FUN_%d%s' % (fun.serial, self.frob_name (fun.name))
                fun.label = 'FUN_%s' % (self.frob_name (fun.name),)
            else:
                fun.label = 'FUN_%d_lambda' % (fun.serial,)
            return fun.label

    def write (self, line):
        "write out a 'line' of indented code, followed by a newline"
        self.out.write ('  ' * self.indent)
        self.out.write (line)
        self.out.write ('\n')

    # set in pxll.h
    head_room = 1024

    def alloc (self, line, insn, size):
        self.current_fun.known_allocs += size
        if self.current_fun.known_allocs >= self.head_room:
            # may have run out of head room, emit a heap check here.
            # 'may' since we're unfairly counting all sides of each branch...
            print 'mid-function heap check: free=%r' % (insn.free_regs)
            # XXX relies on the in-order allocation of registers
            self.check_free_regs (insn.free_regs)
            self.write ('check_heap (%d);' % (len (insn.free_regs)))
            self.current_fun.known_allocs = 0
        self.write (line)

    def insn_lit (self, insn):
        # for now
        lit = insn.params
        if insn.target is 'dead':
            # why bother with dead literals?
            comment = '//'
        else:
            comment = ''
        self.write ('%sr%r = (object *) %d;' % (comment, insn.target, lit))

    def insn_constructed (self, insn):
        # a reference to a constructed literal - refer to its variable directly.
        index = insn.params
        val = self.context.constructed[index]
        if val.is_a ('literal') and val.ltype == 'string':
            self.write ('r%d = (object*) &constructed_%d;' % (insn.target, index))
        else:
            self.write ('r%d = (object*) constructed_%d[0];' % (insn.target, index))

    def insn_return (self, insn):
        val_reg = insn.regs[0]
        self.write ('PXLL_RETURN(%d);' % (val_reg,))

    def insn_jump (self, insn):
        result, target = insn.regs
        # assign a target register for the other side of a jump (used by 'if' and 'vcase')
        if (target != 'dead') and result != target:
            self.write ('r%d = r%d;' % (target, result))

    # wrap_in, wrap_out: provide automatic type conversions when plausible
    def wrap_in (self, types, args):
        result = []
        for i in range (len (types)):
            t = types[i]
            if is_a (t, itypes.t_int):
                result.append ('unbox(%s)' % (args[i],))
            elif is_a (t, itypes.t_string):
                result.append ('((pxll_string*)(%s))->data' % (args[i],))
            elif is_a (t, itypes.t_predicate):
                if itypes.is_pred (t, 'raw'):
                    # 'raw' types...
                    if is_a (t.args[0], itypes.t_string):
                        result.append ('((pxll_string*)(%s))' % (args[i],))
                    else:
                        raise ValueError ("unknown 'raw' type: %r" % (t,))
                elif itypes.is_pred (t, 'arrow'):
                    # function type
                    result.append (args[i])
                elif itypes.is_pred (t, 'vector'):
                    # vectors
                    result.append (args[i])
                elif itypes.is_pred (t, 'symbol'):
                    result.append (args[i])                    
                else:
                    raise ValueError ("unexpected predicate in cexp type sig: %r" % (t,))
            elif is_a (t, itypes.t_var):
                # tvars in cexp types
                result.append (args[i])
            elif is_a (t, itypes.t_base):
                # some other base type, untouched
                result.append (args[i])
            else:
                raise ValueError ("unknown element in cexp type sig: %r" % (t,))
        return tuple (result)

    def wrap_out (self, type, exp):
        if is_a (type, itypes.t_int):
            return 'box(%s)' % (exp,)
        elif itypes.is_pred (type, 'bool'):
            # hmm... this is more like a cast, and should probably be
            # expressed as such.
            return 'PXLL_TEST(%s)' % (exp,)
        else:
            return exp

    def guess_record_type (self, row):
        # XXX memoize
        orow = row
        row = set(row)
        row.discard ('...')
        # can we disambiguate this row type?
        candidates = []
        for sig, tag in self.context.records2.iteritems():
            if set(sig).issuperset (row):
                candidates.append (sig)
        #print self.context.records2
        if len(candidates) == 1:
            #print 'unambiguous', orow, candidates[0]
            # cast to list for python < 2.6 for 'index' method
            return list(candidates[0])
        else:
            #print 'ambiguous', orow, candidates
            return None

    def cexp_subst (self, template, values):
        r = []
        i = 0
        j = 0
        lt = len (template)
        while i < lt:
            ch = template[i]
            if ch == '%':
                r.append (template[j:i])
                i += 1
                ch = template[i]
                if ch == '%':
                    r.append ('%')
                elif ch in '0123456789':
                    r.append (values[int (ch)])
                else:
                    # for more than 10 args, we can introduce a new syntax like %{10}
                    raise ValueError ("bad template: %s" % template)
                j = i + 1
            i += 1
        r.append (template[j:])
        return ''.join (r)

    def insn_primop (self, insn):
        # XXX consider making some of these insns in their own right?
        regs = insn.regs
        primop = insn.params
        name = primop[0]
        if name == '%cexp':
            ignore, form, (tvars, sig) = primop
            if itypes.is_pred (sig, 'arrow'):
                result_type = sig.args[0]
                arg_types = sig.args[1:]
            else:
                result_type, arg_types = sig, ()
            regs = tuple('r%d' % x for x in regs)
            regs = self.wrap_in (arg_types, regs)
            exp = self.wrap_out (result_type, self.cexp_subst (form, regs))
            if insn.target == 'dead':
                # probably a side-effect expression
                self.write ('%s;' % (exp,))
            else:
                self.write ('r%d = %s;' % (insn.target, exp))
        elif name == '%make-tuple':
            ignore, type, tag = primop
            if insn.target == 'dead':
                print 'warning: dead %make-tuple', insn
            else:
                regs = insn.regs
                nargs = len (regs)
                if nargs == 0 and is_a (tag, int):
                    # unit type - use an immediate
                    self.write ('r%d = (object*)UITAG(%d);' % (insn.target, tag))
                else:
                    # tuple type
                    if nargs == 0:
                        # we can't have empty tuples - it breaks the sentinel trick used
                        #   in the garbage collector to avoid address range checks.
                        # each tuple type (that might be empty) needs a corresponding immediate
                        #   type to represent the empty version of it.
                        if tag == 'TC_VECTOR':
                            self.write ('r%d = (object *) TC_EMPTY_VECTOR;' % insn.target)
                        elif is_a (tag, str):
                            self.write ('r%d = (object *) %s;' % (insn.target, tag.upper()))
                        else:
                            raise ValueError ("attempt to create unsupported empty tuple type")
                    else:
                        if is_a (tag, int):
                            tag = '(TC_USEROBJ+%d)' % (tag << 2,)
                        self.alloc ('t = alloc_no_clear (%s, %d);' % (tag, nargs), insn, nargs)
                        self.write (' '.join (['t[%d] = r%d;' % (i+1, regs[i]) for i in range (nargs) ]))
                        self.write ('r%d = t;' % (insn.target,))
        elif name.startswith ('%array-ref'):
            [base, index] = insn.regs
            if name[-1] != '%':
                self.write ('range_check (GET_TUPLE_LENGTH(*(object*)r%d), unbox(r%d));' % (base, index))
            self.write ('r%d = ((pxll_vector*)r%d)->val[unbox(r%d)];' % (insn.target, base, index))
        elif name.startswith ('%array-set'):
            [base, index, val] = insn.regs
            if name[-1] != '%':
                self.write ('range_check (GET_TUPLE_LENGTH(*(object*)r%d), unbox(r%d));' % (base, index))
            self.write ('((pxll_vector*)r%d)->val[unbox(r%d)] = r%d;' % (base, index, val))
        elif name == '%record-get':
            [record] = insn.regs
            ignore, label, sig = primop
            sig = self.guess_record_type (sig)
            label_code = self.context.labels2[label]
            if not sig:
                # runtime lookup
                self.write (
                    'r%d = ((pxll_vector*)r%d)->val[lookup_field((GET_TYPECODE(*r%d)-TC_USEROBJ)>>2,%d)];' % (
                        insn.target, record, record, label_code
                        )
                    )
            else:
                # compile-time lookup
                self.write ('r%d = ((pxll_vector*)r%d)->val[%d];' % (insn.target, record, sig.index (label)))
        elif name == '%record-set':
            [record, val] = insn.regs
            ignore, label, sig = primop
            sig = self.guess_record_type (sig)
            label_code = self.context.labels2[label]
            if not sig:
                # runtime lookup
                self.write (
                    '((pxll_vector*)r%d)->val[lookup_field((GET_TYPECODE(*r%d)-TC_USEROBJ)>>2,%d)] = r%d;' % (
                        record, record, label_code, val
                        )
                    )
            else:
                # compile-time lookup
                self.write ('((pxll_vector*)r%d)->val[%d] = r%d;' % (record, sig.index (label), val))
            # XXX set! vs extend... an 'extension' of a pre-existing field becomes a set!, return the record.
            if (insn.target != 'dead') and record != insn.target:
                self.write ('r%d = r%d;' % (insn.target, record))
        elif name == '%extend-tuple':
            # extend a pre-existing tuple by merging it with one or more new field=value pairs.
            src = insn.regs[0]
            data = insn.regs[1:]
            ignore, new_fields, old_fields, new_tag = insn.params
            new_fields = list (new_fields)
            old_fields = list (old_fields)
            new_len = len (new_fields) + len (old_fields)
            new_tag = '(TC_USEROBJ+%d)' % (new_tag * 4)
            self.alloc ('t = alloc_no_clear (%s, %d);' % (new_tag, new_len), insn, new_len)
            j = 0
            for i in range (new_len):
                if not old_fields or new_fields and new_fields[0] < old_fields[0]:
                    # storing a new field value
                    self.write ('t[%d] = r%d;' % (i+1, data[0]))
                    new_fields.pop (0)
                    data.pop(0)
                else:
                    # copying an old field value
                    self.write ('t[%d] = r%d[%d];' % (i+1, src, j+1))
                    j += 1
                    old_fields.pop (0)
            self.write ('r%d = t;' % (insn.target,))
        elif name == '%make-vector':
            [vlen, vval] = insn.regs
            self.write ('if (unbox(r%d) == 0) { r%d = (object *) TC_EMPTY_VECTOR; } else {' % (vlen, insn.target))
            # XXX currently, this is the only alloc size not known at runtime.  need to check the heap
            #     specifically for this amount of room at runtime.
            self.write ('  t = alloc_no_clear (TC_VECTOR, unbox(r%d));' % (vlen,))
            self.write ('  for (i=0; i < unbox(r%d); i++) { t[i+1] = r%d; }' % (vlen, vval))
            self.write ('  r%d = t;' % (insn.target,))
            self.write ('}')
        elif name == '%make-vec16':
            [vlen] = insn.regs
            self.write ('if (unbox(r%d) == 0) { r%d = (object *) TC_EMPTY_VECTOR; } else {' % (vlen, insn.target))
            self.write ("  t=alloc_no_clear (TC_VEC16, VEC16_TUPLE_LENGTH(unbox(r%d)));" % vlen)
            self.write ("  ((pxll_vec16*)t)->len = unbox(r%d);" % vlen)
            self.write ('  r%d = t;' % (insn.target,))
            self.write ('}')
        elif name == '%vget':
            self.write ('r%d = UOBJ_GET(r%d,%s);' % (insn.target, insn.regs[0], primop[1]))
        elif name == '%vec16-ref':
            [vreg, ireg] = insn.regs
            self.write ('range_check (((pxll_vec16*)r%d)->len, unbox(r%d));' % (vreg, ireg))
            self.write ('r%d = box (((pxll_vec16*)r%d)->data[unbox(r%d)]);' % (insn.target, vreg, ireg))
        elif name == '%vec16-set':
            [vreg, ireg, areg] = insn.regs
            self.write ('range_check (((pxll_vec16*)r%d)->len, unbox(r%d));' % (vreg, ireg))
            self.write ('((pxll_vec16*)r%d)->data[unbox(r%d)] = unbox(r%d);' % (vreg, ireg, areg))
        else:
            raise ValueError ("unknown primop: %s" % name)

    def insn_test (self, insn):
        cexp, then_code, else_code = insn.params
        if cexp:
            # if we know we're testing a cexp, just inline it here
            code, (tvars, sig) = cexp
            regs = tuple ('r%d' % x for x in insn.regs)
            regs = self.wrap_in (sig.args[1:], regs)
            exp = self.wrap_out (sig.args[0], self.cexp_subst (code, regs))
            self.write ('if PXLL_IS_TRUE(%s) {' % exp)
        else:
            # this is a scheme-like definition of test/#t/#f
            self.write ('if PXLL_IS_TRUE(r%d) {' % (insn.regs[0],))
        self.indent += 1
        self.emit (then_code)
        self.indent -= 1
        self.write ('} else {')
        self.indent += 1
        self.emit (else_code)
        self.indent -= 1
        self.write ('}')

    def insn_pvcase (self, insn):
        # this version uses get_pvariant_tag() to avoid segregating into
        #   immediate/pointer cases... note that each unique label is given
        #   a unique tag, regardless of its arity (which can vary!)
        [test_reg] = insn.regs
        alt_formals, alts = insn.params
        units = []
        tuples = []
        self.write ('switch (get_case_noint (r%d)) {' % test_reg)
        for i in range (len (alts)):
            if i < len(alt_formals):
                label, orig_arity, formals = alt_formals[i]
                try:
                    tag = self.context.variant_labels[label]
                except KeyError:
                    raise ValueError ('variant constructor ":%s" never called; no runtime tag available.' % label)
                if orig_arity == 0:
                    tag = 'TC_USERIMM+%d' % (tag<<8)
                else:
                    tag = 'TC_USEROBJ+%d' % (tag * 4)
                case = 'case (%s): {' % (tag,)
            else:
                case = 'default: {'
            self.indent += 1
            self.write (case)
            self.indent += 1
            self.emit (alts[i])
            self.indent -= 1
            self.write ('} break;')
            self.indent -= 1
        self.write ('}')

    def which_typecode_fun (self, dt):
        # XXX cache this!
        # getting the typecode of an unknown object involves three steps:
        # 1) is it an integer? (check the lowest bit, return TC_INT == 0)
        # 2) is it an immediate (check the next lowest bit, return ob)
        # 3) it's a pointer (indirect through it, return (*ob)&0xff
        #
        # if a datatype is all-immediate, or all-tuple, then using a
        # specific form of get_typecode(), can often lead to
        # single-instruction typecode fetching. (also, avoiding a branch)
        if len (dt.uimm):
            # if we're using the uimm hack, we have to check for everything, including TC_INT.
            return 'get_typecode'
        alts = dt.alts
        arity = len (alts[0][1])
        for i in range (1, len (alts)):
            tag, prod = alts[i]
            if len(prod) != arity:
                return 'get_case_noint'
        if arity == 0:
            # no function needed, compare the value directly
            return '(pxll_int)'
        else:
            return 'get_case_tup'

    def insn_nvcase (self, insn):
        [test_reg] = insn.regs
        dtype, tags, alts, ealt = insn.params
        dt = self.context.datatypes[dtype]
        use_else = len(dt.alts) != len(alts)
        if len(alts) == 1 and len (dt.alts) == 1:
            # nothing to switch on, just emit the code
            self.emit (alts[0])
        else:
            get_typecode = self.which_typecode_fun (dt)
            self.write ('switch (%s (r%d)) {' % (get_typecode, test_reg))
            for i in range (len (tags)):
                label = tags[i]
                tag = dt.tags[label]
                arity = dt.arity (label)
                uimm = False
                if is_a (tag, str):
                    # e.g., PXLL_FALSE
                    tag = '((pxll_int)%s)' % (tag,)
                elif arity == 0:
                    # immediate/unit-constructor
                    tag = 'TC_USERIMM+(%d<<8)' % tag
                elif arity == 1 and dt.uimm.has_key (label):
                    typename = dt.uimm[label].name.upper()
                    tag = 'TC_%s' % (typename)
                    uimm = True
                else:
                    # tuple constructor
                    tag = 'TC_USEROBJ+(%d<<2)' % tag
                self.indent += 1
                if uimm:
                    comment = '%s/uimm' % (label,)
                else:
                    comment = label
                if i == len(tags)-1 and not use_else:
                    self.write ('default: { // %s' % (comment,))
                else:
                    self.write ('case (%s): { // %s' % (tag, comment))
                self.indent += 1
                self.emit (alts[i])
                self.indent -= 1
                self.write ('} break;')
                self.indent -= 1
            if use_else:
                self.indent += 1
                self.write ('default: { // <else>')
                self.indent += 1
                self.emit (ealt)
                self.indent -= 1
                self.write ('}')
                self.indent -= 1
            self.write ('}')

    def insn_fatbar (self, insn):
        label, e1, e2 = insn.params
        self.emit (e1)
        self.write ('goto %s_over;' % (label,))
        self.write ('%s:' % (label,))
        self.emit (e2)
        # Note: the extra semicolon here is necessary because C99 requires a 'statement'
        #  to follow a label.  Sometimes there's no code after the label, so this avoids
        #  that problem.  [might be possible to look at the insn's continuation instead]
        self.write ('%s_over: ;' % (label,))

    def insn_fail (self, insn):
        label, npop = insn.params
        if npop:
            self.write ('lenv = ((object %s)lenv)%s;' % ('*' * npop, '[1]' * npop))
        self.write ('goto %s;' % (label,))

    def check_free_regs (self, free_regs):
        "describe the free_regs to new_env() for gc"
        # XXX do we need a bitfield, or an integer?
        # from everything I've seen, they're always in order ,
        # which makes sense because that's how the allocator works!
        # so let's just assume that and check for it here.
        n = len (free_regs)
        while n:
            n = n - 1
            if n not in free_regs:
                raise RegisterError ("Bad assumption about free registers!")

    def insn_new_env (self, insn):
        # val tuple is: <tag next val0 val1 val2 ...>
        size = insn.params
        # XXX our heap check is now done in a way that does not require looking at free regs.
        #self.check_free_regs (insn.regs)
        #self.write ('check_heap (%d); r%d = allocate (TC_TUPLE, %d);' % (len(insn.regs), insn.target, size + 1))
        self.alloc ('r%d = allocate (TC_TUPLE, %d);' % (insn.target, size + 1), insn, size + 1)
        if not self.toplevel_env:
            self.toplevel_env = True
            self.write ('top = r%d;' % (insn.target,))

    def insn_new_tuple (self, insn):
        tag, size = insn.params
        self.alloc ('r%d = allocate (%s, %d);' % (insn.target, tag, size), insn, size)

    def insn_store_tuple (self, insn):
        [arg_reg, tuple_reg] = insn.regs
        i, offset, n = insn.params
        self.write ('r%d[%d] = r%d;' % (tuple_reg, i+1+offset, arg_reg))
        # XXX this is a bit of a hack. Because of the confusing implementation of compile_rands,
        #     we have no way of passing the tuple to its continuation (when it's needed)
        if insn.target != 'dead' and insn.target != tuple_reg:
            self.write ('r%d = r%d;' % (insn.target, tuple_reg))

            
    # based on string.printable, but without the surprises at the end.
    isprint = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~ ' # \t\n\r\x0b\x0c'
    safe_for_c_string = {
        '\r' : '\\r',
        '\n' : '\\n',
        '\t' : '\\t',
        '\\' : '\\\\',
        '"'  : '\\"',
        }

    def c_string (self, s):
        r = []
        for ch in s:
            if self.safe_for_c_string.has_key (ch):
                r.append (self.safe_for_c_string[ch])
            elif ch in self.isprint:
                r.append (ch)
            else:
                # originally, used hex escapes here.  but the tricksy modern C spec allows for more than
                #  two digits (for non-ascii charsets).  that means \x0155 is one char, not three.  octal.
                r.append ('\\%03o' % (ord (ch)))
        return ''.join (r)

    def insn_build_env (self, insn):
        regs = insn.regs
        size = len (regs)
        self.alloc ('t = allocate (TC_TUPLE, %d);' % (size + 1,), insn, size + 1)
        for i in range (len (regs)):
            self.write ('t[%d] = r%d;' % (i+2, regs[i]))
        self.write ('r%d = t;' % (insn.target,))

    def insn_push_env (self, insn):
        [args_reg] = insn.regs
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, ">> [%d] push\\n", __LINE__);')
        self.write ('r%d[1] = lenv; lenv = r%d;' % (args_reg, args_reg))

    def insn_pop_env (self, insn):
        [src] = insn.regs
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, "<< [%d] pop\\n", __LINE__);')
        self.write ('lenv = lenv[1];')
        if insn.target != 'dead' and src != insn.target:
            self.write ('r%d = r%d;' % (insn.target, src))

    def insn_varref (self, insn):
        addr, is_top, var = insn.params
        name = var.name
        depth, index = addr
        if insn.target != 'dead':
            #if self.trace:
            #    self.write ('stack_depth_indent (k); fprintf (stderr, "(%d, %d)\\n");' % (depth, index))
            if is_top:
                self.write ('r%d = top[%d];' % (insn.target, index+2))
            else:
                # gcc generates identical code for these, and the latter is cleaner.
                #self.write ('r%d = ((object *%s) lenv) %s[%d];' % (insn.target, '*' * depth, '[1]' * depth, index+2)) 
               self.write ('r%d = varref (%d,%d);' % (insn.target, depth, index))

    def insn_varset (self, insn):
        val_reg = insn.regs[0]
        addr, is_top, var = insn.params
        #if insn.target != 'dead':
        #    print '[set! result used]',
        depth, index = addr
        if is_top:
            self.write ('top[%d] = r%d;' % (index+2, val_reg))
        else:
            # gcc generates identical code for these, and the latter is cleaner.
            #self.write ('((object *%s)lenv)%s[%d] = r%d;' % ('*' * depth, '[1]' * depth, index+2, val_reg))
            self.write ('varset (%d, %d, r%d);' % (depth, index, val_reg))

    def insn_close (self, insn):
        fun, body, free = insn.params
        proc_label = self.function_label (fun)
        jump_label = self.new_label()
        # emit a jump over function definition
        self.write ('// def %r' % (fun.name,))
        self.write ('goto %s;' % (jump_label,))
        # the function definition itself
        self.write ('%s:' % (proc_label,))
        self.indent += 1
        if self.trace:
            self.write ('stack_depth_indent(k); fprintf (stderr, ">> [%%d] %s\\n", __LINE__);' % ((fun.name or 'lambda'),))
        if self.profile:
            self.write ('prof_count_%s += 1;' % (proc_label,))
        if insn.allocates:
            self.write ('check_heap (0);')
        calling_fun = self.current_fun
        self.current_fun = function (fun.name)
        self.emit (body)
        self.indent -= 1
        self.write ('%s:' % (jump_label,))
        #print 'function %s known_allocs=%d' % (fun.name, self.current_fun.known_allocs)
        self.current_fun = calling_fun
        # create a closure object
        self.alloc ('r%d = allocate (TC_CLOSURE, 2);' % (insn.target,), insn, 2)
        # unfortunately, this is a gcc extension - '&&' takes the
        #   address of a label.  is there some portable way to this?
        self.write ('r%d[1] = &&%s; r%d[2] = lenv;' % (insn.target, proc_label, insn.target))

    def insn_invoke_tail (self, insn):
        closure_reg, args_reg = insn.regs
        fun = insn.params
        # call
        #   extend closure's environment with args, jump
        if fun:
            label = 'goto %s' % (self.function_label (fun),)
        else:
            label = 'goto *r%d[1]' % (closure_reg,)
        if args_reg is not None:
            self.write ('r%d[1] = r%d[2]; lenv = r%d; %s;' % (args_reg, closure_reg, args_reg, label))
        else:
            self.write ('lenv = r%d[2]; %s;' % (closure_reg, label))

    def insn_invoke (self, insn):
        closure_reg, args_reg = insn.regs
        free_regs, fun = insn.params
        return_label = self.new_label()
        nregs = len (free_regs)
        # sort these, might improve things
        free_regs = free_regs[:]
        free_regs.sort()
        # save
        self.alloc ('t = allocate (TC_SAVE, %d);' % (3 + nregs), insn, 3 + nregs)
        saves = []
        for i in range (nregs):
            saves.append ('t[%d] = r%d;' % (i+4, free_regs[i]))
        saves = ' '.join (saves)
        self.write ('t[1] = k; t[2] = lenv; t[3] = &&%s; %s k = t;' % (return_label, saves))
        # call
        #   extend closure's environment with args, jump
        if fun:
            label = 'goto %s' % (self.function_label (fun,),)
        else:
            label = 'goto *r%d[1]' % (closure_reg,)
        if args_reg is not None:
            self.write ('r%d[1] = r%d[2]; lenv = r%d; %s;' % (args_reg, closure_reg, args_reg, label))
        else:
            self.write ('lenv = r%d[2]; %s;' % (closure_reg, label))
        # label
        self.write ('%s:' % (return_label,))
        if self.trace:
            if fun:
                fun_name = fun.name
            else:
                fun_name = 'lambda'
            self.write ('stack_depth_indent(k); fprintf (stderr, "<< [%%d] %s\\n", __LINE__);' % (fun_name,))
        # restore
        restores = []
        for i in range (nregs):
            restores.append ('r%d = k[%d];' % (free_regs[i], i+4))
        restores = ' '.join (restores)
        self.write ('%s; lenv = k[2]; k = k[1];' % restores)
        if insn.target is not 'dead':
            self.write ('r%d = result;' % (insn.target,))

    def insn_tr_call (self, insn):
        regs = insn.regs
        depth, fun = insn.params
        nargs = len (regs)
        # we want to jump up the stack back to the start of this function.
        # to do that, we need to pop a certain number of levels off of <lenv>.
        # <depth> pops would put us at the function itself.
        # <depth-1> points us at the functions args, if any.
        npop = depth - 1
        if nargs == 0:
            # a zero-arg trcall needs an extra level of pop
            npop += 1
        if npop:
            self.write ('lenv = ((object %s)lenv)%s;' % ('*' * npop, '[1]' * npop))
        for i in range (nargs):
            self.write ('lenv[%d] = r%d;' % (2+i, regs[i]))
        self.write ('goto %s;' % (self.function_label (fun),))

    def insn_move (self, insn):
        reg_var, reg_src = insn.regs
        if reg_src is not None:
            # from varset
            self.write ('r%d = r%d;' % (reg_var, reg_src))
        elif insn.target != 'dead':
            # XXX need remove_moves() if we're keeping this code...
            self.write ('r%d = r%d;' % (insn.target, reg_var))
            pass
        else:
            pass
