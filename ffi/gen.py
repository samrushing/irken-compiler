# -*- Mode: Python -*-

import os
import sys
from pprint import pprint as pp

import string
import sys

# using sexp for spec, might later write this gen in irken.
class sexp_reader:

    # input buffering

    def __init__ (self, s):
        self.buffer = s
        self.pos = 0

    def peek (self):
        if self.pos > len(self.buffer):
            return ''
        else:
            return self.buffer[self.pos]

    def next (self):
        self.pos += 1
        return self.buffer[self.pos - 1]

    # form reader

    def skip_whitespace (self):
        while 1:
            ch = self.peek()
            if not ch:
                break
            elif ch == ';':
                while self.next() not in '\r\n':
                    pass
            elif ch not in string.whitespace:
                break
            else:
                self.next()

    def skip_comment (self):
        while 1:
            ch = self.next()
            if ch == '\n':
                return

    def read (self):
        self.skip_whitespace()
        ch = self.peek()
        if ch == '':
            raise EOFError, "Unexpected end of file"
        elif ch == '(':
            return self.read_list()
        elif ch == ';':
            self.skip_comment()
            self.read()
        else:
            return self.read_atom()

    def read_atom (self):
        # read at least one character
        result = self.next()
        while 1:
            ch = self.peek()
            if ch in string.whitespace or ch in '()':
                return result
            else:
                result = result + self.next()

    def read_list (self):
        result = []
        # throw away the paren
        paren = self.next()
        while 1:
            self.skip_whitespace()
            p = self.peek()
            if p == ')':
                # throw away the paren
                ch = self.next()
                return result
            else:
                x = self.read ()
                result.append(x)

def build_clang_input (path, iface):

    f = open (path, 'wb')
    W = f.write

    counter = 0

    # make sure we always get these.
    W ('#include <stdio.h>\n')
    W ('#include <stdint.h>\n')
    W ('#include <stddef.h>\n') # for size_t
    W ('#include <inttypes.h>\n')
    for ifile in iface['includes']:
        W ('#include <%s>\n' % (ifile,))
    W ('\n')
    for struct in iface['structs']:
        W ('struct %s ob%d;\n' % (struct, counter))
        W ('size_t ob%d_size = sizeof(ob%d);\n' % (counter, counter))
        counter += 1
    W ('\n')
    W ('int main (int argc, char * argv[])\n{\n')
    for con in iface['constants']:
        W ('  fprintf (stdout, "%s %%" PRIdPTR "\\n", (intptr_t)%s);\n' % (con, con))
    W ('}\n')
    f.close()

import subprocess

def do_cmd (cmd):
    p = subprocess.Popen (cmd, stdout=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    if err is not None:
        raise SystemError
    else:
        return out.splitlines()

def dump_record_layouts (path, args):
    cmd = (
        '%s -E %s %s | %s -cc1 -fdump-record-layouts' % (
            args.clang, args.incs, path, args.clang
        )
    )
    print 'cmd = %r' % (cmd,)
    return do_cmd (cmd)

# this catches the kinds of typedefs we are looking for.
# it will screw up with things like "typedef x_t *y_t;" (or any weirdness).
def find_typedefs (path, args):
    cmd = '%s %s -E %s' % (args.clang, args.incs, path)
    print 'cmd = %r' % (cmd,)
    lines = do_cmd (cmd)
    tmap = {}
    for line in lines:
        parts = line.split()
        if len(parts) and parts[0] == 'typedef' and parts[-1][-1] == ';':
            t_type = parts[1:-1]
            f_type = parts[-1][:-1]
            tmap[f_type] = t_type
    return tmap

def find_constants (path, iface, args):
    cmd = '%s %s %s -o con' % (args.clang, args.incs, path)
    do_cmd (cmd)
    lines = do_cmd ('./con')
    os.unlink ('./con')
    constants = iface['constants']
    r = []
    for line in lines:
        [name, val] = line.split()
        r.append ((name, val))
    return r

int_sizes_code = """
#include <stdio.h>
#include <stdint.h>

int
main (int argc, char * argv[])
{
  fprintf (stdout, "char %zu\\n", sizeof(char));
  fprintf (stdout, "int %zu\\n", sizeof(int));
  fprintf (stdout, "short %zu\\n", sizeof(short));
  fprintf (stdout, "long %zu\\n", sizeof(long));
  fprintf (stdout, "long-long %zu\\n", sizeof(long long));
  fprintf (stdout, "intptr_t %zu\\n", sizeof(intptr_t));
  return 0;
}
"""

def find_int_sizes():
    # need a program for this purpose because the typedefs
    # in stdint are not (always) enough to resolve all types:
    # e.g., if 'long long' == 'long'
    open ('/tmp/intsizes.c', 'wb').write (int_sizes_code)
    os.system ('clang /tmp/intsizes.c -o /tmp/intsizes')
    lines = do_cmd ('/tmp/intsizes')
    d = {}
    for line in lines:
        t, s = line.split()
        t = tuple (t.split ('-'))
        d[t] = int(s)
    # aliases
    d[('short', 'int')] = d[('short',)]
    d[('long', 'int')] = d[('long',)]
    d[('unsigned',)] = d[('int',)]
    return d

def build_int_size_table():
    int_sizes = find_int_sizes()
    table = {}
    for t in base_int_types:
        if t[0] in ('unsigned', 'signed', '__signed'):
            t = t[1:]
        size = int_sizes[t]
        table[t] = size, 1      # size, signed?
        table[('unsigned',) + t] = size, 0
    return table

def resolve_type (name, tmap):
    while tmap.has_key (name):
        dtype = tmap[name]
        if len(dtype) == 1:
            name = dtype[0]
        else:
            return tuple(dtype)
    return (name,)

# recognize the many flavors of ints.
# (at least the ones seen with dump-record-layouts]

base_int_types = [['long', 'long'], ['long', 'int'], ['short', 'int'], ['long'], ['int'], ['short'], ['char'],]
base_int_types += [['unsigned'] + x for x in base_int_types]
base_int_types.sort (lambda a,b: cmp(len(b),len(a))) # longest match first
base_int_types = [tuple(x) for x in base_int_types] # used as dict keys

base_int_set = set(base_int_types)

# core_type := int-type|void|char|struct name|union name|other
# type := core_type pointer* array* name
# pointer := '*'
# array := '[<size>]'

def parse_array (s, t):
    parts = s[1:-1].split('][')
    return ('array', t, [int(x) for x in parts])

import re

name = r'([A-Za-z_][A-Za-z_0-9]*)'
anon = r'((?:::)?\(anonymous [^)]+\))?'
def_line  = re.compile (r' +([0-9]+) \| ( *)(struct|union) ' + name)
sub_line  = re.compile (r' +([0-9]+) \| ( *)(.+)')
size_line = re.compile (r' +\| \[sizeof=([0-9]+), align=([0-9]+)\]')

def parse_def (ctype):
    ctype = re.sub (anon, '', ctype)
    tokens = ctype.split()
    return tokens

def parse_type (ctype, tmap):

    ctype = re.sub (anon, '', ctype)
    tokens = ctype.split()
    ntok = len(tokens)

    def match (p, i):
        return tuple(tokens[i:i+len(p)]) == p

    r = None
    index = 0
    tok = tokens[index]
    # parse core type
    if tok in ('void', 'char'):
        r = tok
        index += 1
    elif tok in ('struct', 'union'):
        r = (tok, tokens[index+1])
        index += 2
    else:
        # other types
        # integer types
        found = False
        for pat in base_int_types:
            if match (pat, index):
                r = ('int', int_size_table[pat])
                index += len(pat)
                found = True
                break
        if not found:
            # look it up in our type map.
            rtype = resolve_type (tok, tmap)
            if int_size_table.has_key (rtype):
                r = ('int', int_size_table[rtype])
            else:
                # other types
                r = ('other', rtype)
            index += 1
    # check for pointers
    if index < ntok and tokens[index].startswith('*'):
        r = ('*', r)
        index += 1
    # check for array
    if index < ntok and tokens[index].startswith('['):
        r = parse_array (tokens[index], r)
        index += 1
    # last bit should be the name
    return (tokens[index], r)

def parse_one_layout (lines, tmap):
    r = []
    state = 0
    while len(lines):
        line = lines.pop(0)
        m = sub_line.match (line)
        if state == 0:
            offset, indent, stype = m.groups()
            r.append (parse_def (stype))
            state = 1
        elif state == 1:
            if m:
                offset, indent, stype = m.groups()
                r.append ((offset, len(indent), parse_type (stype, tmap)))
            else:
                m = size_line.match (line)
                size, align = m.groups()
                r.append ((size, align))
                return r

def parse_record_layouts (lines, tmap):
    layouts = []
    while len(lines):
        line = lines.pop(0)
        if line == '*** Dumping AST Record Layout':
            layouts.append (parse_one_layout (lines, tmap))
    return layouts

typedef_map = {}

def sexp (ob):
    if type(ob) is list or type(ob) is tuple:
        return '(' + ' '.join ([sexp(x) for x in ob]) + ')'
    elif type(ob) is int:
        return str(ob)
    else:
        if tmap.has_key (ob):
            typedef_map[ob] = resolve_type (ob, tmap)
            return ob
        else:
            return ob

def quote (s):
    return '"' + s + '"'

def emit_ffi (iface, args):
    global tmap
    path = '%s_iface.c' % (iface['name'],)
    build_clang_input (path, iface)
    tmap = find_typedefs (path, args)
    layouts = parse_record_layouts (
        dump_record_layouts (path, args),
        tmap
    )
    constants = find_constants (path, iface, args)
    path = '%s_ffi.scm' % (iface['name'],)
    uname = os.uname()
    plat = uname[0]
    mach = uname[4]
    f = open (path, 'wb')
    f.write (';; -*- Mode: irken -*-\n\n')
    f.write (';; -- do not edit: auto-generated from interface \'%s\'\n' % (iface['path'],))
    f.write (';; --   platform: %s  machine: %s\n\n' % (plat, mach))
    # emit includes
    f.write ('(includes %s)\n' % (' '.join ([quote(x) for x in iface['includes']])))
    # emit struct/union defs
    for layout in layouts:
        if len(layout[0]) == 1:
            # ignore anonymous structs/unions
            continue
        kind, name = layout[0]
        size, align = layout[-1]
        f.write ('(%s %s %s\n' % (kind, name, size))
        for slot in layout[1:-1]:
            offset, indent, t = slot
            if indent == 2:
                # pay attention only to the outermost level.
                field_name, t = t
                f.write ('  (%s %s %s)\n' % (
                    offset, field_name, sexp(t)
                    ))
        f.write ('  )\n')
    # emit sigs.
    for sig in iface['sigs']:
        if len(sig) == 3:
            name, argtypes, rtype = sig
            f.write ('(sig %s (%s -> %s))\n' % (
                name,
                ' '.join ([sexp(x) for x in argtypes]),
                sexp(rtype)
                ))
        else:
            name, obtype = sig
            f.write ('(sig %s %s)\n' % (name, sexp(obtype)))
    for (name, val) in constants:
        f.write ('(con %s %s)\n' % (name, val))
    tdefs = typedef_map.items()
    tdefs.sort()
    for t0, t1 in tdefs:
        if int_size_table.has_key (t1):
            f.write ('(tdef %s %s)\n' % (
                sexp(t0), sexp (('int', int_size_table[t1]))
            ))
        else:
            raise ValueError ("opaque type? %r %r" % (t0, t1))
    f.close()
    print 'wrote %s' % (path,)

def read_spec (path):
    spec = sexp_reader (open (path, 'rb').read()).read()
    name = spec[0]
    includes = []
    structs = []
    sigs = []
    constants = []
    for part in spec[1:]:
        if part[0] == 'includes':
            includes.extend (part[1:])
        elif part[0] == 'structs':
            structs.extend (part[1:])
        elif part[0] == 'sigs':
            sigs.extend (part[1:])
        elif part[0] == 'constants':
            constants.extend (part[1:])

    def parse_spectype (t):
        if type(t) is str:
            return t
        elif t[0] == '*':
            return ['*', parse_spectype (t[1])]
        elif t[0] == 'array':
            return ['array', parse_spectype (t[2]), [int(t[1])]]
        elif t[0] == 'other':
            W ('other type: %r\n' % (t[1],))
            return t
        elif t[0] in ('struct', 'union'):
            return t
        else:
            raise ValueError(t)

    r = {}
    r['path'] = path
    r['name'] = name
    r['includes'] = includes
    r['structs'] = structs
    r['constants'] = constants
    r['includes'] = includes
    r['sigs'] = []
    for sig in sigs:
        name = sig[0]
        if len(sig) > 2 and sig[-2] == '->':
            argtypes = map (parse_spectype, sig[1:-2])
            rtype = parse_spectype (sig[-1])
            r['sigs'].append ((name, argtypes, rtype))
        else:
            assert len(sig) == 2
            r['sigs'].append ((name, parse_spectype (sig[1])))
    return r

int_size_table = build_int_size_table()

import argparse

def main():
    p = argparse.ArgumentParser()
    p.add_argument ('--clang', help="path to clang", default='clang')
    p.add_argument ('--include', '-I', help="add include path for clang", action='append')
    p.add_argument ('file', help="FFI spec file", metavar="FILE", action='append')
    args = p.parse_args()

    incs = []
    for path in args.include:
        incs.append ('-I%s' % (path,))
    incs = ' '.join (incs)
    args.incs = incs

    for ffi in args.file:
        iface = read_spec (ffi)
        emit_ffi (iface, args)

main()
