// -*- Mode: C -*-

#include "header1.c"

#include "pxll.h"
#include <stdio.h>
#include "rdtsc.h"
#include <sys/utsname.h>
#include <dlfcn.h>

#ifdef __APPLE__
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif

char * op_names[] = {
  "lit", "ret", "add", "sub", "mul", "div", "srem", "shl", "ashr",
  "or", "xor", "and", "eq", "lt", "gt", "le", "ge", "cmp", "tst",
  "jmp", "fun", "tail", "tail0", "env", "stor", "ref", "mov", "epush",
  "trcall", "trcall0", "ref0", "call", "call0", "pop", "printo",
  "prints", "topis", "topref", "topset", "set", "set0", "pop0",
  "epop", "tron", "troff", "gc", "imm", "make", "makei", "exit",
  "nvcase", "tupref", "vlen", "vref", "vset", "vmake", "alloc",
  "rref", "rset", "getcc", "putcc", "irk", "getc", "dlsym", "ffi",
  "smake", "slen", "sref", "sset", "scopy", "unchar", "plat", "gist",
  "argv", "quiet", "heap", "readf", "calloc","cget","cset",
};

static int argc;
static char ** argv;

static object * allocate (pxll_int tc, pxll_int size);
static object * alloc_no_clear (pxll_int tc, pxll_int size);
static object * dump_object (object * ob, int depth);
static void print_object (object * ob);
static object do_gc (int nroots);
static pxll_int get_case (object * ob);
object invoke_closure (object * closure, object * args);
pxll_int magic_cmp (object * a, object * b);

typedef void(*kfun0)(void);

uint64_t gc_ticks;
object * lenv;
object * k;
object * top;
object * limit;
object * freep;
object * result;

// XXX typedefs for vm_lenv, vm_tuple, etc...

void DO (object * x);

#define CHECK(exp)                              \
  do {                                          \
    if ((exp) < 0) {                            \
      return -1;                                \
    }                                           \
  } while (0)

int
next (FILE * f, pxll_int * n)
{
  pxll_int b = fgetc (f);
  if (b == -1) {
    return -1;
  } else {
    *n = b;
    return 0;
  }
}

int
read_int (FILE * f, pxll_int * r)
{
  pxll_int n = 0;
  CHECK (next (f, &n));
  if (n == 254) {
    pxll_int r0;
    CHECK (read_int (f, &r0));
    *r = -r0;
    return 0;
  } else if (n == 255) {
    pxll_int bytes;
    n = 0;
    CHECK (next (f, &bytes));
    for (int i=0; i < bytes; i++) {
      n <<= 8;
      n |= fgetc (f);
    }
    *r = n;
    return 0;
  } else {
    *r = n;
    return 0;
  }
}

int
read_string (FILE * f, void * b, pxll_int n)
{
  size_t r = fread (b, 1, n, f);
  if (r == n) {
    return 0;
  } else {
    return -1;
  }
}

object *
vm_list_cons (object * car, object * cdr)
{
  object * cell = allocate (TC_PAIR, 2);
  cell[1] = car;
  cell[2] = cdr;
  return cell;
}

object * bytecode_literals;
object * vm_field_lookup_table;
pxll_int vm_internal_symbol_counter = 0;
object * vm_internal_symbol_list = PXLL_NIL;

pxll_int
read_literal (FILE * f, object * ob)
{
  pxll_int code = 0;
  pxll_int n = 0;
  CHECK (next (f, &code));
  switch (code) {
  case '+':
    CHECK (read_int (f, &n));
    *ob = BOX_INTEGER(n);
    break;
  case '-':
    CHECK (read_int (f, &n));
    *ob =  BOX_INTEGER(-n);
    break;
  case 'T':
    *ob =  PXLL_TRUE;
    break;
  case 'F':
    *ob =  PXLL_FALSE;      
    break;
  case 'u':
    *ob =  PXLL_UNDEFINED;
    break;
  case 'c':
    CHECK (read_int (f, &n));
    *ob =  TO_CHAR (n);
    break;
  case 'S': {
    CHECK (read_int (f, &n));
    pxll_string * t = (pxll_string *) alloc_no_clear (TC_STRING, string_tuple_length (n));
    t->len = n;
    CHECK (read_string (f, t->data, n));
    *ob = (object *) t;
  }
    break;
  case 'Y': { // symbol
    // first create the string
    CHECK (read_int (f, &n));
    pxll_string * t = (pxll_string *) alloc_no_clear (TC_STRING, string_tuple_length (n));
    t->len = n;
    CHECK (read_string (f, t->data, n));
    // now wrap as a symbol
    object * sym = allocate (TC_SYMBOL, 2);
    sym[1] = t;
    sym[2] = BOX_INTEGER (vm_internal_symbol_counter++);
    // push onto the global list (for lib/symbol.scm)
    vm_internal_symbol_list = vm_list_cons (sym, vm_internal_symbol_list);
    *ob = sym;
  }
    break;
  case 'I': // immediate (e.g., (list:nil))
    CHECK (read_int (f, &n));
    *ob = (object) n;
    break;
  case 'C': { // tuple (e.g., (list:cons ...))
    pxll_int tag;
    pxll_int nargs;
    CHECK (read_int (f, &tag));
    CHECK (read_int (f, &nargs));
    object * ob0 = allocate (tag, nargs);
    for (int i=0; i < nargs; i++) {
      CHECK (read_literal (f, &(ob0[i+1])));
    }
    *ob = ob0;
  }
    break;
  case 'V': { // vector
    pxll_int nargs;
    CHECK (read_int (f, &nargs));
    if (nargs > 0) {
      object * ob0 = allocate (TC_VECTOR, nargs);
      for (int i=0; i < nargs; i++) {
        CHECK (read_literal (f, &(ob0[i+1])));
      }
      *ob = ob0;
    } else {
      *ob = (object) TC_EMPTY_VECTOR;
    }
  }
    break;
  case 'P': { // reference to previous literal
    pxll_int index;
    CHECK (read_int (f, &index));
    *ob = bytecode_literals[index+1];
  }
    break;
  default:
    fprintf (stderr, "bad literal code: %ld\n", code);
    return -1;
  }
  return 0;
}

static
pxll_int
read_literals (FILE * f)
{
  // read the outer vector specially so we can resolve
  //   internal pointer references.
  pxll_int code = 0;
  CHECK (next (f, &code));
  if (code == 'V') {
    pxll_int nlits = 0;
    CHECK (read_int (f, &nlits));
    object * lits0 = allocate (TC_VECTOR, nlits);
    bytecode_literals = lits0;
    for (int i=0; i < nlits; i++) {
      CHECK (read_literal (f, &(lits0[i+1])));
    }
    vm_field_lookup_table = bytecode_literals[nlits];
    return 0;
  } else {
    fprintf (stderr, "bytecode file does not start with vector.\n");
    return -1;
  }
}

// Only small programs fit into a 16-bit bytecode.
//typedef uint16_t bytecode_t;
//#define BYTECODE_MAX UINT16_MAX
typedef int32_t bytecode_t;
#define BYTECODE_MAX INT32_MAX

static bytecode_t * bytecode;

static
pxll_int
read_bytecode (FILE * f)
{
  pxll_int codelen = 0;
  CHECK (read_int (f, &codelen));
  bytecode = (bytecode_t *) malloc (sizeof(bytecode_t) * codelen);
  if (!bytecode) {
    fprintf (stderr, "unable to allocate bytecode array.\n");
    return -1;
  } else {
    for (int i=0; i < codelen; i++) {
      pxll_int code = 0;
      CHECK (read_int (f, &code));
      if (code > BYTECODE_MAX) {
        return -1;
      } else {
        bytecode[i] = code;
      }
    }
    return 0;
  }
}

static
pxll_int
read_magic (FILE * f)
{
  char magic0[] = "IRKVM0";
  char magic1[] = "      ";
  CHECK (read_string (f, magic1, strlen(magic0)));
  if (0 == strncmp (magic0, magic1, strlen(magic0))) {
    return 0;
  } else {
    fprintf (stderr, "not a bytecode file.\n");
    return -1;
  }
}

static
pxll_int
read_bytecode_file (char * path)
{
  FILE * f = fopen (path, "rb");
  if (!f) {
    fprintf (stderr, "unable to open '%s'\n", path);
    return -1;
  } else {
    CHECK (read_magic (f));
    CHECK (read_literals (f));
    CHECK (read_bytecode (f));
    return 0;
  }
}

// XXX can we make these register variables in vm_go?
object * vm_lenv = PXLL_NIL;
object * vm_k = PXLL_NIL;
object * vm_top = PXLL_NIL;
object * vm_result = PXLL_NIL;

// Use the higher, (likely) unused user tags for these.
#define TC_VM_CLOSURE (63<<2)
#define TC_VM_LENV    (62<<2)
#define TC_VM_CONT    (61<<2)

static
void
print_object (object * ob)
{
  if (IMMEDIATE (ob)) {
    dump_object (ob, 0);
  } else if (IS_TYPE (TC_VM_CLOSURE, ob[0])) {
    fprintf (stdout, "<closure @%p>", ob);
  } else if (IS_TYPE (TC_VM_LENV, ob[0])) {
    fprintf (stdout, "<lenv>");
  } else if (IS_TYPE (TC_VM_CONT, ob[0])) {
    fprintf (stdout, "<cont>");
  } else {    
    dump_object (ob, 0);
  }
}

static
void
print_stack (object * k)
{
  // VMCONT := stack lenv pc reg0 reg1 ...
  fprintf (stderr, "{");
  while (k != PXLL_NIL) {
    pxll_int n = GET_TUPLE_LENGTH(k[0]) - 3;
    fprintf (stderr, "%ld.%ld(", UNBOX_INTEGER(k[3]), n);
    for (int i=0; i < n; i++) {
      fprintf (stderr, "%d:", i);
      print_object (k[4+i]);
      fprintf (stderr, " ");
    }
    fprintf (stderr, ") ");
    k = k[1];
  }
  fprintf (stderr, "}\n");
}

void
print_regs (object * vm_regs, int nregs)
{
  fprintf (stdout, "regs: ");
  for (int i=0; i < nregs; i++) {
    print_object (vm_regs[i]);
    fprintf (stdout, " ");
  }
  fprintf (stdout, "\n");
  fflush (stdout);
}

static
object *
vm_varref (pxll_int depth, pxll_int index)
{
  object * lenv = vm_lenv;
  for (int i=0; i < depth; i++) {
    lenv = (object *) lenv[1];
  }
  return lenv[index+2];
}

static
void
vm_varset (pxll_int depth, pxll_int index, object * val)
{
  object * lenv = vm_lenv;
  for (int i=0; i < depth; i++) {
    lenv = (object *) lenv[1];
  }
  lenv[index+2] = val;
}

static
void
vm_push_lenv (object * rib)
{
  rib[1] = vm_lenv;
  vm_lenv = rib;
}


static
pxll_int
vm_get_field_offset (pxll_int index, pxll_int label_code)
{
  object * table = vm_field_lookup_table[index+1];
  pxll_int tlen = GET_TUPLE_LENGTH (*table);
  for (int i=0; i < tlen; i++) {
    if (label_code == UNBOX_INTEGER(table[i+1])) {
      return i;
    }
  }
  fprintf (stderr, "vm_get_field_offset() failed\n");
  abort();
}

object * vm_the_closure = PXLL_NIL;

static
pxll_int
vm_set_closure (object * closure)
{
  vm_the_closure = closure;
  return 0;
}

static
object
vm_gc (int nreg)
{
  uint64_t t0, t1;
  object nwords;

  t0 = rdtsc();
  // copy roots
  heap1[0] = (object) vm_lenv;
  heap1[1] = (object) vm_k;  
  heap1[2] = (object) vm_top;
  heap1[3] = (object) bytecode_literals;
  nwords = do_gc (4 + nreg);
  // replace roots
  vm_lenv = (object *) heap0[0];
  vm_k    = (object *) heap0[1];
  vm_top  = (object *) heap0[2];
  bytecode_literals = (object*) heap0[3];
  // XXX kludge
  pxll_int nlits = GET_TUPLE_LENGTH (*(object*)bytecode_literals);
  vm_field_lookup_table = bytecode_literals[nlits];
  // set new limit
  limit = heap0 + (heap_size - 1024);
  t1 = rdtsc();
  gc_ticks += (t1 - t0);
  return nwords;
}

static
object *
vm_copy_string (char * s)
{
  int slen = strlen (s);
  pxll_string * r = (pxll_string *) alloc_no_clear (TC_STRING, string_tuple_length (slen));
  r->len = slen;
  memcpy (GET_STRING_POINTER (r), s, slen);
  return (object *) r;
}

static
object *
vm_make_argv (void)
{
  // Note: skipping first arg.
  object * r = allocate (TC_VECTOR, argc - 1);
  for (int i=0; i < argc - 1; i++) {
    r[i+1] = vm_copy_string (argv[i+1]);
  }
  return r;
}

static
pxll_int
vm_read_file (pxll_string * path, object ** result)
{
  // zero-terminate path
  char path0[path->len + 1];
  memcpy (path0, path->data, path->len + 1);
  path0[path->len] = '\x00';
  FILE * f = fopen (path0, "rb");
  if (f) {
    *result = PXLL_NIL;
    char data[16384];
    while (1) {
      size_t nbytes = fread (data, 1, sizeof(data), f);
      if (nbytes == 0) {
        break;
      } else {
        pxll_string * part = (pxll_string *) allocate (TC_STRING, string_tuple_length (nbytes));
        memcpy (part->data, data, nbytes);
        part->len = nbytes;
        *result = vm_list_cons ((object*)part, *result);
      }
    }
    fclose (f);
    return 0;
  } else {
    fprintf (stderr, "failed to read file '%s'\n", path0);
    return -1;
  }
}

pxll_int
vm_do_ffi (object * vm_regs, pxll_int pc, pxll_int nargs, object * result) 
{
  // FFI target pfun rtype nargs arg0 ...
  ffi_cif cif;
  ffi_type *args[nargs];
  // libffi requires an extra level of indirection for args, because
  //   not all argument types can fit into a `void *` (or register).
  void * vals[nargs];
  void * pvals[nargs];
  int good = 1;
  for (int i=0; i < nargs; i++) {
    object * ob = vm_regs[bytecode[pc+5+i]];
    switch (get_case (ob)) {
    case TC_INT:
      // Note: we can't use sint here, because it is not a synonym for 'intptr_t',
      //  which is what irken uses for its integer representation.  Specifically,
      //  on amd64 'int' is only 32 bits.
      args[i] = &ffi_type_pointer;
      vals[i] = (void*) UNBOX_INTEGER (ob);
      pvals[i] = vals + i;
      break;
    case TC_BOOL:
      args[i] = &ffi_type_sint;
      vals[i] = (void*) (pxll_int) (ob == PXLL_TRUE);
      pvals[i] = vals + i;
      break;
    case TC_STRING:
      args[i] = &ffi_type_pointer;
      vals[i] = (void*) GET_STRING_POINTER (ob);
      pvals[i] = vals + i;
      break;
    case TC_CHAR:
      args[i] = &ffi_type_uchar;
      vals[i] = (void*) GET_CHAR (ob);
      pvals[i] = vals + i;
      break;
    case TC_BUFFER:
      args[i] = &ffi_type_pointer;
      vals[i] = ob + 1;
      pvals[i] = vals + i;
      break;
    default:
      fprintf (stderr, "illegal argument type:");
      print_object (ob);
      fprintf (stderr, "\n");
      return -1;
      break;
    }
  }
  if (good) {
    ffi_type * rtype;
    // parse rtype
    unsigned char rcode = (unsigned char) GET_CHAR (vm_regs[bytecode[pc+3]]);
    switch (rcode) {
    case 'i':
      rtype = &ffi_type_pointer;
      break;
    case 'p':
    case 's':
      rtype = &ffi_type_pointer;
      break;
    case 'u':
      rtype = &ffi_type_sint;
      break;
    case 'c':
      rtype = &ffi_type_uchar;
      break;
    default:
      fprintf (stderr, "unknown return type\n");
      return -1;
    }
    if (FFI_OK == ffi_prep_cif (&cif, FFI_DEFAULT_ABI, nargs, rtype, args)) {
      void * pfun = (void*)UNBOX_INTEGER (vm_regs[bytecode[pc+2]]);
      ffi_arg rc;
      ffi_call (&cif, pfun, &rc, pvals);
      switch (rcode) {
      case 'i':
        *result = BOX_INTEGER (rc);
        break;
      case 's':
        if ((char*)rc == NULL) {
          // XXX silent error.
          *result = vm_copy_string ("");
        } else {
          *result = vm_copy_string ((char*)rc);
        }
        break;
      case 'p':
        *result = BOX_INTEGER ((pxll_int)rc);
        break;
      case 'u':
        *result = (object *) PXLL_UNDEFINED;
        break;
      case 'c':
        *result = TO_CHAR ((uint8_t)rc);
        break;
      }
      return 0;
    } else {
      fprintf (stderr, "ffi_prep_cif failed\n");
      return -1;
    }
  } else {
    return -1;
  }
}

pxll_int
vm_cget (object ** result, object * src, pxll_int off, pxll_int code)
{
  uint8_t * p;
  switch (get_case (src)) {
  case TC_USEROBJ + 0:
    // cmem:buf
    p = (uint8_t *) ((object *) (src[1]) + 1);
    break;
  case TC_USEROBJ + 1:
    // cmem:ptr
    p = (uint8_t *) (src[1]);
    break;
  default:
    fprintf (stderr, "vm_cget: invalid <src>\n");
    return -1;
  }
  p += off;
  switch (code) {
  case 's':
    *result = vm_copy_string ((char *) p);
    break;
  case '1':
    *result = BOX_INTEGER ((pxll_int)(*((uint8_t *)p)));
    break;
  case '2':
    *result = BOX_INTEGER ((pxll_int)*((uint16_t *)p));
    break;
  case '4':
    *result = BOX_INTEGER ((pxll_int)*((uint32_t *)p));
    break;
  case '8':
    *result = BOX_INTEGER ((pxll_int)*((uint64_t *)p));
    break;
  case 'i':
    *result = BOX_INTEGER ((pxll_int)*((int8_t *)p));
    break;
  case 'j':
    *result = BOX_INTEGER ((pxll_int)*((int16_t *)p));
    break;
  case 'k':
    *result = BOX_INTEGER ((pxll_int)*((int32_t *)p));
    break;
  case 'l':
    *result = BOX_INTEGER ((pxll_int)*((int64_t *)p));
    break;
  case 'p':
    *result = BOX_INTEGER ((pxll_int)*p);
    break;
  case 'c':
    *result = TO_CHAR (*p);
    break;
  default:
    fprintf (stderr, "vm_cget: unknown result code\n");
    return -1;
  }
  return 0;
}

pxll_int
vm_cset (object * dst, pxll_int off, pxll_int code, object * val)
{
  uint8_t * p;
  switch (get_case (dst)) {
  case TC_USEROBJ + 0:
    // cmem:buf
    p = (uint8_t *) ((object *) (dst[1]) + 1);
    break;
  case TC_USEROBJ + 1:
    // cmem:ptr
    p = (uint8_t *) (dst[1]);
    break;
  default:
    fprintf (stderr, "vm_cset: invalid <src>\n");
    return -1;
  }
  p += off;
  switch (code) {
  case 's': {
    pxll_string * s = (pxll_string *) val;
    memcpy (p, s->data, s->len);
  }
    break;
  case '1':
    * ((uint8_t*)p) = (uint8_t) UNBOX_INTEGER (val);
    break;
  case '2':
    * ((uint16_t*)p) = (uint16_t) UNBOX_INTEGER (val);
    break;
  case '4':
    * ((uint32_t*)p) = (uint32_t) UNBOX_INTEGER (val);
    break;
  case '8':
    * ((uint64_t*)p) = (uint64_t) UNBOX_INTEGER (val);
    break;
  case 'i':
    * ((int8_t*)p) = (int8_t) UNBOX_INTEGER (val);
    break;
  case 'j':
    * ((int16_t*)p) = (int16_t) UNBOX_INTEGER (val);
    break;
  case 'k':
    * ((int32_t*)p) = (int32_t) UNBOX_INTEGER (val);
    break;
  case 'l':
    * ((int64_t*)p) = (int64_t) UNBOX_INTEGER (val);
    break;
  case 'p':
    * ((pxll_int*)p) = UNBOX_INTEGER (val);
    break;
  default:
    fprintf (stderr, "vm_cset: unknown result code\n");
    return -1;
  }
  return 0;
}

#define NREGS 20
#define BC1 code[pc+1]
#define BC2 code[pc+2]
#define BC3 code[pc+3]
#define BC4 code[pc+4]
#define BC5 code[pc+5]

#define REG1 vm_regs[BC1]
#define REG2 vm_regs[BC2]
#define REG3 vm_regs[BC3]
#define REG4 vm_regs[BC4]
#define REG5 vm_regs[BC5]

// XXX consider splitting the bytecodes into opcodes and prims.  this
//   will mean an extra word in all prim invocations, but should keep
//   the core VM loop small enough to fit in the insn cache. maybe.
//   another possibility: since the insn stream is (currently) 16-bit,
//   we could simply encode prims in the higher byte?
//   this would also be a cleaner way of handling %%cexp stuff, so
//   %%cexp can only work with prims.

object
vm_go (void)
{
  register pxll_int pc = 0;
  register object * vm_regs[NREGS];
  register bytecode_t * code = bytecode;
  for (int i=0; i < NREGS; i++) {
    vm_regs[i] = PXLL_NIL;
  }

  // using direct threading results in a 37% speedup over switch-based dispatch.
  // for more info see:
  // https://en.wikipedia.org/wiki/Threaded_code
  // http://eli.thegreenplace.net/2012/07/12/computed-goto-for-efficient-dispatch-tables

  static void* dispatch_table[] = {
    &&l_lit, &&l_ret, &&l_add, &&l_sub, &&l_mul, &&l_div, &&l_srem,
    &&l_shl, &&l_ashr, &&l_or, &&l_xor, &&l_and, &&l_eq, &&l_lt,
    &&l_gt, &&l_le, &&l_ge, &&l_cmp, &&l_tst, &&l_jmp, &&l_fun,
    &&l_tail, &&l_tail0, &&l_env, &&l_stor, &&l_ref, &&l_mov,
    &&l_epush, &&l_trcall, &&l_trcall0, &&l_ref0, &&l_call, &&l_call0,
    &&l_pop, &&l_printo, &&l_prints, &&l_topis, &&l_topref,
    &&l_topset, &&l_set, &&l_set0, &&l_pop0, &&l_epop, &&l_tron,
    &&l_troff, &&l_gc, &&l_imm, &&l_make, &&l_makei, &&l_exit,
    &&l_nvcase, &&l_tupref, &&l_vlen, &&l_vref, &&l_vset, &&l_vmake,
    &&l_alloc, &&l_rref, &&l_rset, &&l_getcc, &&l_putcc, &&l_irk,
    &&l_getc, &&l_dlsym, &&l_ffi, &&l_smake, &&l_sfromc, &&l_slen,
    &&l_sref, &&l_sset, &&l_scopy, &&l_unchar, &&l_plat, &&l_gist,
    &&l_argv, &&l_quiet, &&l_heap, &&l_readf, &&l_calloc, &&l_cget,
    &&l_cset,
  };

  // XXX what happens when the opcode is out of range? (segfault)
#define DISPATCH() goto *dispatch_table[code[pc]]

  // print_regs ((object*)vm_regs, 10);
  // print_stack (vm_k);
  // fprintf (stderr, "--- %ld %s ", pc, op_names[code[pc]]);
  // for (int i=0; i < 4; i++) {
  //  fprintf (stderr, "%d ", code[pc+1+i]);
  // }
  // fprintf (stderr, "\n");
  DISPATCH();

 l_lit:
  REG1 = bytecode_literals[BC2+1];
  pc += 3;
  DISPATCH();
 l_ret:
  vm_result = REG1;
  if (vm_k == PXLL_NIL) {
    pc += 1;
    return vm_result;
  } else {
    // VMCONT := stack lenv pc reg0 reg1 ...
    pc = UNBOX_INTEGER (vm_k[3]);
  }
  DISPATCH();
 l_add:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) + UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_sub:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) - UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_mul:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) * UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_div:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) / UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_srem:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) % UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_shl:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) << UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_ashr:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) >> UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_or:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) | UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_xor:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) ^ UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_and:
  REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) & UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_eq:
  REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) == UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_lt:
  REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) < UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_gt:
  REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) > UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_le:
  REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) <= UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_ge:
  REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) >= UNBOX_INTEGER (REG3));
  pc += 4;
  DISPATCH();
 l_cmp:
  // CMP target a b
  // note: magic_cmp returns -1|0|+1, we adjust that to UITAG 0|1|2
  //   to match the 'cmp' datatype from core.scm.
  REG1 = (object*) UITAG (1 + magic_cmp (REG2, REG3));
  pc += 4;
  DISPATCH();
 l_tst:
  if (REG1 == PXLL_TRUE) {
    pc += 3;
  } else {
    pc += BC2;
  }
  DISPATCH();
 l_jmp:
  pc += BC1;
  DISPATCH();
 l_fun: {
    // FUN target pc
    // closure := {uN lits code pc lenv}
    // 252 := max pointer type tag (temp)
    // fprintf (stderr, "fun target=%d pc=%d\n", BC1, BC2);
    object * closure = allocate (TC_VM_CLOSURE, 4);
    // temp: lits and code are ignored
    closure[1] = PXLL_NIL;
    closure[2] = PXLL_NIL;
    closure[3] = BOX_INTEGER (pc + 3);
    closure[4] = vm_lenv;
    REG1 = closure;
    pc += BC2;
  }
  DISPATCH();
 l_tail: {
    // TAIL closure args
    // closure:= lits code pc lenv
    // link args into lenv.
    object * rib = REG2;
    rib[1] = REG1[4];
    vm_lenv = rib;
    pc = UNBOX_INTEGER (REG1[3]);
  }
  DISPATCH();
 l_tail0:
  // TAIL0 closure
  vm_lenv = REG1[4];
  pc = UNBOX_INTEGER (REG1[3]);
  DISPATCH();
 l_env:
  // ENV <target> <size>
  REG1 = allocate (TC_VM_LENV, BC2+1);
  pc += 3;
  DISPATCH();
 l_stor:
  // STOR tuple index arg
  REG1[BC2+1] = REG3;
  pc += 4;
  DISPATCH();
 l_ref:
  // REF <target> <depth> <index>
  REG1 = vm_varref (BC2, BC3);
  pc += 4;
  DISPATCH();
 l_mov:
  REG1 = REG2;
  pc += 3;
  DISPATCH();
 l_epush: {
    // EPUSH args
    object * rib = REG1;
    // lenv := next arg0 arg1 ...
    rib[1] = vm_lenv;
    vm_lenv = rib;
    pc += 2;
  }
  DISPATCH();
 l_trcall: {
    // TRCALL pc depth nregs reg0 ...
    pxll_int depth = BC2;
    for (int i=0; i < depth; i++) {
      vm_lenv = (object *) vm_lenv[1];
    }
    pxll_int nregs = BC3;
    object * rib = (object *) vm_lenv;
    // lenv := next arg0 arg1 ...
    for (int i=0; i < nregs; i++) {
      rib[i+2] = vm_regs[code[pc+4+i]];
    }
    pc += BC1;
  }
  DISPATCH();
 l_trcall0: {
    // TRCALL0 pc depth
    pxll_int depth = BC2;
    for (int i=0; i < depth; i++) {
      vm_lenv = (object *) vm_lenv[1];
    }
    pc += BC1;
  }
  DISPATCH();
 l_ref0:
  // REF0 target index
  REG1 = vm_lenv[BC2+2];
  pc += 3;
  DISPATCH();
 l_call: {
    // CALL closure args nregs
    // VMCONT := stack lenv pc reg0 reg1 ...
    pxll_int nregs = BC3;
    object * k = allocate (TC_VM_CONT, 3 + nregs);
    k[1] = vm_k;
    k[2] = vm_lenv;
    k[3] = BOX_INTEGER (pc + 4);
    for (int i=0; i < nregs; i++) {
      k[4+i] = vm_regs[i];
    }
    vm_k = k;
    // CLOSURE := lits code pc lenv
    object * closure = REG1;
    object * rib = REG2;
    rib[1] = closure[4];
    vm_lenv = rib;
    // vm_lits = closure[1];
    // vm_code = closure[2];
    pc = UNBOX_INTEGER (closure[3]);
  }
  DISPATCH();
 l_call0: {
    // CALL0 closure nregs
    // VMCONT := stack lenv pc reg0 reg1 ...
    pxll_int nregs = BC2;
    object * k = allocate (TC_VM_CONT, 3 + nregs);
    k[1] = vm_k;
    k[2] = vm_lenv;
    k[3] = BOX_INTEGER (pc + 3);
    for (int i=0; i < nregs; i++) {
      k[4+i] = vm_regs[i];
    }
    vm_k = k;
    // CLOSURE := lits code pc lenv
    object * closure = REG1;
    vm_lenv = closure[4];
    // vm_lits = closure[1];
    // vm_code = closure[2];
    pc = UNBOX_INTEGER (closure[3]);
  }
  DISPATCH();
 l_pop: {
    // POP target
    // VMCONT := stack lenv pc reg0 reg1 ...
    pxll_int nregs = GET_TUPLE_LENGTH (vm_k[0]) - 3;
    for (int i=0; i < nregs; i++) {
      vm_regs[i] = vm_k[4+i];
    }
    vm_lenv = vm_k[2];
    vm_k = vm_k[1];
    REG1 = vm_result;
    pc += 2;
  }
  DISPATCH();
 l_pop0: {
    pxll_int nregs = GET_TUPLE_LENGTH (vm_k[0]) - 3;
    for (int i=0; i < nregs; i++) {
      vm_regs[i] = vm_k[4+i];
    }
    vm_lenv = vm_k[2];
    vm_k = vm_k[1];
    pc += 1;
  }
  DISPATCH();
 l_printo:
  // PRINTO arg
  print_object (REG1);
  pc += 2;
  DISPATCH();
 l_prints: {
    // PRINTS arg
    pxll_string * s = (pxll_string *) REG1;
    fwrite (s->data, 1, s->len, stdout);
    pc += 2;
  }
  DISPATCH();
 l_topis:
  // TOPIS <env>
  vm_top = (object *) REG1;
  pc += 2;
  DISPATCH();
 l_topref:
  // TOPREF target index
  REG1 = vm_top[BC2+2];
  pc += 3;
  DISPATCH();
 l_topset:
  // TOPSET index val
  vm_top[BC1+2] = REG2;
  pc += 3;
  DISPATCH();
 l_set:
  // SET depth index val
  vm_varset (BC1, BC2, REG3);
  pc += 4;
  DISPATCH();
 l_set0:
  // SET0 index val
  vm_varset (0, BC1, REG2);
  pc += 3;
  DISPATCH();
 l_epop:
  // EPOP
  // lenv := next val0 val1 ...
  vm_lenv = vm_lenv[1];
  pc += 1;
  DISPATCH();
 l_tron:
  // NYI
  pc += 1;
  DISPATCH();
 l_troff:
  // NYI
  pc += 1;
  DISPATCH();
 l_gc:
  if (freep >= limit) {
    vm_gc(0);
  }
  pc += 1;
  DISPATCH();
 l_imm:
  // IMM target tag
  REG1 = (object *) (pxll_int) BC2;
  pc += 3;
  DISPATCH();
 l_make: {
    // MAKE target tag nelem elem0 ...
    pxll_int nelem = BC3;
    object * ob = allocate (BC2, nelem);
    for (int i=0; i < nelem; i++) {
      ob[i+1] = vm_regs[code[pc+4+i]];
    }
    REG1 = ob;
    pc += 4 + nelem;
  }
  DISPATCH();
 l_makei:
  // MAKEI target tag payload
  REG1 = (object*)((UNBOX_INTEGER(REG3)<<8) | (UNBOX_INTEGER(REG2) & 0xff));
  pc += 4;
  DISPATCH();
 l_exit:
  vm_result = REG1;
  return vm_result;
 l_nvcase: {
    // NVCASE ob elabel nalts tag0 label0 tag1 label1 ...
    pxll_int tag = get_case (REG1);
    pxll_int nalts = BC3;
    pxll_int pc0 = BC2;
    //fprintf (stderr, " tag=%d nalts=%d pc=%d\n", tag, nalts, pc);
    for (int i=0; i < nalts; i++) {
      //fprintf (stderr, "  testing %d\n", code[pc+4+(i*2)]);
      if (tag == code[pc+4+(i*2)]) {
        pc0 = code[pc+4+(i*2)+1];
        break;
      }
    }
    pc += pc0;
  }
  DISPATCH();
 l_tupref:
  // TUPREF target ob index
  REG1 = REG2[BC3+1];
  pc += 4;
  DISPATCH();
 l_vlen:
  // VLEN target vec
  if (REG2 == (object*) TC_EMPTY_VECTOR) {
    REG1 = BOX_INTEGER (0);
  } else {
    REG1 = BOX_INTEGER (GET_TUPLE_LENGTH (*REG2));
  }
  pc += 3;
  DISPATCH();
 l_vref:
  // VREF target vec index-reg
  REG1 = REG2[UNBOX_INTEGER(REG3)+1];
  pc += 4;
  DISPATCH();
 l_vset:
  // VSET vec index-reg val
  REG1[UNBOX_INTEGER(REG2)+1] = REG3;
  pc += 4;
  DISPATCH();
 l_vmake: {
    // VMAKE target size val
    // XXX heap check.
    pxll_int nelems = UNBOX_INTEGER(REG2);
    if (nelems == 0) {
      REG1 = (object *) TC_EMPTY_VECTOR;
    } else {
      object * ob = alloc_no_clear (TC_VECTOR, nelems);
      for (int i=0; i < nelems; i++) {
        ob[i+1] = REG3;
      }
      REG1 = ob;
    }
    pc += 4;
  }
  DISPATCH();
 l_alloc:
  // ALLOC <target> <tag> <size>
  REG1 = allocate (BC2, BC3);
  pc += 4;
  DISPATCH();
 l_rref: {
    // RREF target rec label-code
    pxll_int tag = (GET_TYPECODE (REG2[0]) - TC_USEROBJ) >> 2;
    pxll_int index = vm_get_field_offset (tag, BC3);
    REG1 = REG2[index+1];
    pc += 4;
  }
  DISPATCH();
 l_rset: {
    // RSET rec label-code val
    pxll_int tag = (GET_TYPECODE (REG1[0]) - TC_USEROBJ) >> 2;
    pxll_int index = vm_get_field_offset (tag, BC2);
    REG1[index+1] = REG3;
    pc += 4;
  }
  DISPATCH();
 l_getcc:
  // GETCC target
  REG1 = vm_k;
  pc += 2;
  DISPATCH();
 l_putcc:
  // PUTCC target k v
  vm_k = REG2;
  REG1 = REG3;
  pc += 4;
  DISPATCH();
 l_irk: {
    fprintf (stderr, "NOT\n");
    abort();
    // IRK target closure nargs arg0 ...
    pxll_int nargs = UNBOX_INTEGER (REG3);
    object * rib = allocate (TC_ENV, nargs + 1);
    object * closure = REG2;
    for (int i=0; i < nargs; i++) {
      rib[i+2] = vm_regs[code[pc+4+i]];
    }
    invoke_closure (closure, rib);
    REG1 = result;
    pc += 4 + nargs;
  }
  DISPATCH();
 l_getc:
  // GETC target
  REG1 = vm_the_closure;
  pc += 2;
  DISPATCH();
 l_dlsym:
  // DLSYM target name
  REG1 = BOX_INTEGER ((pxll_int)dlsym (RTLD_DEFAULT, GET_STRING_POINTER (REG2)));
  pc += 3;
  DISPATCH();
 l_ffi: {
    // FFI target pfun rtype nargs arg0 ...
    pxll_int nargs = UNBOX_INTEGER (REG4);
    // XXX concerned that passing vm_regs defeats the register decl above.
    object result;
    pxll_int success = vm_do_ffi ((object *) vm_regs, pc, nargs, &result);
    if (success == 0) {
      REG1 = result;
    } else {
      fprintf (stderr, "op_ffi failed\n");
      return BOX_INTEGER ((unsigned)-1);
    }
    pc += nargs + 5;
  }
  DISPATCH();
 l_smake: {
    // SMAKE target size
    // XXX heap check.
    pxll_int slen = UNBOX_INTEGER (REG2);
    pxll_string * s = (pxll_string*)alloc_no_clear (TC_STRING, string_tuple_length (slen));
    s->len = slen;
    REG1 = (object*)s;
  }
  pc += 3;
  DISPATCH();
 l_sfromc:
    // SFROMC target src
  REG1 = vm_copy_string ((char*)UNBOX_INTEGER (REG2));
  pc += 3;
  DISPATCH();
 l_slen:
  // SLEN target string
  REG1 = BOX_INTEGER ((pxll_int)((pxll_string *) REG2)->len);
  pc += 3;
  DISPATCH();
 l_sref: {
    // SREF target string index
    pxll_string * s = (pxll_string *)REG2;
    pxll_int index = UNBOX_INTEGER (REG3);
    if ((index >= 0) && (index < s->len)) {
      REG1 = TO_CHAR ((uint8_t)(s->data[index]));
    } else {
      // XXX error handler
      fprintf (stderr, "string ref out of range: %ld %d\n", index, s->len);
      return BOX_INTEGER ((unsigned)-1);
    }
    pc += 4;
  }
  DISPATCH();
 l_sset: {
    // SSET string index char
    pxll_string * s = (pxll_string *)REG1;
    pxll_int index = UNBOX_INTEGER (REG2);
    pxll_int ch = GET_CHAR (REG3);
    if (ch > 255) {
      fprintf (stderr, "char out of range: %ld\n", ch);
      return BOX_INTEGER ((unsigned)-1);
    } else if ((index >= 0) && (index < s->len)) {
      s->data[index] = (char) ch;
    } else {
      // XXX error handler
      fprintf (stderr, "string set out of range: %ld %d\n", index, s->len);
      return BOX_INTEGER ((unsigned)-1);
    }
    pc += 4;
  }
  DISPATCH();
 l_scopy: {
    // SCOPY src sstart n dst dstart
    pxll_string * src = (pxll_string *) REG1;
    pxll_string * dst = (pxll_string *) REG4;
    pxll_int sstart = UNBOX_INTEGER (REG2);
    pxll_int dstart = UNBOX_INTEGER (REG5);
    pxll_int n = UNBOX_INTEGER (REG3);
    // range check
    if ((sstart >= 0) && (sstart + n <= src->len) &&
        (dstart >= 0) && (dstart + n <= dst->len)) {
      memcpy (dst->data + dstart, src->data + sstart, n);
    } else {
      // XXX error handling
      fprintf (stderr, "scopy out of range\n");
      return BOX_INTEGER ((unsigned)-1);
    }
    pc += 6;
  }
  DISPATCH();
 l_unchar:
  // UNCHAR target char
  REG1 = (object*) BOX_INTEGER ((uintptr_t)GET_CHAR (REG2));
  pc += 3;
  DISPATCH();
 l_plat: {
    // PLAT target
    struct utsname plat;
    if (0 == uname (&plat)) {
      REG1 = vm_list_cons (
        vm_copy_string (plat.sysname),
        vm_list_cons (
          vm_copy_string (plat.nodename),
          vm_list_cons (
            vm_copy_string (plat.release),
            vm_list_cons (
              vm_copy_string (plat.version),
              vm_list_cons (
                vm_copy_string (plat.machine), 
                PXLL_NIL
              )))));
    } else {
      // XXX error handler
      REG1 = PXLL_NIL;
    }
    pc += 2;
  }
  DISPATCH();
 l_gist:
  // GIST target
  REG1 = vm_internal_symbol_list;
  pc += 2;
  DISPATCH();
 l_argv:
  // ARGV target
  REG1 = vm_make_argv();
  pc += 2;
  DISPATCH();
 l_quiet:
  // QUIET yesno
  verbose_gc = (REG1 == PXLL_TRUE);
  pc += 2;
  DISPATCH();
 l_heap: {
    // HEAP size nreg
    pxll_int size = UNBOX_INTEGER (REG1);
    if (freep + size >= limit) {
      pxll_int nreg = BC2;
      for (int i=0; i < nreg; i++) {
        heap1[4+i] = vm_regs[i];
      }
      vm_gc (nreg);
      for (int i=0; i < nreg; i++) {
        vm_regs[i] = heap0[4+i];
      }
    }
  }
  pc += 3;
  DISPATCH();
 l_readf: {
    // READF target path
    object * slist;
    pxll_int r = vm_read_file ((pxll_string *) REG2, &slist);
    if (r == 0) {
      REG1 = slist;
      pc += 3;
      DISPATCH();
    } else {
      return BOX_INTEGER ((unsigned)-1);
    }
  }
 l_calloc: {
    // CALLOC target size kind
    object * result;
    // cmem: buf = 0, ptr = 1
    pxll_int kind = UNBOX_INTEGER (REG3);
    result = allocate (TC_USEROBJ + kind, 1);
    if (kind == 0) {
      result[1] = allocate (TC_BUFFER, HOW_MANY (UNBOX_INTEGER (REG2), sizeof(object)));
    } else {
      object * ptr = malloc ((size_t) UNBOX_INTEGER (REG2));
      if (ptr) {
        result[1] = ptr;
      } else {
        fprintf (stderr, "malloc failed.\n");
        return BOX_INTEGER ((unsigned)-1);
      }
    }
    REG1 = result;
    pc += 3;
    DISPATCH();
  }
 l_cget: {
    // CGET target src off code
    object * result;
    pxll_int r = vm_cget (&result, REG2, UNBOX_INTEGER (REG3), GET_CHAR (REG4));
    if (r == 0) {
      REG1 = result;
      pc += 5;
      DISPATCH();
    } else {
      fprintf (stderr, "vm_cget failed.\n");
      return BOX_INTEGER ((unsigned)-1);
    }
  }
 l_cset: {
    // CSET dst off code val
    pxll_int r = vm_cset (REG1, UNBOX_INTEGER (REG2), GET_CHAR (REG3), REG4);
    if (r == 0) {
      pc += 5;
      DISPATCH();
    } else {
      fprintf (stderr, "vm_cset failed.\n");
      return BOX_INTEGER ((unsigned)-1);
    }
  }
}

void 
toplevel (void) {
  if (argc < 2) {
    fprintf (stderr, "Usage: %s <bytecode-file> <arg0> <arg1> ...\n", argv[0]);
  } else if (-1 == read_bytecode_file (argv[1])) {
    fprintf (stderr, "failed to read bytecode file: %s\n", argv[1]);
  } else {
    object * result = vm_go();
    print_object (result);
    fprintf (stdout, "\n");
    if (is_int (result)) {
      exit ((int)(intptr_t)UNBOX_INTEGER(result));
    } else {
      exit (0);
    }
  }
}

static int lookup_field (int tag, int label) {return 0;}
static void prof_dump() { }
