// -*- Mode: C -*-

// XXX just like in irken proper, collapse TC_VM_TUPLE and TC_VM_LENV

#include "pxll.h"
#include <stdio.h>
#include "rdtsc.h"
#include <dlfcn.h>
#include <sys/utsname.h>

#ifdef __APPLE__
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif

typedef enum {
  op_lit,
  op_ret,
  op_add,
  op_sub,
  op_mul,
  op_div,
  op_srem,
  op_shl,
  op_ashr,
  op_or,
  op_xor,
  op_and,
  op_eq,
  op_lt,
  op_gt,
  op_le,
  op_ge,
  op_cmp,
  op_tst,
  op_jmp,
  op_fun,
  op_tail,
  op_tail0,
  op_env,
  op_stor,
  op_ref,
  op_mov,
  op_epush,
  op_trcall,
  op_trcall0,
  op_ref0,
  op_call,
  op_call0,
  op_pop,
  op_printo,
  op_prints,
  op_topis,
  op_topref,
  op_topset,
  op_set,
  op_pop0,
  op_epop,
  op_tron,
  op_troff,
  op_gc,
  op_imm,
  op_make,
  op_makei,
  op_exit,
  op_nvcase,
  op_tupref,
  op_vlen,
  op_vref,
  op_vset,
  op_vmake,
  op_alloc,
  op_rref,
  op_rset,
  op_getcc,
  op_putcc,
  op_irk,
  op_getc,
  op_dlsym,
  op_ffi,
  op_smake,
  op_slen,
  op_sref,
  op_sset,
  op_scopy,
  op_unchar,
  op_plat,
  op_gist,
  op_heap,
} opcode_t;

char * op_names[] = {
  "lit",
  "ret",
  "add",
  "sub",
  "mul",
  "div",
  "srem",
  "shl",
  "ashr",
  "or",
  "xor",
  "and",
  "eq",
  "lt",
  "gt",
  "le",
  "ge",
  "cmp",
  "tst",
  "jmp",
  "fun",
  "tail",
  "tail0",
  "env",
  "stor",
  "ref",
  "mov",
  "epush",
  "trcall",
  "trcall0",
  "ref0",
  "call",
  "call0",
  "pop",
  "printo",
  "prints",
  "topis",
  "topref",
  "topset",
  "set",
  "pop0",
  "epop",
  "tron",
  "troff",
  "gc",
  "imm",
  "alloc",
  "exit",
  "nvcase",
  "tupref",
  "vlen",
  "vref",
  "vset",
  "vmake",
  "alloc",
  "rref",
  "rset",
  "getcc",
  "putcc",
  "irk",
  "getc",
  "dlsym",
  "ffi",
  "smake",
  "slen",
  "sref",
  "sset",
  "scopy",
  "unchar",
  "plat",
  "gist",
  "heap",
};

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
  if (n == 255) {
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
  object lits0;
  CHECK (read_literal (f, &lits0));
  // fprintf (stdout, "v0 ");
  // print_object (lits0);
  // fprintf (stdout, "\n");
  bytecode_literals = lits0;
  // the last literal is the field lookup table.
  pxll_int nlits = GET_TUPLE_LENGTH (*(object*)lits0);
  vm_field_lookup_table = bytecode_literals[nlits];
  return 0;
}

typedef uint16_t bytecode_t;
#define BYTECODE_MAX UINT16_MAX

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
    // fprintf (stderr, "read %ld bytecodes. (eof? %d)\n", codelen, feof (f));
    // fprintf (stderr, "ftell = %ld\n", ftell (f));
    return 0;
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
#define TC_VM_TUPLE   (62<<2)
#define TC_VM_LENV    (61<<2)
#define TC_VM_CONT    (60<<2)

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
  } else if (IS_TYPE (TC_VM_TUPLE, ob[0])) {
    fprintf (stdout, "{");
    for (int i=0; i < GET_TUPLE_LENGTH (ob[0]); i++) {
      print_object (ob[i+1]);
      fprintf (stdout, " ");
    }
    fprintf (stdout, "}");
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

void
print_lenv()
{
  object * lenv = vm_lenv;
  fprintf (stdout, "lenv: [");
  fflush (stdout);
  while (lenv != PXLL_NIL) {
    object * rib = (object *)lenv[1];
    print_object (rib);
    lenv = lenv[2];
  }
  fprintf (stdout, "]\n");
}

static
object *
vm_varref (pxll_int depth, pxll_int index)
{
  object * lenv = vm_lenv;
  for (int i=0; i < depth; i++) {
    lenv = (object *) lenv[2];
  }
  return ((object*)lenv[1])[index+1];
}

static
void
vm_varset (pxll_int depth, pxll_int index, object * val)
{
  object * lenv = vm_lenv;
  for (int i=0; i < depth; i++) {
    lenv = (object *) lenv[2];
  }
  ((object*)lenv[1])[index+1] = val;
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

pxll_int
vm_set_closure (object * closure)
{
  vm_the_closure = closure;
  return 0;
}

object
vm_gc (void)
{
  uint64_t t0, t1;
  object nwords;

  t0 = rdtsc();
  // copy roots
  heap1[0] = (object) lenv;
  heap1[1] = (object) k;
  heap1[2] = (object) top;
  heap1[3] = (object) vm_lenv;
  heap1[4] = (object) vm_k;  
  heap1[5] = (object) vm_top;
  nwords = do_gc (6);
  // replace roots
  lenv    = (object *) heap0[0];
  k       = (object *) heap0[1];
  top     = (object *) heap0[2];
  vm_lenv = (object *) heap0[3];
  vm_k    = (object *) heap0[4];
  vm_top  = (object *) heap0[5];
  // set new limit
  limit = heap0 + (heap_size - 1024);
  t1 = rdtsc();
  gc_ticks += (t1 - t0);
  return nwords;
}

object *
vm_copy_string (char * s)
{
  int slen = strlen (s);
  pxll_string * r = (pxll_string *) alloc_no_clear (TC_STRING, string_tuple_length (slen));
  r->len = slen;
  memcpy (GET_STRING_POINTER (r), s, slen);
  return (object *) r;
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
      args[i] = &ffi_type_sint;
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
      rtype = &ffi_type_sint;
      break;
    case 'p':
    case 's':
      rtype = &ffi_type_pointer;
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
        *result = vm_copy_string ((char*)rc);
        break;
      case 'p':
        *result = BOX_INTEGER ((pxll_int)rc);
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
  int done = 0;
  for (int i=0; i < NREGS; i++) {
    vm_regs[i] = PXLL_NIL;
  }
  while (!done) {
    // print_lenv();
    // print_regs ((object*)vm_regs, 10);
    // print_stack (vm_k);
    // fprintf (stderr, "--- %ld %s ", pc, op_names[code[pc]]);
    // for (int i=0; i < 4; i++) {
    //  fprintf (stderr, "%d ", code[pc+1+i]);
    // }
    // fprintf (stderr, "\n");
    switch (code[pc]) {
    case op_lit:
      REG1 = bytecode_literals[BC2+1];
      pc += 3;
      break;
    case op_ret:
      vm_result = REG1;
      if (vm_k == PXLL_NIL) {
        pc += 1;
        done = 1;
      } else {
        // VMCONT := stack lenv pc reg0 reg1 ...
        pc = UNBOX_INTEGER (vm_k[3]);
      }
      break;
    case op_add:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) + UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_sub:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) - UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_mul:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) * UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_div:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) / UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_srem:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) % UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_shl:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) << UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_ashr:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) >> UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_or:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) | UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_xor:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) ^ UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_and:
      REG1 = BOX_INTEGER (UNBOX_INTEGER (REG2) & UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_eq:
      REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) == UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_lt:
      REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) < UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_gt:
      REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) > UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_le:
      REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) <= UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_ge:
      REG1 = PXLL_TEST (UNBOX_INTEGER (REG2) >= UNBOX_INTEGER (REG3));
      pc += 4;
      break;
    case op_cmp:
      // CMP target a b
      // note: magic_cmp returns -1|0|+1, we adjust that to UITAG 0|1|2
      //   to match the 'cmp' datatype from core.scm.
      REG1 = (object*) UITAG (1 + magic_cmp (REG2, REG3));
      pc += 4;
      break;
    case op_tst:
      if (REG1 == PXLL_TRUE) {
        pc += 3;
      } else {
        pc = BC2;
      }
      break;
    case op_jmp:
      pc = BC1;
      break;
    case op_fun: {
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
      pc = BC2;
    }
      break;
    case op_tail: {
      // TAIL closure args
      object * rib = allocate (TC_VM_LENV, 2);
      // env := tuple next
      // closure:= lits code pc lenv
      rib[1] = REG2;
      rib[2] = REG1[4];
      vm_lenv = rib;
      pc = UNBOX_INTEGER (REG1[3]);
    }
      break;
    case op_tail0:
      // TAIL0 closure
      vm_lenv = REG1[4];
      pc = UNBOX_INTEGER (REG1[3]);
      break;
    case op_env:
      // ENV <target> <size>
      REG1 = allocate (TC_VM_TUPLE, BC2);
      pc += 3;
      break;
    case op_stor:
      // STOR tuple index arg
      REG1[BC2+1] = REG3;
      pc += 4;
      break;
    case op_ref:
      // REF <target> <depth> <index>
      REG1 = vm_varref (BC2, BC3);
      pc += 4;
      break;
    case op_mov:
      REG1 = REG2;
      pc += 3;
      break;
    case op_epush: {
      // EPUSH args
      object * rib = allocate (TC_VM_LENV, 2);
      rib[1] = REG1;
      rib[2] = vm_lenv;
      vm_lenv = rib;
      pc += 2;
    }
      break;
    case op_trcall: {
      // TRCALL pc depth nregs reg0 ...
      pxll_int depth = BC2;
      for (int i=0; i < depth; i++) {
        vm_lenv = (object *) vm_lenv[2];
      }
      pxll_int nregs = BC3;
      object * args = (object *) vm_lenv[1];
      for (int i=0; i < nregs; i++) {
        args[i+1] = vm_regs[code[pc+4+i]];
      }
      pc = BC1;
    }
      break;
    case op_trcall0: {
      // TRCALL0 pc depth
      pxll_int depth = BC2;
      for (int i=0; i < depth; i++) {
        vm_lenv = (object *) vm_lenv[2];
      }
      pc = BC1;
    }
      break;
    case op_ref0:
      // REF0 target index
      REG1 = ((object*)vm_lenv[1])[BC2+1];
      pc += 3;
      break;
    case op_call: {
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
      object * rib = allocate (TC_VM_LENV, 2);
      rib[1] = REG2;
      rib[2] = closure[4];
      vm_lenv = rib;
      // vm_lits = closure[1];
      // vm_code = closure[2];
      pc = UNBOX_INTEGER (closure[3]);
    }
      break;
    case op_call0: {
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
      break;
    case op_pop: {
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
      break;
    case op_pop0: {
      pxll_int nregs = GET_TUPLE_LENGTH (vm_k[0]) - 3;
      for (int i=0; i < nregs; i++) {
        vm_regs[i] = vm_k[4+i];
      }
      vm_lenv = vm_k[2];
      vm_k = vm_k[1];
      pc += 1;
    }
      break;
    case op_printo:
      // PRINTO arg
      print_object (REG1);
      pc += 2;
      break;
    case op_prints: {
      // PRINTS arg
      pxll_string * s = (pxll_string *) REG1;
      fwrite (s->data, 1, s->len, stdout);
      pc += 2;
    }
      break;
    case op_topis:
      // TOPIS <env>
      vm_top = (object *) REG1;
      pc += 2;
      break;
    case op_topref:
      // TOPREF target index
      REG1 = vm_top[BC2+1];
      pc += 3;
      break;
    case op_topset:
      // TOPSET index val
      vm_top[BC1+1] = REG2;
      pc += 3;
      break;
    case op_set:
      // SET depth index val
      vm_varset (BC1, BC2, REG3);
      pc += 4;
      break;
    case op_epop:
      // EPOP
      // lenv := tuple next
      vm_lenv = vm_lenv[2];
      pc += 1;
      break;
    case op_tron:
      // NYI
      pc += 1;
      break;
    case op_troff:
      // NYI
      pc += 1;
      break;
    case op_gc:
      if (freep >= limit) {
        vm_gc();
      }
      pc += 1;
      break;
    case op_imm:
      // IMM target tag
      REG1 = (object *) (pxll_int) BC2;
      pc += 3;
      break;
    case op_make: {
      // MAKE target tag nelem elem0 ...
      pxll_int nelem = BC3;
      object * ob = allocate (BC2, nelem);
      for (int i=0; i < nelem; i++) {
        ob[i+1] = vm_regs[code[pc+4+i]];
      }
      REG1 = ob;
      pc += 4 + nelem;
    }
      break;
    case op_makei:
      // MAKEI target tag payload
      REG1 = (object*)((UNBOX_INTEGER(REG3)<<8) | (UNBOX_INTEGER(REG2) & 0xff));
      pc += 4;
      break;
    case op_exit:
      vm_result = REG1;
      done = 1;
      break;
    case op_nvcase: {
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
      pc = pc0;
    }
      break;
    case op_tupref:
      // TUPREF target ob index
      REG1 = REG2[BC3+1];
      pc += 4;
      break;
    case op_vlen:
      // VLEN target vec
      REG1 = BOX_INTEGER (GET_TUPLE_LENGTH (*REG2));
      pc += 3;
      break;
    case op_vref:
      // VREF target vec index-reg
      REG1 = REG2[UNBOX_INTEGER(REG3)+1];
      pc += 4;
      break;
    case op_vset:
      // VSET vec index-reg val
      REG1[UNBOX_INTEGER(REG2)+1] = REG3;
      pc += 4;
      break;
    case op_vmake: {
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
      break;
    case op_alloc:
      // ALLOC <target> <tag> <size>
      REG1 = allocate (BC2, BC3);
      pc += 4;
      break;
    case op_rref: {
      // RREF target rec label-code
      pxll_int tag = (GET_TYPECODE (REG2[0]) - TC_USEROBJ) >> 2;
      pxll_int index = vm_get_field_offset (tag, BC3);
      REG1 = REG2[index+1];
      pc += 4;
    }
      break;
    case op_rset: {
      // RSET rec label-code val
      pxll_int tag = (GET_TYPECODE (REG1[0]) - TC_USEROBJ) >> 2;
      pxll_int index = vm_get_field_offset (tag, BC2);
      REG1[index+1] = REG3;
      pc += 4;
    }
      break;
    case op_getcc:
      // GETCC target
      REG1 = vm_k;
      pc += 2;
      break;
    case op_putcc:
      // PUTCC target k v
      vm_k = REG2;
      REG1 = REG3;
      pc += 4;
      break;
    case op_irk: {
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
      break;
    case op_getc:
      // GETC target
      REG1 = vm_the_closure;
      pc += 2;
      break;
    case op_dlsym:
      // DLSYM target name
      REG1 = BOX_INTEGER ((pxll_int)dlsym (RTLD_DEFAULT, GET_STRING_POINTER (REG2)));
      pc += 3;
      break;
    case op_ffi: {
      // FFI target pfun rtype nargs arg0 ...
      pxll_int nargs = UNBOX_INTEGER (REG4);
      // XXX concerned that passing vm_regs defeats the register decl above.
      object result;
      pxll_int success = vm_do_ffi ((object *) vm_regs, pc, nargs, &result);
      if (success == 0) {
        REG1 = result;
      } else {
        fprintf (stderr, "op_ffi failed\n");
        done = 1;
      }
      pc += nargs + 5;
    }
      break;
    case op_smake: {
      // SMAKE target size
      // XXX heap check.
      pxll_int slen = UNBOX_INTEGER (REG2);
      pxll_string * s = (pxll_string*)alloc_no_clear (TC_STRING, string_tuple_length (slen));
      s->len = slen;
      REG1 = (object*)s;
    }
      pc += 3;
      break;
    case op_slen:
      // SLEN target string
      REG1 = BOX_INTEGER ((pxll_int)((pxll_string *) REG2)->len);
      pc += 3;
      break;
    case op_sref: {
      // SREF target string index
      pxll_string * s = (pxll_string *)REG2;
      pxll_int index = UNBOX_INTEGER (REG3);
      if ((index >= 0) && (index < s->len)) {
        REG1 = TO_CHAR (s->data[index]);
      } else {
        // XXX error handler
        fprintf (stderr, "string ref out of range\n");
        done = 1;
      }
      pc += 4;
    }
      break;
    case op_sset: {
      // SSET string index char
      pxll_string * s = (pxll_string *)REG1;
      pxll_int index = UNBOX_INTEGER (REG2);
      pxll_int ch = GET_CHAR (REG3);
      if (ch > 255) {
        fprintf (stderr, "char out of range\n");
        done = 1;
      } else if ((index >= 0) && (index < s->len)) {
        s->data[index] = (char) ch;
      } else {
        // XXX error handler
        fprintf (stderr, "string ref out of range\n");
        done = 1;
      }
      pc += 4;
    }
      break;
    case op_scopy: {
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
        done = 1;
      }
      pc += 6;
    }
      break;
    case op_unchar:
      // UNCHAR target char
      REG1 = (object*) BOX_INTEGER (GET_CHAR (REG2));
      pc += 3;
      break;
    case op_plat: {
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
      break;
    case op_gist:
      // GIST target
      REG1 = vm_internal_symbol_list;
      pc += 2;
      break;
    default:
      fprintf (stderr, "illegal bytecode. (%d)\n", bytecode[pc]);
      break;
    }
  }
  return vm_result;
}
