
#include "pxll.h"
#include <stdio.h>
#include "rdtsc.h"

static object * allocate (pxll_int tc, pxll_int size);
static object * dump_object (object * ob, int depth);
static object do_gc (int nroots);
static pxll_int get_case (object * ob);

uint64_t gc_ticks;
object * lenv;
object * k;
object * top;
object * limit;
object * freep;

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

#define NLITS_MAX 20
int nlits = 0;
object * bytecode_literals[NLITS_MAX];

pxll_int
read_literals (FILE * f)
{
  int done = 0;
  while (!done) {
    pxll_int code = 0;
    pxll_int n = 0;
    CHECK (next (f, &code));
    if (nlits > NLITS_MAX) {
      return -1;
    }
    switch (code) {
    case '+':
      CHECK (read_int (f, &n));
      bytecode_literals[nlits++] =  BOX_INTEGER(n);
      break;
    case '-':
      CHECK (read_int (f, &n));
      bytecode_literals[nlits++] =  BOX_INTEGER(-n);
      break;
    case 'T':
      bytecode_literals[nlits++] =  PXLL_TRUE;
      break;
    case 'F':
      bytecode_literals[nlits++] =  PXLL_FALSE;      
      break;
    case 'u':
      bytecode_literals[nlits++] =  PXLL_UNDEFINED;
      break;
    case 'c':
      CHECK (read_int (f, &n));
      bytecode_literals[nlits++] =  TO_CHAR (n);
      break;
    case '.':
      done = 1;
      break;
    default:
      return -1;
    }
  }
  // for (int i=0; i < nlits; i++) {
  //   fprintf (stderr, "%d: ", i);
  //   DO (bytecode_literals[i]);
  // }
  return 0;
}

#define NCODE_MAX 16384
int ncodes = 0;
uint16_t bytecode[NCODE_MAX];

pxll_int
read_bytecode (FILE * f)
{
  while (1) {
    pxll_int code = 0;
    if (read_int (f, &code) == -1) {
      return 0;
    } else if (code > 0xffff) {
      return -1;
    } else {
      bytecode[ncodes++] = code;
      // fprintf (stderr, "%d ", (int)code);
    }
  }
}

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

pxll_int pc = 0;
object * vm_lenv = PXLL_NIL;
object * vm_k = PXLL_NIL;
object * vm_top = PXLL_NIL;
object * vm_result = PXLL_NIL;

#define NREGS 20
object * vm_regs[NREGS];

enum {
  op_lit,
  op_ret,
  op_add,
  op_sub,
  op_eq,
  op_lt,
  op_gt,
  op_le,
  op_ge,
  op_tst,
  op_jmp,
  op_fun,
  op_tail,
  op_tail0,
  op_env,
  op_arg,
  op_ref,
  op_mov,
  op_epush,
  op_trcall,
  op_ref0,
  op_call,
  op_pop,
  op_print,
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
  op_alloc,
  op_exit,
  op_nvcase,
  op_tupref,
  op_tupset
} opcodes;

char * op_names[] = {
  "lit",
  "ret",
  "add",
  "sub",
  "eq",
  "lt",
  "gt",
  "le",
  "ge",
  "tst",
  "jmp",
  "fun",
  "tail",
  "tail0",
  "env",
  "arg",
  "ref",
  "mov",
  "epush",
  "trcall",
  "ref0",
  "call",
  "pop",
  "print",
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
  "tupset"
};

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

void
print_regs()
{
  fprintf (stdout, "regs: ");
  for (int i=0; i < NREGS; i++) {
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
  pxll_int d0 = depth;
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

#define BC1 bytecode[pc+1]
#define BC2 bytecode[pc+2]
#define BC3 bytecode[pc+3]
#define BC4 bytecode[pc+4]

#define REG1 vm_regs[BC1]
#define REG2 vm_regs[BC2]
#define REG3 vm_regs[BC3]
#define REG4 vm_regs[BC4]

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


object
vm_go (void)
{
  int done = 0;
  pc = 0;
  for (int i=0; i < NREGS; i++) {
    vm_regs[i] = PXLL_NIL;
  }
  while (!done) {
    //print_lenv();
    //print_regs();
    //fprintf (stderr, "--- %d %s\n", pc, op_names[bytecode[pc]]);
    switch (bytecode[pc]) {
    case op_lit:
      REG1 = bytecode_literals[BC2];
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
    case op_arg:
      // ARG tuple arg index
      REG1[BC3+1] = REG2;
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
        args[i+1] = vm_regs[bytecode[pc+4+i]];
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
    case op_print:
      DO (REG1);
      pc += 3;
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
    case op_alloc: {
      // ALLOC target tag nelem elem0 ...
      pxll_int nelem = BC3;
      object * ob = allocate (BC2, nelem);
      for (int i=0; i < nelem; i++) {
        ob[i+1] = vm_regs[bytecode[pc+4+i]];
      }
      REG1 = ob;
      pc += 4 + nelem;
    }
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
        //fprintf (stderr, "  testing %d\n", bytecode[pc+4+(i*2)]);
        if (tag == bytecode[pc+4+(i*2)]) {
          pc0 = bytecode[pc+4+(i*2)+1];
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
    case op_tupset:
      // XXX nearly the same as op_arg, combine.
      // TUPSET ob index val
      REG1[BC2+1] = REG3;
      pc += 4;
      break;
    default:
      fprintf (stderr, "unknown opcode: %d\n", bytecode[pc]);
      return PXLL_NIL;
    }
  }
  return vm_result;
}
