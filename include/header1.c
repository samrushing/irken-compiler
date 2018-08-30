
#include "irken.h"

#ifdef IRK_PROFILE
static int64_t prof_mark0;
static int64_t prof_mark1;
typedef struct {
  int calls;
  int64_t ticks;
  int64_t allocs;
  int64_t alloc_words;
  char * name;
} irk_prof;
static irk_prof prof_funs[];
static int prof_current_fun;
static int prof_num_funs;
#endif

static int lookup_field (int tag, int label);
object * irk_get_metadata();

static
inline
irk_int
get_typecode (object * ob)
{
  if (IMMEDIATE(ob)) {
    if (IS_INTEGER(ob)) {
      return TC_INT;
    } else {
      return (irk_int)ob & 0xff;
    }
  } else {
    return (irk_int)*((irk_int *)ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
irk_int
get_case (object * ob)
{
  if (is_immediate (ob)) {
    if (is_int (ob)) {
      return TC_INT;
    } else {
      return (irk_int) ob;
    }
  } else {
    return (irk_int)*((irk_int *)ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
irk_int
get_case_noint (object * ob)
{
  if (is_immediate (ob)) {
    return (irk_int) ob;
  } else {
    return (irk_int) * ((irk_int*) ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
irk_int
get_case_imm (object * ob)
{
  return (irk_int)ob;
}

static
inline
irk_int
get_case_tup (object * ob)
{
  return (irk_int)*((irk_int *)ob) & 0xff;
}

static
irk_int
get_tuple_size (object * ob)
{
  header * h = (header *) ob;
  return (*h)>>8;
}

// for runtime reflection.
object *
irk_objectptr2int (object * ob)
{
  return box((irk_int)(*ob));
}

// for runtime reflection.
object *
irk_object2int (object * ob)
{
  return box((irk_int)ob);
}

static
void
indent (int n)
{
  while (n--) {
    fprintf (stdout, "  ");
  }
}

static void print_string (object * ob, int quoted);
static void print_list (irk_pair * l);

// this is kinda lame, it's part pretty-printer, part not.
static
object *
dump_object (object * ob, int depth)
{
  // indent (depth);
  if (depth > 100) {
    fprintf (stdout , "...");
    return (object *) IRK_UNDEFINED;
  }
  if (!ob) {
    fprintf (stdout, "<null>");
  } else if (is_int (ob)) {
    // integer
    fprintf (stdout, "%zd", unbox (ob));
  } else {
    int tc = is_immediate (ob);
    switch (tc) {
    case TC_CHAR:
      if ((irk_int)ob>>8 == 257) {
	// deliberately out-of-range character
	fprintf (stdout, "#\\eof");
      } else {
	char ch = ((char)((irk_int)ob>>8));
	switch (ch) {
	case '\000': fprintf (stdout, "#\\nul"); break;
	case ' '   : fprintf (stdout, "#\\space"); break;
	case '\n'  : fprintf (stdout, "#\\newline"); break;
	case '\r'  : fprintf (stdout, "#\\return"); break;
	case '\t'  : fprintf (stdout, "#\\tab"); break;
	default    : fprintf (stdout, "#\\%c", ch);
	}
      }
      break;
    case TC_BOOL:
      fprintf (stdout, ((irk_int)ob >> 8) & 0xff ? "#t" : "#f");
      break;
    case TC_NIL:
      fprintf (stdout, "()");
      break;
    case TC_UNDEFINED:
      fprintf (stdout, "#u");
      break;
    case TC_EMPTY_VECTOR:
      fprintf (stdout, "#()");
      break;
    case TC_USERIMM:
      // a user immediate unit-type...
      fprintf (stdout, "<u%" PRIuPTR ">", (((irk_int)ob)>>8));
      break;
    case 0: {
      // structure
      header h = (header) (ob[0]);
      int tc = h & 0xff;
      switch (tc) {
      case TC_SAVE: {
	// XXX fix me - now holds saved registers
        irk_save * s = (irk_save* ) ob;
        fprintf (stdout, "<save pc=%p\n", s->pc);
        dump_object ((object *) s->lenv, depth+1); fprintf (stdout, "\n");
        dump_object ((object *) s->next, depth+1); fprintf (stdout, ">");
      }
        break;
      case TC_CLOSURE: {
        irk_closure * c = (irk_closure *) ob;
        //fprintf (stdout, "<closure pc=%p\n", c->pc);
        //dump_object ((object *) c->lenv, depth+1); fprintf (stdout, ">\n");
	fprintf (stdout, "<closure pc=%p lenv=%p>", c->pc, c->lenv);
      }
        break;
      case TC_ENV: {
        irk_tuple * t = (irk_tuple *) ob;
        irk_int n = get_tuple_size (ob);
        int i;
	fprintf (stdout, "<tuple\n");
        for (i=0; i < n-1; i++) {
          dump_object ((object *) t->val[i], depth + 1); fprintf (stdout, "\n");
        }
        dump_object ((object *) t->next, depth + 1);
        fprintf (stdout, ">");
      }
	break;
      case TC_VECTOR: {
        irk_vector * t = (irk_vector *) ob;
        irk_int n = get_tuple_size (ob);
        int i;
	fprintf (stdout, "#(");
        for (i=0; i < n; i++) {
          dump_object ((object *) t->val[i], depth+1);
	  if (i < n-1) {
	    fprintf (stdout, " ");
	  }
        }
        fprintf (stdout, ")");
      }
	break;
      case TC_PAIR:
	print_list ((irk_pair *) ob);
        break;
      case TC_STRING:
	print_string (ob, 1);
	break;
      case TC_BUFFER: {
	irk_int n = get_tuple_size (ob);
	fprintf (stdout, "<buffer %" PRIuPTR " words %" PRIuPTR " bytes>", n, n * (sizeof(irk_int)));
	break;
      }
      case TC_FOREIGN:
        switch (GET_TUPLE_LENGTH (*ob)) {
        case 1:
          fprintf (stdout, "<foreign %p>", ob[1]);
          break;
        case 2:
          fprintf (stdout, "<foreign ");
          dump_object (ob[1], depth+1);
          fprintf (stdout, " off=%" PRIuPTR ">", UNTAG_INTEGER(ob[2]));
          break;
        }
        break;
      case TC_SYMBOL:
	print_string ((object*)ob[1], 0);
	break;
      default: {
        irk_vector * t = (irk_vector *) ob;
        irk_int n = get_tuple_size (ob);
        int i;
	fprintf (stdout, "{u%d ", (tc - TC_USEROBJ)>>2);
        for (i=0; i < n; i++) {
          dump_object ((object *) t->val[i], depth+1);
	  if (i < n-1) {
	    fprintf (stdout, " ");
	  }
        }
        fprintf (stdout, "}");
      }
      }
    }
      break;
    }
  }
  return (object *) IRK_UNDEFINED;
}

// 'magic' comparison function.
// This is a generalized comparison function that takes any irken
//   object.  It orders all objects with a lexicographic recursive
//   comparison, and relies on the fact that all tuples are tagged
//   with either builtin object type tags, or are made of tuples of
//   such.  Any future boxless/tagless modification to irken will
//   require automatically-generated comparison functions.

static
inline
irk_int min_int (irk_int a, irk_int b)
{
  if (a < b) {
    return a;
  } else {
    return b;
  }
}

static
irk_int
magic_cmp_int (irk_int a, irk_int b)
{
  if (a == b) {
    return 0;
  } else if (a < b) {
    return -1;
  } else {
    return 1;
  }
}

static
irk_int
magic_cmp_string (irk_string * sa, irk_string * sb)
{
  int cmp = memcmp (sa->data, sb->data, min_int (sa->len, sb->len));
  if (cmp == 0) {
    return magic_cmp_int (sa->len, sb->len);
  } else if (cmp < 0) {
    return -1;
  } else {
    return 1;
  }
}

irk_int
magic_cmp (object * a, object * b)
{
  if (a == b) {
    return 0;
  } else if (is_immediate (a) && is_immediate (b)) {
    return magic_cmp_int ((irk_int)a, (irk_int)b);
  } else if (is_immediate (a)) {
    return -1; // immediates < tuples.
  } else if (is_immediate (b)) {
    return +1; // tuples > immediates.
  } else {
    irk_int tca = GET_TYPECODE (*a);
    irk_int tcb = GET_TYPECODE (*b);
    if (tca < tcb) {
      return -1;
    } else if (tcb < tca) {
      return +1;
    } else if (tca == TC_STRING) {
      return magic_cmp_string ((irk_string *) a, (irk_string *) b);
    } else if (tca == TC_SYMBOL) {
      return magic_cmp_int ((irk_int)a[2],(irk_int)b[2]);
    } else {
      // tags are the same: do per-element comparison.
      // XXX check special internal types like TC_CLOSURE!
      irk_int len_a = GET_TUPLE_LENGTH (*a);
      irk_int len_b = GET_TUPLE_LENGTH (*b);
      for (int i=0; i < min_int (len_a, len_b); i++) {
        irk_int cmp = magic_cmp ((object*)a[i+1], (object*)b[i+1]);
        if (cmp != 0) {
          return cmp;
        }
        // a[i] == b[i], continue to the next...
      }
      // if we are here, items are equal up to min-length.
      return magic_cmp_int (len_a, len_b);
    }
  }
}

// llvm literal relocation.

object * llvm_all_lits;
object * llvm_this_lit = NULL;

// at entry, ob points to a tuple object of some kind.
static
void
relocate_llvm_walk (object * ob)
{
  irk_int tc = GET_TYPECODE (*ob);
  if (tc == TC_STRING) {
    return;
  } else {
    irk_int len = GET_TUPLE_LENGTH(*ob);
    for (int i=0; i < len; i++) {
      object item = ob[i+1];
      irk_int item0 = (irk_int) item;
      if (item0 < 0) {
        // negative values indicate an external relocation
        ob[i+1] = llvm_all_lits[(-item0)];
      } else if (item0 & 3) {
        // an immediate value, leave it
      } else {
        // a relocatable pointer.
        object * ob1 = llvm_this_lit + (item0 >> 2);
        ob[i+1] = ob1;
        relocate_llvm_walk (ob1);
      }
    }
  }
}

void
relocate_llvm_literals (object * vec, uint32_t * offsets)
{
  llvm_all_lits = vec;
  irk_int n = GET_TUPLE_LENGTH (*vec);
  for (int i=0; i < n; i++) {
    llvm_this_lit = vec[i+1];
    object * offset_object = (object*)(vec[i+1]) + offsets[i];
    relocate_llvm_walk (offset_object);
    vec[i+1] = offset_object;
  }
}

// needed by llvm to avoid dealing with `FILE * stdout`, which cannot be accessed
//  portably by llvm code because it is a macro.

object *
irk_write_stdout (object * data, object * size)
{
  return box (fwrite (GET_STRING_POINTER (data), 1, unbox(size), stdout));
}

object *
irk_putc (object * ch)
{
  fputc (GET_CHAR (ch), stdout);
  return IRK_UNDEFINED;
}

object *
irk_flush()
{
  return box (fflush (stdout));
}

object *
irk_dump_object (object * data)
{
  return dump_object (data, 0);
}

static
void
print_string (object * ob, int quoted)
{
  irk_string * s = (irk_string *) ob;
  char * ps = s->data;
  int i;
  //fprintf (stderr, "<printing string of len=%d (tuple-len=%d)>\n", s->len, get_tuple_size (ob));
  if (quoted) {
    fputc ('"', stdout);
  }
  for (i=0; i < (s->len); i++, ps++) {
    if (*ps == '"') {
      fputc ('\\', stdout);
      fputc ('"', stdout);
    } else {
      if (isprint(*ps)) {
	fputc (*ps, stdout);
      } else {
	fprintf (stdout, "\\x%02x", (unsigned char) *ps);
      }
    }
    if (i > 50) {
      fprintf (stdout, "...");
      break;
    }
  }
  if (quoted) {
    fputc ('"', stdout);
  }
}

static
void
print_list (irk_pair * l)
{
  fprintf (stdout, "(");
  while (1) {
    object * car = l->car;
    object * cdr = l->cdr;
    dump_object (car, 0);
    if (cdr == IRK_NIL) {
      fprintf (stdout, ")");
      break;
    } else if (!is_immediate (cdr) && GET_TYPECODE (*cdr) == TC_PAIR) {
      fprintf (stdout, " ");
      l = (irk_pair *) cdr;
    } else {
      fprintf (stdout, " . ");
      dump_object (cdr, 0);
      fprintf (stdout, ")");
      break;
    }
  }
}

static
irk_int
irk_get_vector_length (object * vec)
{
  if (vec == (object *) TC_EMPTY_VECTOR) {
    return 0;
  } else {
    return GET_TUPLE_LENGTH (*vec);
  }
}

static
int
read_header (FILE * file)
{
  int depth = 0;
  // tiny lisp 'skipper' (as opposed to 'reader')
  do {
    char ch = fgetc (file);
    switch (ch) {
    case '(':
      depth++;
      break;
    case ')':
      depth--;
      break;
    case '"':
      while (fgetc (file) != '"') {
        // empty body
      }
      break;
    default:
      break;
    }
  } while (depth);
  // read terminating newline
  fgetc (file);
  return 0;
}

#ifndef NO_RANGE_CHECK
// used to check array references.  some day we might try to teach
//   the compiler when/how to skip doing this...
static
void
inline
range_check (unsigned int length, unsigned int index)
{
  if (index >= length) {
    fprintf (stderr, "array/string reference out of range: %d[%d]\n", length, index);
    abort();
  }
}
#else
static
void
inline
range_check (unsigned int length, unsigned int index)
{
}
#endif

irk_int verbose_gc = 1;
irk_int clear_fromspace = 0;
irk_int clear_tospace = 1;

irk_int vm (int argc, char * argv[]);

#include "rdtsc.h"

#ifdef __aarch64__
// disable llvm.readcyclecounter on arm, where it is expensive and privileged.
#define USE_CYCLECOUNTER 0
#else
#define USE_CYCLECOUNTER 1
#endif

uint64_t gc_ticks = 0;

#if 1
static
void
clear_space (object * p, irk_int n)
{
  while (n--) {
    *p++ = IRK_NIL;
  }
}
#else
void
clear_space (object * p, irk_int n)
{
  memset (p, (int) IRK_NIL, sizeof(object) * n);
}
#endif

object * lenv = IRK_NIL;
object * k = IRK_NIL;
object * top = IRK_NIL; // top-level (i.e. 'global') environment
object * limit; // = heap0 + (heap_size - head_room);
object * freep; // = heap0;
static object * t = 0; // temp - for swaps & building tuples

static int irk_argc;
static char ** irk_argv;

// for gdb...
void
DO (object * x)
{
  dump_object (x, 0);
  fprintf (stdout, "\n");
  fflush (stdout);
}

// for debugging
int
get_stack_depth()
{
  int result = 0;
  object * k0 = k;
  while (k0 != IRK_NIL) {
    result++;
    k0 = (object*) k0[1];
  }
  return result;
}

void
stack_depth_indent()
{
  int depth = get_stack_depth();
  for (int i=0; i < depth; i++) {
    fprintf (stderr, "  ");
  }
}

static
object *
varref (irk_int depth, irk_int index) {
  object * lenv0 = lenv;
  while (depth--) {
    lenv0 = (object*)lenv0[1];
  }
  return (object*)lenv0[index+2];
}

static
void
varset (irk_int depth, irk_int index, object * val) {
  object * lenv0 = lenv;
  while (depth--) {
    lenv0 = (object*)lenv0[1];
  }
  lenv0[index+2] = val;
}

#include "gc1.c"

static object *
allocate (irk_int tc, irk_int size)
{
  object * save = freep;
  *freep = (object*) (size<<8 | (tc & 0xff));
#if 0
  // at least on the g5, this technique is considerably faster than using memset
  //   in gc_flip() to 'pre-clear' the heap... probably a cache effect...
  while (size--) {
    // this keeps gc from being confused by partially-filled objects.
    *(++freep) = IRK_NIL;
  }
  ++freep;
#else
  // if you use this version, be sure to set <clear_tospace>!
  freep += size + 1;
#endif
#ifdef IRK_PROFILE
  prof_funs[prof_current_fun].allocs++;
  prof_funs[prof_current_fun].alloc_words += size;
#endif
  return save;
}

  // this is emitted by the backend for %make-tuple
static object *
alloc_no_clear (irk_int tc, irk_int size)
{
  object * save = freep;
  *freep = (object*) (size<<8 | (tc & 0xff));
  freep += size + 1;
  return save;
}

object *
make_vector (irk_int size, object * val)
{
  if (size == 0) {
    return (object *) TC_EMPTY_VECTOR;
  } else {
    object * v = allocate (TC_VECTOR, size);
    for (int i=0; i < size; i++) {
      v[i+1] = (object *) val;
    }
    return v;
  }
}

object *
make_foreign (void * ptr)
{
  object * r = allocate (TC_FOREIGN, 1);
  r[1] = (object *) ptr;
  return r;
}

object *
make_malloc (irk_int size, irk_int count)
{
  object * r = allocate (TC_FOREIGN, 1);
  r[1] = (object *) malloc (size * count);
  return r;
}

object *
make_halloc (irk_int size, irk_int count)
{
  object * buffer = alloc_no_clear (TC_BUFFER, HOW_MANY (size * count, sizeof(object)));
  object * result = allocate (TC_FOREIGN, 2);
  result[1] = buffer;
  result[2] = box(0);
  return result;
}

void *
get_foreign (object * ob)
{
  if (GET_TUPLE_LENGTH (*ob) == 1) {
    // TC_FOREIGN <pointer>
    return (void *) ob[1];
  } else {
    // TC_FOREIGN <buffer> <offset>
    object * buffer = (object*) ob[1];
    uint8_t * base = (uint8_t *) (buffer + 1);
    irk_int offset = UNTAG_INTEGER (ob[2]);
    return (void *) (base + offset);
  }
}

object *
offset_foreign (object * foreign, irk_int offset)
{
  if (offset == 0) {
    return foreign;
  } else {
    // Note: offset is in bytes
    if (GET_TUPLE_LENGTH (*foreign) == 1) {
      // TC_FOREIGN <pointer>
      object * r = allocate (TC_FOREIGN, 1);
      r[1] = (uint8_t *) (foreign[1]) + offset;
      return r;
    } else {
      // TC_FOREIGN <buffer> <offset>
      object * r = allocate (TC_FOREIGN, 2);
      r[1] = foreign[1];
      r[2] = (object*) TAG_INTEGER ((UNTAG_INTEGER (foreign[2]) + offset));
      return r;
    }
  }
}

object *
free_foreign (object * foreign)
{
  if (GET_TUPLE_LENGTH (*foreign) == 1) {
    free (foreign[1]);
  }
  return (object*) IRK_UNDEFINED;
}

object *
irk_cref_2_string (object * src, object * len)
{
  irk_int len0 = UNTAG_INTEGER (len);
  object * result = make_string (len0);
  uint8_t * src0 = (uint8_t * ) get_foreign (src);
  void * dst = GET_STRING_POINTER (result);
  memcpy (dst, src0, len0);
  return result;
}

object *
irk_string_2_cref (object * src)
{
  return make_foreign (GET_STRING_POINTER (src));
}

object *
irk_cref_2_int (object * src)
{
  return TAG_INTEGER (((irk_int *) src)[1]);
}

object *
irk_int_2_cref (object * src)
{
  return make_foreign ((void*)UNTAG_INTEGER(src));
}

// XXX this needs more thought
#include <errno.h>
// Note: 'errno' is no longer simply an external int.  It can be defined in any of several ways,
//   which means that our llvm generated code cannot access it without going through the C api.
//   (for example, on OSX its location is the result of a call to `_error()`)
object *
irk_get_errno (void)
{
  return TAG_INTEGER ((irk_int) errno);
}


// used to lookup record elements when the index
//  cannot be computed at compile-time.
object *
record_fetch (object * rec, irk_int label)
{
  return ((irk_vector*)rec)->val[
    lookup_field (
      (GET_TYPECODE(*rec)-TC_USEROBJ)>>2,
      label
    )
  ];
}

void
record_store (object * rec, irk_int label, object * val)
{
  ((irk_vector*)rec)->val[
    lookup_field (
      (GET_TYPECODE(*rec)-TC_USEROBJ)>>2,
      label
    )
  ] = val;
}

void
vector_range_check (object * v, irk_int index)
{
  if (v == (object*) TC_EMPTY_VECTOR) {
    range_check (0, index);
  } else {
    range_check (GET_TUPLE_LENGTH (*v), index);
  }
}

void
check_heap()
{
  if (freep >= limit) {
    gc_flip (0);
  }
}

object *
irk_print_string (object * s)
{
  irk_string * s0 = (irk_string *) s;
  return box (fwrite (s0->data, 1, s0->len, stdout));
}

void
DENV0 (object * env)
{
  irk_tuple * t = (irk_tuple *) env;
  fprintf (stdout, "ENV@%p: [", env);
  while (t != (irk_tuple*)IRK_NIL) {
    irk_int n = get_tuple_size ((object *) t);
    fprintf (stdout, "%" PRIdPTR " ", n);
    t = t->next;
  }
  fprintf (stdout, "]\n");
}

void DENV() { DENV0 (lenv); }

void
RIB0 (object * env)
{
  irk_tuple * t = (irk_tuple *) env;
  irk_int n = get_tuple_size ((object *) t);
  fprintf (stdout, " rib: ");
  for (int i = 0; i < n-1; i++) {
    dump_object (t->val[i], 0);
    fprintf (stdout, " ");
  }
  fprintf (stdout, "\n");
}

const char * irk_trace_current_fun = NULL;

void TRACE (const char * name)
{
  // this is wrong, needs to use k, not lenv.
  if (name != irk_trace_current_fun) {
    stack_depth_indent();
    fprintf (stderr, "%s ", name);
    DENV();
    irk_trace_current_fun = name;
  }
}

void T (irk_int n)
{
  fprintf (stderr, "[%" PRIdPTR "]", n);
}

static uint64_t program_start_time;
static uint64_t program_end_time;
void prof_dump (void);

typedef void(*kfun)(object *);
typedef void(*pfun)(void);
void exit_continuation (object * result)
{
#if USE_CYCLECOUNTER
  program_end_time = rdtsc();
#endif
  if ((result != (object*) IRK_UNDEFINED) && !IS_INTEGER(result)) {
    // only print the result object if it's "interesting".
    dump_object ((object *) result, 0);
    fprintf (stdout, "\n");
  }
#if USE_CYCLECOUNTER
  if (verbose_gc) {
    fprintf (
      stderr, "{total ticks: %" PRIu64 " gc ticks: %" PRIu64 "}\n",
      program_end_time - program_start_time,
      gc_ticks
    );
  }
#endif
  prof_dump();
  if (is_int (result)) {
    exit ((int)(intptr_t)UNTAG_INTEGER(result));
  } else {
    exit (0);
  }
}

object *
mem2ptr (object * ob)
{
  switch (get_case_tup (ob)) {
  case TC_USEROBJ + 0:
    // buffer object
    return ob+1;
    break;
  case TC_USEROBJ + 1:
    // pointer
    return (object*) UNTAG_INTEGER (*((irk_int*)(ob+1)));
    break;
  default:
    fprintf (stderr, "bad cmem object\n");
    abort();
  }
}

object *
ptr2mem (void * ptr)
{
  object * result = allocate (TC_USEROBJ + 1, 1);
  result[1] = (object*) TAG_INTEGER ((irk_int)ptr);
  return result;
}

object *
buf2mem (object * buf)
{
  object * result = allocate (TC_USEROBJ + 0, 1);
  result[1] = buf;
  return result;
}

static
object *
make_string (irk_int len)
{
  irk_string * result = (irk_string *) allocate (TC_STRING, STRING_TUPLE_LENGTH (len));
  result->len = len;
  return (object *) result;
}

#if 0
// --------------------------------------------------------------------------------
// invoke-closure is used by the bytecode VM to call back into Irken.
// It could theoretically be used by any C code that needed e.g. a
// callback facility.

void invoke_closure_1 (void);

object
invoke_closure (object * closure, object * args)
{
  object * t = allocate (TC_SAVE, 3);
  t[1] = k;
  t[2] = lenv;
  t[3] = (object *) invoke_closure_1; // see below
  k = t;
  args[1] = closure[2];
  lenv = args;
  ((kfun)(closure[1]))();
  return result;
}

// continuation function for invoke_closure.
void invoke_closure_1 (void)
{
  lenv = (object*) k[2]; k = (object *)k[1];
  // Note: no IRK_RETURN here. that's because we want to return (in the C sense)
  //  to invoke_closure so the result can be returned to the original C caller,
  //  e.g. vm_go().
  // if there *was* a IRK_RETURN here, it would actually call exit_continuation(),
  //  and exit the entire program.
}
#endif

static
object *
irk_copy_string (char * s)
{
  int slen = strlen (s);
  irk_string * r = (irk_string *) alloc_no_clear (TC_STRING, string_tuple_length (slen));
  r->len = slen;
  memcpy (GET_STRING_POINTER (r), s, slen);
  return (object *) r;
}

object *
irk_copy_tuple (object * ob)
{
  if (is_immediate (ob)) {
    return ob;
  } else {
    header h = (header) (ob[0]);
    int tag  = h & 0xff;
    irk_int len = GET_TUPLE_LENGTH (ob[0]);
    //DO(ob);
    object * r = allocate (tag, len);
    for (int i=0; i < len; i++) {
      // r[i+1] = copy(&ob[i+1]);
      r[i+1] = ob[i+1];
    }
    return r;
  }
}


object *
irk_make_argv()
{
  object * r = allocate (TC_VECTOR, irk_argc);
  for (int i=0; i < irk_argc; i++) {
    r[i+1] = irk_copy_string (irk_argv[i]);
  }
  return r;
}

object *
irk_set_verbose_gc (object * data)
{
  irk_int r = verbose_gc;
  verbose_gc = IRK_IS_TRUE (data);
  return (object*) r;
}

object *
irk_make_string (object * len)
{
  irk_int len0 = unbox(len);
  irk_string * r = (irk_string*) alloc_no_clear (TC_STRING, string_tuple_length (len0));
  r->len = len0;
  return (object *) r;
}

object *
irk_string_cmp (irk_string * a, irk_string * b)
{
  return (object*) UITAG (1 + magic_cmp_string (a, b));
}

// --------------------------------------------------------------------------------

// used by bignum.scm

#ifdef __LP64__

// note: these functions take a result vector in order to avoid
//   allocating a result tuple in these functions.  If they are
//   used in a tight loop that otherwise does not allocate, they
//   can avoid a heap check and overflow the heap.

// 60-bit limbs
object *
irk_mul2 (irk_int a, irk_int b, object * rvo)
{
  __int128_t r;
  __int128_t mask = (__int128_t)((((int64_t)1)<<60)-1);
  irk_vector * rv = (irk_vector *) rvo;
  r = ((__int128_t) a) * ((__int128_t) b);
  rv->val[0] = box ((int64_t) (r >> 60));
  rv->val[1] = box ((int64_t) (r & mask));
  return (object*) IRK_UNDEFINED;
}

// divide two digits by one digit.
// assumptions: b >= big/halfmask (i.e. base/2)
object *
irk_div2b1 (irk_int ah, irk_int al, irk_int b, object * rvo)
{
  irk_vector * rv = (irk_vector *) rvo;
  __int128_t mask = (__int128_t)((((int64_t)1)<<60)-1);
  __int128_t a = (((__int128_t)ah)<<60) | ((__int128_t)al);
  __int128_t q = a / b;
  __int128_t r = a % b;
  rv->val[0] = box ((int64_t) (q & mask));
  rv->val[1] = box ((int64_t) (r & mask));
  return (object*) IRK_UNDEFINED;
}

#else

// 28-bit limbs
object *
irk_mul2 (irk_int a, irk_int b, object * rvo)
{
  int64_t r;
  int64_t mask = (int64_t)((((int32_t)1)<<28)-1);
  irk_vector * rv = (irk_vector *) rvo;
  r = ((int64_t) a) * ((int64_t) b);
  rv->val[0] = box ((int32_t) (r >> 28));
  rv->val[1] = box ((int32_t) (r & mask));
  return (object*) IRK_UNDEFINED;
}

// divide two digits by one digit.
// assumptions: b >= big/halfmask (i.e. base/2)
object *
irk_div2b1 (irk_int ah, irk_int al, irk_int b, object * rvo)
{
  irk_vector * rv = (irk_vector *) rvo;
  int64_t mask = (int64_t)((((int32_t)1)<<28)-1);
  int64_t a = (((int64_t)ah)<<28) | ((int64_t)al);
  int64_t q = a / b;
  int64_t r = a % b;
  rv->val[0] = box ((int32_t) (q & mask));
  rv->val[1] = box ((int32_t) (r & mask));
  return (object*) IRK_UNDEFINED;
}

#endif

// --------------------------------------------------------------------------------

extern uint32_t irk_ambig_size;
extern int32_t G[];
extern int32_t V[];

static
uint32_t
p_hash (uint32_t d, int k0, int k1)
{
  d = ((d * 0x01000193) ^ k1) & 0xffffffff;
  d = ((d * 0x01000193) ^ k0) & 0xffffffff;
  return d % irk_ambig_size;
}

static int lookup_field (int tag, int label)
{
  int32_t d = G[p_hash(0x01000193, tag, label)];
  if (d < 0) {
    return V[-d-1];
  } else {
    return V[p_hash(d, tag, label)];
  }
}

// --------------------------------------------------------------------------------

void toplevel (void);

int
main (int argc, char * argv[])
{
  heap0 = (object *) malloc (sizeof (object) * heap_size);
  heap1 = (object *) malloc (sizeof (object) * heap_size);
  if (!heap0 || !heap1) {
    fprintf (stderr, "unable to allocate heap\n");
    return -1;
  } else {
    if (clear_tospace) {
      clear_space (heap0, heap_size);
    }
    irk_argc = argc;
    irk_argv = argv;
    limit = heap0 + (heap_size - head_room);
    freep = heap0;
    k = allocate (TC_SAVE, 3);
    k[1] = (object *) IRK_NIL; // top of stack
    k[2] = (object *) IRK_NIL; // null environment
    k[3] = (object *) exit_continuation;
#if USE_CYCLECOUNTER
    program_start_time = rdtsc();
#endif
    toplevel();
    return 0;
  }
}

#define IRK_RETURN(d) ((kfun)(k[3]))(r##d);
#define O object *

// With the C backend, generated code follow this point.
// With the LLVM backend, this file is linked with the generated code.
