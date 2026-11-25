#include <sys/utsname.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <signal.h>
#include <sys/signal.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

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
      return (irk_int)ob & TAGMASK;
    }
  } else {
    return (irk_int)*((irk_int *)ob) & TAGMASK;
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
    return (irk_int)*((irk_int *)ob) & TAGMASK;
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
    return (irk_int) * ((irk_int*) ob) & TAGMASK;
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
  return (irk_int)*((irk_int *)ob) & TAGMASK;
}

static
irk_int
get_tuple_size (object * ob)
{
  header * h = (header *) ob;
  return (*h)>>TAGSIZE;
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
      if ((irk_int)ob>>TAGSIZE == 257) {
	// deliberately out-of-range character
	fprintf (stdout, "#\\eof");
      } else {
	char ch = ((char)((irk_int)ob>>TAGSIZE));
        if (ch > 127) {
          // on-the-fly utf-8
          uint8_t ch_lo = 0x80 | (ch & 0x3f);
          uint8_t ch_hi = 0xc0 | (ch >> 6);
          fprintf (stdout, "#\\%c%c", ch_hi, ch_lo);
        } else {
          switch (ch) {
          case '\000': fprintf (stdout, "#\\nul"); break;
          case ' '   : fprintf (stdout, "#\\space"); break;
          case '\n'  : fprintf (stdout, "#\\newline"); break;
          case '\r'  : fprintf (stdout, "#\\return"); break;
          case '\t'  : fprintf (stdout, "#\\tab"); break;
          default    : fprintf (stdout, "#\\%c", ch);
          }
        }
      }
      break;
    case TC_BOOL:
      fprintf (stdout, ((irk_int)ob >> TAGSIZE) & TAGMASK ? "#t" : "#f");
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
      fprintf (stdout, "<u%" PRIuPTR ">", (((irk_int)ob)>>TAGSIZE));
      break;
    case 0: {
      // structure
      header h = (header) (ob[0]);
      int tc = h & TAGMASK;
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
    if (*ps == '"' || (quoted && *ps == '\\')) {
      // (escape backslashes too, or "a\\x41" and "a\x41" print the same)
      fputc ('\\', stdout);
      fputc (*ps, stdout);
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
    fprintf (stderr, "array/string reference out of range: size=%d index=%d\n", length, index);
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
// see gc1.c: compiled code that doesn't need this sets it to 0.
irk_int clear_tospace = 1;

irk_int vm (int argc, char * argv[]);

#include "rdtsc.h"

#define USE_CYCLECOUNTER 1

uint64_t gc_ticks = 0;

#if defined(IRK_GC_POISON)
// debugging: fill free heap space with a word the collector will notice
//  (see copy() in gc1.c), to catch objects whose slots are still unset
//  when a collection happens.  build with -DIRK_GC_POISON, and maybe a
//  small -DIRK_HEAP_SIZE so that collections are frequent.
#define IRK_POISON ((object) 0x00DEAD00DEAD0000ULL)
static
void
clear_space (object * p, irk_int n)
{
  while (n--) {
    *p++ = IRK_POISON;
  }
}
#elif 1
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

void
dump_lenv (object * env, int depth)
{
  int elen = GET_TUPLE_LENGTH (env);
  fprintf (stderr, "env {\n");
  for (int i=0; i < (elen - 1); i++) {
    dump_object (env[2+i], depth + 1);
  }
  if (env[1] != IRK_NIL) {
    dump_lenv (env[1], depth + 1);
  }
  fprintf (stderr, "}\n");
}

object *
varref (irk_int depth, irk_int index) {
  object * lenv0 = lenv;
  while (depth--) {
    lenv0 = (object*)lenv0[1];
  }
  return (object*)lenv0[index+2];
}

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
  *freep = (object*) (size<<TAGSIZE | (tc & TAGMASK));
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

// an object whose slots are filled in later, while something that can
//  trigger a collection runs: zero them first (see clear_tospace in gc1.c).
static object *
alloc_cleared (irk_int tc, irk_int size)
{
  object * save = allocate (tc, size);
  memset (save + 1, 0, sizeof (object) * size);
  return save;
}

  // this is emitted by the backend for %make-tuple
static object *
alloc_no_clear (irk_int tc, irk_int size)
{
  object * save = freep;
  *freep = (object*) (size<<TAGSIZE | (tc & TAGMASK));
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

// these 'foreign' functions manage pointers to external/C pointer data.
// they are boxed with the tag TC_FOREIGN.
// two flavors:
//
//  1) a raw external pointer
//  2) a TC_BUFFER object plus an offset.

object *
make_foreign (void * ptr)
{
  object * r = allocate (TC_FOREIGN, 1);
  r[1] = (object *) ptr;
  return r;
}

// a pointer to a string's characters (%string->cref).  this is the
//   <buffer> <offset> flavor, so the pointer follows the string when the
//   collector moves it.  a raw pointer into the heap would go stale, and
//   worse, the collector would take it for an object and try to copy it.
object *
make_string_foreign (object * s)
{
  object * r = allocate (TC_FOREIGN, 2);
  r[1] = s;
  r[2] = (object *) TAG_INTEGER ((uint8_t *) GET_STRING_POINTER (s) - (uint8_t *) (s + 1));
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

// %c-get-ptr / %c-set-ptr for the native backends: read or write a
//   pointer stored at a foreign address.  get-ptr allocates (the result
//   is a new TC_FOREIGN), so callers must save/restore freep.
object *
irk_c_get_ptr (object * src)
{
  return make_foreign (*((void **) get_foreign (src)));
}

void
irk_c_set_ptr (object * dst, object * src)
{
  *((void **) get_foreign (dst)) = get_foreign (src);
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

#define LOOKUP_FIELD(rec,label) lookup_field ((GET_TYPECODE(*rec)-TC_USEROBJ)>>2,label)

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

// this is called by the arm64 backend.
// ideally, we'd like to actually have this raise an exception. TBD.
// we'll need a saved continuation that we can invoke, that will raise
// an exception for us.
void
out_of_range_exception (irk_int index, irk_int size)
{
  fprintf (stderr, "vector reference out of range: size=%ld index=%ld\n", size, index);
  abort();
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
    // XXX FIXME this check needed by the VM.
    return ob;
  } else {
    header h = (header) (ob[0]);
    int tag  = h & TAGMASK;
    irk_int len = GET_TUPLE_LENGTH (ob[0]);
    object * r = allocate (tag, len);
    for (int i=0; i < len; i++) {
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
irk_buffer_copy (object * src, object * sstart, object * n, object * dst, object * dstart)
{
  void * src0 = GET_STRING_POINTER (src);
  irk_int sstart0 = unbox (sstart);
  irk_int n0 = unbox (n);
  void * dst0 = GET_STRING_POINTER (dst);
  irk_int dstart0 = unbox (dstart);
  memcpy (dst0 + dstart0, src0 + sstart0, n0);
  return (object *) IRK_UNDEFINED;
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

static
int lookup_field (int tag, int label)
{
  int32_t d = G[p_hash(0x01000193, tag, label)];
  if (d < 0) {
    return V[-d-1];
  } else {
    return V[p_hash(d, tag, label)];
  }
}

// replacement for backend.scm:hash2 & hash-item on 32-bit platforms.
uint32_t
hash_item (uint32_t d, uint32_t k0, uint32_t k1, uint32_t size)
{
  d = ((d * 0x01000193) ^ k1);
  d = ((d * 0x01000193) ^ k0);
  return d % size;
}

// --------------------------------------------------------------------------------

void toplevel (void);

int
main (int argc, char * argv[])
{
  // calloc: the heaps start out zeroed, and (for big allocations) the
  //  pages aren't touched until they're used.
  heap0 = (object *) calloc (heap_size, sizeof (object));
  heap1 = (object *) calloc (heap_size, sizeof (object));
  heap1_size = heap_size;
  if (!heap0 || !heap1) {
    fprintf (stderr, "unable to allocate heap\n");
    return -1;
  } else {
#if defined(IRK_GC_POISON)
    clear_space (heap0, heap_size);
#endif
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

// With the C backend, generated code follows this point.
// With the other backends/vm, this file is linked in.
irk_string constructed_0 = {STRING_HEADER(2), 2, "}\n" };
irk_string constructed_1 = {STRING_HEADER(10), 10, "headers {\n" };
irk_string constructed_2 = {STRING_HEADER(1), 1, "\n" };
irk_int constructed_3[] = {UPTR(3,1),CONS_HEADER,UPTR0(2),TC_NIL};
irk_string constructed_4 = {STRING_HEADER(2), 2, ": " };
irk_string constructed_5 = {STRING_HEADER(2), 2, "  " };
irk_string constructed_6 = {STRING_HEADER(23), 23, "odd-length input string" };
irk_string constructed_7 = {STRING_HEADER(45), 45, "--------------------------------------------\n" };
irk_string constructed_8 = {STRING_HEADER(2), 2, "7 " };
irk_string constructed_9 = {STRING_HEADER(1), 1, "\"" };
irk_int constructed_10[] = {UPTR(10,1),CONS_HEADER,UPTR0(9),TC_NIL};
irk_string constructed_11 = {STRING_HEADER(5), 5, " val " };
irk_string constructed_12 = {STRING_HEADER(6), 6, " name " };
irk_string constructed_13 = {STRING_HEADER(2), 2, "6 " };
irk_string constructed_14 = {STRING_HEADER(2), 2, "4 " };
irk_string constructed_15 = {STRING_HEADER(18), 18, "get-header byte = " };
irk_string constructed_16 = {STRING_HEADER(1), 1, "\377" };
irk_string constructed_17 = {STRING_HEADER(0), 0, "" };
irk_string constructed_18 = {STRING_HEADER(9), 9, "set-size " };
irk_string constructed_19 = {STRING_HEADER(9), 9, " nstatic " };
irk_string constructed_20 = {STRING_HEADER(4), 4, "get " };
irk_string constructed_21 = {STRING_HEADER(8), 8, " bytes.\n" };
irk_int constructed_22[] = {UPTR(22,1),CONS_HEADER,UPTR0(21),TC_NIL};
irk_string constructed_23 = {STRING_HEADER(9), 9, "evicting " };
irk_string constructed_24 = {STRING_HEADER(102), 102, "885f87497ca589d34d1f628fe4c7f21742f3e275a6db5d97021fe76496df3dbf4a05b521b665040134a085702ddc65953168df" };
irk_string constructed_25 = {STRING_HEADER(68), 68, "885f8b352398ac23bcdfefcd347f6291e4c7f216c4ebef01c75a5c4eb2fb2eff3fc0" };
irk_string constructed_26 = {STRING_HEADER(44), 44, "88bf6291e4c7f216c4ebef34fb4f2ed3e07da7bf9fc1" };
irk_string constructed_27 = {STRING_HEADER(44), 44, "88c06291e4c7f216c4ebec8590b4bbcebc06dffe7fc2" };
irk_int constructed_28[] = {UPTR(28,10),CONS_HEADER,UPTR0(27),TC_NIL,CONS_HEADER,UPTR0(26),UPTR(28,1),CONS_HEADER,UPTR0(25),UPTR(28,4),CONS_HEADER,
	UPTR0(24),UPTR(28,7)};
irk_int constructed_29[] = {UPTR(29,1),UOHEAD(1,1),513};
irk_string constructed_30 = {STRING_HEADER(64), 64, ".....3031.3261..6365.696f...7374..2025.2d2e...2f33.3435..3637.38" };
irk_string constructed_31 = {STRING_HEADER(64), 64, "39.....3d41.5f62..6466.6768...6c6d.6e70..7275..3a42.4344.....454" };
irk_string constructed_32 = {STRING_HEADER(64), 64, "6.4748..494a.4b4c...4d4e.4f50..5152.5354....5556.5759..6a6b.7176" };
irk_string constructed_33 = {STRING_HEADER(64), 64, "...7778.797a...262a.2c3b..585a...2122.2829..3f.272b..7c.233e...0" };
irk_string constructed_34 = {STRING_HEADER(64), 64, "024.405b..5d7e..5e7d..3c60.7b....5cc3.d0.8082...83a2.b8c2..e0e2." };
irk_string constructed_35 = {STRING_HEADER(64), 64, ".99a1.a7ac.....b0b1.b3d1..d8d9.e3e5...e6.8184..8586.8892...9a9c." };
irk_string constructed_36 = {STRING_HEADER(64), 64, "a0a3..a4a9.aaad.....b2b5.b9ba..bbbd.bec4...c6e4.e8e9...0187.898a" };
irk_string constructed_37 = {STRING_HEADER(64), 64, "..8b8c.8d8f.....9395.9697..989b.9d9e...a5a6.a8ae..afb4.b6b7....b" };
irk_string constructed_38 = {STRING_HEADER(64), 64, "cbf.c5e7..ef.098e..9091.949f....abce.d7e1..eced..c7cf.eaeb.....c" };
irk_string constructed_39 = {STRING_HEADER(64), 64, "0c1.c8c9..cacd.d2d5...dadb.eef0..f2f3.ff.cbcc.....d3d4.d6dd..ded" };
irk_string constructed_40 = {STRING_HEADER(64), 64, "f.f1f4...f5f6.f7f8..fafb.fcfd....fe.0203..0405.0607...080b.0c0e." };
irk_string constructed_41 = {STRING_HEADER(65), 65, ".0f10.1112....1314.1517..1819.1a1b...1c1d.1e1f..7fdc.f9..0a0d.16Z" };
irk_int constructed_42[] = {UPTR(42,34),CONS_HEADER,UPTR0(41),TC_NIL,CONS_HEADER,UPTR0(40),UPTR(42,1),CONS_HEADER,UPTR0(39),UPTR(42,4),CONS_HEADER,
	UPTR0(38),UPTR(42,7),CONS_HEADER,UPTR0(37),UPTR(42,10),CONS_HEADER,UPTR0(36),UPTR(42,13),CONS_HEADER,UPTR0(35),
	UPTR(42,16),CONS_HEADER,UPTR0(34),UPTR(42,19),CONS_HEADER,UPTR0(33),UPTR(42,22),CONS_HEADER,UPTR0(32),UPTR(42,25),
	CONS_HEADER,UPTR0(31),UPTR(42,28),CONS_HEADER,UPTR0(30),UPTR(42,31)};
irk_string constructed_43 = {STRING_HEADER(16), 16, "www-authenticate" };
irk_string constructed_44 = {STRING_HEADER(3), 3, "via" };
irk_string constructed_45 = {STRING_HEADER(4), 4, "vary" };
irk_string constructed_46 = {STRING_HEADER(10), 10, "user-agent" };
irk_string constructed_47 = {STRING_HEADER(17), 17, "transfer-encoding" };
irk_string constructed_48 = {STRING_HEADER(25), 25, "strict-transport-security" };
irk_string constructed_49 = {STRING_HEADER(10), 10, "set-cookie" };
irk_string constructed_50 = {STRING_HEADER(6), 6, "server" };
irk_string constructed_51 = {STRING_HEADER(11), 11, "retry-after" };
irk_string constructed_52 = {STRING_HEADER(7), 7, "refresh" };
irk_string constructed_53 = {STRING_HEADER(7), 7, "referer" };
irk_string constructed_54 = {STRING_HEADER(5), 5, "range" };
irk_string constructed_55 = {STRING_HEADER(19), 19, "proxy-authorization" };
irk_string constructed_56 = {STRING_HEADER(18), 18, "proxy-authenticate" };
irk_string constructed_57 = {STRING_HEADER(12), 12, "max-forwards" };
irk_string constructed_58 = {STRING_HEADER(8), 8, "location" };
irk_string constructed_59 = {STRING_HEADER(4), 4, "link" };
irk_string constructed_60 = {STRING_HEADER(13), 13, "last-modified" };
irk_string constructed_61 = {STRING_HEADER(19), 19, "if-unmodified-since" };
irk_string constructed_62 = {STRING_HEADER(8), 8, "if-range" };
irk_string constructed_63 = {STRING_HEADER(13), 13, "if-none-match" };
irk_string constructed_64 = {STRING_HEADER(17), 17, "if-modified-since" };
irk_string constructed_65 = {STRING_HEADER(8), 8, "if-match" };
irk_string constructed_66 = {STRING_HEADER(4), 4, "host" };
irk_string constructed_67 = {STRING_HEADER(4), 4, "from" };
irk_string constructed_68 = {STRING_HEADER(7), 7, "expires" };
irk_string constructed_69 = {STRING_HEADER(6), 6, "expect" };
irk_string constructed_70 = {STRING_HEADER(4), 4, "etag" };
irk_string constructed_71 = {STRING_HEADER(4), 4, "date" };
irk_string constructed_72 = {STRING_HEADER(6), 6, "cookie" };
irk_string constructed_73 = {STRING_HEADER(12), 12, "content-type" };
irk_string constructed_74 = {STRING_HEADER(13), 13, "content-range" };
irk_string constructed_75 = {STRING_HEADER(16), 16, "content-location" };
irk_string constructed_76 = {STRING_HEADER(14), 14, "content-length" };
irk_string constructed_77 = {STRING_HEADER(16), 16, "content-language" };
irk_string constructed_78 = {STRING_HEADER(16), 16, "content-encoding" };
irk_string constructed_79 = {STRING_HEADER(19), 19, "content-disposition" };
irk_string constructed_80 = {STRING_HEADER(13), 13, "cache-control" };
irk_string constructed_81 = {STRING_HEADER(13), 13, "authorization" };
irk_string constructed_82 = {STRING_HEADER(5), 5, "allow" };
irk_string constructed_83 = {STRING_HEADER(3), 3, "age" };
irk_string constructed_84 = {STRING_HEADER(27), 27, "access-control-allow-origin" };
irk_string constructed_85 = {STRING_HEADER(6), 6, "accept" };
irk_string constructed_86 = {STRING_HEADER(13), 13, "accept-ranges" };
irk_string constructed_87 = {STRING_HEADER(15), 15, "accept-language" };
irk_string constructed_88 = {STRING_HEADER(13), 13, "gzip, deflate" };
irk_string constructed_89 = {STRING_HEADER(15), 15, "accept-encoding" };
irk_string constructed_90 = {STRING_HEADER(14), 14, "accept-charset" };
irk_string constructed_91 = {STRING_HEADER(3), 3, "500" };
irk_string constructed_92 = {STRING_HEADER(7), 7, ":status" };
irk_string constructed_93 = {STRING_HEADER(3), 3, "404" };
irk_string constructed_94 = {STRING_HEADER(3), 3, "400" };
irk_string constructed_95 = {STRING_HEADER(3), 3, "304" };
irk_string constructed_96 = {STRING_HEADER(3), 3, "206" };
irk_string constructed_97 = {STRING_HEADER(3), 3, "204" };
irk_string constructed_98 = {STRING_HEADER(3), 3, "200" };
irk_string constructed_99 = {STRING_HEADER(5), 5, "https" };
irk_string constructed_100 = {STRING_HEADER(7), 7, ":scheme" };
irk_string constructed_101 = {STRING_HEADER(4), 4, "http" };
irk_string constructed_102 = {STRING_HEADER(11), 11, "/index.html" };
irk_string constructed_103 = {STRING_HEADER(5), 5, ":path" };
irk_string constructed_104 = {STRING_HEADER(1), 1, "/" };
irk_string constructed_105 = {STRING_HEADER(4), 4, "POST" };
irk_string constructed_106 = {STRING_HEADER(7), 7, ":method" };
irk_string constructed_107 = {STRING_HEADER(3), 3, "GET" };
irk_string constructed_108 = {STRING_HEADER(10), 10, ":authority" };
irk_string constructed_109 = {STRING_HEADER(13), 13, "bad hex digit" };
irk_string constructed_110 = {STRING_HEADER(11), 11, "   values: " };
irk_string constructed_111 = {STRING_HEADER(10), 10, "\" raised.\n" };
irk_int constructed_112[] = {UPTR(112,1),CONS_HEADER,UPTR0(111),TC_NIL};
irk_string constructed_113 = {STRING_HEADER(11), 11, "Exception \"" };
irk_string constructed_114 = {STRING_HEADER(3), 3, "int" };
// symbol int
irk_int constructed_115[] = {UPTR(115,1), SYMBOL_HEADER, UPTR0(114), INTCON(0)};
irk_string constructed_116 = {STRING_HEADER(7), 7, "unknown" };
// symbol unknown
irk_int constructed_117[] = {UPTR(117,1), SYMBOL_HEADER, UPTR0(116), INTCON(1)};
irk_string constructed_118 = {STRING_HEADER(8), 8, "variants" };
// symbol variants
irk_int constructed_119[] = {UPTR(119,1), SYMBOL_HEADER, UPTR0(118), INTCON(2)};
irk_string constructed_120 = {STRING_HEADER(8), 8, "metadata" };
// symbol metadata
irk_int constructed_121[] = {UPTR(121,1), SYMBOL_HEADER, UPTR0(120), INTCON(3)};
irk_string constructed_122 = {STRING_HEADER(4), 4, "none" };
// symbol none
irk_int constructed_123[] = {UPTR(123,1), SYMBOL_HEADER, UPTR0(122), INTCON(4)};
irk_string constructed_124 = {STRING_HEADER(4), 4, "save" };
// symbol save
irk_int constructed_125[] = {UPTR(125,1), SYMBOL_HEADER, UPTR0(124), INTCON(5)};
irk_string constructed_126 = {STRING_HEADER(7), 7, "closure" };
// symbol closure
irk_int constructed_127[] = {UPTR(127,1), SYMBOL_HEADER, UPTR0(126), INTCON(6)};
irk_string constructed_128 = {STRING_HEADER(3), 3, "env" };
// symbol env
irk_int constructed_129[] = {UPTR(129,1), SYMBOL_HEADER, UPTR0(128), INTCON(7)};
irk_string constructed_130 = {STRING_HEADER(6), 6, "string" };
// symbol string
irk_int constructed_131[] = {UPTR(131,1), SYMBOL_HEADER, UPTR0(130), INTCON(8)};
irk_string constructed_132 = {STRING_HEADER(6), 6, "vector" };
// symbol vector
irk_int constructed_133[] = {UPTR(133,1), SYMBOL_HEADER, UPTR0(132), INTCON(9)};
irk_string constructed_134 = {STRING_HEADER(4), 4, "pair" };
// symbol pair
irk_int constructed_135[] = {UPTR(135,1), SYMBOL_HEADER, UPTR0(134), INTCON(10)};
irk_string constructed_136 = {STRING_HEADER(6), 6, "symbol" };
// symbol symbol
irk_int constructed_137[] = {UPTR(137,1), SYMBOL_HEADER, UPTR0(136), INTCON(11)};
irk_string constructed_138 = {STRING_HEADER(6), 6, "buffer" };
// symbol buffer
irk_int constructed_139[] = {UPTR(139,1), SYMBOL_HEADER, UPTR0(138), INTCON(12)};
irk_string constructed_140 = {STRING_HEADER(7), 7, "foreign" };
// symbol foreign
irk_int constructed_141[] = {UPTR(141,1), SYMBOL_HEADER, UPTR0(140), INTCON(13)};
irk_int constructed_142[] = {UPTR(142,1),(10<<TAGSIZE)|TC_VECTOR,UPTR(123,1),UPTR(125,1),UPTR(127,1),UPTR(129,1),UPTR(131,1),UPTR(133,1),UPTR(135,1),UPTR(137,1),UPTR(139,1),
	UPTR(141,1)};
irk_string constructed_143 = {STRING_HEADER(4), 4, "char" };
// symbol char
irk_int constructed_144[] = {UPTR(144,1), SYMBOL_HEADER, UPTR0(143), INTCON(14)};
irk_string constructed_145 = {STRING_HEADER(4), 4, "bool" };
// symbol bool
irk_int constructed_146[] = {UPTR(146,1), SYMBOL_HEADER, UPTR0(145), INTCON(15)};
irk_string constructed_147 = {STRING_HEADER(3), 3, "nil" };
// symbol nil
irk_int constructed_148[] = {UPTR(148,1), SYMBOL_HEADER, UPTR0(147), INTCON(16)};
irk_string constructed_149 = {STRING_HEADER(9), 9, "undefined" };
// symbol undefined
irk_int constructed_150[] = {UPTR(150,1), SYMBOL_HEADER, UPTR0(149), INTCON(17)};
irk_string constructed_151 = {STRING_HEADER(12), 12, "empty-vector" };
// symbol empty-vector
irk_int constructed_152[] = {UPTR(152,1), SYMBOL_HEADER, UPTR0(151), INTCON(18)};
irk_int constructed_153[] = {UPTR(153,1),(5<<TAGSIZE)|TC_VECTOR,UPTR(144,1),UPTR(146,1),UPTR(148,1),UPTR(150,1),UPTR(152,1)};
irk_string constructed_154 = {STRING_HEADER(79), 79, "Why, sometimes I've believed as many as six impossible things before breakfast." };
irk_string constructed_155 = {STRING_HEADER(29), 29, "\n***\nRuntime Error, halting: " };
irk_string constructed_156 = {STRING_HEADER(2), 2, "\\\\" };
irk_string constructed_157 = {STRING_HEADER(2), 2, "\\\"" };
irk_string constructed_158 = {STRING_HEADER(2), 2, "\\r" };
irk_string constructed_159 = {STRING_HEADER(2), 2, "\\t" };
irk_string constructed_160 = {STRING_HEADER(2), 2, "\\n" };
irk_string constructed_161 = {STRING_HEADER(1), 1, " " };
irk_string constructed_162 = {STRING_HEADER(2), 2, "\\x" };
irk_string constructed_163 = {STRING_HEADER(32), 32, "!\"#$%&'*+,-./:;<=>?@\\^_`[({|})]~" };
irk_string constructed_164 = {STRING_HEADER(10), 10, "0123456789" };
irk_string constructed_165 = {STRING_HEADER(26), 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ" };
irk_string constructed_166 = {STRING_HEADER(26), 26, "abcdefghijklmnopqrstuvwxyz" };
irk_int constructed_167[] = {UPTR(167,1),(16<<TAGSIZE)|TC_VECTOR,3145730,3211266,3276802,3342338,3407874,3473410,3538946,3604482,3670018,
	3735554,6356994,6422530,6488066,6553602,6619138,6684674};
irk_string constructed_168 = {STRING_HEADER(18), 18, "uncaught exception" };
irk_string constructed_169 = {STRING_HEADER(9), 9, "datatypes" };
// symbol datatypes
irk_int constructed_170[] = {UPTR(170,1), SYMBOL_HEADER, UPTR0(169), INTCON(19)};
irk_string constructed_171 = {STRING_HEADER(1), 1, "t" };
// symbol t
irk_int constructed_172[] = {UPTR(172,1), SYMBOL_HEADER, UPTR0(171), INTCON(20)};
irk_string constructed_173 = {STRING_HEADER(3), 3, "cmp" };
// symbol cmp
irk_int constructed_174[] = {UPTR(174,1), SYMBOL_HEADER, UPTR0(173), INTCON(21)};
irk_string constructed_175 = {STRING_HEADER(1), 1, "<" };
// symbol <
irk_int constructed_176[] = {UPTR(176,1), SYMBOL_HEADER, UPTR0(175), INTCON(22)};
irk_string constructed_177 = {STRING_HEADER(1), 1, "=" };
// symbol =
irk_int constructed_178[] = {UPTR(178,1), SYMBOL_HEADER, UPTR0(177), INTCON(23)};
irk_string constructed_179 = {STRING_HEADER(1), 1, ">" };
// symbol >
irk_int constructed_180[] = {UPTR(180,1), SYMBOL_HEADER, UPTR0(179), INTCON(24)};
irk_string constructed_181 = {STRING_HEADER(5), 5, "maybe" };
// symbol maybe
irk_int constructed_182[] = {UPTR(182,1), SYMBOL_HEADER, UPTR0(181), INTCON(25)};
irk_string constructed_183 = {STRING_HEADER(3), 3, "yes" };
// symbol yes
irk_int constructed_184[] = {UPTR(184,1), SYMBOL_HEADER, UPTR0(183), INTCON(26)};
irk_string constructed_185 = {STRING_HEADER(5), 5, "quote" };
// symbol quote
irk_int constructed_186[] = {UPTR(186,1), SYMBOL_HEADER, UPTR0(185), INTCON(27)};
irk_string constructed_187 = {STRING_HEADER(1), 1, "a" };
// symbol a
irk_int constructed_188[] = {UPTR(188,1), SYMBOL_HEADER, UPTR0(187), INTCON(28)};
irk_string constructed_189 = {STRING_HEADER(2), 2, "no" };
// symbol no
irk_int constructed_190[] = {UPTR(190,1), SYMBOL_HEADER, UPTR0(189), INTCON(29)};
irk_string constructed_191 = {STRING_HEADER(4), 4, "true" };
// symbol true
irk_int constructed_192[] = {UPTR(192,1), SYMBOL_HEADER, UPTR0(191), INTCON(30)};
irk_string constructed_193 = {STRING_HEADER(5), 5, "false" };
// symbol false
irk_int constructed_194[] = {UPTR(194,1), SYMBOL_HEADER, UPTR0(193), INTCON(31)};
irk_string constructed_195 = {STRING_HEADER(6), 6, "result" };
// symbol result
irk_int constructed_196[] = {UPTR(196,1), SYMBOL_HEADER, UPTR0(195), INTCON(32)};
irk_string constructed_197 = {STRING_HEADER(2), 2, "ok" };
// symbol ok
irk_int constructed_198[] = {UPTR(198,1), SYMBOL_HEADER, UPTR0(197), INTCON(33)};
irk_string constructed_199 = {STRING_HEADER(3), 3, "err" };
// symbol err
irk_int constructed_200[] = {UPTR(200,1), SYMBOL_HEADER, UPTR0(199), INTCON(34)};
irk_string constructed_201 = {STRING_HEADER(1), 1, "b" };
// symbol b
irk_int constructed_202[] = {UPTR(202,1), SYMBOL_HEADER, UPTR0(201), INTCON(35)};
irk_string constructed_203 = {STRING_HEADER(4), 4, "list" };
// symbol list
irk_int constructed_204[] = {UPTR(204,1), SYMBOL_HEADER, UPTR0(203), INTCON(36)};
irk_string constructed_205 = {STRING_HEADER(4), 4, "cons" };
// symbol cons
irk_int constructed_206[] = {UPTR(206,1), SYMBOL_HEADER, UPTR0(205), INTCON(37)};
irk_string constructed_207 = {STRING_HEADER(4), 4, "sexp" };
// symbol sexp
irk_int constructed_208[] = {UPTR(208,1), SYMBOL_HEADER, UPTR0(207), INTCON(38)};
irk_string constructed_209 = {STRING_HEADER(3), 3, "src" };
// symbol src
irk_int constructed_210[] = {UPTR(210,1), SYMBOL_HEADER, UPTR0(209), INTCON(39)};
irk_string constructed_211 = {STRING_HEADER(5), 5, "undef" };
// symbol undef
irk_int constructed_212[] = {UPTR(212,1), SYMBOL_HEADER, UPTR0(211), INTCON(40)};
irk_string constructed_213 = {STRING_HEADER(6), 6, "record" };
// symbol record
irk_int constructed_214[] = {UPTR(214,1), SYMBOL_HEADER, UPTR0(213), INTCON(41)};
irk_string constructed_215 = {STRING_HEADER(5), 5, "field" };
// symbol field
irk_int constructed_216[] = {UPTR(216,1), SYMBOL_HEADER, UPTR0(215), INTCON(42)};
irk_string constructed_217 = {STRING_HEADER(4), 4, "attr" };
// symbol attr
irk_int constructed_218[] = {UPTR(218,1), SYMBOL_HEADER, UPTR0(217), INTCON(43)};
irk_string constructed_219 = {STRING_HEADER(4), 4, "base" };
// symbol base
irk_int constructed_220[] = {UPTR(220,1), SYMBOL_HEADER, UPTR0(219), INTCON(44)};
irk_string constructed_221 = {STRING_HEADER(8), 8, "rproduct" };
// symbol rproduct
irk_int constructed_222[] = {UPTR(222,1), SYMBOL_HEADER, UPTR0(221), INTCON(45)};
irk_string constructed_223 = {STRING_HEADER(6), 6, "rlabel" };
// symbol rlabel
irk_int constructed_224[] = {UPTR(224,1), SYMBOL_HEADER, UPTR0(223), INTCON(46)};
irk_string constructed_225 = {STRING_HEADER(4), 4, "path" };
// symbol path
irk_int constructed_226[] = {UPTR(226,1), SYMBOL_HEADER, UPTR0(225), INTCON(47)};
irk_string constructed_227 = {STRING_HEADER(3), 3, "pre" };
// symbol pre
irk_int constructed_228[] = {UPTR(228,1), SYMBOL_HEADER, UPTR0(227), INTCON(48)};
irk_string constructed_229 = {STRING_HEADER(4), 4, "line" };
// symbol line
irk_int constructed_230[] = {UPTR(230,1), SYMBOL_HEADER, UPTR0(229), INTCON(49)};
irk_string constructed_231 = {STRING_HEADER(3), 3, "pos" };
// symbol pos
irk_int constructed_232[] = {UPTR(232,1), SYMBOL_HEADER, UPTR0(231), INTCON(50)};
irk_string constructed_233 = {STRING_HEADER(8), 8, "rdefault" };
// symbol rdefault
irk_int constructed_234[] = {UPTR(234,1), SYMBOL_HEADER, UPTR0(233), INTCON(51)};
irk_string constructed_235 = {STRING_HEADER(3), 3, "abs" };
// symbol abs
irk_int constructed_236[] = {UPTR(236,1), SYMBOL_HEADER, UPTR0(235), INTCON(52)};
irk_string constructed_237 = {STRING_HEADER(6), 6, "inline" };
// symbol inline
irk_int constructed_238[] = {UPTR(238,1), SYMBOL_HEADER, UPTR0(237), INTCON(53)};
irk_string constructed_239 = {STRING_HEADER(5), 5, "macro" };
// symbol macro
irk_int constructed_240[] = {UPTR(240,1), SYMBOL_HEADER, UPTR0(239), INTCON(54)};
irk_string constructed_241 = {STRING_HEADER(8), 8, "src-step" };
// symbol src-step
irk_int constructed_242[] = {UPTR(242,1), SYMBOL_HEADER, UPTR0(241), INTCON(55)};
irk_string constructed_243 = {STRING_HEADER(2), 2, "at" };
// symbol at
irk_int constructed_244[] = {UPTR(244,1), SYMBOL_HEADER, UPTR0(243), INTCON(56)};
irk_string constructed_245 = {STRING_HEADER(7), 7, "inlined" };
// symbol inlined
irk_int constructed_246[] = {UPTR(246,1), SYMBOL_HEADER, UPTR0(245), INTCON(57)};
irk_string constructed_247 = {STRING_HEADER(6), 6, "fqueue" };
// symbol fqueue
irk_int constructed_248[] = {UPTR(248,1), SYMBOL_HEADER, UPTR0(247), INTCON(58)};
irk_string constructed_249 = {STRING_HEADER(3), 3, "set" };
// symbol set
irk_int constructed_250[] = {UPTR(250,1), SYMBOL_HEADER, UPTR0(249), INTCON(59)};
irk_string constructed_251 = {STRING_HEADER(3), 3, "red" };
// symbol red
irk_int constructed_252[] = {UPTR(252,1), SYMBOL_HEADER, UPTR0(251), INTCON(60)};
irk_string constructed_253 = {STRING_HEADER(5), 5, "black" };
// symbol black
irk_int constructed_254[] = {UPTR(254,1), SYMBOL_HEADER, UPTR0(253), INTCON(61)};
irk_string constructed_255 = {STRING_HEADER(5), 5, "empty" };
// symbol empty
irk_int constructed_256[] = {UPTR(256,1), SYMBOL_HEADER, UPTR0(255), INTCON(62)};
irk_string constructed_257 = {STRING_HEADER(5), 5, "alist" };
// symbol alist
irk_int constructed_258[] = {UPTR(258,1), SYMBOL_HEADER, UPTR0(257), INTCON(63)};
irk_string constructed_259 = {STRING_HEADER(5), 5, "entry" };
// symbol entry
irk_int constructed_260[] = {UPTR(260,1), SYMBOL_HEADER, UPTR0(259), INTCON(64)};
irk_string constructed_261 = {STRING_HEADER(8), 8, "alist-ob" };
// symbol alist-ob
irk_int constructed_262[] = {UPTR(262,1), SYMBOL_HEADER, UPTR0(261), INTCON(65)};
irk_string constructed_263 = {STRING_HEADER(4), 4, "cint" };
// symbol cint
irk_int constructed_264[] = {UPTR(264,1), SYMBOL_HEADER, UPTR0(263), INTCON(66)};
irk_string constructed_265 = {STRING_HEADER(5), 5, "short" };
// symbol short
irk_int constructed_266[] = {UPTR(266,1), SYMBOL_HEADER, UPTR0(265), INTCON(67)};
irk_string constructed_267 = {STRING_HEADER(4), 4, "long" };
// symbol long
irk_int constructed_268[] = {UPTR(268,1), SYMBOL_HEADER, UPTR0(267), INTCON(68)};
irk_string constructed_269 = {STRING_HEADER(8), 8, "longlong" };
// symbol longlong
irk_int constructed_270[] = {UPTR(270,1), SYMBOL_HEADER, UPTR0(269), INTCON(69)};
irk_string constructed_271 = {STRING_HEADER(5), 5, "width" };
// symbol width
irk_int constructed_272[] = {UPTR(272,1), SYMBOL_HEADER, UPTR0(271), INTCON(70)};
irk_string constructed_273 = {STRING_HEADER(5), 5, "ctype" };
// symbol ctype
irk_int constructed_274[] = {UPTR(274,1), SYMBOL_HEADER, UPTR0(273), INTCON(71)};
irk_string constructed_275 = {STRING_HEADER(4), 4, "name" };
// symbol name
irk_int constructed_276[] = {UPTR(276,1), SYMBOL_HEADER, UPTR0(275), INTCON(72)};
irk_string constructed_277 = {STRING_HEADER(5), 5, "array" };
// symbol array
irk_int constructed_278[] = {UPTR(278,1), SYMBOL_HEADER, UPTR0(277), INTCON(73)};
irk_string constructed_279 = {STRING_HEADER(7), 7, "pointer" };
// symbol pointer
irk_int constructed_280[] = {UPTR(280,1), SYMBOL_HEADER, UPTR0(279), INTCON(74)};
irk_string constructed_281 = {STRING_HEADER(6), 6, "struct" };
// symbol struct
irk_int constructed_282[] = {UPTR(282,1), SYMBOL_HEADER, UPTR0(281), INTCON(75)};
irk_string constructed_283 = {STRING_HEADER(5), 5, "union" };
// symbol union
irk_int constructed_284[] = {UPTR(284,1), SYMBOL_HEADER, UPTR0(283), INTCON(76)};
irk_string constructed_285 = {STRING_HEADER(6), 6, "cfield" };
// symbol cfield
irk_int constructed_286[] = {UPTR(286,1), SYMBOL_HEADER, UPTR0(285), INTCON(77)};
irk_string constructed_287 = {STRING_HEADER(4), 4, "cdef" };
// symbol cdef
irk_int constructed_288[] = {UPTR(288,1), SYMBOL_HEADER, UPTR0(287), INTCON(78)};
irk_string constructed_289 = {STRING_HEADER(4), 4, "csig" };
// symbol csig
irk_int constructed_290[] = {UPTR(290,1), SYMBOL_HEADER, UPTR0(289), INTCON(79)};
irk_string constructed_291 = {STRING_HEADER(3), 3, "fun" };
// symbol fun
irk_int constructed_292[] = {UPTR(292,1), SYMBOL_HEADER, UPTR0(291), INTCON(80)};
irk_string constructed_293 = {STRING_HEADER(3), 3, "obj" };
// symbol obj
irk_int constructed_294[] = {UPTR(294,1), SYMBOL_HEADER, UPTR0(293), INTCON(81)};
irk_string constructed_295 = {STRING_HEADER(4), 4, "tree" };
// symbol tree
irk_int constructed_296[] = {UPTR(296,1), SYMBOL_HEADER, UPTR0(295), INTCON(82)};
irk_string constructed_297 = {STRING_HEADER(5), 5, "ident" };
// symbol ident
irk_int constructed_298[] = {UPTR(298,1), SYMBOL_HEADER, UPTR0(297), INTCON(83)};
irk_string constructed_299 = {STRING_HEADER(7), 7, "userimm" };
// symbol userimm
irk_int constructed_300[] = {UPTR(300,1), SYMBOL_HEADER, UPTR0(299), INTCON(84)};
irk_string constructed_301 = {STRING_HEADER(7), 7, "usertup" };
// symbol usertup
irk_int constructed_302[] = {UPTR(302,1), SYMBOL_HEADER, UPTR0(301), INTCON(85)};
irk_string constructed_303 = {STRING_HEADER(6), 6, "sysimm" };
// symbol sysimm
irk_int constructed_304[] = {UPTR(304,1), SYMBOL_HEADER, UPTR0(303), INTCON(86)};
irk_string constructed_305 = {STRING_HEADER(6), 6, "systup" };
// symbol systup
irk_int constructed_306[] = {UPTR(306,1), SYMBOL_HEADER, UPTR0(305), INTCON(87)};
irk_string constructed_307 = {STRING_HEADER(6), 6, "map-ob" };
// symbol map-ob
irk_int constructed_308[] = {UPTR(308,1), SYMBOL_HEADER, UPTR0(307), INTCON(88)};
irk_string constructed_309 = {STRING_HEADER(5), 5, "arrow" };
// symbol arrow
irk_int constructed_310[] = {UPTR(310,1), SYMBOL_HEADER, UPTR0(309), INTCON(89)};
irk_string constructed_311 = {STRING_HEADER(7), 7, "set2-ob" };
// symbol set2-ob
irk_int constructed_312[] = {UPTR(312,1), SYMBOL_HEADER, UPTR0(311), INTCON(90)};
irk_string constructed_313 = {STRING_HEADER(8), 8, "hufftree" };
// symbol hufftree
irk_int constructed_314[] = {UPTR(314,1), SYMBOL_HEADER, UPTR0(313), INTCON(91)};
irk_string constructed_315 = {STRING_HEADER(4), 4, "node" };
// symbol node
irk_int constructed_316[] = {UPTR(316,1), SYMBOL_HEADER, UPTR0(315), INTCON(92)};
irk_string constructed_317 = {STRING_HEADER(4), 4, "leaf" };
// symbol leaf
irk_int constructed_318[] = {UPTR(318,1), SYMBOL_HEADER, UPTR0(317), INTCON(93)};
irk_string constructed_319 = {STRING_HEADER(16), 16, "dirent-file-type" };
// symbol dirent-file-type
irk_int constructed_320[] = {UPTR(320,1), SYMBOL_HEADER, UPTR0(319), INTCON(94)};
irk_string constructed_321 = {STRING_HEADER(7), 7, "regular" };
// symbol regular
irk_int constructed_322[] = {UPTR(322,1), SYMBOL_HEADER, UPTR0(321), INTCON(95)};
irk_string constructed_323 = {STRING_HEADER(9), 9, "directory" };
// symbol directory
irk_int constructed_324[] = {UPTR(324,1), SYMBOL_HEADER, UPTR0(323), INTCON(96)};
irk_string constructed_325 = {STRING_HEADER(4), 4, "fifo" };
// symbol fifo
irk_int constructed_326[] = {UPTR(326,1), SYMBOL_HEADER, UPTR0(325), INTCON(97)};
irk_string constructed_327 = {STRING_HEADER(6), 6, "socket" };
// symbol socket
irk_int constructed_328[] = {UPTR(328,1), SYMBOL_HEADER, UPTR0(327), INTCON(98)};
irk_string constructed_329 = {STRING_HEADER(5), 5, "block" };
// symbol block
irk_int constructed_330[] = {UPTR(330,1), SYMBOL_HEADER, UPTR0(329), INTCON(99)};
irk_string constructed_331 = {STRING_HEADER(7), 7, "symlink" };
// symbol symlink
irk_int constructed_332[] = {UPTR(332,1), SYMBOL_HEADER, UPTR0(331), INTCON(100)};
irk_string constructed_333 = {STRING_HEADER(10), 10, "exceptions" };
// symbol exceptions
irk_int constructed_334[] = {UPTR(334,1), SYMBOL_HEADER, UPTR0(333), INTCON(101)};
irk_string constructed_335 = {STRING_HEADER(12), 12, "String/Range" };
// symbol String/Range
irk_int constructed_336[] = {UPTR(336,1), SYMBOL_HEADER, UPTR0(335), INTCON(102)};
irk_string constructed_337 = {STRING_HEADER(7), 7, "product" };
// symbol product
irk_int constructed_338[] = {UPTR(338,1), SYMBOL_HEADER, UPTR0(337), INTCON(103)};
irk_string constructed_339 = {STRING_HEADER(17), 17, "Metadata/KeyError" };
// symbol Metadata/KeyError
irk_int constructed_340[] = {UPTR(340,1), SYMBOL_HEADER, UPTR0(339), INTCON(104)};
irk_string constructed_341 = {STRING_HEADER(18), 18, "Metadata/Malformed" };
// symbol Metadata/Malformed
irk_int constructed_342[] = {UPTR(342,1), SYMBOL_HEADER, UPTR0(341), INTCON(105)};
irk_string constructed_343 = {STRING_HEADER(12), 12, "DivideByZero" };
// symbol DivideByZero
irk_int constructed_344[] = {UPTR(344,1), SYMBOL_HEADER, UPTR0(343), INTCON(106)};
irk_string constructed_345 = {STRING_HEADER(11), 11, "BadHexDigit" };
// symbol BadHexDigit
irk_int constructed_346[] = {UPTR(346,1), SYMBOL_HEADER, UPTR0(345), INTCON(107)};
irk_string constructed_347 = {STRING_HEADER(12), 12, "Hex/BadDigit" };
// symbol Hex/BadDigit
irk_int constructed_348[] = {UPTR(348,1), SYMBOL_HEADER, UPTR0(347), INTCON(108)};
irk_string constructed_349 = {STRING_HEADER(10), 10, "Tree/Empty" };
// symbol Tree/Empty
irk_int constructed_350[] = {UPTR(350,1), SYMBOL_HEADER, UPTR0(349), INTCON(109)};
irk_string constructed_351 = {STRING_HEADER(14), 14, "AssertionError" };
// symbol AssertionError
irk_int constructed_352[] = {UPTR(352,1), SYMBOL_HEADER, UPTR0(351), INTCON(110)};
irk_string constructed_353 = {STRING_HEADER(14), 14, "Hpack/BadIndex" };
// symbol Hpack/BadIndex
irk_int constructed_354[] = {UPTR(354,1), SYMBOL_HEADER, UPTR0(353), INTCON(111)};
irk_string constructed_355 = {STRING_HEADER(19), 19, "Hpack/BadHeaderCode" };
// symbol Hpack/BadHeaderCode
irk_int constructed_356[] = {UPTR(356,1), SYMBOL_HEADER, UPTR0(355), INTCON(112)};
irk_string constructed_357 = {STRING_HEADER(18), 18, "Hex/BadInputString" };
// symbol Hex/BadInputString
irk_int constructed_358[] = {UPTR(358,1), SYMBOL_HEADER, UPTR0(357), INTCON(113)};
irk_string constructed_359 = {STRING_HEADER(5), 5, "tuple" };
// symbol tuple
irk_int constructed_360[] = {UPTR(360,1), SYMBOL_HEADER, UPTR0(359), INTCON(114)};
irk_int constructed_361[] = {UPTR(361,5479),UOHEAD(2,1),UPTR(121,1),UITAG(0),UOHEAD(2,1),UPTR(170,1),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),
	UPTR(172,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,16),
	TC_NIL,UOHEAD(2,0),UPTR(361,19),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,25),TC_NIL,
	UOHEAD(2,0),UPTR(361,28),UITAG(0),CONS_HEADER,UPTR(361,31),TC_NIL,CONS_HEADER,UPTR(361,22),UPTR(361,34),UOHEAD(2,0),
	UPTR(361,37),UITAG(0),CONS_HEADER,UPTR(361,40),TC_NIL,CONS_HEADER,UPTR(361,13),UPTR(361,43),CONS_HEADER,UPTR(361,10),
	UPTR(361,46),UOHEAD(2,0),UPTR(361,49),UITAG(0),CONS_HEADER,UPTR(361,52),TC_NIL,UOHEAD(2,0),UPTR(361,55),UITAG(0),
	CONS_HEADER,UPTR(361,58),TC_NIL,CONS_HEADER,UPTR(361,7),UPTR(361,61),UOHEAD(2,0),UPTR(361,64),UITAG(0),UOHEAD(2,1),
	UPTR(174,1),UITAG(0),UOHEAD(2,1),UPTR(176,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(361,79),TC_NIL,CONS_HEADER,UPTR(361,76),UPTR(361,82),CONS_HEADER,UPTR(361,73),UPTR(361,85),
	UOHEAD(2,0),UPTR(361,88),UITAG(0),UOHEAD(2,1),UPTR(178,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,100),TC_NIL,CONS_HEADER,UPTR(361,97),UPTR(361,103),CONS_HEADER,UPTR(361,94),
	UPTR(361,106),UOHEAD(2,0),UPTR(361,109),UITAG(0),UOHEAD(2,1),UPTR(180,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,121),TC_NIL,CONS_HEADER,UPTR(361,118),UPTR(361,124),CONS_HEADER,
	UPTR(361,115),UPTR(361,127),UOHEAD(2,0),UPTR(361,130),UITAG(0),CONS_HEADER,UPTR(361,133),TC_NIL,CONS_HEADER,UPTR(361,112),
	UPTR(361,136),CONS_HEADER,UPTR(361,91),UPTR(361,139),UOHEAD(2,0),UPTR(361,142),UITAG(0),CONS_HEADER,UPTR(361,145),TC_NIL,
	CONS_HEADER,UPTR(361,70),UPTR(361,148),UOHEAD(2,0),UPTR(361,151),UITAG(0),UOHEAD(2,1),UPTR(182,1),UITAG(0),UOHEAD(2,1),
	UPTR(184,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),
	UITAG(0),CONS_HEADER,UPTR(361,169),TC_NIL,CONS_HEADER,UPTR(361,166),UPTR(361,172),UOHEAD(2,0),UPTR(361,175),UITAG(0),
	CONS_HEADER,UPTR(361,178),TC_NIL,UOHEAD(2,0),UPTR(361,181),UITAG(0),CONS_HEADER,UPTR(361,184),TC_NIL,CONS_HEADER,
	UPTR(361,163),UPTR(361,187),CONS_HEADER,UPTR(361,160),UPTR(361,190),UOHEAD(2,0),UPTR(361,193),UITAG(0),UOHEAD(2,1),UPTR(190,1),
	UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,205),TC_NIL,
	CONS_HEADER,UPTR(361,202),UPTR(361,208),CONS_HEADER,UPTR(361,199),UPTR(361,211),UOHEAD(2,0),UPTR(361,214),UITAG(0),CONS_HEADER,
	UPTR(361,217),TC_NIL,CONS_HEADER,UPTR(361,196),UPTR(361,220),UOHEAD(2,0),UPTR(361,223),UITAG(0),CONS_HEADER,UPTR(361,226),
	TC_NIL,CONS_HEADER,UPTR(361,157),UPTR(361,229),UOHEAD(2,0),UPTR(361,232),UITAG(0),UOHEAD(2,1),UPTR(146,1),UITAG(0),
	UOHEAD(2,1),UPTR(192,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(361,247),TC_NIL,CONS_HEADER,UPTR(361,244),UPTR(361,250),CONS_HEADER,UPTR(361,241),UPTR(361,253),UOHEAD(2,0),UPTR(361,256),
	UITAG(0),UOHEAD(2,1),UPTR(194,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(361,268),TC_NIL,CONS_HEADER,UPTR(361,265),UPTR(361,271),CONS_HEADER,UPTR(361,262),UPTR(361,274),UOHEAD(2,0),
	UPTR(361,277),UITAG(0),CONS_HEADER,UPTR(361,280),TC_NIL,CONS_HEADER,UPTR(361,259),UPTR(361,283),UOHEAD(2,0),UPTR(361,286),
	UITAG(0),CONS_HEADER,UPTR(361,289),TC_NIL,CONS_HEADER,UPTR(361,238),UPTR(361,292),UOHEAD(2,0),UPTR(361,295),UITAG(0),
	UOHEAD(2,1),UPTR(196,1),UITAG(0),UOHEAD(2,1),UPTR(198,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,313),TC_NIL,CONS_HEADER,UPTR(361,310),
	UPTR(361,316),UOHEAD(2,0),UPTR(361,319),UITAG(0),CONS_HEADER,UPTR(361,322),TC_NIL,UOHEAD(2,0),UPTR(361,325),UITAG(0),
	CONS_HEADER,UPTR(361,328),TC_NIL,CONS_HEADER,UPTR(361,307),UPTR(361,331),CONS_HEADER,UPTR(361,304),UPTR(361,334),UOHEAD(2,0),
	UPTR(361,337),UITAG(0),UOHEAD(2,1),UPTR(200,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(186,1),
	UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,352),TC_NIL,CONS_HEADER,UPTR(361,349),UPTR(361,355),
	UOHEAD(2,0),UPTR(361,358),UITAG(0),CONS_HEADER,UPTR(361,361),TC_NIL,UOHEAD(2,0),UPTR(361,364),UITAG(0),CONS_HEADER,
	UPTR(361,367),TC_NIL,CONS_HEADER,UPTR(361,346),UPTR(361,370),CONS_HEADER,UPTR(361,343),UPTR(361,373),UOHEAD(2,0),UPTR(361,376),
	UITAG(0),CONS_HEADER,UPTR(361,379),TC_NIL,CONS_HEADER,UPTR(361,340),UPTR(361,382),UOHEAD(2,0),UPTR(361,385),UITAG(0),
	CONS_HEADER,UPTR(361,388),TC_NIL,CONS_HEADER,UPTR(361,301),UPTR(361,391),UOHEAD(2,0),UPTR(361,394),UITAG(0),UOHEAD(2,1),
	UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(148,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(361,409),TC_NIL,CONS_HEADER,UPTR(361,406),UPTR(361,412),CONS_HEADER,UPTR(361,403),UPTR(361,415),
	UOHEAD(2,0),UPTR(361,418),UITAG(0),UOHEAD(2,1),UPTR(206,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,433),TC_NIL,CONS_HEADER,UPTR(361,430),
	UPTR(361,436),UOHEAD(2,0),UPTR(361,439),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,451),TC_NIL,CONS_HEADER,UPTR(361,448),UPTR(361,454),UOHEAD(2,0),
	UPTR(361,457),UITAG(0),CONS_HEADER,UPTR(361,460),TC_NIL,CONS_HEADER,UPTR(361,445),UPTR(361,463),UOHEAD(2,0),UPTR(361,466),
	UITAG(0),CONS_HEADER,UPTR(361,469),TC_NIL,CONS_HEADER,UPTR(361,442),UPTR(361,472),UOHEAD(2,0),UPTR(361,475),UITAG(0),
	CONS_HEADER,UPTR(361,478),TC_NIL,CONS_HEADER,UPTR(361,427),UPTR(361,481),CONS_HEADER,UPTR(361,424),UPTR(361,484),UOHEAD(2,0),
	UPTR(361,487),UITAG(0),CONS_HEADER,UPTR(361,490),TC_NIL,CONS_HEADER,UPTR(361,421),UPTR(361,493),UOHEAD(2,0),UPTR(361,496),
	UITAG(0),CONS_HEADER,UPTR(361,499),TC_NIL,CONS_HEADER,UPTR(361,400),UPTR(361,502),UOHEAD(2,0),UPTR(361,505),UITAG(0),
	UOHEAD(2,1),UPTR(208,1),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(208,1),UITAG(0),CONS_HEADER,UPTR(361,523),TC_NIL,UOHEAD(2,0),UPTR(361,526),
	UITAG(0),CONS_HEADER,UPTR(361,529),TC_NIL,CONS_HEADER,UPTR(361,520),UPTR(361,532),UOHEAD(2,0),UPTR(361,535),UITAG(0),
	UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,541),TC_NIL,UOHEAD(2,0),UPTR(361,544),UITAG(0),CONS_HEADER,
	UPTR(361,547),TC_NIL,CONS_HEADER,UPTR(361,538),UPTR(361,550),UOHEAD(2,0),UPTR(361,553),UITAG(0),CONS_HEADER,UPTR(361,556),
	TC_NIL,CONS_HEADER,UPTR(361,517),UPTR(361,559),CONS_HEADER,UPTR(361,514),UPTR(361,562),UOHEAD(2,0),UPTR(361,565),UITAG(0),
	UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,
	UPTR(361,577),TC_NIL,UOHEAD(2,0),UPTR(361,580),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,586),
	TC_NIL,UOHEAD(2,0),UPTR(361,589),UITAG(0),CONS_HEADER,UPTR(361,592),TC_NIL,CONS_HEADER,UPTR(361,583),UPTR(361,595),
	UOHEAD(2,0),UPTR(361,598),UITAG(0),CONS_HEADER,UPTR(361,601),TC_NIL,CONS_HEADER,UPTR(361,574),UPTR(361,604),CONS_HEADER,
	UPTR(361,571),UPTR(361,607),UOHEAD(2,0),UPTR(361,610),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),UOHEAD(2,5),5,
	UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,622),TC_NIL,UOHEAD(2,0),UPTR(361,625),UITAG(0),
	UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,631),TC_NIL,UOHEAD(2,0),UPTR(361,634),UITAG(0),CONS_HEADER,
	UPTR(361,637),TC_NIL,CONS_HEADER,UPTR(361,628),UPTR(361,640),UOHEAD(2,0),UPTR(361,643),UITAG(0),CONS_HEADER,UPTR(361,646),
	TC_NIL,CONS_HEADER,UPTR(361,619),UPTR(361,649),CONS_HEADER,UPTR(361,616),UPTR(361,652),UOHEAD(2,0),UPTR(361,655),UITAG(0),
	UOHEAD(2,1),UPTR(144,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(144,1),UITAG(0),CONS_HEADER,
	UPTR(361,667),TC_NIL,UOHEAD(2,0),UPTR(361,670),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,676),
	TC_NIL,UOHEAD(2,0),UPTR(361,679),UITAG(0),CONS_HEADER,UPTR(361,682),TC_NIL,CONS_HEADER,UPTR(361,673),UPTR(361,685),
	UOHEAD(2,0),UPTR(361,688),UITAG(0),CONS_HEADER,UPTR(361,691),TC_NIL,CONS_HEADER,UPTR(361,664),UPTR(361,694),CONS_HEADER,
	UPTR(361,661),UPTR(361,697),UOHEAD(2,0),UPTR(361,700),UITAG(0),UOHEAD(2,1),UPTR(146,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,1),UPTR(146,1),UITAG(0),CONS_HEADER,UPTR(361,712),TC_NIL,UOHEAD(2,0),UPTR(361,715),UITAG(0),
	UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,721),TC_NIL,UOHEAD(2,0),UPTR(361,724),UITAG(0),CONS_HEADER,
	UPTR(361,727),TC_NIL,CONS_HEADER,UPTR(361,718),UPTR(361,730),UOHEAD(2,0),UPTR(361,733),UITAG(0),CONS_HEADER,UPTR(361,736),
	TC_NIL,CONS_HEADER,UPTR(361,709),UPTR(361,739),CONS_HEADER,UPTR(361,706),UPTR(361,742),UOHEAD(2,0),UPTR(361,745),UITAG(0),
	UOHEAD(2,1),UPTR(115,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,
	UPTR(361,757),TC_NIL,UOHEAD(2,0),UPTR(361,760),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,766),
	TC_NIL,UOHEAD(2,0),UPTR(361,769),UITAG(0),CONS_HEADER,UPTR(361,772),TC_NIL,CONS_HEADER,UPTR(361,763),UPTR(361,775),
	UOHEAD(2,0),UPTR(361,778),UITAG(0),CONS_HEADER,UPTR(361,781),TC_NIL,CONS_HEADER,UPTR(361,754),UPTR(361,784),CONS_HEADER,
	UPTR(361,751),UPTR(361,787),UOHEAD(2,0),UPTR(361,790),UITAG(0),UOHEAD(2,1),UPTR(212,1),UITAG(0),UOHEAD(2,5),13,
	UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,802),TC_NIL,UOHEAD(2,0),UPTR(361,805),UITAG(0),
	CONS_HEADER,UPTR(361,808),TC_NIL,UOHEAD(2,0),UPTR(361,811),UITAG(0),CONS_HEADER,UPTR(361,814),TC_NIL,CONS_HEADER,
	UPTR(361,799),UPTR(361,817),CONS_HEADER,UPTR(361,796),UPTR(361,820),UOHEAD(2,0),UPTR(361,823),UITAG(0),UOHEAD(2,1),UPTR(133,1),
	UITAG(0),UOHEAD(2,5),15,UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(208,1),UITAG(0),
	CONS_HEADER,UPTR(361,838),TC_NIL,UOHEAD(2,0),UPTR(361,841),UITAG(0),CONS_HEADER,UPTR(361,844),TC_NIL,CONS_HEADER,
	UPTR(361,835),UPTR(361,847),UOHEAD(2,0),UPTR(361,850),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,856),
	TC_NIL,UOHEAD(2,0),UPTR(361,859),UITAG(0),CONS_HEADER,UPTR(361,862),TC_NIL,CONS_HEADER,UPTR(361,853),UPTR(361,865),
	UOHEAD(2,0),UPTR(361,868),UITAG(0),CONS_HEADER,UPTR(361,871),TC_NIL,CONS_HEADER,UPTR(361,832),UPTR(361,874),CONS_HEADER,
	UPTR(361,829),UPTR(361,877),UOHEAD(2,0),UPTR(361,880),UITAG(0),UOHEAD(2,1),UPTR(214,1),UITAG(0),UOHEAD(2,5),17,
	UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(216,1),UITAG(0),CONS_HEADER,UPTR(361,895),TC_NIL,
	UOHEAD(2,0),UPTR(361,898),UITAG(0),CONS_HEADER,UPTR(361,901),TC_NIL,CONS_HEADER,UPTR(361,892),UPTR(361,904),UOHEAD(2,0),
	UPTR(361,907),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,913),TC_NIL,UOHEAD(2,0),UPTR(361,916),
	UITAG(0),CONS_HEADER,UPTR(361,919),TC_NIL,CONS_HEADER,UPTR(361,910),UPTR(361,922),UOHEAD(2,0),UPTR(361,925),UITAG(0),
	CONS_HEADER,UPTR(361,928),TC_NIL,CONS_HEADER,UPTR(361,889),UPTR(361,931),CONS_HEADER,UPTR(361,886),UPTR(361,934),UOHEAD(2,0),
	UPTR(361,937),UITAG(0),UOHEAD(2,1),UPTR(206,1),UITAG(0),UOHEAD(2,5),19,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),CONS_HEADER,UPTR(361,949),TC_NIL,UOHEAD(2,0),UPTR(361,952),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),
	CONS_HEADER,UPTR(361,958),TC_NIL,UOHEAD(2,0),UPTR(361,961),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,
	UPTR(361,967),TC_NIL,UOHEAD(2,0),UPTR(361,970),UITAG(0),CONS_HEADER,UPTR(361,973),TC_NIL,CONS_HEADER,UPTR(361,964),
	UPTR(361,976),CONS_HEADER,UPTR(361,955),UPTR(361,979),UOHEAD(2,0),UPTR(361,982),UITAG(0),CONS_HEADER,UPTR(361,985),TC_NIL,
	CONS_HEADER,UPTR(361,946),UPTR(361,988),CONS_HEADER,UPTR(361,943),UPTR(361,991),UOHEAD(2,0),UPTR(361,994),UITAG(0),UOHEAD(2,1),
	UPTR(218,1),UITAG(0),UOHEAD(2,5),21,UITAG(0),UOHEAD(2,1),UPTR(208,1),UITAG(0),CONS_HEADER,UPTR(361,1006),
	TC_NIL,UOHEAD(2,0),UPTR(361,1009),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,1015),TC_NIL,
	UOHEAD(2,0),UPTR(361,1018),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,1024),TC_NIL,UOHEAD(2,0),
	UPTR(361,1027),UITAG(0),CONS_HEADER,UPTR(361,1030),TC_NIL,CONS_HEADER,UPTR(361,1021),UPTR(361,1033),CONS_HEADER,UPTR(361,1012),
	UPTR(361,1036),UOHEAD(2,0),UPTR(361,1039),UITAG(0),CONS_HEADER,UPTR(361,1042),TC_NIL,CONS_HEADER,UPTR(361,1003),UPTR(361,1045),
	CONS_HEADER,UPTR(361,1000),UPTR(361,1048),UOHEAD(2,0),UPTR(361,1051),UITAG(0),CONS_HEADER,UPTR(361,1054),TC_NIL,CONS_HEADER,
	UPTR(361,997),UPTR(361,1057),CONS_HEADER,UPTR(361,940),UPTR(361,1060),CONS_HEADER,UPTR(361,883),UPTR(361,1063),CONS_HEADER,UPTR(361,826),
	UPTR(361,1066),CONS_HEADER,UPTR(361,793),UPTR(361,1069),CONS_HEADER,UPTR(361,748),UPTR(361,1072),CONS_HEADER,UPTR(361,703),UPTR(361,1075),
	CONS_HEADER,UPTR(361,658),UPTR(361,1078),CONS_HEADER,UPTR(361,613),UPTR(361,1081),CONS_HEADER,UPTR(361,568),UPTR(361,1084),UOHEAD(2,0),
	UPTR(361,1087),UITAG(0),CONS_HEADER,UPTR(361,1090),TC_NIL,CONS_HEADER,UPTR(361,511),UPTR(361,1093),UOHEAD(2,0),UPTR(361,1096),
	UITAG(0),UOHEAD(2,1),UPTR(216,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),
	UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,1111),TC_NIL,UOHEAD(2,0),UPTR(361,1114),UITAG(0),UOHEAD(2,1),
	UPTR(208,1),UITAG(0),CONS_HEADER,UPTR(361,1120),TC_NIL,UOHEAD(2,0),UPTR(361,1123),UITAG(0),UOHEAD(2,1),UPTR(210,1),
	UITAG(0),CONS_HEADER,UPTR(361,1129),TC_NIL,UOHEAD(2,0),UPTR(361,1132),UITAG(0),CONS_HEADER,UPTR(361,1135),TC_NIL,
	CONS_HEADER,UPTR(361,1126),UPTR(361,1138),CONS_HEADER,UPTR(361,1117),UPTR(361,1141),UOHEAD(2,0),UPTR(361,1144),UITAG(0),CONS_HEADER,
	UPTR(361,1147),TC_NIL,CONS_HEADER,UPTR(361,1108),UPTR(361,1150),CONS_HEADER,UPTR(361,1105),UPTR(361,1153),UOHEAD(2,0),UPTR(361,1156),
	UITAG(0),CONS_HEADER,UPTR(361,1159),TC_NIL,UOHEAD(2,0),UPTR(361,1162),UITAG(0),CONS_HEADER,UPTR(361,1165),TC_NIL,
	CONS_HEADER,UPTR(361,1102),UPTR(361,1168),UOHEAD(2,0),UPTR(361,1171),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),UOHEAD(2,1),
	UPTR(148,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,1186),
	TC_NIL,CONS_HEADER,UPTR(361,1183),UPTR(361,1189),CONS_HEADER,UPTR(361,1180),UPTR(361,1192),UOHEAD(2,0),UPTR(361,1195),UITAG(0),
	UOHEAD(2,1),UPTR(220,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(222,1),UITAG(0),UOHEAD(2,1),
	UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(226,1),UITAG(0),CONS_HEADER,UPTR(361,1213),TC_NIL,UOHEAD(2,0),UPTR(361,1216),
	UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,1225),TC_NIL,
	UOHEAD(2,0),UPTR(361,1228),UITAG(0),CONS_HEADER,UPTR(361,1231),TC_NIL,CONS_HEADER,UPTR(361,1222),UPTR(361,1234),UOHEAD(2,0),
	UPTR(361,1237),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(230,1),UITAG(0),CONS_HEADER,UPTR(361,1246),
	TC_NIL,UOHEAD(2,0),UPTR(361,1249),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),
	CONS_HEADER,UPTR(361,1258),TC_NIL,UOHEAD(2,0),UPTR(361,1261),UITAG(0),CONS_HEADER,UPTR(361,1264),TC_NIL,CONS_HEADER,
	UPTR(361,1255),UPTR(361,1267),UOHEAD(2,0),UPTR(361,1270),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(232,1),
	UITAG(0),CONS_HEADER,UPTR(361,1279),TC_NIL,UOHEAD(2,0),UPTR(361,1282),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),
	UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,1291),TC_NIL,UOHEAD(2,0),UPTR(361,1294),UITAG(0),CONS_HEADER,
	UPTR(361,1297),TC_NIL,CONS_HEADER,UPTR(361,1288),UPTR(361,1300),UOHEAD(2,0),UPTR(361,1303),UITAG(0),UOHEAD(2,1),UPTR(234,1),
	UITAG(0),UOHEAD(2,1),UPTR(236,1),UITAG(0),CONS_HEADER,UPTR(361,1312),TC_NIL,UOHEAD(2,0),UPTR(361,1315),UITAG(0),
	CONS_HEADER,UPTR(361,1318),TC_NIL,CONS_HEADER,UPTR(361,1309),UPTR(361,1321),UOHEAD(2,0),UPTR(361,1324),UITAG(0),CONS_HEADER,
	UPTR(361,1327),TC_NIL,CONS_HEADER,UPTR(361,1306),UPTR(361,1330),CONS_HEADER,UPTR(361,1285),UPTR(361,1333),CONS_HEADER,UPTR(361,1276),
	UPTR(361,1336),UOHEAD(2,0),UPTR(361,1339),UITAG(0),CONS_HEADER,UPTR(361,1342),TC_NIL,CONS_HEADER,UPTR(361,1273),UPTR(361,1345),
	CONS_HEADER,UPTR(361,1252),UPTR(361,1348),CONS_HEADER,UPTR(361,1243),UPTR(361,1351),UOHEAD(2,0),UPTR(361,1354),UITAG(0),CONS_HEADER,
	UPTR(361,1357),TC_NIL,CONS_HEADER,UPTR(361,1240),UPTR(361,1360),CONS_HEADER,UPTR(361,1219),UPTR(361,1363),CONS_HEADER,UPTR(361,1210),
	UPTR(361,1366),UOHEAD(2,0),UPTR(361,1369),UITAG(0),CONS_HEADER,UPTR(361,1372),TC_NIL,CONS_HEADER,UPTR(361,1207),UPTR(361,1375),
	UOHEAD(2,0),UPTR(361,1378),UITAG(0),CONS_HEADER,UPTR(361,1381),TC_NIL,UOHEAD(2,0),UPTR(361,1384),UITAG(0),CONS_HEADER,
	UPTR(361,1387),TC_NIL,CONS_HEADER,UPTR(361,1204),UPTR(361,1390),CONS_HEADER,UPTR(361,1201),UPTR(361,1393),UOHEAD(2,0),UPTR(361,1396),
	UITAG(0),UOHEAD(2,1),UPTR(238,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),
	CONS_HEADER,UPTR(361,1408),TC_NIL,UOHEAD(2,0),UPTR(361,1411),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,
	UPTR(361,1417),TC_NIL,UOHEAD(2,0),UPTR(361,1420),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,1426),
	TC_NIL,UOHEAD(2,0),UPTR(361,1429),UITAG(0),CONS_HEADER,UPTR(361,1432),TC_NIL,CONS_HEADER,UPTR(361,1423),UPTR(361,1435),
	CONS_HEADER,UPTR(361,1414),UPTR(361,1438),UOHEAD(2,0),UPTR(361,1441),UITAG(0),CONS_HEADER,UPTR(361,1444),TC_NIL,CONS_HEADER,
	UPTR(361,1405),UPTR(361,1447),CONS_HEADER,UPTR(361,1402),UPTR(361,1450),UOHEAD(2,0),UPTR(361,1453),UITAG(0),UOHEAD(2,1),UPTR(240,1),
	UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,1465),TC_NIL,
	UOHEAD(2,0),UPTR(361,1468),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,1474),TC_NIL,UOHEAD(2,0),
	UPTR(361,1477),UITAG(0),UOHEAD(2,1),UPTR(210,1),UITAG(0),CONS_HEADER,UPTR(361,1483),TC_NIL,UOHEAD(2,0),UPTR(361,1486),
	UITAG(0),CONS_HEADER,UPTR(361,1489),TC_NIL,CONS_HEADER,UPTR(361,1480),UPTR(361,1492),CONS_HEADER,UPTR(361,1471),UPTR(361,1495),
	UOHEAD(2,0),UPTR(361,1498),UITAG(0),CONS_HEADER,UPTR(361,1501),TC_NIL,CONS_HEADER,UPTR(361,1462),UPTR(361,1504),CONS_HEADER,
	UPTR(361,1459),UPTR(361,1507),UOHEAD(2,0),UPTR(361,1510),UITAG(0),CONS_HEADER,UPTR(361,1513),TC_NIL,CONS_HEADER,UPTR(361,1456),
	UPTR(361,1516),CONS_HEADER,UPTR(361,1399),UPTR(361,1519),CONS_HEADER,UPTR(361,1198),UPTR(361,1522),UOHEAD(2,0),UPTR(361,1525),UITAG(0),
	CONS_HEADER,UPTR(361,1528),TC_NIL,CONS_HEADER,UPTR(361,1177),UPTR(361,1531),UOHEAD(2,0),UPTR(361,1534),UITAG(0),UOHEAD(2,1),
	UPTR(242,1),UITAG(0),UOHEAD(2,1),UPTR(244,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(222,1),
	UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(226,1),UITAG(0),CONS_HEADER,UPTR(361,1555),TC_NIL,
	UOHEAD(2,0),UPTR(361,1558),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,
	UPTR(361,1567),TC_NIL,UOHEAD(2,0),UPTR(361,1570),UITAG(0),CONS_HEADER,UPTR(361,1573),TC_NIL,CONS_HEADER,UPTR(361,1564),
	UPTR(361,1576),UOHEAD(2,0),UPTR(361,1579),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(230,1),UITAG(0),
	CONS_HEADER,UPTR(361,1588),TC_NIL,UOHEAD(2,0),UPTR(361,1591),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),
	UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,1600),TC_NIL,UOHEAD(2,0),UPTR(361,1603),UITAG(0),CONS_HEADER,UPTR(361,1606),
	TC_NIL,CONS_HEADER,UPTR(361,1597),UPTR(361,1609),UOHEAD(2,0),UPTR(361,1612),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),
	UOHEAD(2,1),UPTR(232,1),UITAG(0),CONS_HEADER,UPTR(361,1621),TC_NIL,UOHEAD(2,0),UPTR(361,1624),UITAG(0),UOHEAD(2,1),
	UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,1633),TC_NIL,UOHEAD(2,0),UPTR(361,1636),
	UITAG(0),CONS_HEADER,UPTR(361,1639),TC_NIL,CONS_HEADER,UPTR(361,1630),UPTR(361,1642),UOHEAD(2,0),UPTR(361,1645),UITAG(0),
	UOHEAD(2,1),UPTR(234,1),UITAG(0),UOHEAD(2,1),UPTR(236,1),UITAG(0),CONS_HEADER,UPTR(361,1654),TC_NIL,UOHEAD(2,0),
	UPTR(361,1657),UITAG(0),CONS_HEADER,UPTR(361,1660),TC_NIL,CONS_HEADER,UPTR(361,1651),UPTR(361,1663),UOHEAD(2,0),UPTR(361,1666),
	UITAG(0),CONS_HEADER,UPTR(361,1669),TC_NIL,CONS_HEADER,UPTR(361,1648),UPTR(361,1672),CONS_HEADER,UPTR(361,1627),UPTR(361,1675),
	CONS_HEADER,UPTR(361,1618),UPTR(361,1678),UOHEAD(2,0),UPTR(361,1681),UITAG(0),CONS_HEADER,UPTR(361,1684),TC_NIL,CONS_HEADER,
	UPTR(361,1615),UPTR(361,1687),CONS_HEADER,UPTR(361,1594),UPTR(361,1690),CONS_HEADER,UPTR(361,1585),UPTR(361,1693),UOHEAD(2,0),UPTR(361,1696),
	UITAG(0),CONS_HEADER,UPTR(361,1699),TC_NIL,CONS_HEADER,UPTR(361,1582),UPTR(361,1702),CONS_HEADER,UPTR(361,1561),UPTR(361,1705),
	CONS_HEADER,UPTR(361,1552),UPTR(361,1708),UOHEAD(2,0),UPTR(361,1711),UITAG(0),CONS_HEADER,UPTR(361,1714),TC_NIL,CONS_HEADER,
	UPTR(361,1549),UPTR(361,1717),UOHEAD(2,0),UPTR(361,1720),UITAG(0),CONS_HEADER,UPTR(361,1723),TC_NIL,UOHEAD(2,0),UPTR(361,1726),
	UITAG(0),CONS_HEADER,UPTR(361,1729),TC_NIL,CONS_HEADER,UPTR(361,1546),UPTR(361,1732),CONS_HEADER,UPTR(361,1543),UPTR(361,1735),
	UOHEAD(2,0),UPTR(361,1738),UITAG(0),UOHEAD(2,1),UPTR(240,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,1750),TC_NIL,UOHEAD(2,0),UPTR(361,1753),UITAG(0),CONS_HEADER,UPTR(361,1756),
	TC_NIL,UOHEAD(2,0),UPTR(361,1759),UITAG(0),CONS_HEADER,UPTR(361,1762),TC_NIL,CONS_HEADER,UPTR(361,1747),UPTR(361,1765),
	CONS_HEADER,UPTR(361,1744),UPTR(361,1768),UOHEAD(2,0),UPTR(361,1771),UITAG(0),UOHEAD(2,1),UPTR(246,1),UITAG(0),UOHEAD(2,5),
	5,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,1783),TC_NIL,UOHEAD(2,0),UPTR(361,1786),
	UITAG(0),CONS_HEADER,UPTR(361,1789),TC_NIL,UOHEAD(2,0),UPTR(361,1792),UITAG(0),CONS_HEADER,UPTR(361,1795),TC_NIL,
	CONS_HEADER,UPTR(361,1780),UPTR(361,1798),CONS_HEADER,UPTR(361,1777),UPTR(361,1801),UOHEAD(2,0),UPTR(361,1804),UITAG(0),CONS_HEADER,
	UPTR(361,1807),TC_NIL,CONS_HEADER,UPTR(361,1774),UPTR(361,1810),CONS_HEADER,UPTR(361,1741),UPTR(361,1813),UOHEAD(2,0),UPTR(361,1816),
	UITAG(0),CONS_HEADER,UPTR(361,1819),TC_NIL,CONS_HEADER,UPTR(361,1540),UPTR(361,1822),UOHEAD(2,0),UPTR(361,1825),UITAG(0),
	UOHEAD(2,1),UPTR(248,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,1846),
	TC_NIL,CONS_HEADER,UPTR(361,1843),UPTR(361,1849),UOHEAD(2,0),UPTR(361,1852),UITAG(0),CONS_HEADER,UPTR(361,1855),TC_NIL,
	CONS_HEADER,UPTR(361,1840),UPTR(361,1858),UOHEAD(2,0),UPTR(361,1861),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,1873),TC_NIL,CONS_HEADER,UPTR(361,1870),
	UPTR(361,1876),UOHEAD(2,0),UPTR(361,1879),UITAG(0),CONS_HEADER,UPTR(361,1882),TC_NIL,CONS_HEADER,UPTR(361,1867),UPTR(361,1885),
	UOHEAD(2,0),UPTR(361,1888),UITAG(0),CONS_HEADER,UPTR(361,1891),TC_NIL,CONS_HEADER,UPTR(361,1864),UPTR(361,1894),UOHEAD(2,0),
	UPTR(361,1897),UITAG(0),CONS_HEADER,UPTR(361,1900),TC_NIL,CONS_HEADER,UPTR(361,1837),UPTR(361,1903),CONS_HEADER,UPTR(361,1834),
	UPTR(361,1906),UOHEAD(2,0),UPTR(361,1909),UITAG(0),CONS_HEADER,UPTR(361,1912),TC_NIL,UOHEAD(2,0),UPTR(361,1915),UITAG(0),
	CONS_HEADER,UPTR(361,1918),TC_NIL,CONS_HEADER,UPTR(361,1831),UPTR(361,1921),UOHEAD(2,0),UPTR(361,1924),UITAG(0),UOHEAD(2,1),
	UPTR(250,1),UITAG(0),UOHEAD(2,1),UPTR(252,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(250,1),
	UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,1945),TC_NIL,
	CONS_HEADER,UPTR(361,1942),UPTR(361,1948),UOHEAD(2,0),UPTR(361,1951),UITAG(0),CONS_HEADER,UPTR(361,1954),TC_NIL,CONS_HEADER,
	UPTR(361,1939),UPTR(361,1957),UOHEAD(2,0),UPTR(361,1960),UITAG(0),UOHEAD(2,1),UPTR(250,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),
	UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,1972),TC_NIL,CONS_HEADER,UPTR(361,1969),UPTR(361,1975),
	UOHEAD(2,0),UPTR(361,1978),UITAG(0),CONS_HEADER,UPTR(361,1981),TC_NIL,CONS_HEADER,UPTR(361,1966),UPTR(361,1984),UOHEAD(2,0),
	UPTR(361,1987),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,1996),
	TC_NIL,CONS_HEADER,UPTR(361,1993),UPTR(361,1999),UOHEAD(2,0),UPTR(361,2002),UITAG(0),CONS_HEADER,UPTR(361,2005),TC_NIL,
	CONS_HEADER,UPTR(361,1990),UPTR(361,2008),CONS_HEADER,UPTR(361,1963),UPTR(361,2011),UOHEAD(2,0),UPTR(361,2014),UITAG(0),CONS_HEADER,
	UPTR(361,2017),TC_NIL,CONS_HEADER,UPTR(361,1936),UPTR(361,2020),CONS_HEADER,UPTR(361,1933),UPTR(361,2023),UOHEAD(2,0),UPTR(361,2026),
	UITAG(0),UOHEAD(2,1),UPTR(254,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(250,1),UITAG(0),
	UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,2044),TC_NIL,CONS_HEADER,
	UPTR(361,2041),UPTR(361,2047),UOHEAD(2,0),UPTR(361,2050),UITAG(0),CONS_HEADER,UPTR(361,2053),TC_NIL,CONS_HEADER,UPTR(361,2038),
	UPTR(361,2056),UOHEAD(2,0),UPTR(361,2059),UITAG(0),UOHEAD(2,1),UPTR(250,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,2071),TC_NIL,CONS_HEADER,UPTR(361,2068),UPTR(361,2074),UOHEAD(2,0),
	UPTR(361,2077),UITAG(0),CONS_HEADER,UPTR(361,2080),TC_NIL,CONS_HEADER,UPTR(361,2065),UPTR(361,2083),UOHEAD(2,0),UPTR(361,2086),
	UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,2095),TC_NIL,
	CONS_HEADER,UPTR(361,2092),UPTR(361,2098),UOHEAD(2,0),UPTR(361,2101),UITAG(0),CONS_HEADER,UPTR(361,2104),TC_NIL,CONS_HEADER,
	UPTR(361,2089),UPTR(361,2107),CONS_HEADER,UPTR(361,2062),UPTR(361,2110),UOHEAD(2,0),UPTR(361,2113),UITAG(0),CONS_HEADER,UPTR(361,2116),
	TC_NIL,CONS_HEADER,UPTR(361,2035),UPTR(361,2119),CONS_HEADER,UPTR(361,2032),UPTR(361,2122),UOHEAD(2,0),UPTR(361,2125),UITAG(0),
	UOHEAD(2,1),UPTR(256,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(361,2137),TC_NIL,CONS_HEADER,UPTR(361,2134),UPTR(361,2140),CONS_HEADER,UPTR(361,2131),UPTR(361,2143),UOHEAD(2,0),UPTR(361,2146),
	UITAG(0),CONS_HEADER,UPTR(361,2149),TC_NIL,CONS_HEADER,UPTR(361,2128),UPTR(361,2152),CONS_HEADER,UPTR(361,2029),UPTR(361,2155),
	UOHEAD(2,0),UPTR(361,2158),UITAG(0),CONS_HEADER,UPTR(361,2161),TC_NIL,CONS_HEADER,UPTR(361,1930),UPTR(361,2164),UOHEAD(2,0),
	UPTR(361,2167),UITAG(0),UOHEAD(2,1),UPTR(258,1),UITAG(0),UOHEAD(2,1),UPTR(148,1),UITAG(0),UOHEAD(2,5),1,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,2182),TC_NIL,CONS_HEADER,UPTR(361,2179),UPTR(361,2185),
	CONS_HEADER,UPTR(361,2176),UPTR(361,2188),UOHEAD(2,0),UPTR(361,2191),UITAG(0),UOHEAD(2,1),UPTR(260,1),UITAG(0),UOHEAD(2,5),
	3,UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,2206),
	TC_NIL,CONS_HEADER,UPTR(361,2203),UPTR(361,2209),UOHEAD(2,0),UPTR(361,2212),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,2221),TC_NIL,CONS_HEADER,UPTR(361,2218),UPTR(361,2224),UOHEAD(2,0),
	UPTR(361,2227),UITAG(0),UOHEAD(2,1),UPTR(258,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),
	UITAG(0),CONS_HEADER,UPTR(361,2239),TC_NIL,CONS_HEADER,UPTR(361,2236),UPTR(361,2242),UOHEAD(2,0),UPTR(361,2245),UITAG(0),
	UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,2254),TC_NIL,CONS_HEADER,
	UPTR(361,2251),UPTR(361,2257),UOHEAD(2,0),UPTR(361,2260),UITAG(0),CONS_HEADER,UPTR(361,2263),TC_NIL,CONS_HEADER,UPTR(361,2248),
	UPTR(361,2266),CONS_HEADER,UPTR(361,2233),UPTR(361,2269),UOHEAD(2,0),UPTR(361,2272),UITAG(0),CONS_HEADER,UPTR(361,2275),TC_NIL,
	CONS_HEADER,UPTR(361,2230),UPTR(361,2278),CONS_HEADER,UPTR(361,2215),UPTR(361,2281),UOHEAD(2,0),UPTR(361,2284),UITAG(0),CONS_HEADER,
	UPTR(361,2287),TC_NIL,CONS_HEADER,UPTR(361,2200),UPTR(361,2290),CONS_HEADER,UPTR(361,2197),UPTR(361,2293),UOHEAD(2,0),UPTR(361,2296),
	UITAG(0),CONS_HEADER,UPTR(361,2299),TC_NIL,CONS_HEADER,UPTR(361,2194),UPTR(361,2302),UOHEAD(2,0),UPTR(361,2305),UITAG(0),
	CONS_HEADER,UPTR(361,2308),TC_NIL,CONS_HEADER,UPTR(361,2173),UPTR(361,2311),UOHEAD(2,0),UPTR(361,2314),UITAG(0),UOHEAD(2,1),
	UPTR(262,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(222,1),
	UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(258,1),UITAG(0),CONS_HEADER,UPTR(361,2335),TC_NIL,
	UOHEAD(2,0),UPTR(361,2338),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(258,1),UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,2353),TC_NIL,CONS_HEADER,UPTR(361,2350),
	UPTR(361,2356),UOHEAD(2,0),UPTR(361,2359),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),
	CONS_HEADER,UPTR(361,2368),TC_NIL,CONS_HEADER,UPTR(361,2365),UPTR(361,2371),UOHEAD(2,0),UPTR(361,2374),UITAG(0),CONS_HEADER,
	UPTR(361,2377),TC_NIL,CONS_HEADER,UPTR(361,2362),UPTR(361,2380),CONS_HEADER,UPTR(361,2347),UPTR(361,2383),UOHEAD(2,0),UPTR(361,2386),
	UITAG(0),CONS_HEADER,UPTR(361,2389),TC_NIL,CONS_HEADER,UPTR(361,2344),UPTR(361,2392),UOHEAD(2,0),UPTR(361,2395),UITAG(0),
	UOHEAD(2,1),UPTR(234,1),UITAG(0),UOHEAD(2,1),UPTR(236,1),UITAG(0),CONS_HEADER,UPTR(361,2404),TC_NIL,UOHEAD(2,0),
	UPTR(361,2407),UITAG(0),CONS_HEADER,UPTR(361,2410),TC_NIL,CONS_HEADER,UPTR(361,2401),UPTR(361,2413),UOHEAD(2,0),UPTR(361,2416),
	UITAG(0),CONS_HEADER,UPTR(361,2419),TC_NIL,CONS_HEADER,UPTR(361,2398),UPTR(361,2422),CONS_HEADER,UPTR(361,2341),UPTR(361,2425),
	CONS_HEADER,UPTR(361,2332),UPTR(361,2428),UOHEAD(2,0),UPTR(361,2431),UITAG(0),CONS_HEADER,UPTR(361,2434),TC_NIL,CONS_HEADER,
	UPTR(361,2329),UPTR(361,2437),UOHEAD(2,0),UPTR(361,2440),UITAG(0),CONS_HEADER,UPTR(361,2443),TC_NIL,UOHEAD(2,0),UPTR(361,2446),
	UITAG(0),CONS_HEADER,UPTR(361,2449),TC_NIL,CONS_HEADER,UPTR(361,2326),UPTR(361,2452),CONS_HEADER,UPTR(361,2323),UPTR(361,2455),
	UOHEAD(2,0),UPTR(361,2458),UITAG(0),CONS_HEADER,UPTR(361,2461),TC_NIL,UOHEAD(2,0),UPTR(361,2464),UITAG(0),CONS_HEADER,
	UPTR(361,2467),TC_NIL,CONS_HEADER,UPTR(361,2320),UPTR(361,2470),UOHEAD(2,0),UPTR(361,2473),UITAG(0),UOHEAD(2,1),UPTR(264,1),
	UITAG(0),UOHEAD(2,1),UPTR(144,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(361,2488),TC_NIL,CONS_HEADER,UPTR(361,2485),UPTR(361,2491),CONS_HEADER,UPTR(361,2482),UPTR(361,2494),UOHEAD(2,0),
	UPTR(361,2497),UITAG(0),UOHEAD(2,1),UPTR(266,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(361,2509),TC_NIL,CONS_HEADER,UPTR(361,2506),UPTR(361,2512),CONS_HEADER,UPTR(361,2503),UPTR(361,2515),
	UOHEAD(2,0),UPTR(361,2518),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,2530),TC_NIL,CONS_HEADER,UPTR(361,2527),UPTR(361,2533),CONS_HEADER,UPTR(361,2524),
	UPTR(361,2536),UOHEAD(2,0),UPTR(361,2539),UITAG(0),UOHEAD(2,1),UPTR(268,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,2551),TC_NIL,CONS_HEADER,UPTR(361,2548),UPTR(361,2554),CONS_HEADER,
	UPTR(361,2545),UPTR(361,2557),UOHEAD(2,0),UPTR(361,2560),UITAG(0),UOHEAD(2,1),UPTR(270,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,2572),TC_NIL,CONS_HEADER,UPTR(361,2569),UPTR(361,2575),
	CONS_HEADER,UPTR(361,2566),UPTR(361,2578),UOHEAD(2,0),UPTR(361,2581),UITAG(0),UOHEAD(2,1),UPTR(272,1),UITAG(0),UOHEAD(2,5),
	11,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,2593),TC_NIL,UOHEAD(2,0),UPTR(361,2596),
	UITAG(0),CONS_HEADER,UPTR(361,2599),TC_NIL,UOHEAD(2,0),UPTR(361,2602),UITAG(0),CONS_HEADER,UPTR(361,2605),TC_NIL,
	CONS_HEADER,UPTR(361,2590),UPTR(361,2608),CONS_HEADER,UPTR(361,2587),UPTR(361,2611),UOHEAD(2,0),UPTR(361,2614),UITAG(0),CONS_HEADER,
	UPTR(361,2617),TC_NIL,CONS_HEADER,UPTR(361,2584),UPTR(361,2620),CONS_HEADER,UPTR(361,2563),UPTR(361,2623),CONS_HEADER,UPTR(361,2542),
	UPTR(361,2626),CONS_HEADER,UPTR(361,2521),UPTR(361,2629),CONS_HEADER,UPTR(361,2500),UPTR(361,2632),UOHEAD(2,0),UPTR(361,2635),UITAG(0),
	CONS_HEADER,UPTR(361,2638),TC_NIL,CONS_HEADER,UPTR(361,2479),UPTR(361,2641),UOHEAD(2,0),UPTR(361,2644),UITAG(0),UOHEAD(2,1),
	UPTR(274,1),UITAG(0),UOHEAD(2,1),UPTR(276,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),CONS_HEADER,UPTR(361,2659),TC_NIL,UOHEAD(2,0),UPTR(361,2662),UITAG(0),CONS_HEADER,UPTR(361,2665),TC_NIL,
	UOHEAD(2,0),UPTR(361,2668),UITAG(0),CONS_HEADER,UPTR(361,2671),TC_NIL,CONS_HEADER,UPTR(361,2656),UPTR(361,2674),CONS_HEADER,
	UPTR(361,2653),UPTR(361,2677),UOHEAD(2,0),UPTR(361,2680),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),UOHEAD(2,5),3,
	UITAG(0),UOHEAD(2,1),UPTR(264,1),UITAG(0),CONS_HEADER,UPTR(361,2692),TC_NIL,UOHEAD(2,0),UPTR(361,2695),UITAG(0),
	UOHEAD(2,1),UPTR(146,1),UITAG(0),CONS_HEADER,UPTR(361,2701),TC_NIL,UOHEAD(2,0),UPTR(361,2704),UITAG(0),CONS_HEADER,
	UPTR(361,2707),TC_NIL,CONS_HEADER,UPTR(361,2698),UPTR(361,2710),UOHEAD(2,0),UPTR(361,2713),UITAG(0),CONS_HEADER,UPTR(361,2716),
	TC_NIL,CONS_HEADER,UPTR(361,2689),UPTR(361,2719),CONS_HEADER,UPTR(361,2686),UPTR(361,2722),UOHEAD(2,0),UPTR(361,2725),UITAG(0),
	UOHEAD(2,1),UPTR(278,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,
	UPTR(361,2737),TC_NIL,UOHEAD(2,0),UPTR(361,2740),UITAG(0),UOHEAD(2,1),UPTR(274,1),UITAG(0),CONS_HEADER,UPTR(361,2746),
	TC_NIL,UOHEAD(2,0),UPTR(361,2749),UITAG(0),CONS_HEADER,UPTR(361,2752),TC_NIL,CONS_HEADER,UPTR(361,2743),UPTR(361,2755),
	UOHEAD(2,0),UPTR(361,2758),UITAG(0),CONS_HEADER,UPTR(361,2761),TC_NIL,CONS_HEADER,UPTR(361,2734),UPTR(361,2764),CONS_HEADER,
	UPTR(361,2731),UPTR(361,2767),UOHEAD(2,0),UPTR(361,2770),UITAG(0),UOHEAD(2,1),UPTR(280,1),UITAG(0),UOHEAD(2,5),7,
	UITAG(0),UOHEAD(2,1),UPTR(274,1),UITAG(0),CONS_HEADER,UPTR(361,2782),TC_NIL,UOHEAD(2,0),UPTR(361,2785),UITAG(0),
	CONS_HEADER,UPTR(361,2788),TC_NIL,UOHEAD(2,0),UPTR(361,2791),UITAG(0),CONS_HEADER,UPTR(361,2794),TC_NIL,CONS_HEADER,
	UPTR(361,2779),UPTR(361,2797),CONS_HEADER,UPTR(361,2776),UPTR(361,2800),UOHEAD(2,0),UPTR(361,2803),UITAG(0),UOHEAD(2,1),UPTR(282,1),
	UITAG(0),UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,2815),TC_NIL,
	UOHEAD(2,0),UPTR(361,2818),UITAG(0),CONS_HEADER,UPTR(361,2821),TC_NIL,UOHEAD(2,0),UPTR(361,2824),UITAG(0),CONS_HEADER,
	UPTR(361,2827),TC_NIL,CONS_HEADER,UPTR(361,2812),UPTR(361,2830),CONS_HEADER,UPTR(361,2809),UPTR(361,2833),UOHEAD(2,0),UPTR(361,2836),
	UITAG(0),UOHEAD(2,1),UPTR(284,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),
	CONS_HEADER,UPTR(361,2848),TC_NIL,UOHEAD(2,0),UPTR(361,2851),UITAG(0),CONS_HEADER,UPTR(361,2854),TC_NIL,UOHEAD(2,0),
	UPTR(361,2857),UITAG(0),CONS_HEADER,UPTR(361,2860),TC_NIL,CONS_HEADER,UPTR(361,2845),UPTR(361,2863),CONS_HEADER,UPTR(361,2842),
	UPTR(361,2866),UOHEAD(2,0),UPTR(361,2869),UITAG(0),CONS_HEADER,UPTR(361,2872),TC_NIL,CONS_HEADER,UPTR(361,2839),UPTR(361,2875),
	CONS_HEADER,UPTR(361,2806),UPTR(361,2878),CONS_HEADER,UPTR(361,2773),UPTR(361,2881),CONS_HEADER,UPTR(361,2728),UPTR(361,2884),CONS_HEADER,
	UPTR(361,2683),UPTR(361,2887),UOHEAD(2,0),UPTR(361,2890),UITAG(0),CONS_HEADER,UPTR(361,2893),TC_NIL,CONS_HEADER,UPTR(361,2650),
	UPTR(361,2896),UOHEAD(2,0),UPTR(361,2899),UITAG(0),UOHEAD(2,1),UPTR(286,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,2914),TC_NIL,UOHEAD(2,0),
	UPTR(361,2917),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,2923),TC_NIL,UOHEAD(2,0),UPTR(361,2926),
	UITAG(0),UOHEAD(2,1),UPTR(274,1),UITAG(0),CONS_HEADER,UPTR(361,2932),TC_NIL,UOHEAD(2,0),UPTR(361,2935),UITAG(0),
	CONS_HEADER,UPTR(361,2938),TC_NIL,CONS_HEADER,UPTR(361,2929),UPTR(361,2941),CONS_HEADER,UPTR(361,2920),UPTR(361,2944),UOHEAD(2,0),
	UPTR(361,2947),UITAG(0),CONS_HEADER,UPTR(361,2950),TC_NIL,CONS_HEADER,UPTR(361,2911),UPTR(361,2953),CONS_HEADER,UPTR(361,2908),
	UPTR(361,2956),UOHEAD(2,0),UPTR(361,2959),UITAG(0),CONS_HEADER,UPTR(361,2962),TC_NIL,UOHEAD(2,0),UPTR(361,2965),UITAG(0),
	CONS_HEADER,UPTR(361,2968),TC_NIL,CONS_HEADER,UPTR(361,2905),UPTR(361,2971),UOHEAD(2,0),UPTR(361,2974),UITAG(0),UOHEAD(2,1),
	UPTR(288,1),UITAG(0),UOHEAD(2,1),UPTR(282,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(115,1),
	UITAG(0),CONS_HEADER,UPTR(361,2989),TC_NIL,UOHEAD(2,0),UPTR(361,2992),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),
	CONS_HEADER,UPTR(361,2998),TC_NIL,UOHEAD(2,0),UPTR(361,3001),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),
	UPTR(286,1),UITAG(0),CONS_HEADER,UPTR(361,3010),TC_NIL,UOHEAD(2,0),UPTR(361,3013),UITAG(0),CONS_HEADER,UPTR(361,3016),
	TC_NIL,CONS_HEADER,UPTR(361,3007),UPTR(361,3019),UOHEAD(2,0),UPTR(361,3022),UITAG(0),CONS_HEADER,UPTR(361,3025),TC_NIL,
	CONS_HEADER,UPTR(361,3004),UPTR(361,3028),CONS_HEADER,UPTR(361,2995),UPTR(361,3031),UOHEAD(2,0),UPTR(361,3034),UITAG(0),CONS_HEADER,
	UPTR(361,3037),TC_NIL,CONS_HEADER,UPTR(361,2986),UPTR(361,3040),CONS_HEADER,UPTR(361,2983),UPTR(361,3043),UOHEAD(2,0),UPTR(361,3046),
	UITAG(0),UOHEAD(2,1),UPTR(284,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),
	CONS_HEADER,UPTR(361,3058),TC_NIL,UOHEAD(2,0),UPTR(361,3061),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,
	UPTR(361,3067),TC_NIL,UOHEAD(2,0),UPTR(361,3070),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(286,1),
	UITAG(0),CONS_HEADER,UPTR(361,3079),TC_NIL,UOHEAD(2,0),UPTR(361,3082),UITAG(0),CONS_HEADER,UPTR(361,3085),TC_NIL,
	CONS_HEADER,UPTR(361,3076),UPTR(361,3088),UOHEAD(2,0),UPTR(361,3091),UITAG(0),CONS_HEADER,UPTR(361,3094),TC_NIL,CONS_HEADER,
	UPTR(361,3073),UPTR(361,3097),CONS_HEADER,UPTR(361,3064),UPTR(361,3100),UOHEAD(2,0),UPTR(361,3103),UITAG(0),CONS_HEADER,UPTR(361,3106),
	TC_NIL,CONS_HEADER,UPTR(361,3055),UPTR(361,3109),CONS_HEADER,UPTR(361,3052),UPTR(361,3112),UOHEAD(2,0),UPTR(361,3115),UITAG(0),
	CONS_HEADER,UPTR(361,3118),TC_NIL,CONS_HEADER,UPTR(361,3049),UPTR(361,3121),UOHEAD(2,0),UPTR(361,3124),UITAG(0),CONS_HEADER,
	UPTR(361,3127),TC_NIL,CONS_HEADER,UPTR(361,2980),UPTR(361,3130),UOHEAD(2,0),UPTR(361,3133),UITAG(0),UOHEAD(2,1),UPTR(290,1),
	UITAG(0),UOHEAD(2,1),UPTR(292,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),
	CONS_HEADER,UPTR(361,3148),TC_NIL,UOHEAD(2,0),UPTR(361,3151),UITAG(0),UOHEAD(2,1),UPTR(274,1),UITAG(0),CONS_HEADER,
	UPTR(361,3157),TC_NIL,UOHEAD(2,0),UPTR(361,3160),UITAG(0),UOHEAD(2,1),UPTR(204,1),UITAG(0),UOHEAD(2,1),UPTR(274,1),
	UITAG(0),CONS_HEADER,UPTR(361,3169),TC_NIL,UOHEAD(2,0),UPTR(361,3172),UITAG(0),CONS_HEADER,UPTR(361,3175),TC_NIL,
	CONS_HEADER,UPTR(361,3166),UPTR(361,3178),UOHEAD(2,0),UPTR(361,3181),UITAG(0),UOHEAD(2,1),UPTR(146,1),UITAG(0),CONS_HEADER,
	UPTR(361,3187),TC_NIL,UOHEAD(2,0),UPTR(361,3190),UITAG(0),CONS_HEADER,UPTR(361,3193),TC_NIL,CONS_HEADER,UPTR(361,3184),
	UPTR(361,3196),CONS_HEADER,UPTR(361,3163),UPTR(361,3199),CONS_HEADER,UPTR(361,3154),UPTR(361,3202),UOHEAD(2,0),UPTR(361,3205),UITAG(0),
	CONS_HEADER,UPTR(361,3208),TC_NIL,CONS_HEADER,UPTR(361,3145),UPTR(361,3211),CONS_HEADER,UPTR(361,3142),UPTR(361,3214),UOHEAD(2,0),
	UPTR(361,3217),UITAG(0),UOHEAD(2,1),UPTR(294,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),CONS_HEADER,UPTR(361,3229),TC_NIL,UOHEAD(2,0),UPTR(361,3232),UITAG(0),UOHEAD(2,1),UPTR(274,1),UITAG(0),
	CONS_HEADER,UPTR(361,3238),TC_NIL,UOHEAD(2,0),UPTR(361,3241),UITAG(0),CONS_HEADER,UPTR(361,3244),TC_NIL,CONS_HEADER,
	UPTR(361,3235),UPTR(361,3247),UOHEAD(2,0),UPTR(361,3250),UITAG(0),CONS_HEADER,UPTR(361,3253),TC_NIL,CONS_HEADER,UPTR(361,3226),
	UPTR(361,3256),CONS_HEADER,UPTR(361,3223),UPTR(361,3259),UOHEAD(2,0),UPTR(361,3262),UITAG(0),CONS_HEADER,UPTR(361,3265),TC_NIL,
	CONS_HEADER,UPTR(361,3220),UPTR(361,3268),UOHEAD(2,0),UPTR(361,3271),UITAG(0),CONS_HEADER,UPTR(361,3274),TC_NIL,CONS_HEADER,
	UPTR(361,3139),UPTR(361,3277),UOHEAD(2,0),UPTR(361,3280),UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),UOHEAD(2,1),UPTR(252,1),
	UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3301),TC_NIL,CONS_HEADER,UPTR(361,3298),UPTR(361,3304),UOHEAD(2,0),
	UPTR(361,3307),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,3316),
	TC_NIL,CONS_HEADER,UPTR(361,3313),UPTR(361,3319),UOHEAD(2,0),UPTR(361,3322),UITAG(0),CONS_HEADER,UPTR(361,3325),TC_NIL,
	CONS_HEADER,UPTR(361,3310),UPTR(361,3328),CONS_HEADER,UPTR(361,3295),UPTR(361,3331),UOHEAD(2,0),UPTR(361,3334),UITAG(0),UOHEAD(2,1),
	UPTR(296,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3346),
	TC_NIL,CONS_HEADER,UPTR(361,3343),UPTR(361,3349),UOHEAD(2,0),UPTR(361,3352),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,3361),TC_NIL,CONS_HEADER,UPTR(361,3358),UPTR(361,3364),UOHEAD(2,0),
	UPTR(361,3367),UITAG(0),CONS_HEADER,UPTR(361,3370),TC_NIL,CONS_HEADER,UPTR(361,3355),UPTR(361,3373),CONS_HEADER,UPTR(361,3340),
	UPTR(361,3376),UOHEAD(2,0),UPTR(361,3379),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),
	CONS_HEADER,UPTR(361,3388),TC_NIL,CONS_HEADER,UPTR(361,3385),UPTR(361,3391),UOHEAD(2,0),UPTR(361,3394),UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,3403),TC_NIL,CONS_HEADER,UPTR(361,3400),
	UPTR(361,3406),UOHEAD(2,0),UPTR(361,3409),UITAG(0),CONS_HEADER,UPTR(361,3412),TC_NIL,CONS_HEADER,UPTR(361,3397),UPTR(361,3415),
	CONS_HEADER,UPTR(361,3382),UPTR(361,3418),CONS_HEADER,UPTR(361,3337),UPTR(361,3421),UOHEAD(2,0),UPTR(361,3424),UITAG(0),CONS_HEADER,
	UPTR(361,3427),TC_NIL,CONS_HEADER,UPTR(361,3292),UPTR(361,3430),CONS_HEADER,UPTR(361,3289),UPTR(361,3433),UOHEAD(2,0),UPTR(361,3436),
	UITAG(0),UOHEAD(2,1),UPTR(254,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),
	UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3454),TC_NIL,CONS_HEADER,
	UPTR(361,3451),UPTR(361,3457),UOHEAD(2,0),UPTR(361,3460),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),
	UITAG(0),CONS_HEADER,UPTR(361,3469),TC_NIL,CONS_HEADER,UPTR(361,3466),UPTR(361,3472),UOHEAD(2,0),UPTR(361,3475),UITAG(0),
	CONS_HEADER,UPTR(361,3478),TC_NIL,CONS_HEADER,UPTR(361,3463),UPTR(361,3481),CONS_HEADER,UPTR(361,3448),UPTR(361,3484),UOHEAD(2,0),
	UPTR(361,3487),UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),
	UITAG(0),CONS_HEADER,UPTR(361,3499),TC_NIL,CONS_HEADER,UPTR(361,3496),UPTR(361,3502),UOHEAD(2,0),UPTR(361,3505),UITAG(0),
	UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,3514),TC_NIL,CONS_HEADER,
	UPTR(361,3511),UPTR(361,3517),UOHEAD(2,0),UPTR(361,3520),UITAG(0),CONS_HEADER,UPTR(361,3523),TC_NIL,CONS_HEADER,UPTR(361,3508),
	UPTR(361,3526),CONS_HEADER,UPTR(361,3493),UPTR(361,3529),UOHEAD(2,0),UPTR(361,3532),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3541),TC_NIL,CONS_HEADER,UPTR(361,3538),UPTR(361,3544),UOHEAD(2,0),
	UPTR(361,3547),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,3556),
	TC_NIL,CONS_HEADER,UPTR(361,3553),UPTR(361,3559),UOHEAD(2,0),UPTR(361,3562),UITAG(0),CONS_HEADER,UPTR(361,3565),TC_NIL,
	CONS_HEADER,UPTR(361,3550),UPTR(361,3568),CONS_HEADER,UPTR(361,3535),UPTR(361,3571),CONS_HEADER,UPTR(361,3490),UPTR(361,3574),UOHEAD(2,0),
	UPTR(361,3577),UITAG(0),CONS_HEADER,UPTR(361,3580),TC_NIL,CONS_HEADER,UPTR(361,3445),UPTR(361,3583),CONS_HEADER,UPTR(361,3442),
	UPTR(361,3586),UOHEAD(2,0),UPTR(361,3589),UITAG(0),UOHEAD(2,1),UPTR(256,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,3601),TC_NIL,CONS_HEADER,UPTR(361,3598),UPTR(361,3604),CONS_HEADER,
	UPTR(361,3595),UPTR(361,3607),UOHEAD(2,0),UPTR(361,3610),UITAG(0),CONS_HEADER,UPTR(361,3613),TC_NIL,CONS_HEADER,UPTR(361,3592),
	UPTR(361,3616),CONS_HEADER,UPTR(361,3439),UPTR(361,3619),UOHEAD(2,0),UPTR(361,3622),UITAG(0),CONS_HEADER,UPTR(361,3625),TC_NIL,
	CONS_HEADER,UPTR(361,3286),UPTR(361,3628),UOHEAD(2,0),UPTR(361,3631),UITAG(0),UOHEAD(2,1),UPTR(298,1),UITAG(0),UOHEAD(2,1),
	UPTR(115,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,3646),
	TC_NIL,UOHEAD(2,0),UPTR(361,3649),UITAG(0),CONS_HEADER,UPTR(361,3652),TC_NIL,UOHEAD(2,0),UPTR(361,3655),UITAG(0),
	CONS_HEADER,UPTR(361,3658),TC_NIL,CONS_HEADER,UPTR(361,3643),UPTR(361,3661),CONS_HEADER,UPTR(361,3640),UPTR(361,3664),UOHEAD(2,0),
	UPTR(361,3667),UITAG(0),UOHEAD(2,1),UPTR(300,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(115,1),
	UITAG(0),CONS_HEADER,UPTR(361,3679),TC_NIL,UOHEAD(2,0),UPTR(361,3682),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),
	CONS_HEADER,UPTR(361,3688),TC_NIL,UOHEAD(2,0),UPTR(361,3691),UITAG(0),CONS_HEADER,UPTR(361,3694),TC_NIL,CONS_HEADER,
	UPTR(361,3685),UPTR(361,3697),UOHEAD(2,0),UPTR(361,3700),UITAG(0),CONS_HEADER,UPTR(361,3703),TC_NIL,CONS_HEADER,UPTR(361,3676),
	UPTR(361,3706),CONS_HEADER,UPTR(361,3673),UPTR(361,3709),UOHEAD(2,0),UPTR(361,3712),UITAG(0),UOHEAD(2,1),UPTR(302,1),UITAG(0),
	UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,3724),TC_NIL,UOHEAD(2,0),
	UPTR(361,3727),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,3733),TC_NIL,UOHEAD(2,0),UPTR(361,3736),
	UITAG(0),CONS_HEADER,UPTR(361,3739),TC_NIL,CONS_HEADER,UPTR(361,3730),UPTR(361,3742),UOHEAD(2,0),UPTR(361,3745),UITAG(0),
	CONS_HEADER,UPTR(361,3748),TC_NIL,CONS_HEADER,UPTR(361,3721),UPTR(361,3751),CONS_HEADER,UPTR(361,3718),UPTR(361,3754),UOHEAD(2,0),
	UPTR(361,3757),UITAG(0),UOHEAD(2,1),UPTR(304,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),CONS_HEADER,UPTR(361,3769),TC_NIL,UOHEAD(2,0),UPTR(361,3772),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),
	CONS_HEADER,UPTR(361,3778),TC_NIL,UOHEAD(2,0),UPTR(361,3781),UITAG(0),CONS_HEADER,UPTR(361,3784),TC_NIL,CONS_HEADER,
	UPTR(361,3775),UPTR(361,3787),UOHEAD(2,0),UPTR(361,3790),UITAG(0),CONS_HEADER,UPTR(361,3793),TC_NIL,CONS_HEADER,UPTR(361,3766),
	UPTR(361,3796),CONS_HEADER,UPTR(361,3763),UPTR(361,3799),UOHEAD(2,0),UPTR(361,3802),UITAG(0),UOHEAD(2,1),UPTR(306,1),UITAG(0),
	UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,3814),TC_NIL,UOHEAD(2,0),
	UPTR(361,3817),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,3823),TC_NIL,UOHEAD(2,0),UPTR(361,3826),
	UITAG(0),CONS_HEADER,UPTR(361,3829),TC_NIL,CONS_HEADER,UPTR(361,3820),UPTR(361,3832),UOHEAD(2,0),UPTR(361,3835),UITAG(0),
	CONS_HEADER,UPTR(361,3838),TC_NIL,CONS_HEADER,UPTR(361,3811),UPTR(361,3841),CONS_HEADER,UPTR(361,3808),UPTR(361,3844),UOHEAD(2,0),
	UPTR(361,3847),UITAG(0),CONS_HEADER,UPTR(361,3850),TC_NIL,CONS_HEADER,UPTR(361,3805),UPTR(361,3853),CONS_HEADER,UPTR(361,3760),
	UPTR(361,3856),CONS_HEADER,UPTR(361,3715),UPTR(361,3859),CONS_HEADER,UPTR(361,3670),UPTR(361,3862),UOHEAD(2,0),UPTR(361,3865),UITAG(0),
	CONS_HEADER,UPTR(361,3868),TC_NIL,CONS_HEADER,UPTR(361,3637),UPTR(361,3871),UOHEAD(2,0),UPTR(361,3874),UITAG(0),UOHEAD(2,1),
	UPTR(308,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(222,1),
	UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),CONS_HEADER,UPTR(361,3895),TC_NIL,
	UOHEAD(2,0),UPTR(361,3898),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3913),TC_NIL,CONS_HEADER,UPTR(361,3910),
	UPTR(361,3916),UOHEAD(2,0),UPTR(361,3919),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),
	CONS_HEADER,UPTR(361,3928),TC_NIL,CONS_HEADER,UPTR(361,3925),UPTR(361,3931),UOHEAD(2,0),UPTR(361,3934),UITAG(0),CONS_HEADER,
	UPTR(361,3937),TC_NIL,CONS_HEADER,UPTR(361,3922),UPTR(361,3940),CONS_HEADER,UPTR(361,3907),UPTR(361,3943),UOHEAD(2,0),UPTR(361,3946),
	UITAG(0),CONS_HEADER,UPTR(361,3949),TC_NIL,CONS_HEADER,UPTR(361,3904),UPTR(361,3952),UOHEAD(2,0),UPTR(361,3955),UITAG(0),
	UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(174,1),UITAG(0),CONS_HEADER,UPTR(361,3964),TC_NIL,UOHEAD(2,0),
	UPTR(361,3967),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(310,1),UITAG(0),UOHEAD(2,1),UPTR(174,1),
	UITAG(0),CONS_HEADER,UPTR(361,3979),TC_NIL,UOHEAD(2,0),UPTR(361,3982),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,3991),TC_NIL,CONS_HEADER,UPTR(361,3988),UPTR(361,3994),UOHEAD(2,0),
	UPTR(361,3997),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,4006),
	TC_NIL,CONS_HEADER,UPTR(361,4003),UPTR(361,4009),UOHEAD(2,0),UPTR(361,4012),UITAG(0),CONS_HEADER,UPTR(361,4015),TC_NIL,
	CONS_HEADER,UPTR(361,4000),UPTR(361,4018),CONS_HEADER,UPTR(361,3985),UPTR(361,4021),CONS_HEADER,UPTR(361,3976),UPTR(361,4024),UOHEAD(2,0),
	UPTR(361,4027),UITAG(0),CONS_HEADER,UPTR(361,4030),TC_NIL,CONS_HEADER,UPTR(361,3973),UPTR(361,4033),UOHEAD(2,0),UPTR(361,4036),
	UITAG(0),UOHEAD(2,1),UPTR(234,1),UITAG(0),UOHEAD(2,1),UPTR(236,1),UITAG(0),CONS_HEADER,UPTR(361,4045),TC_NIL,
	UOHEAD(2,0),UPTR(361,4048),UITAG(0),CONS_HEADER,UPTR(361,4051),TC_NIL,CONS_HEADER,UPTR(361,4042),UPTR(361,4054),UOHEAD(2,0),
	UPTR(361,4057),UITAG(0),CONS_HEADER,UPTR(361,4060),TC_NIL,CONS_HEADER,UPTR(361,4039),UPTR(361,4063),CONS_HEADER,UPTR(361,3970),
	UPTR(361,4066),CONS_HEADER,UPTR(361,3961),UPTR(361,4069),UOHEAD(2,0),UPTR(361,4072),UITAG(0),CONS_HEADER,UPTR(361,4075),TC_NIL,
	CONS_HEADER,UPTR(361,3958),UPTR(361,4078),CONS_HEADER,UPTR(361,3901),UPTR(361,4081),CONS_HEADER,UPTR(361,3892),UPTR(361,4084),UOHEAD(2,0),
	UPTR(361,4087),UITAG(0),CONS_HEADER,UPTR(361,4090),TC_NIL,CONS_HEADER,UPTR(361,3889),UPTR(361,4093),UOHEAD(2,0),UPTR(361,4096),
	UITAG(0),CONS_HEADER,UPTR(361,4099),TC_NIL,UOHEAD(2,0),UPTR(361,4102),UITAG(0),CONS_HEADER,UPTR(361,4105),TC_NIL,
	CONS_HEADER,UPTR(361,3886),UPTR(361,4108),CONS_HEADER,UPTR(361,3883),UPTR(361,4111),UOHEAD(2,0),UPTR(361,4114),UITAG(0),CONS_HEADER,
	UPTR(361,4117),TC_NIL,UOHEAD(2,0),UPTR(361,4120),UITAG(0),CONS_HEADER,UPTR(361,4123),TC_NIL,CONS_HEADER,UPTR(361,3880),
	UPTR(361,4126),UOHEAD(2,0),UPTR(361,4129),UITAG(0),UOHEAD(2,1),UPTR(312,1),UITAG(0),UOHEAD(2,1),UPTR(172,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(222,1),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),
	UPTR(172,1),UITAG(0),CONS_HEADER,UPTR(361,4150),TC_NIL,UOHEAD(2,0),UPTR(361,4153),UITAG(0),UOHEAD(2,1),UPTR(228,1),
	UITAG(0),UOHEAD(2,1),UPTR(296,1),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),
	CONS_HEADER,UPTR(361,4168),TC_NIL,CONS_HEADER,UPTR(361,4165),UPTR(361,4171),UOHEAD(2,0),UPTR(361,4174),UITAG(0),UOHEAD(2,1),
	UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(202,1),UITAG(0),CONS_HEADER,UPTR(361,4183),TC_NIL,CONS_HEADER,UPTR(361,4180),
	UPTR(361,4186),UOHEAD(2,0),UPTR(361,4189),UITAG(0),CONS_HEADER,UPTR(361,4192),TC_NIL,CONS_HEADER,UPTR(361,4177),UPTR(361,4195),
	CONS_HEADER,UPTR(361,4162),UPTR(361,4198),UOHEAD(2,0),UPTR(361,4201),UITAG(0),CONS_HEADER,UPTR(361,4204),TC_NIL,CONS_HEADER,
	UPTR(361,4159),UPTR(361,4207),UOHEAD(2,0),UPTR(361,4210),UITAG(0),UOHEAD(2,1),UPTR(224,1),UITAG(0),UOHEAD(2,1),UPTR(174,1),
	UITAG(0),CONS_HEADER,UPTR(361,4219),TC_NIL,UOHEAD(2,0),UPTR(361,4222),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),
	UOHEAD(2,1),UPTR(310,1),UITAG(0),UOHEAD(2,1),UPTR(174,1),UITAG(0),CONS_HEADER,UPTR(361,4234),TC_NIL,UOHEAD(2,0),
	UPTR(361,4237),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,4246),
	TC_NIL,CONS_HEADER,UPTR(361,4243),UPTR(361,4249),UOHEAD(2,0),UPTR(361,4252),UITAG(0),UOHEAD(2,1),UPTR(186,1),UITAG(0),
	UOHEAD(2,1),UPTR(188,1),UITAG(0),CONS_HEADER,UPTR(361,4261),TC_NIL,CONS_HEADER,UPTR(361,4258),UPTR(361,4264),UOHEAD(2,0),
	UPTR(361,4267),UITAG(0),CONS_HEADER,UPTR(361,4270),TC_NIL,CONS_HEADER,UPTR(361,4255),UPTR(361,4273),CONS_HEADER,UPTR(361,4240),
	UPTR(361,4276),CONS_HEADER,UPTR(361,4231),UPTR(361,4279),UOHEAD(2,0),UPTR(361,4282),UITAG(0),CONS_HEADER,UPTR(361,4285),TC_NIL,
	CONS_HEADER,UPTR(361,4228),UPTR(361,4288),UOHEAD(2,0),UPTR(361,4291),UITAG(0),UOHEAD(2,1),UPTR(234,1),UITAG(0),UOHEAD(2,1),
	UPTR(236,1),UITAG(0),CONS_HEADER,UPTR(361,4300),TC_NIL,UOHEAD(2,0),UPTR(361,4303),UITAG(0),CONS_HEADER,UPTR(361,4306),
	TC_NIL,CONS_HEADER,UPTR(361,4297),UPTR(361,4309),UOHEAD(2,0),UPTR(361,4312),UITAG(0),CONS_HEADER,UPTR(361,4315),TC_NIL,
	CONS_HEADER,UPTR(361,4294),UPTR(361,4318),CONS_HEADER,UPTR(361,4225),UPTR(361,4321),CONS_HEADER,UPTR(361,4216),UPTR(361,4324),UOHEAD(2,0),
	UPTR(361,4327),UITAG(0),CONS_HEADER,UPTR(361,4330),TC_NIL,CONS_HEADER,UPTR(361,4213),UPTR(361,4333),CONS_HEADER,UPTR(361,4156),
	UPTR(361,4336),CONS_HEADER,UPTR(361,4147),UPTR(361,4339),UOHEAD(2,0),UPTR(361,4342),UITAG(0),CONS_HEADER,UPTR(361,4345),TC_NIL,
	CONS_HEADER,UPTR(361,4144),UPTR(361,4348),UOHEAD(2,0),UPTR(361,4351),UITAG(0),CONS_HEADER,UPTR(361,4354),TC_NIL,UOHEAD(2,0),
	UPTR(361,4357),UITAG(0),CONS_HEADER,UPTR(361,4360),TC_NIL,CONS_HEADER,UPTR(361,4141),UPTR(361,4363),CONS_HEADER,UPTR(361,4138),
	UPTR(361,4366),UOHEAD(2,0),UPTR(361,4369),UITAG(0),CONS_HEADER,UPTR(361,4372),TC_NIL,UOHEAD(2,0),UPTR(361,4375),UITAG(0),
	CONS_HEADER,UPTR(361,4378),TC_NIL,CONS_HEADER,UPTR(361,4135),UPTR(361,4381),UOHEAD(2,0),UPTR(361,4384),UITAG(0),UOHEAD(2,1),
	UPTR(314,1),UITAG(0),UOHEAD(2,1),UPTR(316,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(314,1),
	UITAG(0),CONS_HEADER,UPTR(361,4399),TC_NIL,UOHEAD(2,0),UPTR(361,4402),UITAG(0),UOHEAD(2,1),UPTR(314,1),UITAG(0),
	CONS_HEADER,UPTR(361,4408),TC_NIL,UOHEAD(2,0),UPTR(361,4411),UITAG(0),CONS_HEADER,UPTR(361,4414),TC_NIL,CONS_HEADER,
	UPTR(361,4405),UPTR(361,4417),UOHEAD(2,0),UPTR(361,4420),UITAG(0),CONS_HEADER,UPTR(361,4423),TC_NIL,CONS_HEADER,UPTR(361,4396),
	UPTR(361,4426),CONS_HEADER,UPTR(361,4393),UPTR(361,4429),UOHEAD(2,0),UPTR(361,4432),UITAG(0),UOHEAD(2,1),UPTR(318,1),UITAG(0),
	UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,4444),TC_NIL,UOHEAD(2,0),
	UPTR(361,4447),UITAG(0),CONS_HEADER,UPTR(361,4450),TC_NIL,UOHEAD(2,0),UPTR(361,4453),UITAG(0),CONS_HEADER,UPTR(361,4456),
	TC_NIL,CONS_HEADER,UPTR(361,4441),UPTR(361,4459),CONS_HEADER,UPTR(361,4438),UPTR(361,4462),UOHEAD(2,0),UPTR(361,4465),UITAG(0),
	CONS_HEADER,UPTR(361,4468),TC_NIL,CONS_HEADER,UPTR(361,4435),UPTR(361,4471),UOHEAD(2,0),UPTR(361,4474),UITAG(0),CONS_HEADER,
	UPTR(361,4477),TC_NIL,CONS_HEADER,UPTR(361,4390),UPTR(361,4480),UOHEAD(2,0),UPTR(361,4483),UITAG(0),UOHEAD(2,1),UPTR(320,1),
	UITAG(0),UOHEAD(2,1),UPTR(322,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(361,4498),TC_NIL,CONS_HEADER,UPTR(361,4495),UPTR(361,4501),CONS_HEADER,UPTR(361,4492),UPTR(361,4504),UOHEAD(2,0),
	UPTR(361,4507),UITAG(0),UOHEAD(2,1),UPTR(324,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(361,4519),TC_NIL,CONS_HEADER,UPTR(361,4516),UPTR(361,4522),CONS_HEADER,UPTR(361,4513),UPTR(361,4525),
	UOHEAD(2,0),UPTR(361,4528),UITAG(0),UOHEAD(2,1),UPTR(326,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,4540),TC_NIL,CONS_HEADER,UPTR(361,4537),UPTR(361,4543),CONS_HEADER,UPTR(361,4534),
	UPTR(361,4546),UOHEAD(2,0),UPTR(361,4549),UITAG(0),UOHEAD(2,1),UPTR(328,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,4561),TC_NIL,CONS_HEADER,UPTR(361,4558),UPTR(361,4564),CONS_HEADER,
	UPTR(361,4555),UPTR(361,4567),UOHEAD(2,0),UPTR(361,4570),UITAG(0),UOHEAD(2,1),UPTR(144,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,4582),TC_NIL,CONS_HEADER,UPTR(361,4579),UPTR(361,4585),
	CONS_HEADER,UPTR(361,4576),UPTR(361,4588),UOHEAD(2,0),UPTR(361,4591),UITAG(0),UOHEAD(2,1),UPTR(330,1),UITAG(0),UOHEAD(2,5),
	11,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,4603),TC_NIL,CONS_HEADER,UPTR(361,4600),
	UPTR(361,4606),CONS_HEADER,UPTR(361,4597),UPTR(361,4609),UOHEAD(2,0),UPTR(361,4612),UITAG(0),UOHEAD(2,1),UPTR(332,1),UITAG(0),
	UOHEAD(2,5),13,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(361,4624),TC_NIL,CONS_HEADER,
	UPTR(361,4621),UPTR(361,4627),CONS_HEADER,UPTR(361,4618),UPTR(361,4630),UOHEAD(2,0),UPTR(361,4633),UITAG(0),CONS_HEADER,UPTR(361,4636),
	TC_NIL,CONS_HEADER,UPTR(361,4615),UPTR(361,4639),CONS_HEADER,UPTR(361,4594),UPTR(361,4642),CONS_HEADER,UPTR(361,4573),UPTR(361,4645),
	CONS_HEADER,UPTR(361,4552),UPTR(361,4648),CONS_HEADER,UPTR(361,4531),UPTR(361,4651),CONS_HEADER,UPTR(361,4510),UPTR(361,4654),UOHEAD(2,0),
	UPTR(361,4657),UITAG(0),CONS_HEADER,UPTR(361,4660),TC_NIL,CONS_HEADER,UPTR(361,4489),UPTR(361,4663),UOHEAD(2,0),UPTR(361,4666),
	UITAG(0),CONS_HEADER,UPTR(361,4669),TC_NIL,CONS_HEADER,UPTR(361,4486),UPTR(361,4672),CONS_HEADER,UPTR(361,4387),UPTR(361,4675),
	CONS_HEADER,UPTR(361,4132),UPTR(361,4678),CONS_HEADER,UPTR(361,3877),UPTR(361,4681),CONS_HEADER,UPTR(361,3634),UPTR(361,4684),CONS_HEADER,
	UPTR(361,3283),UPTR(361,4687),CONS_HEADER,UPTR(361,3136),UPTR(361,4690),CONS_HEADER,UPTR(361,2977),UPTR(361,4693),CONS_HEADER,UPTR(361,2902),
	UPTR(361,4696),CONS_HEADER,UPTR(361,2647),UPTR(361,4699),CONS_HEADER,UPTR(361,2476),UPTR(361,4702),CONS_HEADER,UPTR(361,2317),UPTR(361,4705),
	CONS_HEADER,UPTR(361,2170),UPTR(361,4708),CONS_HEADER,UPTR(361,1927),UPTR(361,4711),CONS_HEADER,UPTR(361,1828),UPTR(361,4714),CONS_HEADER,
	UPTR(361,1537),UPTR(361,4717),CONS_HEADER,UPTR(361,1174),UPTR(361,4720),CONS_HEADER,UPTR(361,1099),UPTR(361,4723),CONS_HEADER,UPTR(361,508),
	UPTR(361,4726),CONS_HEADER,UPTR(361,397),UPTR(361,4729),CONS_HEADER,UPTR(361,298),UPTR(361,4732),CONS_HEADER,UPTR(361,235),UPTR(361,4735),
	CONS_HEADER,UPTR(361,154),UPTR(361,4738),CONS_HEADER,UPTR(361,67),UPTR(361,4741),CONS_HEADER,UPTR(361,4),UPTR(361,4744),UOHEAD(2,0),
	UPTR(361,4747),UITAG(0),UOHEAD(2,1),UPTR(334,1),UITAG(0),UOHEAD(2,1),UPTR(336,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),
	UITAG(0),UOHEAD(2,1),UPTR(338,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,4765),TC_NIL,
	UOHEAD(2,0),UPTR(361,4768),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,4774),TC_NIL,UOHEAD(2,0),
	UPTR(361,4777),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,4783),TC_NIL,UOHEAD(2,0),UPTR(361,4786),
	UITAG(0),CONS_HEADER,UPTR(361,4789),TC_NIL,CONS_HEADER,UPTR(361,4780),UPTR(361,4792),CONS_HEADER,UPTR(361,4771),UPTR(361,4795),
	CONS_HEADER,UPTR(361,4762),UPTR(361,4798),UOHEAD(2,0),UPTR(361,4801),UITAG(0),CONS_HEADER,UPTR(361,4804),TC_NIL,CONS_HEADER,
	UPTR(361,4759),UPTR(361,4807),UOHEAD(2,0),UPTR(361,4810),UITAG(0),CONS_HEADER,UPTR(361,4813),TC_NIL,CONS_HEADER,UPTR(361,4756),
	UPTR(361,4816),UOHEAD(2,0),UPTR(361,4819),UITAG(0),UOHEAD(2,1),UPTR(340,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),
	UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(361,4831),TC_NIL,UOHEAD(2,0),UPTR(361,4834),UITAG(0),CONS_HEADER,
	UPTR(361,4837),TC_NIL,CONS_HEADER,UPTR(361,4828),UPTR(361,4840),UOHEAD(2,0),UPTR(361,4843),UITAG(0),CONS_HEADER,UPTR(361,4846),
	TC_NIL,CONS_HEADER,UPTR(361,4825),UPTR(361,4849),UOHEAD(2,0),UPTR(361,4852),UITAG(0),UOHEAD(2,1),UPTR(342,1),UITAG(0),
	UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(208,1),UITAG(0),CONS_HEADER,UPTR(361,4864),TC_NIL,UOHEAD(2,0),
	UPTR(361,4867),UITAG(0),CONS_HEADER,UPTR(361,4870),TC_NIL,CONS_HEADER,UPTR(361,4861),UPTR(361,4873),UOHEAD(2,0),UPTR(361,4876),
	UITAG(0),CONS_HEADER,UPTR(361,4879),TC_NIL,CONS_HEADER,UPTR(361,4858),UPTR(361,4882),UOHEAD(2,0),UPTR(361,4885),UITAG(0),
	UOHEAD(2,1),UPTR(344,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,
	UPTR(361,4897),TC_NIL,UOHEAD(2,0),UPTR(361,4900),UITAG(0),CONS_HEADER,UPTR(361,4903),TC_NIL,CONS_HEADER,UPTR(361,4894),
	UPTR(361,4906),UOHEAD(2,0),UPTR(361,4909),UITAG(0),CONS_HEADER,UPTR(361,4912),TC_NIL,CONS_HEADER,UPTR(361,4891),UPTR(361,4915),
	UOHEAD(2,0),UPTR(361,4918),UITAG(0),UOHEAD(2,1),UPTR(346,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),
	UPTR(338,1),UITAG(0),CONS_HEADER,UPTR(361,4930),TC_NIL,UOHEAD(2,0),UPTR(361,4933),UITAG(0),CONS_HEADER,UPTR(361,4936),
	TC_NIL,CONS_HEADER,UPTR(361,4927),UPTR(361,4939),UOHEAD(2,0),UPTR(361,4942),UITAG(0),CONS_HEADER,UPTR(361,4945),TC_NIL,
	CONS_HEADER,UPTR(361,4924),UPTR(361,4948),UOHEAD(2,0),UPTR(361,4951),UITAG(0),UOHEAD(2,1),UPTR(348,1),UITAG(0),UOHEAD(2,1),
	UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(338,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,4966),
	TC_NIL,UOHEAD(2,0),UPTR(361,4969),UITAG(0),UOHEAD(2,1),UPTR(144,1),UITAG(0),CONS_HEADER,UPTR(361,4975),TC_NIL,
	UOHEAD(2,0),UPTR(361,4978),UITAG(0),CONS_HEADER,UPTR(361,4981),TC_NIL,CONS_HEADER,UPTR(361,4972),UPTR(361,4984),CONS_HEADER,
	UPTR(361,4963),UPTR(361,4987),UOHEAD(2,0),UPTR(361,4990),UITAG(0),CONS_HEADER,UPTR(361,4993),TC_NIL,CONS_HEADER,UPTR(361,4960),
	UPTR(361,4996),UOHEAD(2,0),UPTR(361,4999),UITAG(0),CONS_HEADER,UPTR(361,5002),TC_NIL,CONS_HEADER,UPTR(361,4957),UPTR(361,5005),
	UOHEAD(2,0),UPTR(361,5008),UITAG(0),UOHEAD(2,1),UPTR(350,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),
	UPTR(338,1),UITAG(0),CONS_HEADER,UPTR(361,5020),TC_NIL,UOHEAD(2,0),UPTR(361,5023),UITAG(0),CONS_HEADER,UPTR(361,5026),
	TC_NIL,CONS_HEADER,UPTR(361,5017),UPTR(361,5029),UOHEAD(2,0),UPTR(361,5032),UITAG(0),CONS_HEADER,UPTR(361,5035),TC_NIL,
	CONS_HEADER,UPTR(361,5014),UPTR(361,5038),UOHEAD(2,0),UPTR(361,5041),UITAG(0),UOHEAD(2,1),UPTR(352,1),UITAG(0),UOHEAD(2,1),
	UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(338,1),UITAG(0),CONS_HEADER,UPTR(361,5053),TC_NIL,UOHEAD(2,0),UPTR(361,5056),
	UITAG(0),CONS_HEADER,UPTR(361,5059),TC_NIL,CONS_HEADER,UPTR(361,5050),UPTR(361,5062),UOHEAD(2,0),UPTR(361,5065),UITAG(0),
	CONS_HEADER,UPTR(361,5068),TC_NIL,CONS_HEADER,UPTR(361,5047),UPTR(361,5071),UOHEAD(2,0),UPTR(361,5074),UITAG(0),UOHEAD(2,1),
	UPTR(354,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(361,5086),
	TC_NIL,UOHEAD(2,0),UPTR(361,5089),UITAG(0),CONS_HEADER,UPTR(361,5092),TC_NIL,CONS_HEADER,UPTR(361,5083),UPTR(361,5095),
	UOHEAD(2,0),UPTR(361,5098),UITAG(0),CONS_HEADER,UPTR(361,5101),TC_NIL,CONS_HEADER,UPTR(361,5080),UPTR(361,5104),UOHEAD(2,0),
	UPTR(361,5107),UITAG(0),UOHEAD(2,1),UPTR(356,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),
	UITAG(0),CONS_HEADER,UPTR(361,5119),TC_NIL,UOHEAD(2,0),UPTR(361,5122),UITAG(0),CONS_HEADER,UPTR(361,5125),TC_NIL,
	CONS_HEADER,UPTR(361,5116),UPTR(361,5128),UOHEAD(2,0),UPTR(361,5131),UITAG(0),CONS_HEADER,UPTR(361,5134),TC_NIL,CONS_HEADER,
	UPTR(361,5113),UPTR(361,5137),UOHEAD(2,0),UPTR(361,5140),UITAG(0),UOHEAD(2,1),UPTR(358,1),UITAG(0),UOHEAD(2,1),UPTR(228,1),
	UITAG(0),UOHEAD(2,1),UPTR(338,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,5155),TC_NIL,
	UOHEAD(2,0),UPTR(361,5158),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(361,5164),TC_NIL,UOHEAD(2,0),
	UPTR(361,5167),UITAG(0),CONS_HEADER,UPTR(361,5170),TC_NIL,CONS_HEADER,UPTR(361,5161),UPTR(361,5173),CONS_HEADER,UPTR(361,5152),
	UPTR(361,5176),UOHEAD(2,0),UPTR(361,5179),UITAG(0),CONS_HEADER,UPTR(361,5182),TC_NIL,CONS_HEADER,UPTR(361,5149),UPTR(361,5185),
	UOHEAD(2,0),UPTR(361,5188),UITAG(0),CONS_HEADER,UPTR(361,5191),TC_NIL,CONS_HEADER,UPTR(361,5146),UPTR(361,5194),UOHEAD(2,0),
	UPTR(361,5197),UITAG(0),CONS_HEADER,UPTR(361,5200),TC_NIL,CONS_HEADER,UPTR(361,5143),UPTR(361,5203),CONS_HEADER,UPTR(361,5110),
	UPTR(361,5206),CONS_HEADER,UPTR(361,5077),UPTR(361,5209),CONS_HEADER,UPTR(361,5044),UPTR(361,5212),CONS_HEADER,UPTR(361,5011),UPTR(361,5215),
	CONS_HEADER,UPTR(361,4954),UPTR(361,5218),CONS_HEADER,UPTR(361,4921),UPTR(361,5221),CONS_HEADER,UPTR(361,4888),UPTR(361,5224),CONS_HEADER,
	UPTR(361,4855),UPTR(361,5227),CONS_HEADER,UPTR(361,4822),UPTR(361,5230),CONS_HEADER,UPTR(361,4753),UPTR(361,5233),UOHEAD(2,0),UPTR(361,5236),
	UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(336,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),
	CONS_HEADER,UPTR(361,5248),TC_NIL,CONS_HEADER,UPTR(361,5245),UPTR(361,5251),UOHEAD(2,0),UPTR(361,5254),UITAG(0),UOHEAD(2,1),
	UPTR(340,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),CONS_HEADER,UPTR(361,5263),TC_NIL,CONS_HEADER,UPTR(361,5260),
	UPTR(361,5266),UOHEAD(2,0),UPTR(361,5269),UITAG(0),UOHEAD(2,1),UPTR(342,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	CONS_HEADER,UPTR(361,5278),TC_NIL,CONS_HEADER,UPTR(361,5275),UPTR(361,5281),UOHEAD(2,0),UPTR(361,5284),UITAG(0),UOHEAD(2,1),
	UPTR(360,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),CONS_HEADER,UPTR(361,5293),TC_NIL,CONS_HEADER,UPTR(361,5290),
	UPTR(361,5296),UOHEAD(2,0),UPTR(361,5299),UITAG(0),UOHEAD(2,1),UPTR(344,1),UITAG(0),UOHEAD(2,5),9,UITAG(0),
	CONS_HEADER,UPTR(361,5308),TC_NIL,CONS_HEADER,UPTR(361,5305),UPTR(361,5311),UOHEAD(2,0),UPTR(361,5314),UITAG(0),UOHEAD(2,1),
	UPTR(346,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),CONS_HEADER,UPTR(361,5323),TC_NIL,CONS_HEADER,UPTR(361,5320),
	UPTR(361,5326),UOHEAD(2,0),UPTR(361,5329),UITAG(0),UOHEAD(2,1),UPTR(348,1),UITAG(0),UOHEAD(2,5),13,UITAG(0),
	CONS_HEADER,UPTR(361,5338),TC_NIL,CONS_HEADER,UPTR(361,5335),UPTR(361,5341),UOHEAD(2,0),UPTR(361,5344),UITAG(0),UOHEAD(2,1),
	UPTR(350,1),UITAG(0),UOHEAD(2,5),15,UITAG(0),CONS_HEADER,UPTR(361,5353),TC_NIL,CONS_HEADER,UPTR(361,5350),
	UPTR(361,5356),UOHEAD(2,0),UPTR(361,5359),UITAG(0),UOHEAD(2,1),UPTR(352,1),UITAG(0),UOHEAD(2,5),17,UITAG(0),
	CONS_HEADER,UPTR(361,5368),TC_NIL,CONS_HEADER,UPTR(361,5365),UPTR(361,5371),UOHEAD(2,0),UPTR(361,5374),UITAG(0),UOHEAD(2,1),
	UPTR(354,1),UITAG(0),UOHEAD(2,5),19,UITAG(0),CONS_HEADER,UPTR(361,5383),TC_NIL,CONS_HEADER,UPTR(361,5380),
	UPTR(361,5386),UOHEAD(2,0),UPTR(361,5389),UITAG(0),UOHEAD(2,1),UPTR(356,1),UITAG(0),UOHEAD(2,5),21,UITAG(0),
	CONS_HEADER,UPTR(361,5398),TC_NIL,CONS_HEADER,UPTR(361,5395),UPTR(361,5401),UOHEAD(2,0),UPTR(361,5404),UITAG(0),UOHEAD(2,1),
	UPTR(358,1),UITAG(0),UOHEAD(2,5),23,UITAG(0),CONS_HEADER,UPTR(361,5413),TC_NIL,CONS_HEADER,UPTR(361,5410),
	UPTR(361,5416),UOHEAD(2,0),UPTR(361,5419),UITAG(0),CONS_HEADER,UPTR(361,5422),TC_NIL,CONS_HEADER,UPTR(361,5407),UPTR(361,5425),
	CONS_HEADER,UPTR(361,5392),UPTR(361,5428),CONS_HEADER,UPTR(361,5377),UPTR(361,5431),CONS_HEADER,UPTR(361,5362),UPTR(361,5434),CONS_HEADER,
	UPTR(361,5347),UPTR(361,5437),CONS_HEADER,UPTR(361,5332),UPTR(361,5440),CONS_HEADER,UPTR(361,5317),UPTR(361,5443),CONS_HEADER,UPTR(361,5302),
	UPTR(361,5446),CONS_HEADER,UPTR(361,5287),UPTR(361,5449),CONS_HEADER,UPTR(361,5272),UPTR(361,5452),CONS_HEADER,UPTR(361,5257),UPTR(361,5455),
	CONS_HEADER,UPTR(361,5242),UPTR(361,5458),UOHEAD(2,0),UPTR(361,5461),UITAG(0),CONS_HEADER,UPTR(361,5464),TC_NIL,CONS_HEADER,
	UPTR(361,5239),UPTR(361,5467),CONS_HEADER,UPTR(361,4750),UPTR(361,5470),CONS_HEADER,UPTR(361,1),UPTR(361,5473),UOHEAD(2,0),UPTR(361,5476),
	UITAG(0)};
irk_int irk_internal_symbols[] = {(115<<TAGSIZE)|TC_VECTOR,
	UPTR(356,1),UPTR(354,1),UPTR(346,1),UPTR(318,1),UPTR(314,1),UPTR(358,1),UPTR(348,1),UPTR(332,1),UPTR(330,1),UPTR(328,1),
	UPTR(326,1),UPTR(324,1),UPTR(322,1),UPTR(320,1),UPTR(350,1),UPTR(352,1),UPTR(342,1),UPTR(340,1),UPTR(344,1),UPTR(336,1),
	UPTR(316,1),UPTR(312,1),UPTR(308,1),UPTR(306,1),UPTR(304,1),UPTR(302,1),UPTR(300,1),UPTR(298,1),UPTR(296,1),UPTR(294,1),
	UPTR(290,1),UPTR(288,1),UPTR(286,1),UPTR(280,1),UPTR(276,1),UPTR(274,1),UPTR(272,1),UPTR(264,1),UPTR(262,1),UPTR(260,1),
	UPTR(258,1),UPTR(256,1),UPTR(254,1),UPTR(252,1),UPTR(248,1),UPTR(246,1),UPTR(244,1),UPTR(242,1),UPTR(240,1),UPTR(238,1),
	UPTR(232,1),UPTR(230,1),UPTR(226,1),UPTR(220,1),UPTR(216,1),UPTR(200,1),UPTR(198,1),UPTR(196,1),UPTR(190,1),UPTR(184,1),
	UPTR(182,1),UPTR(180,1),UPTR(178,1),UPTR(176,1),UPTR(152,1),UPTR(141,1),UPTR(135,1),UPTR(127,1),UPTR(125,1),UPTR(123,1),
	UPTR(117,1),UPTR(150,1),UPTR(218,1),UPTR(212,1),UPTR(284,1),UPTR(186,1),UPTR(222,1),UPTR(139,1),UPTR(278,1),UPTR(224,1),
	UPTR(236,1),UPTR(234,1),UPTR(338,1),UPTR(228,1),UPTR(119,1),UPTR(334,1),UPTR(170,1),UPTR(121,1),UPTR(188,1),UPTR(174,1),
	UPTR(360,1),UPTR(202,1),UPTR(144,1),UPTR(266,1),UPTR(115,1),UPTR(268,1),UPTR(270,1),UPTR(250,1),UPTR(129,1),UPTR(210,1),
	UPTR(208,1),UPTR(292,1),UPTR(206,1),UPTR(137,1),UPTR(172,1),UPTR(204,1),UPTR(148,1),UPTR(146,1),UPTR(192,1),UPTR(194,1),
	UPTR(282,1),UPTR(310,1),UPTR(131,1),UPTR(214,1),UPTR(133,1)};
object * irk_internal_symbols_p = (object*) &irk_internal_symbols;
// datatype table
// name: dirent-file-type
//  (:symlink )
//  (:block )
//  (:char )
//  (:socket )
//  (:fifo )
//  (:directory )
//  (:regular )
// name: hufftree
//  (:leaf int)
//  (:node hufftree hufftree)
// name: set2-ob
//  (:t {t=(tree t14 t15) cmp=(t14 t14 -> cmp) })
// name: map-ob
//  (:t {t=(tree t12 t13) cmp=(t12 t12 -> cmp) })
// name: ident
//  (:systup symbol int)
//  (:sysimm symbol int)
//  (:usertup int int)
//  (:userimm int int)
//  (:int int)
// name: tree
//  (:empty )
//  (:black (tree t10 t11) (tree t10 t11) t10 t11)
//  (:red (tree t10 t11) (tree t10 t11) t10 t11)
// name: csig
//  (:obj symbol ctype)
//  (:fun symbol ctype (list ctype) bool)
// name: cdef
//  (:union int symbol (list cfield))
//  (:struct int symbol (list cfield))
// name: cfield
//  (:t int symbol ctype)
// name: ctype
//  (:union symbol)
//  (:struct symbol)
//  (:pointer ctype)
//  (:array int ctype)
//  (:int cint bool)
//  (:name symbol)
// name: cint
//  (:width int)
//  (:longlong )
//  (:long )
//  (:int )
//  (:short )
//  (:char )
// name: alist-ob
//  (:t {alist=(alist t8 t9) })
// name: alist
//  (:entry t6 t7 (alist t6 t7))
//  (:nil )
// name: set
//  (:empty )
//  (:black (set t5) (set t5) t5)
//  (:red (set t5) (set t5) t5)
// name: fqueue
//  (:t (list t4) (list t4))
// name: src-step
//  (:inlined symbol)
//  (:macro symbol)
//  (:at {path=string line=int pos=int })
// name: src
//  (:macro symbol src src)
//  (:inline symbol src src)
//  (:base {path=string line=int pos=int })
//  (:nil )
// name: field
//  (:t symbol sexp src)
// name: sexp
//  (:attr sexp symbol src)
//  (:cons symbol symbol src)
//  (:record (list field) src)
//  (:vector (list sexp) src)
//  (:undef src)
//  (:int int src)
//  (:bool bool src)
//  (:char char src)
//  (:string string src)
//  (:symbol symbol src)
//  (:list (list sexp) src)
// name: list
//  (:cons t3 (list t3))
//  (:nil )
// name: result
//  (:err t2)
//  (:ok t1)
// name: bool
//  (:false )
//  (:true )
// name: maybe
//  (:no )
//  (:yes t0)
// name: cmp
//  (:> )
//  (:= )
//  (:< )
// name: symbol
//  (:t string int)
void prof_dump (void) {}
static void FUN_base_exception_handler_255_0(void);
static void FUN_truncate_remainder_196_0(void);
static void FUN_euclidean_2f_200_0(void);
static void FUN_euclidean_quotient_201_0(void);
static void FUN_int_cmp_220_0(void);
static void FUN_magic_cmp_221_0(void);
static void FUN_reverse_onto_269_0(void);
static void FUN_length_splat_272_0(void);
static void FUN_n_of_291_0(void);
static void FUN_map_292_0(void);
static void FUN_for_each_297_0(void);
static void FUN_fold_299_0(void);
static void FUN_buffer_copy_330_0(void);
static void FUN_string_set_bang_337_0(void);
static void FUN_char__3estring_333_0(void);
static void FUN_string_ref_336_0(void);
static void FUN_string_concat_338_0(void);
static void FUN_list__3estring_352_0(void);
static void FUN_string__3elist_353_0(void);
static void FUN_int__3ehex_string_372_0(void);
static void FUN_toplevel_1(O);
static void FUN_toplevel_2(O);
static void FUN_toplevel_3(O);
static void FUN_toplevel_4(O);
static void FUN_toplevel_5(O);
static void FUN_toplevel_6(O);
static void FUN_toplevel_7(O);
static void FUN_toplevel_8(O);
static void FUN_toplevel_9(O);
static void FUN_toplevel_10(O);
static void FUN_in_class_question_1372_i1_0(void);
static void FUN__24loop31_1374_i1_0(void);
static void FUN_toplevel_11(O);
static void FUN_safe_char_359_0(void);
static void FUN_int__3estring_363_0(void);
static void FUN_lbalance_563_0(void);
static void FUN_rbalance_564_0(void);
static void FUN_tree_2finsert_565_0(void);
static void FUN_tree_2fdelete_567_0(void);
static void FUN_tree_2fmember_570_0(void);
static void FUN_tree_2fmin_572_0(void);
static void FUN_print_exception_exit_592_0(void);
static void FUN_hexdigit_649_0(void);
static void FUN_hexdig__3eint_652_0(void);
static void FUN_recur_1164_i2_0(void);
static void JUMP_65(O, O);
static void FUN_toplevel_12(O);
static void FUN_toplevel_13(O);
static void FUN__24loop126_3522_0(void);
static void FUN_toplevel_14(O);
static void FUN_toplevel_15(O);
static void FUN_recur_3281_i376_0(void);
static void FUN_toplevel_16(O);
static void FUN__24loop128_3534_0(void);
static void FUN_toplevel_17(O);
static void JUMP_52();
static void FUN_toplevel_18(O);
static void FUN_evict_one_3328_i2_0(void);
static void FUN_set_3330_i2_0(void);
static void FUN_get_3331_i2_0(void);
static void FUN_set_size_3332_i2_0(void);
static void FUN_name_range_3335_i2_0(void);
static void FUN_lookup_name_only_3336_i2_0(void);
static void FUN_nvals_3337_i2_0(void);
static void FUN_adjust_3340_i2_0(void);
static void FUN_get_index_3341_i2_0(void);
static void FUN_forward_3405_i304_i386_0(void);
static void FUN_get_integer_3407_i304_i386_0(void);
static void FUN_get_literal_3409_i304_i386_0(void);
static void FUN_get_pair0_3408_i304_i386_0(void);
static void FUN_unpack_3411_i304_i386_0(void);
static void FUN__24loop119_3498_i386_0(void);
static void FUN__24loop119_3498_i386_19(O);
static void JUMP_4(O, O, O);
static void FUN__24loop119_3498_i386_20(O);
static void FUN__24loop93_3269_i306_i386_0(void);
static void FUN__24loop119_3498_i386_21(O);
static void FUN__24loop93_3269_i306_i386_22(O);
static void FUN__24loop93_3269_i306_i386_23(O);
static void FUN__24loop93_3269_i306_i386_24(O);
static void FUN__24loop93_3269_i306_i386_25(O);
static void FUN__24loop93_3269_i306_i386_26(O);
static void FUN__24loop93_3269_i306_i386_27(O);
static void FUN_print_pair_3492_i307_i386_0(void);
static void FUN__24loop93_3269_i306_i386_28(O);
static void FUN_print_pair_3492_i307_i386_29(O);
static void FUN_unpack_3411_i304_i386_30(O);
static void FUN__24loop103_3434_i304_i386_0(void);
static void FUN_unpack_3411_i304_i386_31(O);
static void JUMP_8();
static void FUN__24loop103_3434_i304_i386_32(O);
static void FUN__24loop103_3434_i304_i386_33(O);
static void FUN__24loop103_3434_i304_i386_34(O);
static void JUMP_9(O, O);
static void FUN__24loop103_3434_i304_i386_35(O);
static void JUMP_10(O, O);
static void FUN__24loop103_3434_i304_i386_36(O);
static void JUMP_12(O, O);
static void FUN__24loop103_3434_i304_i386_37(O);
static void FUN__24loop103_3434_i304_i386_38(O);
static void FUN__24loop103_3434_i304_i386_39(O);
static void FUN__24loop103_3434_i304_i386_40(O);
static void FUN__24loop103_3434_i304_i386_41(O);
static void FUN__24loop103_3434_i304_i386_42(O);
static void FUN__24loop103_3434_i304_i386_43(O);
static void FUN__24loop103_3434_i304_i386_44(O);
static void FUN__24loop103_3434_i304_i386_45(O);
static void FUN__24loop103_3434_i304_i386_46(O);
static void FUN__24loop103_3434_i304_i386_47(O);
static void FUN__24loop103_3434_i304_i386_48(O);
static void FUN__24loop103_3434_i304_i386_49(O);
static void FUN__24loop103_3434_i304_i386_50(O);
static void FUN__24loop103_3434_i304_i386_51(O);
static void FUN__24loop103_3434_i304_i386_52(O);
static void FUN__24loop103_3434_i304_i386_53(O);
static void JUMP_14(O, O);
static void FUN_get_pair0_3408_i304_i386_54(O);
static void FUN_get_pair0_3408_i304_i386_55(O);
static void FUN_get_pair0_3408_i304_i386_56(O);
static void FUN_get_literal_3409_i304_i386_57(O);
static void JUMP_16(O, O);
static void FUN__24loop100_3316_i280_i304_i386_0(void);
static void FUN_get_literal_3409_i304_i386_58(O);
static void FUN_get_literal_3409_i304_i386_59(O);
static void FUN_get_literal_3409_i304_i386_60(O);
static void FUN_get_literal_3409_i304_i386_61(O);
static void FUN_loop_3317_i280_i304_i386_0(void);
static void FUN__24loop100_3316_i280_i304_i386_62(O);
static void JUMP_21();
static void FUN_loop_3317_i280_i304_i386_63(O);
static void JUMP_20(O);
static void FUN_loop_3317_i280_i304_i386_64(O);
static void FUN_loop_3416_i304_i386_0(void);
static void FUN_get_integer_3407_i304_i386_65(O);
static void FUN_get_integer_3407_i304_i386_66(O);
static void FUN_get_integer_3407_i304_i386_67(O);
static void FUN_loop_3416_i304_i386_68(O);
static void FUN_forward_3405_i304_i386_69(O);
static void FUN_get_index_3341_i2_70(O);
static void JUMP_34(O, O);
static void FUN_get_index_3341_i2_71(O);
static void FUN_get_index_3341_i2_72(O);
static void JUMP_33(O, O);
static void FUN_get_index_3341_i2_73(O);
static void FUN_get_index_3341_i2_74(O);
static void JUMP_32(O);
static void JUMP_31(O);
static void FUN_get_index_3341_i2_75(O);
static void FAIL_35();
static void FUN_nvals_3337_i2_76(O);
static void FUN_lookup_name_only_3336_i2_77(O);
static void FUN_lambda_22340_i2_0(void);
static void FUN_walk_2987_i244_i2_0(void);
static void FUN_visit_2986_i244_i2_0(void);
static void FUN_name_range_3335_i2_78(O);
static void JUMP_43();
static void FUN_visit_2986_i244_i2_79(O);
static void JUMP_42(O);
static void JUMP_41();
static void FUN_visit_2986_i244_i2_80(O);
static void FUN_set_size_3332_i2_81(O);
static void FUN_set_size_3332_i2_82(O);
static void FUN__24loop102_3365_i2_0(void);
static void FUN__24loop102_3365_i2_83(O);
static void FUN_get_3331_i2_84(O);
static void FUN_get_3331_i2_85(O);
static void FUN_get_3331_i2_86(O);
static void FUN_get_3331_i2_87(O);
static void FUN__24loop101_3358_i2_0(void);
static void FUN_set_3330_i2_88(O);
static void FUN_set_3330_i2_89(O);
static void FUN_set_3330_i2_90(O);
static void FUN__24loop101_3358_i2_91(O);
static void FUN_evict_one_3328_i2_92(O);
static void FUN_evict_one_3328_i2_93(O);
static void FUN_evict_one_3328_i2_94(O);
static void FUN_evict_one_3328_i2_95(O);
static void FUN_evict_one_3328_i2_96(O);
static void FUN_recur_3281_i376_97(O);
static void FAIL_55();
static void FUN_recur_3281_i376_98(O);
static void FAIL_59();
static void FUN_recur_3281_i376_99(O);
static void FUN_recur_3281_i376_100(O);
static void FUN_recur_3281_i376_101(O);
static void FUN_recur_3281_i376_102(O);
static void FUN_recur_3281_i376_103(O);
static void FUN__24loop126_3522_104(O);
static void FAIL_67();
static void FAIL_71();
static void FAIL_75();
static void FAIL_79();
static void FAIL_83();
static void FAIL_87();
static void FAIL_91();
static void FAIL_95();
static void FAIL_99();
static void FAIL_103();
static void FAIL_107();
static void FAIL_111();
static void FAIL_115();
static void FAIL_119();
static void FAIL_123();
static void FAIL_127();
static void FAIL_131();
static void FAIL_135();
static void FAIL_139();
static void FAIL_143();
static void FAIL_147();
static void FAIL_151();
static void FAIL_155();
static void FAIL_159();
static void FAIL_163();
static void FAIL_167();
static void FAIL_171();
static void FAIL_175();
static void FAIL_179();
static void FAIL_183();
static void FAIL_187();
static void FAIL_191();
static void FAIL_195();
static void FAIL_199();
static void FAIL_203();
static void FAIL_207();
static void FAIL_211();
static void FAIL_215();
static void JUMP_233(O);
static void FUN_search_sexp_list_3009_i112_i141_i142_i147_0(void);
static void JUMP_244(O, O, O);
static void JUMP_245(O, O, O);
static void JUMP_246(O, O, O);
static void JUMP_247(O, O, O);
static void JUMP_249(O, O, O);
static void FUN_print_exception_exit_592_105(O);
static void FAIL_243(O, O);
static void FAIL_243(O, O);
static void FAIL_243(O, O);
static void FAIL_243(O, O);
static void FUN_print_exception_exit_592_106(O);
static void FUN_loop_3024_i141_i142_i147_0(void);
static void FUN_print_exception_exit_592_107(O);
static void FAIL_234();
static void FAIL_234();
static void FAIL_234();
static void FAIL_234();
static void FAIL_234();
static void FAIL_234();
static void FAIL_234();
static void FUN_loop_3024_i141_i142_i147_108(O);
static void FAIL_250();
static void FAIL_250();
static void FAIL_250();
static void FAIL_250();
static void JUMP_231(O, O, O);
static void JUMP_232(O, O, O);
static void JUMP_230(O, O, O);
static void JUMP_221(O, O, O);
static void JUMP_228(O, O, O);
static void JUMP_222(O, O, O);
static void JUMP_224(O, O, O);
static void JUMP_226(O, O, O);
static void JUMP_219(O, O);
static void FUN_search_sexp_list_3009_i112_i141_i142_i147_109(O);
static void FAIL_259(O);
static void FAIL_262(O);
static void FUN_member0_2890_0(void);
static void FUN_member0_2890_110(O);
static void FUN_member0_2890_111(O);
static void FUN_lunbalanced_2739_i102_0(void);
static void FUN_runbalanced_2740_i102_0(void);
static void FUN_remove_min_2741_i102_0(void);
static void FUN_remove_aux_2743_i102_0(void);
static void FUN_tree_2fdelete_567_112(O);
static void FUN_remove_aux_2743_i102_113(O);
static void FUN_remove_aux_2743_i102_114(O);
static void FUN_remove_aux_2743_i102_115(O);
static void FUN_remove_aux_2743_i102_116(O);
static void FAIL_288();
static void FUN_remove_aux_2743_i102_117(O);
static void FUN_remove_aux_2743_i102_118(O);
static void FUN_remove_aux_2743_i102_119(O);
static void FAIL_278();
static void FAIL_275();
static void FUN_remove_aux_2743_i102_120(O);
static void FAIL_294();
static void FAIL_301();
static void FUN_remove_min_2741_i102_121(O);
static void FUN_remove_min_2741_i102_122(O);
static void FUN_remove_min_2741_i102_123(O);
static void FUN_remove_min_2741_i102_124(O);
static void FUN_runbalanced_2740_i102_125(O);
static void FAIL_307();
static void FUN_runbalanced_2740_i102_126(O);
static void FUN_runbalanced_2740_i102_127(O);
static void FAIL_307();
static void FAIL_307();
static void FAIL_307();
static void FUN_lunbalanced_2739_i102_128(O);
static void FAIL_313();
static void FUN_lunbalanced_2739_i102_129(O);
static void FUN_lunbalanced_2739_i102_130(O);
static void FAIL_313();
static void FAIL_313();
static void FAIL_313();
static void FUN_ins_2719_0(void);
static void FUN_tree_2finsert_565_131(O);
static void FAIL_319();
static void FUN_ins_2719_132(O);
static void FUN_ins_2719_133(O);
static void FUN_ins_2719_134(O);
static void FUN_ins_2719_135(O);
static void FUN_ins_2719_136(O);
static void FUN_ins_2719_137(O);
static void FAIL_328();
static void FAIL_325();
static void FAIL_325();
static void FAIL_335();
static void FAIL_332();
static void FAIL_332();
static void FUN_loop_1410_0(void);
static void JUMP_342(O, O);
static void JUMP_346(O, O);
static void FUN_loop_1410_138(O);
static void FUN_loop_1410_139(O);
static void FAIL_347();
static void FAIL_351();
static void FAIL_355();
static void FAIL_359();
static void FAIL_363();
static void FAIL_367();
static void FUN_safe_char_359_140(O);
static void FUN_safe_char_359_141(O);
static void JUMP_373(O, O, O);
static void FUN_loop_1044_i0_0(void);
static void FUN_safe_char_359_142(O);
static void FUN_safe_char_359_143(O);
static void FUN_safe_char_359_144(O);
static void FUN_loop_1446_0(void);
static void JUMP_378(O, O);
static void JUMP_382(O, O);
static void FUN_loop_1367_0(void);
static void FUN_loop_1367_145(O);
static void FUN_list__3estring_352_146(O);
static void FUN__24loop28_1362_0(void);
static void FUN_list__3estring_352_147(O);
static void FUN__24loop28_1362_148(O);
static void FAIL_386();
static void FUN_lambda_3907_0(void);
static void FUN_string_concat_338_149(O);
static void FUN_loop_1295_0(void);
static void FUN_loop_1295_150(O);
static void JUMP_393(O);
static void JUMP_391();
static void FUN_string_ref_336_151(O);
static void FUN_char__3estring_333_152(O);
static void JUMP_396(O);
static void JUMP_394();
static void FUN_string_set_bang_337_153(O);
static void JUMP_403(O, O);
static void JUMP_404(O, O);
static void JUMP_401();
static void FUN_buffer_copy_330_154(O);
static void JUMP_399(O, O);
static void JUMP_400(O, O);
static void JUMP_397();
static void FUN_buffer_copy_330_155(O);
static void FUN_fold_299_156(O);
static void FUN_for_each_297_157(O);
static void FUN_map_292_158(O);
static void FUN_map_292_159(O);
static void FUN_loop_1044_0(void);
static void FUN_euclidean_quotient_201_160(O);
static void FAIL_418();
static void FAIL_415();
void toplevel (void) {
  clear_tospace = 0;
  // env 0:
  //   (|...t24| -> t25) (|...t111| -> t110) (int int -> int) (int int -> |tuple=(product int int) ...t968|) (int int -> int) (int int -> cmp) (t1204 t1204 -> cmp) ((list t791) (list t791) -> (list t791)) ((list t90) int -> int) (int t2283 -> (list t2283)) ((t814 -> t815) (list t814) -> (list t815)) ((t1118 -> t1121) (list t1118) -> undefined) ((t98 t95 -> t95) t95 (list t98) -> t95) (string int int string int -> undefined) (string int char -> undefined) (char -> string) (string int -> char) ((list string) -> string) ((list char) -> string) (string -> (list char)) (vector char) (int -> string) (list char) (list char) (list char) (list char) (list char) (char -> bool) (char -> string) (int int -> int) (int int -> int) (int -> string) ((tree t1209 t1210) (tree t1209 t1210) t1209 t1210 -> (tree t1209 t1210)) ((tree t1265 t1266) (tree t1265 t1266) t1265 t1266 -> (tree t1265 t1266)) ((tree t1321 t1322) (t1321 t1321 -> cmp) t1321 t1322 -> (tree t1321 t1322)) ((tree t2171 t2172) (t1389 t1862 -> cmp) t1389 -> (tree t2171 t2172)) ((tree t2267 t2268) (t2231 t2236 -> cmp) t2231 -> (maybe t2268)) ((tree t2186 t2185) -> |tuple=(product t2186 t2185) ...t2207|) int int (vector symbol) (vector symbol) (vector symbol) (|...t29| -> t30) (char -> int) (char -> int) (int int -> |tuple=(product int int) ...t1124|) (vector |tuple=(product string string) |) int (tree |tuple=(product string string) | int) string hufftree (vector int) (list string)
  O r1306 = alloc_cleared (TC_ENV, 55);
  top = r1306;
  r1306[1] = lenv; lenv = r1306;
  O r1307 = allocate (TC_CLOSURE, 2);
  r1307[1] = FUN_base_exception_handler_255_0; r1307[2] = lenv;
  r1306[2] = r1307;
  O r1308 = top[2];
  r1306[3] = r1308;
  O r1309 = allocate (TC_CLOSURE, 2);
  r1309[1] = FUN_truncate_remainder_196_0; r1309[2] = lenv;
  r1306[4] = r1309;
  O r1310 = allocate (TC_CLOSURE, 2);
  r1310[1] = FUN_euclidean_2f_200_0; r1310[2] = lenv;
  r1306[5] = r1310;
  O r1311 = allocate (TC_CLOSURE, 2);
  r1311[1] = FUN_euclidean_quotient_201_0; r1311[2] = lenv;
  r1306[6] = r1311;
  O r1312 = allocate (TC_CLOSURE, 2);
  r1312[1] = FUN_int_cmp_220_0; r1312[2] = lenv;
  r1306[7] = r1312;
  O r1313 = allocate (TC_CLOSURE, 2);
  r1313[1] = FUN_magic_cmp_221_0; r1313[2] = lenv;
  r1306[8] = r1313;
  O r1314 = allocate (TC_CLOSURE, 2);
  r1314[1] = FUN_reverse_onto_269_0; r1314[2] = lenv;
  r1306[9] = r1314;
  O r1315 = allocate (TC_CLOSURE, 2);
  r1315[1] = FUN_length_splat_272_0; r1315[2] = lenv;
  r1306[10] = r1315;
  O r1316 = allocate (TC_CLOSURE, 2);
  r1316[1] = FUN_n_of_291_0; r1316[2] = lenv;
  r1306[11] = r1316;
  O r1317 = allocate (TC_CLOSURE, 2);
  r1317[1] = FUN_map_292_0; r1317[2] = lenv;
  r1306[12] = r1317;
  O r1318 = allocate (TC_CLOSURE, 2);
  r1318[1] = FUN_for_each_297_0; r1318[2] = lenv;
  r1306[13] = r1318;
  O r1319 = allocate (TC_CLOSURE, 2);
  r1319[1] = FUN_fold_299_0; r1319[2] = lenv;
  r1306[14] = r1319;
  O r1320 = allocate (TC_CLOSURE, 2);
  r1320[1] = FUN_buffer_copy_330_0; r1320[2] = lenv;
  r1306[15] = r1320;
  O r1321 = allocate (TC_CLOSURE, 2);
  r1321[1] = FUN_string_set_bang_337_0; r1321[2] = lenv;
  r1306[16] = r1321;
  O r1322 = allocate (TC_CLOSURE, 2);
  r1322[1] = FUN_char__3estring_333_0; r1322[2] = lenv;
  r1306[17] = r1322;
  O r1323 = allocate (TC_CLOSURE, 2);
  r1323[1] = FUN_string_ref_336_0; r1323[2] = lenv;
  r1306[18] = r1323;
  O r1324 = allocate (TC_CLOSURE, 2);
  r1324[1] = FUN_string_concat_338_0; r1324[2] = lenv;
  r1306[19] = r1324;
  O r1325 = allocate (TC_CLOSURE, 2);
  r1325[1] = FUN_list__3estring_352_0; r1325[2] = lenv;
  r1306[20] = r1325;
  O r1326 = allocate (TC_CLOSURE, 2);
  r1326[1] = FUN_string__3elist_353_0; r1326[2] = lenv;
  r1306[21] = r1326;
  O r1327 = irk_copy_tuple ((O) constructed_167[0]);
  r1306[22] = r1327;
  O r1328 = allocate (TC_CLOSURE, 2);
  r1328[1] = FUN_int__3ehex_string_372_0; r1328[2] = lenv;
  r1306[23] = r1328;
  // env 1:
  //   string
  O r4037 = allocate (TC_ENV, 2);
  O r4039 = (O) &constructed_166;
  r4037[2] = r4039;
  O r4038 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_1; t[4] = r1306; k = t;
  r4037[1] = r4038[2]; lenv = r4037; FUN_string__3elist_353_0();
}
static void FUN_toplevel_1 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1329 = rr;
  r1306[24] = r1329;
  // env 2:
  //   string
  O r4034 = allocate (TC_ENV, 2);
  O r4036 = (O) &constructed_165;
  r4034[2] = r4036;
  O r4035 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_2; t[4] = r1306; k = t;
  r4034[1] = r4035[2]; lenv = r4034; FUN_string__3elist_353_0();
}
static void FUN_toplevel_2 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1330 = rr;
  r1306[25] = r1330;
  // env 3:
  //   (list char) (list char)
  O r4026 = alloc_cleared (TC_ENV, 3);
  // env 4:
  //   (list char) (list |tuple=(product string string) |)
  O r4030 = allocate (TC_ENV, 3);
  O r4032 = top[24];
  r4030[2] = r4032;
  O r4033 = (object *) 10;
  r4030[3] = r4033;
  O r4031 = top[9];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_3; t[4] = r1306; t[5] = r4026; k = t;
  r4030[1] = r4031[2]; lenv = r4030; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_3 (O rr) {
  O r1306 = k[4]; O r4026 = k[5]; lenv = k[2]; k = k[1];
  O r4028 = rr;
  r4026[2] = r4028;
  O r4029 = top[25];
  r4026[3] = r4029;
  O r4027 = top[9];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_4; t[4] = r1306; k = t;
  r4026[1] = r4027[2]; lenv = r4026; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_4 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1331 = rr;
  r1306[26] = r1331;
  // env 5:
  //   string
  O r4023 = allocate (TC_ENV, 2);
  O r4025 = (O) &constructed_164;
  r4023[2] = r4025;
  O r4024 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_5; t[4] = r1306; k = t;
  r4023[1] = r4024[2]; lenv = r4023; FUN_string__3elist_353_0();
}
static void FUN_toplevel_5 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1332 = rr;
  r1306[27] = r1332;
  // env 6:
  //   (list char) (list char)
  O r4009 = alloc_cleared (TC_ENV, 3);
  r4009[1] = lenv; lenv = r4009;
  // env 7:
  //   string
  O r4020 = allocate (TC_ENV, 2);
  O r4022 = (O) &constructed_163;
  r4020[2] = r4022;
  O r4021 = top[21];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_6; t[4] = r1306; t[5] = r4009; k = t;
  r4020[1] = r4021[2]; lenv = r4020; FUN_string__3elist_353_0();
}
static void FUN_toplevel_6 (O rr) {
  O r1306 = k[4]; O r4009 = k[5]; lenv = k[2]; k = k[1];
  O r4010 = rr;
  r4009[2] = r4010;
  // env 8:
  //   (list char) (list char)
  O r4012 = alloc_cleared (TC_ENV, 3);
  // env 9:
  //   (list char) (list |tuple=(product string string) |)
  O r4016 = allocate (TC_ENV, 3);
  O r4018 = top[26];
  r4016[2] = r4018;
  O r4019 = (object *) 10;
  r4016[3] = r4019;
  O r4017 = top[9];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_7; t[4] = r1306; t[5] = r4009; t[6] = r4012; k = t;
  r4016[1] = r4017[2]; lenv = r4016; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_7 (O rr) {
  O r1306 = k[4]; O r4009 = k[5]; O r4012 = k[6]; lenv = k[2]; k = k[1];
  O r4014 = rr;
  r4012[2] = r4014;
  O r4015 = ((object*) lenv) [2];
  r4012[3] = r4015;
  O r4013 = top[9];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_8; t[4] = r1306; t[5] = r4009; k = t;
  r4012[1] = r4013[2]; lenv = r4012; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_8 (O rr) {
  O r1306 = k[4]; O r4009 = k[5]; lenv = k[2]; k = k[1];
  O r4011 = rr;
  r4009[3] = r4011;
  // env 10:
  //   (list char) (list char)
  O r4001 = alloc_cleared (TC_ENV, 3);
  // env 11:
  //   (list char) (list |tuple=(product string string) |)
  O r4005 = allocate (TC_ENV, 3);
  O r4007 = top[27];
  r4005[2] = r4007;
  O r4008 = (object *) 10;
  r4005[3] = r4008;
  O r4006 = top[9];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_9; t[4] = r1306; t[5] = r4001; k = t;
  r4005[1] = r4006[2]; lenv = r4005; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_9 (O rr) {
  O r1306 = k[4]; O r4001 = k[5]; lenv = k[2]; k = k[1];
  O r4003 = rr;
  r4001[2] = r4003;
  O r4004 = ((object*) lenv) [3];
  r4001[3] = r4004;
  O r4002 = top[9];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_10; t[4] = r1306; k = t;
  r4001[1] = r4002[2]; lenv = r4001; FUN_reverse_onto_269_0();
}
static void FUN_toplevel_10 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r4000 = rr;
  lenv = lenv[1];
  O r1333 = r4000;
  r1306[28] = r1333;
  // env 12:
  //   int (vector bool)
  O r3994 = alloc_cleared (TC_ENV, 3);
  r3994[1] = lenv; lenv = r3994;
  O r3995 = (object *) 513;
  r3994[2] = r3995;
  O r3999 = ((object*) lenv) [2];
  if (freep + unbox(r3999) >= limit) {
    heap_request = unbox(r3999);
    heap1[3] = r3994;
    heap1[4] = r1306;
    gc_flip (2);
    r3994 = heap0[3];
    r1306 = heap0[4];
  }
  O r3997 = ((object*) lenv) [2];
  O r3998 = (object *) 6;
  O r3996;
  if (unbox(r3997) == 0) { r3996 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r3997));
    for (int i=0; i<unbox(r3997); i++) { t[i+1] = r3998; }
    r3996 = t;
  }
  r3994[3] = r3996;
  // env 13:
  //   (char -> bool)
  O r3987 = alloc_cleared (TC_ENV, 2);
  r3987[1] = lenv; lenv = r3987;
  O r3988 = allocate (TC_CLOSURE, 2);
  r3988[1] = FUN_in_class_question_1372_i1_0; r3988[2] = lenv;
  r3987[2] = r3988;
  // env 14:
  //   ((list char) -> undefined)
  O r3966 = alloc_cleared (TC_ENV, 2);
  r3966[1] = lenv; lenv = r3966;
  O r3967 = allocate (TC_CLOSURE, 2);
  r3967[1] = FUN__24loop31_1374_i1_0; r3967[2] = lenv;
  r3966[2] = r3967;
  // env 15:
  //   (list char)
  O r3963 = allocate (TC_ENV, 2);
  O r3965 = top[28];
  r3963[2] = r3965;
  O r3964 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_11; t[4] = r1306; k = t;
  r3963[1] = r3964[2]; lenv = r3963; FUN__24loop31_1374_i1_0();
}
static void FUN_toplevel_11 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r3962 = rr;
  lenv = lenv[1];
  O r3961 = ((object*) lenv) [2];
  lenv = lenv[1];
  O r3960 = r3961;
  lenv = lenv[1];
  O r1334 = r3960;
  r1306[29] = r1334;
  O r1335 = allocate (TC_CLOSURE, 2);
  r1335[1] = FUN_safe_char_359_0; r1335[2] = lenv;
  r1306[30] = r1335;
  O r1336 = top[6];
  r1306[31] = r1336;
  O r1337 = top[4];
  r1306[32] = r1337;
  O r1338 = allocate (TC_CLOSURE, 2);
  r1338[1] = FUN_int__3estring_363_0; r1338[2] = lenv;
  r1306[33] = r1338;
  O r1339 = allocate (TC_CLOSURE, 2);
  r1339[1] = FUN_lbalance_563_0; r1339[2] = lenv;
  r1306[34] = r1339;
  O r1340 = allocate (TC_CLOSURE, 2);
  r1340[1] = FUN_rbalance_564_0; r1340[2] = lenv;
  r1306[35] = r1340;
  O r1341 = allocate (TC_CLOSURE, 2);
  r1341[1] = FUN_tree_2finsert_565_0; r1341[2] = lenv;
  r1306[36] = r1341;
  O r1342 = allocate (TC_CLOSURE, 2);
  r1342[1] = FUN_tree_2fdelete_567_0; r1342[2] = lenv;
  r1306[37] = r1342;
  O r1343 = allocate (TC_CLOSURE, 2);
  r1343[1] = FUN_tree_2fmember_570_0; r1343[2] = lenv;
  r1306[38] = r1343;
  O r1344 = allocate (TC_CLOSURE, 2);
  r1344[1] = FUN_tree_2fmin_572_0; r1344[2] = lenv;
  r1306[39] = r1344;
  O r1345 = (object *) 33;
  r1306[40] = r1345;
  O r1346 = (object *) 131071;
  r1306[41] = r1346;
  O r1347 = (object *) 18;
  r1306[42] = r1347;
  O r1348 = irk_copy_tuple ((O) constructed_153[0]);
  r1306[43] = r1348;
  O r1349 = irk_copy_tuple ((O) constructed_142[0]);
  r1306[44] = r1349;
  O r1350 = allocate (TC_CLOSURE, 2);
  r1350[1] = FUN_print_exception_exit_592_0; r1350[2] = lenv;
  r1306[45] = r1350;
  O r1351 = allocate (TC_CLOSURE, 2);
  r1351[1] = FUN_hexdigit_649_0; r1351[2] = lenv;
  r1306[46] = r1351;
  O r1352 = allocate (TC_CLOSURE, 2);
  r1352[1] = FUN_hexdig__3eint_652_0; r1352[2] = lenv;
  r1306[47] = r1352;
  O r1353 = top[5];
  r1306[48] = r1353;
  // env 16:
  //   (list |tuple=(product string string) |)
  O r1569 = allocate (TC_ENV, 2);
  r1569[1] = lenv; lenv = r1569;
  O r1571 = allocate (UOTAG(3), 2);
  O r1817 = (O) &constructed_17;
  r1571[1] = r1817;
  O r1818 = (O) &constructed_17;
  r1571[2] = r1818;
  O r1573 = allocate (UOTAG(3), 2);
  O r1815 = (O) &constructed_108;
  r1573[1] = r1815;
  O r1816 = (O) &constructed_17;
  r1573[2] = r1816;
  O r1575 = allocate (UOTAG(3), 2);
  O r1813 = (O) &constructed_106;
  r1575[1] = r1813;
  O r1814 = (O) &constructed_107;
  r1575[2] = r1814;
  O r1577 = allocate (UOTAG(3), 2);
  O r1811 = (O) &constructed_106;
  r1577[1] = r1811;
  O r1812 = (O) &constructed_105;
  r1577[2] = r1812;
  O r1579 = allocate (UOTAG(3), 2);
  O r1809 = (O) &constructed_103;
  r1579[1] = r1809;
  O r1810 = (O) &constructed_104;
  r1579[2] = r1810;
  O r1581 = allocate (UOTAG(3), 2);
  O r1807 = (O) &constructed_103;
  r1581[1] = r1807;
  O r1808 = (O) &constructed_102;
  r1581[2] = r1808;
  O r1583 = allocate (UOTAG(3), 2);
  O r1805 = (O) &constructed_100;
  r1583[1] = r1805;
  O r1806 = (O) &constructed_101;
  r1583[2] = r1806;
  O r1585 = allocate (UOTAG(3), 2);
  O r1803 = (O) &constructed_100;
  r1585[1] = r1803;
  O r1804 = (O) &constructed_99;
  r1585[2] = r1804;
  O r1587 = allocate (UOTAG(3), 2);
  O r1801 = (O) &constructed_92;
  r1587[1] = r1801;
  O r1802 = (O) &constructed_98;
  r1587[2] = r1802;
  O r1589 = allocate (UOTAG(3), 2);
  O r1799 = (O) &constructed_92;
  r1589[1] = r1799;
  O r1800 = (O) &constructed_97;
  r1589[2] = r1800;
  O r1591 = allocate (UOTAG(3), 2);
  O r1797 = (O) &constructed_92;
  r1591[1] = r1797;
  O r1798 = (O) &constructed_96;
  r1591[2] = r1798;
  O r1593 = allocate (UOTAG(3), 2);
  O r1795 = (O) &constructed_92;
  r1593[1] = r1795;
  O r1796 = (O) &constructed_95;
  r1593[2] = r1796;
  O r1595 = allocate (UOTAG(3), 2);
  O r1793 = (O) &constructed_92;
  r1595[1] = r1793;
  O r1794 = (O) &constructed_94;
  r1595[2] = r1794;
  O r1597 = allocate (UOTAG(3), 2);
  O r1791 = (O) &constructed_92;
  r1597[1] = r1791;
  O r1792 = (O) &constructed_93;
  r1597[2] = r1792;
  O r1599 = allocate (UOTAG(3), 2);
  O r1789 = (O) &constructed_92;
  r1599[1] = r1789;
  O r1790 = (O) &constructed_91;
  r1599[2] = r1790;
  O r1601 = allocate (UOTAG(3), 2);
  O r1787 = (O) &constructed_90;
  r1601[1] = r1787;
  O r1788 = (O) &constructed_17;
  r1601[2] = r1788;
  O r1603 = allocate (UOTAG(3), 2);
  O r1785 = (O) &constructed_89;
  r1603[1] = r1785;
  O r1786 = (O) &constructed_88;
  r1603[2] = r1786;
  O r1605 = allocate (UOTAG(3), 2);
  O r1783 = (O) &constructed_87;
  r1605[1] = r1783;
  O r1784 = (O) &constructed_17;
  r1605[2] = r1784;
  O r1607 = allocate (UOTAG(3), 2);
  O r1781 = (O) &constructed_86;
  r1607[1] = r1781;
  O r1782 = (O) &constructed_17;
  r1607[2] = r1782;
  O r1609 = allocate (UOTAG(3), 2);
  O r1779 = (O) &constructed_85;
  r1609[1] = r1779;
  O r1780 = (O) &constructed_17;
  r1609[2] = r1780;
  O r1611 = allocate (UOTAG(3), 2);
  O r1777 = (O) &constructed_84;
  r1611[1] = r1777;
  O r1778 = (O) &constructed_17;
  r1611[2] = r1778;
  O r1613 = allocate (UOTAG(3), 2);
  O r1775 = (O) &constructed_83;
  r1613[1] = r1775;
  O r1776 = (O) &constructed_17;
  r1613[2] = r1776;
  O r1615 = allocate (UOTAG(3), 2);
  O r1773 = (O) &constructed_82;
  r1615[1] = r1773;
  O r1774 = (O) &constructed_17;
  r1615[2] = r1774;
  O r1617 = allocate (UOTAG(3), 2);
  O r1771 = (O) &constructed_81;
  r1617[1] = r1771;
  O r1772 = (O) &constructed_17;
  r1617[2] = r1772;
  O r1619 = allocate (UOTAG(3), 2);
  O r1769 = (O) &constructed_80;
  r1619[1] = r1769;
  O r1770 = (O) &constructed_17;
  r1619[2] = r1770;
  O r1621 = allocate (UOTAG(3), 2);
  O r1767 = (O) &constructed_79;
  r1621[1] = r1767;
  O r1768 = (O) &constructed_17;
  r1621[2] = r1768;
  O r1623 = allocate (UOTAG(3), 2);
  O r1765 = (O) &constructed_78;
  r1623[1] = r1765;
  O r1766 = (O) &constructed_17;
  r1623[2] = r1766;
  O r1625 = allocate (UOTAG(3), 2);
  O r1763 = (O) &constructed_77;
  r1625[1] = r1763;
  O r1764 = (O) &constructed_17;
  r1625[2] = r1764;
  O r1627 = allocate (UOTAG(3), 2);
  O r1761 = (O) &constructed_76;
  r1627[1] = r1761;
  O r1762 = (O) &constructed_17;
  r1627[2] = r1762;
  O r1629 = allocate (UOTAG(3), 2);
  O r1759 = (O) &constructed_75;
  r1629[1] = r1759;
  O r1760 = (O) &constructed_17;
  r1629[2] = r1760;
  O r1631 = allocate (UOTAG(3), 2);
  O r1757 = (O) &constructed_74;
  r1631[1] = r1757;
  O r1758 = (O) &constructed_17;
  r1631[2] = r1758;
  O r1633 = allocate (UOTAG(3), 2);
  O r1755 = (O) &constructed_73;
  r1633[1] = r1755;
  O r1756 = (O) &constructed_17;
  r1633[2] = r1756;
  O r1635 = allocate (UOTAG(3), 2);
  O r1753 = (O) &constructed_72;
  r1635[1] = r1753;
  O r1754 = (O) &constructed_17;
  r1635[2] = r1754;
  O r1637 = allocate (UOTAG(3), 2);
  O r1751 = (O) &constructed_71;
  r1637[1] = r1751;
  O r1752 = (O) &constructed_17;
  r1637[2] = r1752;
  O r1639 = allocate (UOTAG(3), 2);
  O r1749 = (O) &constructed_70;
  r1639[1] = r1749;
  O r1750 = (O) &constructed_17;
  r1639[2] = r1750;
  O r1641 = allocate (UOTAG(3), 2);
  O r1747 = (O) &constructed_69;
  r1641[1] = r1747;
  O r1748 = (O) &constructed_17;
  r1641[2] = r1748;
  O r1643 = allocate (UOTAG(3), 2);
  O r1745 = (O) &constructed_68;
  r1643[1] = r1745;
  O r1746 = (O) &constructed_17;
  r1643[2] = r1746;
  O r1645 = allocate (UOTAG(3), 2);
  O r1743 = (O) &constructed_67;
  r1645[1] = r1743;
  O r1744 = (O) &constructed_17;
  r1645[2] = r1744;
  O r1647 = allocate (UOTAG(3), 2);
  O r1741 = (O) &constructed_66;
  r1647[1] = r1741;
  O r1742 = (O) &constructed_17;
  r1647[2] = r1742;
  O r1649 = allocate (UOTAG(3), 2);
  O r1739 = (O) &constructed_65;
  r1649[1] = r1739;
  O r1740 = (O) &constructed_17;
  r1649[2] = r1740;
  O r1651 = allocate (UOTAG(3), 2);
  O r1737 = (O) &constructed_64;
  r1651[1] = r1737;
  O r1738 = (O) &constructed_17;
  r1651[2] = r1738;
  O r1653 = allocate (UOTAG(3), 2);
  O r1735 = (O) &constructed_63;
  r1653[1] = r1735;
  O r1736 = (O) &constructed_17;
  r1653[2] = r1736;
  O r1655 = allocate (UOTAG(3), 2);
  O r1733 = (O) &constructed_62;
  r1655[1] = r1733;
  O r1734 = (O) &constructed_17;
  r1655[2] = r1734;
  O r1657 = allocate (UOTAG(3), 2);
  O r1731 = (O) &constructed_61;
  r1657[1] = r1731;
  O r1732 = (O) &constructed_17;
  r1657[2] = r1732;
  O r1659 = allocate (UOTAG(3), 2);
  O r1729 = (O) &constructed_60;
  r1659[1] = r1729;
  O r1730 = (O) &constructed_17;
  r1659[2] = r1730;
  O r1661 = allocate (UOTAG(3), 2);
  O r1727 = (O) &constructed_59;
  r1661[1] = r1727;
  O r1728 = (O) &constructed_17;
  r1661[2] = r1728;
  O r1663 = allocate (UOTAG(3), 2);
  O r1725 = (O) &constructed_58;
  r1663[1] = r1725;
  O r1726 = (O) &constructed_17;
  r1663[2] = r1726;
  O r1665 = allocate (UOTAG(3), 2);
  O r1723 = (O) &constructed_57;
  r1665[1] = r1723;
  O r1724 = (O) &constructed_17;
  r1665[2] = r1724;
  O r1667 = allocate (UOTAG(3), 2);
  O r1721 = (O) &constructed_56;
  r1667[1] = r1721;
  O r1722 = (O) &constructed_17;
  r1667[2] = r1722;
  O r1669 = allocate (UOTAG(3), 2);
  O r1719 = (O) &constructed_55;
  r1669[1] = r1719;
  O r1720 = (O) &constructed_17;
  r1669[2] = r1720;
  O r1671 = allocate (UOTAG(3), 2);
  O r1717 = (O) &constructed_54;
  r1671[1] = r1717;
  O r1718 = (O) &constructed_17;
  r1671[2] = r1718;
  O r1673 = allocate (UOTAG(3), 2);
  O r1715 = (O) &constructed_53;
  r1673[1] = r1715;
  O r1716 = (O) &constructed_17;
  r1673[2] = r1716;
  O r1675 = allocate (UOTAG(3), 2);
  O r1713 = (O) &constructed_52;
  r1675[1] = r1713;
  O r1714 = (O) &constructed_17;
  r1675[2] = r1714;
  O r1677 = allocate (UOTAG(3), 2);
  O r1711 = (O) &constructed_51;
  r1677[1] = r1711;
  O r1712 = (O) &constructed_17;
  r1677[2] = r1712;
  O r1679 = allocate (UOTAG(3), 2);
  O r1709 = (O) &constructed_50;
  r1679[1] = r1709;
  O r1710 = (O) &constructed_17;
  r1679[2] = r1710;
  O r1681 = allocate (UOTAG(3), 2);
  O r1707 = (O) &constructed_49;
  r1681[1] = r1707;
  O r1708 = (O) &constructed_17;
  r1681[2] = r1708;
  O r1683 = allocate (UOTAG(3), 2);
  O r1705 = (O) &constructed_48;
  r1683[1] = r1705;
  O r1706 = (O) &constructed_17;
  r1683[2] = r1706;
  O r1685 = allocate (UOTAG(3), 2);
  O r1703 = (O) &constructed_47;
  r1685[1] = r1703;
  O r1704 = (O) &constructed_17;
  r1685[2] = r1704;
  O r1687 = allocate (UOTAG(3), 2);
  O r1701 = (O) &constructed_46;
  r1687[1] = r1701;
  O r1702 = (O) &constructed_17;
  r1687[2] = r1702;
  O r1689 = allocate (UOTAG(3), 2);
  O r1699 = (O) &constructed_45;
  r1689[1] = r1699;
  O r1700 = (O) &constructed_17;
  r1689[2] = r1700;
  O r1691 = allocate (UOTAG(3), 2);
  O r1697 = (O) &constructed_44;
  r1691[1] = r1697;
  O r1698 = (O) &constructed_17;
  r1691[2] = r1698;
  O r1693 = allocate (UOTAG(3), 2);
  O r1695 = (O) &constructed_43;
  r1693[1] = r1695;
  O r1696 = (O) &constructed_17;
  r1693[2] = r1696;
  O r1694 = (object *) 10;
  O r1692 = alloc_no_clear (TC_PAIR,2);
  r1692[1] = r1693;
  r1692[2] = r1694;
  O r1690 = alloc_no_clear (TC_PAIR,2);
  r1690[1] = r1691;
  r1690[2] = r1692;
  O r1688 = alloc_no_clear (TC_PAIR,2);
  r1688[1] = r1689;
  r1688[2] = r1690;
  O r1686 = alloc_no_clear (TC_PAIR,2);
  r1686[1] = r1687;
  r1686[2] = r1688;
  O r1684 = alloc_no_clear (TC_PAIR,2);
  r1684[1] = r1685;
  r1684[2] = r1686;
  O r1682 = alloc_no_clear (TC_PAIR,2);
  r1682[1] = r1683;
  r1682[2] = r1684;
  O r1680 = alloc_no_clear (TC_PAIR,2);
  r1680[1] = r1681;
  r1680[2] = r1682;
  O r1678 = alloc_no_clear (TC_PAIR,2);
  r1678[1] = r1679;
  r1678[2] = r1680;
  O r1676 = alloc_no_clear (TC_PAIR,2);
  r1676[1] = r1677;
  r1676[2] = r1678;
  O r1674 = alloc_no_clear (TC_PAIR,2);
  r1674[1] = r1675;
  r1674[2] = r1676;
  O r1672 = alloc_no_clear (TC_PAIR,2);
  r1672[1] = r1673;
  r1672[2] = r1674;
  O r1670 = alloc_no_clear (TC_PAIR,2);
  r1670[1] = r1671;
  r1670[2] = r1672;
  O r1668 = alloc_no_clear (TC_PAIR,2);
  r1668[1] = r1669;
  r1668[2] = r1670;
  O r1666 = alloc_no_clear (TC_PAIR,2);
  r1666[1] = r1667;
  r1666[2] = r1668;
  O r1664 = alloc_no_clear (TC_PAIR,2);
  r1664[1] = r1665;
  r1664[2] = r1666;
  O r1662 = alloc_no_clear (TC_PAIR,2);
  r1662[1] = r1663;
  r1662[2] = r1664;
  O r1660 = alloc_no_clear (TC_PAIR,2);
  r1660[1] = r1661;
  r1660[2] = r1662;
  O r1658 = alloc_no_clear (TC_PAIR,2);
  r1658[1] = r1659;
  r1658[2] = r1660;
  O r1656 = alloc_no_clear (TC_PAIR,2);
  r1656[1] = r1657;
  r1656[2] = r1658;
  O r1654 = alloc_no_clear (TC_PAIR,2);
  r1654[1] = r1655;
  r1654[2] = r1656;
  O r1652 = alloc_no_clear (TC_PAIR,2);
  r1652[1] = r1653;
  r1652[2] = r1654;
  O r1650 = alloc_no_clear (TC_PAIR,2);
  r1650[1] = r1651;
  r1650[2] = r1652;
  O r1648 = alloc_no_clear (TC_PAIR,2);
  r1648[1] = r1649;
  r1648[2] = r1650;
  O r1646 = alloc_no_clear (TC_PAIR,2);
  r1646[1] = r1647;
  r1646[2] = r1648;
  O r1644 = alloc_no_clear (TC_PAIR,2);
  r1644[1] = r1645;
  r1644[2] = r1646;
  O r1642 = alloc_no_clear (TC_PAIR,2);
  r1642[1] = r1643;
  r1642[2] = r1644;
  O r1640 = alloc_no_clear (TC_PAIR,2);
  r1640[1] = r1641;
  r1640[2] = r1642;
  O r1638 = alloc_no_clear (TC_PAIR,2);
  r1638[1] = r1639;
  r1638[2] = r1640;
  O r1636 = alloc_no_clear (TC_PAIR,2);
  r1636[1] = r1637;
  r1636[2] = r1638;
  O r1634 = alloc_no_clear (TC_PAIR,2);
  r1634[1] = r1635;
  r1634[2] = r1636;
  O r1632 = alloc_no_clear (TC_PAIR,2);
  r1632[1] = r1633;
  r1632[2] = r1634;
  O r1630 = alloc_no_clear (TC_PAIR,2);
  r1630[1] = r1631;
  r1630[2] = r1632;
  O r1628 = alloc_no_clear (TC_PAIR,2);
  r1628[1] = r1629;
  r1628[2] = r1630;
  O r1626 = alloc_no_clear (TC_PAIR,2);
  r1626[1] = r1627;
  r1626[2] = r1628;
  O r1624 = alloc_no_clear (TC_PAIR,2);
  r1624[1] = r1625;
  r1624[2] = r1626;
  O r1622 = alloc_no_clear (TC_PAIR,2);
  r1622[1] = r1623;
  r1622[2] = r1624;
  O r1620 = alloc_no_clear (TC_PAIR,2);
  r1620[1] = r1621;
  r1620[2] = r1622;
  O r1618 = alloc_no_clear (TC_PAIR,2);
  r1618[1] = r1619;
  r1618[2] = r1620;
  O r1616 = alloc_no_clear (TC_PAIR,2);
  r1616[1] = r1617;
  r1616[2] = r1618;
  O r1614 = alloc_no_clear (TC_PAIR,2);
  r1614[1] = r1615;
  r1614[2] = r1616;
  O r1612 = alloc_no_clear (TC_PAIR,2);
  r1612[1] = r1613;
  r1612[2] = r1614;
  O r1610 = alloc_no_clear (TC_PAIR,2);
  r1610[1] = r1611;
  r1610[2] = r1612;
  O r1608 = alloc_no_clear (TC_PAIR,2);
  r1608[1] = r1609;
  r1608[2] = r1610;
  O r1606 = alloc_no_clear (TC_PAIR,2);
  r1606[1] = r1607;
  r1606[2] = r1608;
  O r1604 = alloc_no_clear (TC_PAIR,2);
  r1604[1] = r1605;
  r1604[2] = r1606;
  O r1602 = alloc_no_clear (TC_PAIR,2);
  r1602[1] = r1603;
  r1602[2] = r1604;
  O r1600 = alloc_no_clear (TC_PAIR,2);
  r1600[1] = r1601;
  r1600[2] = r1602;
  O r1598 = alloc_no_clear (TC_PAIR,2);
  r1598[1] = r1599;
  r1598[2] = r1600;
  O r1596 = alloc_no_clear (TC_PAIR,2);
  r1596[1] = r1597;
  r1596[2] = r1598;
  O r1594 = alloc_no_clear (TC_PAIR,2);
  r1594[1] = r1595;
  r1594[2] = r1596;
  O r1592 = alloc_no_clear (TC_PAIR,2);
  r1592[1] = r1593;
  r1592[2] = r1594;
  O r1590 = alloc_no_clear (TC_PAIR,2);
  r1590[1] = r1591;
  r1590[2] = r1592;
  O r1588 = alloc_no_clear (TC_PAIR,2);
  r1588[1] = r1589;
  r1588[2] = r1590;
  O r1586 = alloc_no_clear (TC_PAIR,2);
  r1586[1] = r1587;
  r1586[2] = r1588;
  O r1584 = alloc_no_clear (TC_PAIR,2);
  r1584[1] = r1585;
  r1584[2] = r1586;
  O r1582 = alloc_no_clear (TC_PAIR,2);
  r1582[1] = r1583;
  r1582[2] = r1584;
  O r1580 = alloc_no_clear (TC_PAIR,2);
  r1580[1] = r1581;
  r1580[2] = r1582;
  O r1578 = alloc_no_clear (TC_PAIR,2);
  r1578[1] = r1579;
  r1578[2] = r1580;
  O r1576 = alloc_no_clear (TC_PAIR,2);
  r1576[1] = r1577;
  r1576[2] = r1578;
  O r1574 = alloc_no_clear (TC_PAIR,2);
  r1574[1] = r1575;
  r1574[2] = r1576;
  O r1572 = alloc_no_clear (TC_PAIR,2);
  r1572[1] = r1573;
  r1572[2] = r1574;
  O r1570 = alloc_no_clear (TC_PAIR,2);
  r1570[1] = r1571;
  r1570[2] = r1572;
  r1569[2] = r1570;
  // env 17:
  //   ((vector t764) int (list t764) -> (vector t764))
  O r1545 = alloc_cleared (TC_ENV, 2);
  r1545[1] = lenv; lenv = r1545;
  O r1546 = allocate (TC_CLOSURE, 2);
  r1546[1] = FUN_recur_1164_i2_0; r1546[2] = lenv;
  r1545[2] = r1546;
  O r1523 = ((object**) lenv) [1][2];
  switch (get_case (r1523)) {
    case (TC_NIL): { // nil
      O r1524 = (object *) 18;
      O r1522 = r1524;
      JUMP_65(r1522, r1306);
    } break;
    default: {
      // env 18:
      //   |tuple=(product string string) | int int (vector |tuple=(product string string) |)
      O r1532 = alloc_cleared (TC_ENV, 5);
      r1532[1] = lenv; lenv = r1532;
      O r1544 = ((object***) lenv) [1][1][2];
      O r1533 = UOBJ_GET(r1544,0);
      r1532[2] = r1533;
      // env 19:
      //   (list |tuple=(product string string) |) int
      O r1540 = allocate (TC_ENV, 3);
      O r1542 = ((object***) lenv) [1][1][2];
      r1540[2] = r1542;
      O r1543 = (object *) 1;
      r1540[3] = r1543;
      O r1541 = top[10];
      O t = allocate (TC_SAVE, 5);
      t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_12; t[4] = r1306; t[5] = r1532; k = t;
      r1540[1] = r1541[2]; lenv = r1540; FUN_length_splat_272_0();
    } break;
  }
}
static void FUN_toplevel_12 (O rr) {
  O r1306 = k[4]; O r1532 = k[5]; lenv = k[2]; k = k[1];
  O r1534 = rr;
  r1532[3] = r1534;
  O r1535 = ((object*) lenv) [3];
  r1532[4] = r1535;
  O r1539 = ((object*) lenv) [4];
  if (freep + unbox(r1539) >= limit) {
    heap_request = unbox(r1539);
    heap1[3] = r1532;
    heap1[4] = r1306;
    gc_flip (2);
    r1532 = heap0[3];
    r1306 = heap0[4];
  }
  O r1537 = ((object*) lenv) [4];
  O r1538 = ((object*) lenv) [2];
  O r1536;
  if (unbox(r1537) == 0) { r1536 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r1537));
    for (int i=0; i<unbox(r1537); i++) { t[i+1] = r1538; }
    r1536 = t;
  }
  r1532[5] = r1536;
  // env 20:
  //   (vector |tuple=(product string string) |) int (list |tuple=(product string string) |)
  O r1527 = allocate (TC_ENV, 4);
  O r1529 = ((object*) lenv) [5];
  r1527[2] = r1529;
  O r1530 = (object *) 1;
  r1527[3] = r1530;
  O r1531 = ((object***) lenv) [1][1][2];
  r1527[4] = r1531;
  O r1528 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_13; t[4] = r1306; k = t;
  r1527[1] = r1528[2]; lenv = r1527; FUN_recur_1164_i2_0();
}
static void FUN_toplevel_13 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1526 = rr;
  lenv = lenv[1];
  O r1525 = r1526;
  O r1522 = r1525;
  JUMP_65(r1522, r1306);
}
static void JUMP_65(O r1522, O r1306) {
  lenv = lenv[1];
  O r1521 = r1522;
  lenv = lenv[1];
  O r1354 = r1521;
  r1306[49] = r1354;
  O r1355 = (object *) 125;
  r1306[50] = r1355;
  // env 21:
  //   (tree int |tuple=(product string string) |)
  O r1519 = allocate (TC_ENV, 2);
  r1519[1] = lenv; lenv = r1519;
  O r1520 = (object *) 131094;
  r1519[2] = r1520;
  // env 22:
  //   (int int -> undefined)
  O r1495 = alloc_cleared (TC_ENV, 2);
  r1495[1] = lenv; lenv = r1495;
  O r1496 = allocate (TC_CLOSURE, 2);
  r1496[1] = FUN__24loop126_3522_0; r1496[2] = lenv;
  r1495[2] = r1496;
  // env 23:
  //   int int
  O r1491 = allocate (TC_ENV, 3);
  O r1493 = (object *) 1;
  r1491[2] = r1493;
  O r1494 = top[50];
  r1491[3] = r1494;
  O r1492 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_14; t[4] = r1306; k = t;
  r1491[1] = r1492[2]; lenv = r1491; FUN__24loop126_3522_0();
}
static void FUN_toplevel_14 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1490 = rr;
  lenv = lenv[1];
  O r1489 = ((object*) lenv) [2];
  lenv = lenv[1];
  O r1356 = r1489;
  r1306[51] = r1356;
  // env 24:
  //   (list string)
  O r1486 = allocate (TC_ENV, 2);
  O r1488 = (object*) constructed_42[0];
  r1486[2] = r1488;
  O r1487 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_15; t[4] = r1306; k = t;
  r1486[1] = r1487[2]; lenv = r1486; FUN_string_concat_338_0();
}
static void FUN_toplevel_15 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1357 = rr;
  r1306[52] = r1357;
  // env 25:
  //   string int
  O r1483 = allocate (TC_ENV, 3);
  r1483[1] = lenv; lenv = r1483;
  O r1484 = top[52];
  r1483[2] = r1484;
  O r1485 = (object *) 1;
  r1483[3] = r1485;
  // env 26:
  //   ( -> hufftree)
  O r1405 = alloc_cleared (TC_ENV, 2);
  r1405[1] = lenv; lenv = r1405;
  O r1406 = allocate (TC_CLOSURE, 2);
  r1406[1] = FUN_recur_3281_i376_0; r1406[2] = lenv;
  r1405[2] = r1406;
  O r1404 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_16; t[4] = r1306; k = t;
  lenv = r1404[2]; FUN_recur_3281_i376_0();
}
static void FUN_toplevel_16 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1403 = rr;
  lenv = lenv[1];
  O r1402 = r1403;
  lenv = lenv[1];
  O r1358 = r1402;
  r1306[53] = r1358;
  // env 27:
  //   int (vector int)
  O r1396 = alloc_cleared (TC_ENV, 3);
  r1396[1] = lenv; lenv = r1396;
  O r1397 = (object *) 19;
  r1396[2] = r1397;
  O r1401 = ((object*) lenv) [2];
  if (freep + unbox(r1401) >= limit) {
    heap_request = unbox(r1401);
    heap1[3] = r1396;
    heap1[4] = r1306;
    gc_flip (2);
    r1396 = heap0[3];
    r1306 = heap0[4];
  }
  O r1399 = ((object*) lenv) [2];
  O r1400 = (object *) 1;
  O r1398;
  if (unbox(r1399) == 0) { r1398 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r1399));
    for (int i=0; i<unbox(r1399); i++) { t[i+1] = r1400; }
    r1398 = t;
  }
  r1396[3] = r1398;
  // env 28:
  //   (int int -> undefined)
  O r1367 = alloc_cleared (TC_ENV, 2);
  r1367[1] = lenv; lenv = r1367;
  O r1368 = allocate (TC_CLOSURE, 2);
  r1368[1] = FUN__24loop128_3534_0; r1368[2] = lenv;
  r1367[2] = r1368;
  // env 29:
  //   int int
  O r1363 = allocate (TC_ENV, 3);
  O r1365 = (object *) 1;
  r1363[2] = r1365;
  O r1366 = (object *) 17;
  r1363[3] = r1366;
  O r1364 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_17; t[4] = r1306; k = t;
  r1363[1] = r1364[2]; lenv = r1363; FUN__24loop128_3534_0();
}
static void FUN_toplevel_17 (O rr) {
  O r1306 = k[4]; lenv = k[2]; k = k[1];
  O r1362 = rr;
  lenv = lenv[1];
  O r1361 = ((object*) lenv) [3];
  lenv = lenv[1];
  O r1359 = r1361;
  r1306[54] = r1359;
  O r1360 = (object*) constructed_28[0];
  r1306[55] = r1360;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  O r1305 = top[45];
  top[3] = r1305;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  O r1304 = top[49];
  O r1300 = TAG_INTEGER((irk_int)(r1304 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*r1304));
  O r1302 = top[50];
  O r1303 = r1300; // reg varref
  O r1301 = IRK_TEST(UNTAG_INTEGER(r1302)==UNTAG_INTEGER(r1303));
  O r1294 = r1301; // reg varref
  if IRK_IS_TRUE(r1294) {
    O r1295 = (object *) 14;
    JUMP_52();
  } else {
    // env 30:
    //   |AssertionError=product ...t2299|
    O r1297 = alloc_cleared (TC_ENV, 2);
    O r1299 = (object*)UITAG(8);
    r1297[2] = r1299;
    O r1298 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_18; ; k = t;
    r1297[1] = r1298[2]; lenv = r1297; ((pfun)(r1298[1]))();
  }
}
static void FUN_toplevel_18 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1296 = rr;
  JUMP_52();
}
static void JUMP_52() {
  // env 31:
  //   string int int int int (tree int |tuple=(product string string) |) (tree int |tuple=(product string string) |) int int {get=(int -> |tuple=(product string string) |) set=(string string -> undefined) set-size=(int -> undefined) get-index=(string t2690 -> |tuple=(product (maybe int) bool) ...t2689|) nvals=(string -> int) } (string -> (list |tuple=(product string string) |))
  O r177 = alloc_cleared (TC_ENV, 12);
  r177[1] = lenv; lenv = r177;
  O r178 = (O) &constructed_17;
  r177[2] = r178;
  O r179 = (object *) -1;
  r177[3] = r179;
  O r180 = (object *) 1;
  r177[4] = r180;
  O r181 = (object *) 1;
  r177[5] = r181;
  O r182 = (object *) 8193;
  r177[6] = r182;
  O r183 = (object *) 131094;
  r177[7] = r183;
  O r184 = (object *) 131094;
  r177[8] = r184;
  O r185 = (object *) 1;
  r177[9] = r185;
  O r186 = (object *) 1;
  r177[10] = r186;
  // env 32:
  //   ( -> undefined) (string string -> undefined) (int -> |tuple=(product string string) |) (int -> undefined) ((tree |tuple=(product t2427 string) ...t2434| t2442) t2427 -> (list |tuple=(product |tuple=(product t2427 string) ...t2434| t2442) ...t2449|)) ((tree |tuple=(product t2505 string) ...t2508| t2507) t2505 -> (maybe t2507)) (string -> int) ((maybe int) -> (maybe int)) (string t2540 -> |tuple=(product (maybe int) bool) ...t2595|)
  O r732 = alloc_cleared (TC_ENV, 10);
  r732[1] = lenv; lenv = r732;
  O r733 = allocate (TC_CLOSURE, 2);
  r733[1] = FUN_evict_one_3328_i2_0; r733[2] = lenv;
  r732[2] = r733;
  O r734 = allocate (TC_CLOSURE, 2);
  r734[1] = FUN_set_3330_i2_0; r734[2] = lenv;
  r732[3] = r734;
  O r735 = allocate (TC_CLOSURE, 2);
  r735[1] = FUN_get_3331_i2_0; r735[2] = lenv;
  r732[4] = r735;
  O r736 = allocate (TC_CLOSURE, 2);
  r736[1] = FUN_set_size_3332_i2_0; r736[2] = lenv;
  r732[5] = r736;
  O r737 = allocate (TC_CLOSURE, 2);
  r737[1] = FUN_name_range_3335_i2_0; r737[2] = lenv;
  r732[6] = r737;
  O r738 = allocate (TC_CLOSURE, 2);
  r738[1] = FUN_lookup_name_only_3336_i2_0; r738[2] = lenv;
  r732[7] = r738;
  O r739 = allocate (TC_CLOSURE, 2);
  r739[1] = FUN_nvals_3337_i2_0; r739[2] = lenv;
  r732[8] = r739;
  O r740 = allocate (TC_CLOSURE, 2);
  r740[1] = FUN_adjust_3340_i2_0; r740[2] = lenv;
  r732[9] = r740;
  O r741 = allocate (TC_CLOSURE, 2);
  r741[1] = FUN_get_index_3341_i2_0; r741[2] = lenv;
  r732[10] = r741;
  O r726 = allocate (UOTAG(1), 5);
  O r727 = ((object*) lenv) [4];
  r726[1] = r727;
  O r728 = ((object*) lenv) [10];
  r726[2] = r728;
  O r729 = ((object*) lenv) [8];
  r726[3] = r729;
  O r730 = ((object*) lenv) [3];
  r726[4] = r730;
  O r731 = ((object*) lenv) [5];
  r726[5] = r731;
  lenv = lenv[1];
  O r187 = r726;
  r177[11] = r187;
  // env 33:
  //   (int -> undefined) (int -> int) ( -> string) (int -> |tuple=(product string string) ...t2756|) (string -> (list |tuple=(product string string) |))
  O r190 = alloc_cleared (TC_ENV, 6);
  r190[1] = lenv; lenv = r190;
  O r191 = allocate (TC_CLOSURE, 2);
  r191[1] = FUN_forward_3405_i304_i386_0; r191[2] = lenv;
  r190[2] = r191;
  O r192 = allocate (TC_CLOSURE, 2);
  r192[1] = FUN_get_integer_3407_i304_i386_0; r192[2] = lenv;
  r190[3] = r192;
  O r193 = allocate (TC_CLOSURE, 2);
  r193[1] = FUN_get_literal_3409_i304_i386_0; r193[2] = lenv;
  r190[4] = r193;
  O r194 = allocate (TC_CLOSURE, 2);
  r194[1] = FUN_get_pair0_3408_i304_i386_0; r194[2] = lenv;
  r190[5] = r194;
  O r195 = allocate (TC_CLOSURE, 2);
  r195[1] = FUN_unpack_3411_i304_i386_0; r195[2] = lenv;
  r190[6] = r195;
  O r189 = ((object*) lenv) [6];
  lenv = lenv[1];
  O r188 = r189;
  r177[12] = r188;
  // env 34:
  //   ((list string) -> undefined)
  O r11 = alloc_cleared (TC_ENV, 2);
  r11[1] = lenv; lenv = r11;
  O r12 = allocate (TC_CLOSURE, 2);
  r12[1] = FUN__24loop119_3498_i386_0; r12[2] = lenv;
  r11[2] = r12;
  // env 35:
  //   (list string)
  O r8 = allocate (TC_ENV, 2);
  O r10 = top[55];
  r8[2] = r10;
  O r9 = ((object*) lenv) [2];
  r8[1] = r9[2]; lenv = r8; FUN__24loop119_3498_i386_0();
}
static void FUN__24loop119_3498_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r15 = ((object*) lenv) [2];
  switch (get_case (r15)) {
    case (TC_NIL): { // nil
      O r17 = (object *) 14;
      IRK_RETURN(17);
    } break;
    default: {
      // env 36:
      //   string (list string)
      O r172 = alloc_cleared (TC_ENV, 3);
      r172[1] = lenv; lenv = r172;
      O r176 = ((object**) lenv) [1][2];
      O r173 = UOBJ_GET(r176,0);
      r172[2] = r173;
      O r175 = ((object**) lenv) [1][2];
      O r174 = UOBJ_GET(r175,1);
      r172[3] = r174;
      O r167 = (O) &constructed_7;
      O r169 = r167; // reg varref
      O r171 = r167; // reg varref
      O r170 = TAG_INTEGER((irk_int)((irk_string*)(r171))->len);
      TAG_INTEGER((irk_int)fwrite (((irk_string*)(r169))->data, 1, UNTAG_INTEGER(r170), stdout));
      O r168 = (object *) 14;
      // env 37:
      //   (list |tuple=(product string string) |)
      O r68 = alloc_cleared (TC_ENV, 2);
      r68[1] = lenv; lenv = r68;
      // env 38:
      //   string
      O r70 = alloc_cleared (TC_ENV, 2);
      // env 39:
      //   string |tuple=(product int int) |
      O r159 = alloc_cleared (TC_ENV, 3);
      r159[1] = lenv; lenv = r159;
      O r160 = ((object***) lenv) [1][1][2];
      r159[2] = r160;
      // env 40:
      //   int int
      O r162 = allocate (TC_ENV, 3);
      O r166 = ((object*) lenv) [2];
      O r164 = TAG_INTEGER((irk_int)((irk_string*)(r166))->len);
      r162[2] = r164;
      O r165 = (object *) 5;
      r162[3] = r165;
      O r163 = top[48];
      O t = allocate (TC_SAVE, 6);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop119_3498_i386_19; t[4] = r68; t[5] = r70; t[6] = r159; k = t;
      r162[1] = r163[2]; lenv = r162; ((pfun)(r163[1]))();
    } break;
  }
}
static void FUN__24loop119_3498_i386_19 (O rr) {
  O r68 = k[4]; O r70 = k[5]; O r159 = k[6]; lenv = k[2]; k = k[1];
  O r161 = rr;
  r159[3] = r161;
  // env 41:
  //   int int
  O r154 = alloc_cleared (TC_ENV, 3);
  r154[1] = lenv; lenv = r154;
  O r158 = ((object**) lenv) [1][3];
  O r155 = UOBJ_GET(r158,0);
  r154[2] = r155;
  O r157 = ((object**) lenv) [1][3];
  O r156 = UOBJ_GET(r157,1);
  r154[3] = r156;
  O r153 = ((object*) lenv) [3];
  O r149 = IRK_TEST(UNTAG_INTEGER(r153)==0);
  O r151 = r149; // reg varref
  O r152 = (object *) 6;
  O r150 = IRK_TEST(r151==r152);
  O r75 = r150; // reg varref
  if IRK_IS_TRUE(r75) {
    // env 42:
    //   |Hex/BadInputString=(product string string) ...t2879|
    O r77 = alloc_cleared (TC_ENV, 2);
    O r79 = allocate (UOTAG(11), 2);
    O r80 = (O) &constructed_6;
    r79[1] = r80;
    O r81 = ((object**) lenv) [1][2];
    r79[2] = r81;
    r77[2] = r79;
    O r78 = top[3];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop119_3498_i386_20; t[4] = r68; t[5] = r70; k = t;
    r77[1] = r78[2]; lenv = r77; ((pfun)(r78[1]))();
  } else {
    // env 43:
    //   int string
    O r143 = alloc_cleared (TC_ENV, 3);
    r143[1] = lenv; lenv = r143;
    O r144 = ((object**) lenv) [1][2];
    r143[2] = r144;
    O r148 = ((object*) lenv) [2];
    O r147 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r148)));
    if (freep + unbox(r147) >= limit) {
      heap_request = unbox(r147);
      heap1[3] = r143;
      heap1[4] = r70;
      heap1[5] = r68;
      gc_flip (3);
      r143 = heap0[3];
      r70 = heap0[4];
      r68 = heap0[5];
    }
    O r146 = ((object*) lenv) [2];
    O r145 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r146))), ((irk_string*)(t))->len = UNTAG_INTEGER(r146), t);
    r143[3] = r145;
    // env 44:
    //   (int int -> undefined)
    O r89 = alloc_cleared (TC_ENV, 2);
    r89[1] = lenv; lenv = r89;
    O r90 = allocate (TC_CLOSURE, 2);
    r90[1] = FUN__24loop93_3269_i306_i386_0; r90[2] = lenv;
    r89[2] = r90;
    // env 45:
    //   int int
    O r85 = allocate (TC_ENV, 3);
    O r87 = (object *) 1;
    r85[2] = r87;
    O r88 = ((object***) lenv) [1][1][2];
    r85[3] = r88;
    O r86 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop119_3498_i386_21; t[4] = r68; t[5] = r70; k = t;
    r85[1] = r86[2]; lenv = r85; FUN__24loop93_3269_i306_i386_0();
  }
}
static void FUN__24loop119_3498_i386_21 (O rr) {
  O r68 = k[4]; O r70 = k[5]; lenv = k[2]; k = k[1];
  O r84 = rr;
  lenv = lenv[1];
  O r83 = ((object*) lenv) [3];
  lenv = lenv[1];
  O r82 = r83;
  O r74 = r82;
  JUMP_4(r74, r70, r68);
}
static void FUN__24loop93_3269_i306_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r93 = ((object*) lenv) [2];
  O r94 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r93)==UNTAG_INTEGER(r94))) {
    O r96 = (object *) 14;
    IRK_RETURN(96);
  } else {
    // env 46:
    //   int int
    O r118 = alloc_cleared (TC_ENV, 3);
    r118[1] = lenv; lenv = r118;
    // env 47:
    //   char
    O r134 = alloc_cleared (TC_ENV, 2);
    // env 48:
    //   string int
    O r137 = allocate (TC_ENV, 3);
    O r139 = ((object******) lenv) [1][1][1][1][1][2];
    r137[2] = r139;
    O r141 = (object *) 5;
    O r142 = ((object**) lenv) [1][2];
    O r140 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r141)*UNTAG_INTEGER(r142));
    r137[3] = r140;
    O r138 = top[18];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_22; t[4] = r118; t[5] = r134; k = t;
    r137[1] = r138[2]; lenv = r137; FUN_string_ref_336_0();
  }
}
static void FUN__24loop93_3269_i306_i386_22 (O rr) {
  O r118 = k[4]; O r134 = k[5]; lenv = k[2]; k = k[1];
  O r136 = rr;
  r134[2] = r136;
  O r135 = top[46];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_23; t[4] = r118; k = t;
  r134[1] = r135[2]; lenv = r134; FUN_hexdigit_649_0();
}
static void FUN__24loop93_3269_i306_i386_23 (O rr) {
  O r118 = k[4]; lenv = k[2]; k = k[1];
  O r119 = rr;
  r118[2] = r119;
  // env 49:
  //   char
  O r121 = alloc_cleared (TC_ENV, 2);
  // env 50:
  //   string int
  O r124 = alloc_cleared (TC_ENV, 3);
  O r126 = ((object******) lenv) [1][1][1][1][1][2];
  r124[2] = r126;
  O r132 = (object *) 5;
  O r133 = ((object**) lenv) [1][2];
  O r128 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r132)*UNTAG_INTEGER(r133));
  O r130 = (object *) 3;
  O r131 = r128; // reg varref
  O r129 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r130)+UNTAG_INTEGER(r131));
  O r127 = r129; // reg varref
  r124[3] = r127;
  O r125 = top[18];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_24; t[4] = r118; t[5] = r121; k = t;
  r124[1] = r125[2]; lenv = r124; FUN_string_ref_336_0();
}
static void FUN__24loop93_3269_i306_i386_24 (O rr) {
  O r118 = k[4]; O r121 = k[5]; lenv = k[2]; k = k[1];
  O r123 = rr;
  r121[2] = r123;
  O r122 = top[46];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_25; t[4] = r118; k = t;
  r121[1] = r122[2]; lenv = r121; FUN_hexdigit_649_0();
}
static void FUN__24loop93_3269_i306_i386_25 (O rr) {
  O r118 = k[4]; lenv = k[2]; k = k[1];
  O r120 = rr;
  r118[3] = r120;
  // env 51:
  //   string int char
  O r105 = alloc_cleared (TC_ENV, 4);
  O r107 = ((object****) lenv) [1][1][1][3];
  r105[2] = r107;
  O r108 = ((object**) lenv) [1][2];
  r105[3] = r108;
  O r116 = ((object*) lenv) [2];
  O r117 = (object *) 9;
  O r110 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r116)<<UNTAG_INTEGER(r117));
  O r114 = r110; // reg varref
  O r115 = ((object*) lenv) [3];
  O r111 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r114)|UNTAG_INTEGER(r115));
  O r113 = r111; // reg varref
  O r112 = TO_CHAR(UNTAG_INTEGER(r113));
  O r109 = r112; // reg varref
  r105[4] = r109;
  O r106 = top[16];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_26; ; k = t;
  r105[1] = r106[2]; lenv = r105; FUN_string_set_bang_337_0();
}
static void FUN__24loop93_3269_i306_i386_26 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r104 = rr;
  lenv = lenv[1];
  O r102 = ((object*) lenv) [2];
  O r103 = (object *) 3;
  O r100 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r102)+UNTAG_INTEGER(r103));
  O r101 = ((object*) lenv) [3];
  lenv[2] = r100;
  lenv[3] = r101;
  FUN__24loop93_3269_i306_i386_0();
}
static void FUN__24loop119_3498_i386_20 (O rr) {
  O r68 = k[4]; O r70 = k[5]; lenv = k[2]; k = k[1];
  O r76 = rr;
  O r74 = r76;
  JUMP_4(r74, r70, r68);
}
static void JUMP_4(O r74, O r70, O r68) {
  lenv = lenv[1];
  O r73 = r74;
  lenv = lenv[1];
  O r72 = r73;
  r70[2] = r72;
  O r71 = ((object*****) lenv) [1][1][1][1][12];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_27; t[4] = r68; k = t;
  r70[1] = r71[2]; lenv = r70; ((pfun)(r71[1]))();
}
static void FUN__24loop93_3269_i306_i386_27 (O rr) {
  O r68 = k[4]; lenv = k[2]; k = k[1];
  O r69 = rr;
  r68[2] = r69;
  // env 52:
  //   (|tuple=(product string string) | -> undefined)
  O r40 = alloc_cleared (TC_ENV, 2);
  r40[1] = lenv; lenv = r40;
  O r41 = allocate (TC_CLOSURE, 2);
  r41[1] = FUN_print_pair_3492_i307_i386_0; r41[2] = lenv;
  r40[2] = r41;
  O r35 = (O) &constructed_1;
  O r37 = r35; // reg varref
  O r39 = r35; // reg varref
  O r38 = TAG_INTEGER((irk_int)((irk_string*)(r39))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r37))->data, 1, UNTAG_INTEGER(r38), stdout));
  O r36 = (object *) 14;
  // env 53:
  //   (|tuple=(product string string) | -> undefined) (list |tuple=(product string string) |)
  O r31 = allocate (TC_ENV, 3);
  O r33 = ((object*) lenv) [2];
  r31[2] = r33;
  O r34 = ((object**) lenv) [1][2];
  r31[3] = r34;
  O r32 = top[13];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop93_3269_i306_i386_28; ; k = t;
  r31[1] = r32[2]; lenv = r31; FUN_for_each_297_0();
}
static void FUN__24loop93_3269_i306_i386_28 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r26 = (O) &constructed_0;
  O r28 = r26; // reg varref
  O r30 = r26; // reg varref
  O r29 = TAG_INTEGER((irk_int)((irk_string*)(r30))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r28))->data, 1, UNTAG_INTEGER(r29), stdout));
  O r27 = (object *) 14;
  O r25 = r27; // reg varref
  lenv = lenv[1];
  O r24 = r25;
  lenv = lenv[1];
  O r23 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r23;
  FUN__24loop119_3498_i386_0();
}
static void FUN_print_pair_3492_i307_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 54:
  //   string string string
  O r51 = alloc_cleared (TC_ENV, 4);
  r51[1] = lenv; lenv = r51;
  O r67 = ((object**) lenv) [1][2];
  O r52 = UOBJ_GET(r67,0);
  r51[2] = r52;
  O r66 = ((object**) lenv) [1][2];
  O r53 = UOBJ_GET(r66,1);
  r51[3] = r53;
  // env 55:
  //   (list string)
  O r55 = allocate (TC_ENV, 2);
  O r58 = (O) &constructed_5;
  O r60 = ((object*) lenv) [2];
  O r62 = (O) &constructed_4;
  O r64 = ((object*) lenv) [3];
  O r65 = (object*) constructed_3[0];
  O r63 = alloc_no_clear (TC_PAIR,2);
  r63[1] = r64;
  r63[2] = r65;
  O r61 = alloc_no_clear (TC_PAIR,2);
  r61[1] = r62;
  r61[2] = r63;
  O r59 = alloc_no_clear (TC_PAIR,2);
  r59[1] = r60;
  r59[2] = r61;
  O r57 = alloc_no_clear (TC_PAIR,2);
  r57[1] = r58;
  r57[2] = r59;
  r55[2] = r57;
  O r56 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_pair_3492_i307_i386_29; t[4] = r51; k = t;
  r55[1] = r56[2]; lenv = r55; FUN_string_concat_338_0();
}
static void FUN_print_pair_3492_i307_i386_29 (O rr) {
  O r51 = k[4]; lenv = k[2]; k = k[1];
  O r54 = rr;
  r51[4] = r54;
  O r48 = ((object*) lenv) [4];
  O r50 = ((object*) lenv) [4];
  O r49 = TAG_INTEGER((irk_int)((irk_string*)(r50))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r48))->data, 1, UNTAG_INTEGER(r49), stdout));
  O r47 = (object *) 14;
  IRK_RETURN(47);
}
static void FUN_unpack_3411_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 56:
  //   (list |tuple=(product string string) |)
  O r427 = allocate (TC_ENV, 2);
  r427[1] = lenv; lenv = r427;
  O r428 = (object *) 10;
  r427[2] = r428;
  O r426 = ((object**) lenv) [1][2];
  ((object****) lenv) [1][1][1][2] = r426;
  O r425 = (object *) -1;
  ((object****) lenv) [1][1][1][3] = r425;
  O r422 = ((object****) lenv) [1][1][1][2];
  O r424 = r422; // reg varref
  O r423 = TAG_INTEGER((irk_int)((irk_string*)(r424))->len);
  O r421 = r423; // reg varref
  ((object****) lenv) [1][1][1][5] = r421;
  // env 57:
  //   int
  O r418 = allocate (TC_ENV, 2);
  O r420 = (object *) 3;
  r418[2] = r420;
  O r419 = ((object***) lenv) [1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_unpack_3411_i304_i386_30; ; k = t;
  r418[1] = r419[2]; lenv = r418; FUN_forward_3405_i304_i386_0();
}
static void FUN_unpack_3411_i304_i386_30 (O rr) {
  ; lenv = k[2]; k = k[1];
  // env 58:
  //   ( -> undefined)
  O r210 = alloc_cleared (TC_ENV, 2);
  r210[1] = lenv; lenv = r210;
  O r211 = allocate (TC_CLOSURE, 2);
  r211[1] = FUN__24loop103_3434_i304_i386_0; r211[2] = lenv;
  r210[2] = r211;
  O r209 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_unpack_3411_i304_i386_31; ; k = t;
  lenv = r209[2]; FUN__24loop103_3434_i304_i386_0();
}
static void FUN_unpack_3411_i304_i386_31 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r208 = rr;
  lenv = lenv[1];
  O r201 = ((object*) lenv) [2];
  // env 59:
  //   (list |tuple=(product string string) |) (list |tuple=(product string string) |)
  O r204 = allocate (TC_ENV, 3);
  O r206 = r201; // reg varref
  r204[2] = r206;
  O r207 = (object *) 10;
  r204[3] = r207;
  O r205 = top[9];
  r204[1] = r205[2]; lenv = r204; FUN_reverse_onto_269_0();
}
static void FUN__24loop103_3434_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r413 = ((object*****) lenv) [1][1][1][1][3];
  O r414 = ((object*****) lenv) [1][1][1][1][5];
  O r416 = r413; // reg varref
  O r417 = r414; // reg varref
  O r415 = IRK_TEST(UNTAG_INTEGER(r416)<UNTAG_INTEGER(r417));
  O r214 = r415; // reg varref
  if IRK_IS_TRUE(r214) {
    O r404 = ((object*****) lenv) [1][1][1][1][4];
    O r409 = r404; // reg varref
    O r410 = (object *) 11;
    O r405 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r409)>>UNTAG_INTEGER(r410));
    O r407 = (object *) 3;
    O r408 = r405; // reg varref
    O r406 = IRK_TEST(UNTAG_INTEGER(r407)==UNTAG_INTEGER(r408));
    O r218 = r406; // reg varref
    if IRK_IS_TRUE(r218) {
      // env 60:
      //   int
      O r220 = alloc_cleared (TC_ENV, 2);
      // env 61:
      //   int
      O r224 = allocate (TC_ENV, 2);
      O r226 = (object *) 11;
      r224[2] = r226;
      O r225 = ((object****) lenv) [1][1][1][3];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_32; t[4] = r220; k = t;
      r224[1] = r225[2]; lenv = r224; FUN_get_integer_3407_i304_i386_0();
    } else {
      // env 62:
      //   |tuple=(product string string) |
      O r232 = alloc_cleared (TC_ENV, 2);
      r232[1] = lenv; lenv = r232;
      // env 63:
      //   string
      O r392 = alloc_cleared (TC_ENV, 2);
      r392[1] = lenv; lenv = r392;
      // env 64:
      //   (list string)
      O r394 = alloc_cleared (TC_ENV, 2);
      O r397 = (O) &constructed_15;
      // env 65:
      //   int
      O r401 = allocate (TC_ENV, 2);
      O r403 = ((object*******) lenv) [1][1][1][1][1][1][4];
      r401[2] = r403;
      O r402 = top[23];
      O t = allocate (TC_SAVE, 7);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_33; t[4] = r232; t[5] = r392; t[6] = r394; t[7] = r397; k = t;
      r401[1] = r402[2]; lenv = r401; FUN_int__3ehex_string_372_0();
    }
  } else {
    O r412 = (object *) 14;
    IRK_RETURN(412);
  }
}
static void FUN__24loop103_3434_i304_i386_33 (O rr) {
  O r232 = k[4]; O r392 = k[5]; O r394 = k[6]; O r397 = k[7]; lenv = k[2]; k = k[1];
  O r399 = rr;
  O r400 = (object*) constructed_3[0];
  O r398 = alloc_no_clear (TC_PAIR,2);
  r398[1] = r399;
  r398[2] = r400;
  O r396 = alloc_no_clear (TC_PAIR,2);
  r396[1] = r397;
  r396[2] = r398;
  r394[2] = r396;
  O r395 = top[19];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_34; t[4] = r232; t[5] = r392; k = t;
  r394[1] = r395[2]; lenv = r394; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_34 (O rr) {
  O r232 = k[4]; O r392 = k[5]; lenv = k[2]; k = k[1];
  O r393 = rr;
  r392[2] = r393;
  O r389 = ((object*) lenv) [2];
  O r391 = ((object*) lenv) [2];
  O r390 = TAG_INTEGER((irk_int)((irk_string*)(r391))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r389))->data, 1, UNTAG_INTEGER(r390), stdout));
  O r388 = (object *) 14;
  lenv = lenv[1];
  O r381 = ((object******) lenv) [1][1][1][1][1][4];
  O r386 = r381; // reg varref
  O r387 = (object *) 15;
  O r382 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r386)>>UNTAG_INTEGER(r387));
  O r384 = (object *) 3;
  O r385 = r382; // reg varref
  O r383 = IRK_TEST(UNTAG_INTEGER(r384)==UNTAG_INTEGER(r385));
  O r234 = r383; // reg varref
  if IRK_IS_TRUE(r234) {
    // env 66:
    //   int
    O r257 = alloc_cleared (TC_ENV, 2);
    r257[1] = lenv; lenv = r257;
    // env 67:
    //   int
    O r259 = allocate (TC_ENV, 2);
    O r261 = (object *) 15;
    r259[2] = r261;
    O r260 = ((object******) lenv) [1][1][1][1][1][3];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_35; t[4] = r232; t[5] = r257; k = t;
    r259[1] = r260[2]; lenv = r259; FUN_get_integer_3407_i304_i386_0();
  } else {
    O r374 = ((object******) lenv) [1][1][1][1][1][4];
    O r379 = r374; // reg varref
    O r380 = (object *) 13;
    O r375 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r379)>>UNTAG_INTEGER(r380));
    O r377 = (object *) 3;
    O r378 = r375; // reg varref
    O r376 = IRK_TEST(UNTAG_INTEGER(r377)==UNTAG_INTEGER(r378));
    O r263 = r376; // reg varref
    if IRK_IS_TRUE(r263) {
      // env 68:
      //   int |tuple=(product string string) |
      O r318 = alloc_cleared (TC_ENV, 3);
      r318[1] = lenv; lenv = r318;
      // env 69:
      //   int
      O r324 = allocate (TC_ENV, 2);
      O r326 = (object *) 13;
      r324[2] = r326;
      O r325 = ((object******) lenv) [1][1][1][1][1][3];
      O t = allocate (TC_SAVE, 5);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_36; t[4] = r232; t[5] = r318; k = t;
      r324[1] = r325[2]; lenv = r324; FUN_get_integer_3407_i304_i386_0();
    } else {
      O r367 = ((object******) lenv) [1][1][1][1][1][4];
      O r372 = r367; // reg varref
      O r373 = (object *) 9;
      O r368 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r372)>>UNTAG_INTEGER(r373));
      O r370 = r368; // reg varref
      O r371 = (object *) 5;
      O r369 = IRK_TEST(UNTAG_INTEGER(r370)<UNTAG_INTEGER(r371));
      O r328 = r369; // reg varref
      if IRK_IS_TRUE(r328) {
        // env 70:
        //   int int bool int
        O r350 = alloc_cleared (TC_ENV, 5);
        r350[1] = lenv; lenv = r350;
        O r351 = ((object*******) lenv) [1][1][1][1][1][1][4];
        r350[2] = r351;
        O r360 = ((object*) lenv) [2];
        O r361 = (object *) 9;
        O r352 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r360)>>UNTAG_INTEGER(r361));
        r350[3] = r352;
        O r358 = (object *) 3;
        O r359 = ((object*) lenv) [3];
        O r353 = IRK_TEST(UNTAG_INTEGER(r358)==UNTAG_INTEGER(r359));
        r350[4] = r353;
        // env 71:
        //   int
        O r355 = allocate (TC_ENV, 2);
        O r357 = (object *) 9;
        r355[2] = r357;
        O r356 = ((object******) lenv) [1][1][1][1][1][3];
        O t = allocate (TC_SAVE, 5);
        t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_37; t[4] = r232; t[5] = r350; k = t;
        r355[1] = r356[2]; lenv = r355; FUN_get_integer_3407_i304_i386_0();
      } else {
        // env 72:
        //   |Hpack/BadHeaderCode=int ...t2837|
        O r363 = alloc_cleared (TC_ENV, 2);
        O r365 = allocate (UOTAG(10), 1);
        O r366 = ((object******) lenv) [1][1][1][1][1][4];
        r365[1] = r366;
        r363[2] = r365;
        O r364 = top[3];
        O t = allocate (TC_SAVE, 4);
        t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_38; t[4] = r232; k = t;
        r363[1] = r364[2]; lenv = r363; ((pfun)(r364[1]))();
      }
    }
  }
}
static void FUN__24loop103_3434_i304_i386_38 (O rr) {
  O r232 = k[4]; lenv = k[2]; k = k[1];
  O r362 = rr;
  O r327 = r362;
  JUMP_12(r327, r232);
}
static void FUN__24loop103_3434_i304_i386_37 (O rr) {
  O r232 = k[4]; O r350 = k[5]; lenv = k[2]; k = k[1];
  O r354 = rr;
  r350[5] = r354;
  // env 73:
  //   string
  O r338 = alloc_cleared (TC_ENV, 2);
  r338[1] = lenv; lenv = r338;
  // env 74:
  //   (list string)
  O r340 = alloc_cleared (TC_ENV, 2);
  O r343 = (O) &constructed_14;
  // env 75:
  //   int
  O r347 = allocate (TC_ENV, 2);
  O r349 = ((object**) lenv) [1][5];
  r347[2] = r349;
  O r348 = top[33];
  O t = allocate (TC_SAVE, 7);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_39; t[4] = r232; t[5] = r338; t[6] = r340; t[7] = r343; k = t;
  r347[1] = r348[2]; lenv = r347; FUN_int__3estring_363_0();
}
static void FUN__24loop103_3434_i304_i386_39 (O rr) {
  O r232 = k[4]; O r338 = k[5]; O r340 = k[6]; O r343 = k[7]; lenv = k[2]; k = k[1];
  O r345 = rr;
  O r346 = (object*) constructed_3[0];
  O r344 = alloc_no_clear (TC_PAIR,2);
  r344[1] = r345;
  r344[2] = r346;
  O r342 = alloc_no_clear (TC_PAIR,2);
  r342[1] = r343;
  r342[2] = r344;
  r340[2] = r342;
  O r341 = top[19];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_40; t[4] = r232; t[5] = r338; k = t;
  r340[1] = r341[2]; lenv = r340; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_40 (O rr) {
  O r232 = k[4]; O r338 = k[5]; lenv = k[2]; k = k[1];
  O r339 = rr;
  r338[2] = r339;
  O r335 = ((object*) lenv) [2];
  O r337 = ((object*) lenv) [2];
  O r336 = TAG_INTEGER((irk_int)((irk_string*)(r337))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r335))->data, 1, UNTAG_INTEGER(r336), stdout));
  O r334 = (object *) 14;
  lenv = lenv[1];
  // env 76:
  //   int
  O r331 = allocate (TC_ENV, 2);
  O r333 = ((object*) lenv) [5];
  r331[2] = r333;
  O r332 = ((object******) lenv) [1][1][1][1][1][5];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_41; t[4] = r232; k = t;
  r331[1] = r332[2]; lenv = r331; FUN_get_pair0_3408_i304_i386_0();
}
static void FUN__24loop103_3434_i304_i386_41 (O rr) {
  O r232 = k[4]; lenv = k[2]; k = k[1];
  O r330 = rr;
  lenv = lenv[1];
  O r329 = r330;
  O r327 = r329;
  JUMP_12(r327, r232);
}
static void JUMP_12(O r327, O r232) {
  O r262 = r327;
  JUMP_10(r262, r232);
}
static void FUN__24loop103_3434_i304_i386_36 (O rr) {
  O r232 = k[4]; O r318 = k[5]; lenv = k[2]; k = k[1];
  O r319 = rr;
  r318[2] = r319;
  // env 77:
  //   int
  O r321 = allocate (TC_ENV, 2);
  O r323 = ((object*) lenv) [2];
  r321[2] = r323;
  O r322 = ((object******) lenv) [1][1][1][1][1][5];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_42; t[4] = r232; t[5] = r318; k = t;
  r321[1] = r322[2]; lenv = r321; FUN_get_pair0_3408_i304_i386_0();
}
static void FUN__24loop103_3434_i304_i386_42 (O rr) {
  O r232 = k[4]; O r318 = k[5]; lenv = k[2]; k = k[1];
  O r320 = rr;
  r318[3] = r320;
  // env 78:
  //   string string
  O r313 = alloc_cleared (TC_ENV, 3);
  r313[1] = lenv; lenv = r313;
  O r317 = ((object**) lenv) [1][3];
  O r314 = UOBJ_GET(r317,0);
  r313[2] = r314;
  O r316 = ((object**) lenv) [1][3];
  O r315 = UOBJ_GET(r316,1);
  r313[3] = r315;
  // env 79:
  //   string
  O r276 = alloc_cleared (TC_ENV, 2);
  r276[1] = lenv; lenv = r276;
  // env 80:
  //   (list string)
  O r278 = alloc_cleared (TC_ENV, 2);
  O r281 = (O) &constructed_13;
  // env 81:
  //   int
  O r310 = allocate (TC_ENV, 2);
  O r312 = ((object***) lenv) [1][1][2];
  r310[2] = r312;
  O r311 = top[33];
  O t = allocate (TC_SAVE, 7);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_43; t[4] = r232; t[5] = r276; t[6] = r278; t[7] = r281; k = t;
  r310[1] = r311[2]; lenv = r310; FUN_int__3estring_363_0();
}
static void FUN__24loop103_3434_i304_i386_43 (O rr) {
  O r232 = k[4]; O r276 = k[5]; O r278 = k[6]; O r281 = k[7]; lenv = k[2]; k = k[1];
  O r283 = rr;
  O r285 = (O) &constructed_12;
  O r287 = ((object**) lenv) [1][2];
  O r289 = (O) &constructed_11;
  // env 82:
  //   (list string)
  O r293 = alloc_cleared (TC_ENV, 2);
  O r296 = (O) &constructed_9;
  // env 83:
  //   (list string)
  O r300 = alloc_cleared (TC_ENV, 2);
  // env 84:
  //   (char -> string) (list char)
  O r303 = alloc_cleared (TC_ENV, 3);
  O r305 = top[30];
  r303[2] = r305;
  // env 85:
  //   string
  O r307 = allocate (TC_ENV, 2);
  O r309 = ((object**) lenv) [1][3];
  r307[2] = r309;
  O r308 = top[21];
  O t = allocate (TC_SAVE, 15);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_44; t[4] = r232; t[5] = r276; t[6] = r278; t[7] = r281; t[8] = r283; t[9] = r285; t[10] = r287; t[11] = r289; t[12] = r293; t[13] = r296; t[14] = r300; t[15] = r303; k = t;
  r307[1] = r308[2]; lenv = r307; FUN_string__3elist_353_0();
}
static void FUN__24loop103_3434_i304_i386_44 (O rr) {
  O r232 = k[4]; O r276 = k[5]; O r278 = k[6]; O r281 = k[7]; O r283 = k[8]; O r285 = k[9]; O r287 = k[10]; O r289 = k[11]; O r293 = k[12]; O r296 = k[13]; O r300 = k[14]; O r303 = k[15]; lenv = k[2]; k = k[1];
  O r306 = rr;
  r303[3] = r306;
  O r304 = top[12];
  O t = allocate (TC_SAVE, 14);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_45; t[4] = r232; t[5] = r276; t[6] = r278; t[7] = r281; t[8] = r283; t[9] = r285; t[10] = r287; t[11] = r289; t[12] = r293; t[13] = r296; t[14] = r300; k = t;
  r303[1] = r304[2]; lenv = r303; FUN_map_292_0();
}
static void FUN__24loop103_3434_i304_i386_45 (O rr) {
  O r232 = k[4]; O r276 = k[5]; O r278 = k[6]; O r281 = k[7]; O r283 = k[8]; O r285 = k[9]; O r287 = k[10]; O r289 = k[11]; O r293 = k[12]; O r296 = k[13]; O r300 = k[14]; lenv = k[2]; k = k[1];
  O r302 = rr;
  r300[2] = r302;
  O r301 = top[19];
  O t = allocate (TC_SAVE, 13);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_46; t[4] = r232; t[5] = r276; t[6] = r278; t[7] = r281; t[8] = r283; t[9] = r285; t[10] = r287; t[11] = r289; t[12] = r293; t[13] = r296; k = t;
  r300[1] = r301[2]; lenv = r300; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_46 (O rr) {
  O r232 = k[4]; O r276 = k[5]; O r278 = k[6]; O r281 = k[7]; O r283 = k[8]; O r285 = k[9]; O r287 = k[10]; O r289 = k[11]; O r293 = k[12]; O r296 = k[13]; lenv = k[2]; k = k[1];
  O r298 = rr;
  O r299 = (object*) constructed_10[0];
  O r297 = alloc_no_clear (TC_PAIR,2);
  r297[1] = r298;
  r297[2] = r299;
  O r295 = alloc_no_clear (TC_PAIR,2);
  r295[1] = r296;
  r295[2] = r297;
  r293[2] = r295;
  O r294 = top[19];
  O t = allocate (TC_SAVE, 11);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_47; t[4] = r232; t[5] = r276; t[6] = r278; t[7] = r281; t[8] = r283; t[9] = r285; t[10] = r287; t[11] = r289; k = t;
  r293[1] = r294[2]; lenv = r293; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_47 (O rr) {
  O r232 = k[4]; O r276 = k[5]; O r278 = k[6]; O r281 = k[7]; O r283 = k[8]; O r285 = k[9]; O r287 = k[10]; O r289 = k[11]; lenv = k[2]; k = k[1];
  O r291 = rr;
  O r292 = (object*) constructed_3[0];
  O r290 = alloc_no_clear (TC_PAIR,2);
  r290[1] = r291;
  r290[2] = r292;
  O r288 = alloc_no_clear (TC_PAIR,2);
  r288[1] = r289;
  r288[2] = r290;
  O r286 = alloc_no_clear (TC_PAIR,2);
  r286[1] = r287;
  r286[2] = r288;
  O r284 = alloc_no_clear (TC_PAIR,2);
  r284[1] = r285;
  r284[2] = r286;
  O r282 = alloc_no_clear (TC_PAIR,2);
  r282[1] = r283;
  r282[2] = r284;
  O r280 = alloc_no_clear (TC_PAIR,2);
  r280[1] = r281;
  r280[2] = r282;
  r278[2] = r280;
  O r279 = top[19];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_48; t[4] = r232; t[5] = r276; k = t;
  r278[1] = r279[2]; lenv = r278; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_48 (O rr) {
  O r232 = k[4]; O r276 = k[5]; lenv = k[2]; k = k[1];
  O r277 = rr;
  r276[2] = r277;
  O r273 = ((object*) lenv) [2];
  O r275 = ((object*) lenv) [2];
  O r274 = TAG_INTEGER((irk_int)((irk_string*)(r275))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r273))->data, 1, UNTAG_INTEGER(r274), stdout));
  O r272 = (object *) 14;
  lenv = lenv[1];
  // env 86:
  //   string string
  O r267 = allocate (TC_ENV, 3);
  O r270 = ((object*) lenv) [2];
  r267[2] = r270;
  O r271 = ((object*) lenv) [3];
  r267[3] = r271;
  O r269 = ((object********) lenv) [1][1][1][1][1][1][1][11];
  O r268 = ((irk_vector*)r269)->val[3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_49; t[4] = r232; k = t;
  r267[1] = r268[2]; lenv = r267; ((pfun)(r268[1]))();
}
static void FUN__24loop103_3434_i304_i386_49 (O rr) {
  O r232 = k[4]; lenv = k[2]; k = k[1];
  O r266 = ((object**) lenv) [1][3];
  lenv = lenv[1];
  O r265 = r266;
  lenv = lenv[1];
  O r264 = r265;
  O r262 = r264;
  JUMP_10(r262, r232);
}
static void JUMP_10(O r262, O r232) {
  O r233 = r262;
  JUMP_9(r233, r232);
}
static void FUN__24loop103_3434_i304_i386_35 (O rr) {
  O r232 = k[4]; O r257 = k[5]; lenv = k[2]; k = k[1];
  O r258 = rr;
  r257[2] = r258;
  // env 87:
  //   string
  O r245 = alloc_cleared (TC_ENV, 2);
  r245[1] = lenv; lenv = r245;
  // env 88:
  //   (list string)
  O r247 = alloc_cleared (TC_ENV, 2);
  O r250 = (O) &constructed_8;
  // env 89:
  //   int
  O r254 = allocate (TC_ENV, 2);
  O r256 = ((object**) lenv) [1][2];
  r254[2] = r256;
  O r255 = top[33];
  O t = allocate (TC_SAVE, 7);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_50; t[4] = r232; t[5] = r245; t[6] = r247; t[7] = r250; k = t;
  r254[1] = r255[2]; lenv = r254; FUN_int__3estring_363_0();
}
static void FUN__24loop103_3434_i304_i386_50 (O rr) {
  O r232 = k[4]; O r245 = k[5]; O r247 = k[6]; O r250 = k[7]; lenv = k[2]; k = k[1];
  O r252 = rr;
  O r253 = (object*) constructed_3[0];
  O r251 = alloc_no_clear (TC_PAIR,2);
  r251[1] = r252;
  r251[2] = r253;
  O r249 = alloc_no_clear (TC_PAIR,2);
  r249[1] = r250;
  r249[2] = r251;
  r247[2] = r249;
  O r248 = top[19];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_51; t[4] = r232; t[5] = r245; k = t;
  r247[1] = r248[2]; lenv = r247; FUN_string_concat_338_0();
}
static void FUN__24loop103_3434_i304_i386_51 (O rr) {
  O r232 = k[4]; O r245 = k[5]; lenv = k[2]; k = k[1];
  O r246 = rr;
  r245[2] = r246;
  O r242 = ((object*) lenv) [2];
  O r244 = ((object*) lenv) [2];
  O r243 = TAG_INTEGER((irk_int)((irk_string*)(r244))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r242))->data, 1, UNTAG_INTEGER(r243), stdout));
  O r241 = (object *) 14;
  lenv = lenv[1];
  // env 90:
  //   int
  O r237 = allocate (TC_ENV, 2);
  O r240 = ((object*) lenv) [2];
  r237[2] = r240;
  O r239 = ((object*******) lenv) [1][1][1][1][1][1][11];
  O r238 = ((irk_vector*)r239)->val[0];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_52; t[4] = r232; k = t;
  r237[1] = r238[2]; lenv = r237; ((pfun)(r238[1]))();
}
static void FUN__24loop103_3434_i304_i386_52 (O rr) {
  O r232 = k[4]; lenv = k[2]; k = k[1];
  O r236 = rr;
  lenv = lenv[1];
  O r235 = r236;
  O r233 = r235;
  JUMP_9(r233, r232);
}
static void JUMP_9(O r233, O r232) {
  r232[2] = r233;
  O r230 = ((object*) lenv) [2];
  O r231 = ((object***) lenv) [1][1][2];
  O r229 = alloc_no_clear (TC_PAIR,2);
  r229[1] = r230;
  r229[2] = r231;
  ((object***) lenv) [1][1][2] = r229;
  O r228 = (object *) TC_UNDEFINED;
  lenv = lenv[1];
  O r227 = r228;
  JUMP_8();
}
static void FUN__24loop103_3434_i304_i386_32 (O rr) {
  O r220 = k[4]; lenv = k[2]; k = k[1];
  O r223 = rr;
  r220[2] = r223;
  O r222 = ((object*****) lenv) [1][1][1][1][11];
  O r221 = ((irk_vector*)r222)->val[4];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3434_i304_i386_53; ; k = t;
  r220[1] = r221[2]; lenv = r220; ((pfun)(r221[1]))();
}
static void FUN__24loop103_3434_i304_i386_53 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r219 = rr;
  JUMP_8();
}
static void JUMP_8() {
  FUN__24loop103_3434_i304_i386_0();
}
static void FUN_get_pair0_3408_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 91:
  //   string
  O r436 = alloc_cleared (TC_ENV, 2);
  r436[1] = lenv; lenv = r436;
  O r438 = ((object**) lenv) [1][2];
  O r439 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r438)==UNTAG_INTEGER(r439))) {
    O r441 = ((object***) lenv) [1][1][4];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_pair0_3408_i304_i386_54; t[4] = r436; k = t;
    lenv = r441[2]; FUN_get_literal_3409_i304_i386_0();
  } else {
    // env 92:
    //   |tuple=(product string string) |
    O r447 = alloc_cleared (TC_ENV, 2);
    r447[1] = lenv; lenv = r447;
    // env 93:
    //   int
    O r449 = allocate (TC_ENV, 2);
    O r452 = ((object***) lenv) [1][1][2];
    r449[2] = r452;
    O r451 = ((object*****) lenv) [1][1][1][1][11];
    O r450 = ((irk_vector*)r451)->val[0];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_pair0_3408_i304_i386_55; t[4] = r436; t[5] = r447; k = t;
    r449[1] = r450[2]; lenv = r449; ((pfun)(r450[1]))();
  }
}
static void FUN_get_pair0_3408_i304_i386_55 (O rr) {
  O r436 = k[4]; O r447 = k[5]; lenv = k[2]; k = k[1];
  O r448 = rr;
  r447[2] = r448;
  O r446 = ((object*) lenv) [2];
  O r444 = UOBJ_GET(r446,0);
  O r445 = r444; // reg varref
  O r443 = r445; // reg varref
  lenv = lenv[1];
  O r442 = r443;
  O r437 = r442;
  JUMP_14(r437, r436);
}
static void FUN_get_pair0_3408_i304_i386_54 (O rr) {
  O r436 = k[4]; lenv = k[2]; k = k[1];
  O r440 = rr;
  O r437 = r440;
  JUMP_14(r437, r436);
}
static void JUMP_14(O r437, O r436) {
  r436[2] = r437;
  O r432 = alloc_cleared (UOTAG(3), 2);
  O r433 = ((object*) lenv) [2];
  r432[1] = r433;
  O r435 = ((object***) lenv) [1][1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_pair0_3408_i304_i386_56; t[4] = r432; k = t;
  lenv = r435[2]; FUN_get_literal_3409_i304_i386_0();
}
static void FUN_get_pair0_3408_i304_i386_56 (O rr) {
  O r432 = k[4]; lenv = k[2]; k = k[1];
  O r434 = rr;
  r432[2] = r434;
  IRK_RETURN(432);
}
static void FUN_get_literal_3409_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 94:
  //   int int bool int string
  O r461 = alloc_cleared (TC_ENV, 6);
  r461[1] = lenv; lenv = r461;
  O r462 = ((object***) lenv) [1][1][4];
  r461[2] = r462;
  O r620 = (object *) 257;
  O r621 = ((object*) lenv) [2];
  O r463 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r620)&UNTAG_INTEGER(r621));
  r461[3] = r463;
  O r618 = ((object*) lenv) [3];
  O r619 = (object *) 257;
  O r464 = IRK_TEST(UNTAG_INTEGER(r618)==UNTAG_INTEGER(r619));
  r461[4] = r464;
  // env 95:
  //   int
  O r615 = allocate (TC_ENV, 2);
  O r617 = (object *) 15;
  r615[2] = r617;
  O r616 = ((object**) lenv) [1][3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_literal_3409_i304_i386_57; t[4] = r461; k = t;
  r615[1] = r616[2]; lenv = r615; FUN_get_integer_3407_i304_i386_0();
}
static void FUN_get_literal_3409_i304_i386_57 (O rr) {
  O r461 = k[4]; lenv = k[2]; k = k[1];
  O r465 = rr;
  r461[5] = r465;
  O r467 = ((object*) lenv) [4];
  if IRK_IS_TRUE(r467) {
    // env 96:
    //   string int (list char) int int int
    O r582 = allocate (TC_ENV, 7);
    r582[1] = lenv; lenv = r582;
    O r583 = ((object****) lenv) [1][1][1][2];
    r582[2] = r583;
    O r584 = ((object****) lenv) [1][1][1][3];
    r582[3] = r584;
    O r585 = (object *) 10;
    r582[4] = r585;
    O r589 = ((object**) lenv) [1][5];
    O r590 = (object *) 17;
    O r586 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r589)*UNTAG_INTEGER(r590));
    r582[5] = r586;
    O r587 = (object *) -1;
    r582[6] = r587;
    O r588 = (object *) 1;
    r582[7] = r588;
    // env 97:
    //   ( -> undefined)
    O r482 = alloc_cleared (TC_ENV, 2);
    r482[1] = lenv; lenv = r482;
    O r483 = allocate (TC_CLOSURE, 2);
    r483[1] = FUN__24loop100_3316_i280_i304_i386_0; r483[2] = lenv;
    r482[2] = r483;
    O r481 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_literal_3409_i304_i386_58; t[4] = r461; k = t;
    lenv = r481[2]; FUN__24loop100_3316_i280_i304_i386_0();
  } else {
    // env 98:
    //   string int int int int int string
    O r600 = alloc_cleared (TC_ENV, 8);
    r600[1] = lenv; lenv = r600;
    O r601 = ((object****) lenv) [1][1][1][2];
    r600[2] = r601;
    O r602 = ((object****) lenv) [1][1][1][3];
    r600[3] = r602;
    O r603 = ((object****) lenv) [1][1][1][3];
    r600[4] = r603;
    O r613 = ((object*) lenv) [4];
    O r614 = ((object**) lenv) [1][5];
    O r604 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r613)+UNTAG_INTEGER(r614));
    r600[5] = r604;
    O r611 = ((object*) lenv) [5];
    O r612 = ((object*) lenv) [3];
    O r605 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r611)-UNTAG_INTEGER(r612));
    r600[6] = r605;
    O r606 = ((object*) lenv) [6];
    r600[7] = r606;
    O r610 = ((object*) lenv) [7];
    O r609 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r610)));
    if (freep + unbox(r609) >= limit) {
      heap_request = unbox(r609);
      heap1[3] = r600;
      heap1[4] = r461;
      gc_flip (2);
      r600 = heap0[3];
      r461 = heap0[4];
    }
    O r608 = ((object*) lenv) [7];
    O r607 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r608))), ((irk_string*)(t))->len = UNTAG_INTEGER(r608), t);
    r600[8] = r607;
    // env 99:
    //   string int int string int
    O r593 = allocate (TC_ENV, 6);
    O r595 = ((object*) lenv) [2];
    r593[2] = r595;
    O r596 = ((object*) lenv) [3];
    r593[3] = r596;
    O r597 = ((object*) lenv) [6];
    r593[4] = r597;
    O r598 = ((object*) lenv) [8];
    r593[5] = r598;
    O r599 = (object *) 1;
    r593[6] = r599;
    O r594 = top[15];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_literal_3409_i304_i386_59; t[4] = r461; k = t;
    r593[1] = r594[2]; lenv = r593; FUN_buffer_copy_330_0();
  }
}
static void FUN_get_literal_3409_i304_i386_59 (O rr) {
  O r461 = k[4]; lenv = k[2]; k = k[1];
  O r592 = ((object*) lenv) [8];
  lenv = lenv[1];
  O r591 = r592;
  O r466 = r591;
  JUMP_16(r466, r461);
}
static void FUN_get_literal_3409_i304_i386_58 (O rr) {
  O r461 = k[4]; lenv = k[2]; k = k[1];
  O r480 = rr;
  lenv = lenv[1];
  // env 100:
  //   (list char)
  O r470 = alloc_cleared (TC_ENV, 2);
  // env 101:
  //   (list char)
  O r478 = allocate (TC_ENV, 2);
  r478[1] = lenv; lenv = r478;
  O r479 = ((object**) lenv) [1][4];
  r478[2] = r479;
  // env 102:
  //   (list char) (list |tuple=(product string string) |)
  O r474 = allocate (TC_ENV, 3);
  O r476 = ((object*) lenv) [2];
  r474[2] = r476;
  O r477 = (object *) 10;
  r474[3] = r477;
  O r475 = top[9];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_literal_3409_i304_i386_60; t[4] = r461; t[5] = r470; k = t;
  r474[1] = r475[2]; lenv = r474; FUN_reverse_onto_269_0();
}
static void FUN_get_literal_3409_i304_i386_60 (O rr) {
  O r461 = k[4]; O r470 = k[5]; lenv = k[2]; k = k[1];
  O r473 = rr;
  lenv = lenv[1];
  O r472 = r473;
  r470[2] = r472;
  O r471 = top[20];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_literal_3409_i304_i386_61; t[4] = r461; k = t;
  r470[1] = r471[2]; lenv = r470; FUN_list__3estring_352_0();
}
static void FUN_get_literal_3409_i304_i386_61 (O rr) {
  O r461 = k[4]; lenv = k[2]; k = k[1];
  O r469 = rr;
  lenv = lenv[1];
  O r468 = r469;
  O r466 = r468;
  JUMP_16(r466, r461);
}
static void FUN__24loop100_3316_i280_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r578 = ((object**) lenv) [1][5];
  O r580 = r578; // reg varref
  O r581 = (object *) 11;
  O r579 = IRK_TEST(UNTAG_INTEGER(r580)>=UNTAG_INTEGER(r581));
  O r486 = r579; // reg varref
  if IRK_IS_TRUE(r486) {
    // env 103:
    //   (hufftree -> undefined)
    O r494 = alloc_cleared (TC_ENV, 2);
    r494[1] = lenv; lenv = r494;
    O r495 = allocate (TC_CLOSURE, 2);
    r495[1] = FUN_loop_3317_i280_i304_i386_0; r495[2] = lenv;
    r494[2] = r495;
    // env 104:
    //   hufftree
    O r491 = allocate (TC_ENV, 2);
    O r493 = top[53];
    r491[2] = r493;
    O r492 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop100_3316_i280_i304_i386_62; ; k = t;
    r491[1] = r492[2]; lenv = r491; FUN_loop_3317_i280_i304_i386_0();
  } else {
    O r577 = (object *) 14;
    IRK_RETURN(577);
  }
}
static void FUN__24loop100_3316_i280_i304_i386_62 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r490 = rr;
  lenv = lenv[1];
  FUN__24loop100_3316_i280_i304_i386_0();
}
static void FUN_loop_3317_i280_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r498 = ((object*) lenv) [2];
  switch (get_case (r498)) {
    case (UOTAG(1)): { // leaf
      O r508 = ((object*) lenv) [2];
      O r501 = UOBJ_GET(r508,0);
      O r507 = r501; // reg varref
      O r505 = TO_CHAR(UNTAG_INTEGER(r507));
      O r506 = ((object****) lenv) [1][1][1][4];
      O r504 = alloc_no_clear (TC_PAIR,2);
      r504[1] = r505;
      r504[2] = r506;
      ((object****) lenv) [1][1][1][4] = r504;
      O r503 = (object *) TC_UNDEFINED;
      IRK_RETURN(503);
    } break;
    default: {
      // env 105:
      //   hufftree hufftree
      O r571 = alloc_cleared (TC_ENV, 3);
      r571[1] = lenv; lenv = r571;
      O r575 = ((object**) lenv) [1][2];
      O r572 = UOBJ_GET(r575,0);
      r571[2] = r572;
      O r574 = ((object**) lenv) [1][2];
      O r573 = UOBJ_GET(r574,1);
      r571[3] = r573;
      O r567 = ((object*****) lenv) [1][1][1][1][5];
      O r569 = r567; // reg varref
      O r570 = (object *) 1;
      O r568 = IRK_TEST(UNTAG_INTEGER(r569)>UNTAG_INTEGER(r570));
      O r513 = r568; // reg varref
      if IRK_IS_TRUE(r513) {
        O r561 = ((object*****) lenv) [1][1][1][1][6];
        O r563 = r561; // reg varref
        O r564 = (object *) 1;
        O r562 = IRK_TEST(UNTAG_INTEGER(r563)<UNTAG_INTEGER(r564));
        O r543 = r562; // reg varref
        if IRK_IS_TRUE(r543) {
          // env 106:
          //   char
          O r554 = alloc_cleared (TC_ENV, 2);
          r554[1] = lenv; lenv = r554;
          // env 107:
          //   string int
          O r556 = allocate (TC_ENV, 3);
          O r558 = ((object******) lenv) [1][1][1][1][1][2];
          r556[2] = r558;
          O r559 = ((object******) lenv) [1][1][1][1][1][3];
          r556[3] = r559;
          O r557 = top[18];
          O t = allocate (TC_SAVE, 4);
          t[1] = k; t[2] = lenv; t[3] = FUN_loop_3317_i280_i304_i386_63; t[4] = r554; k = t;
          r556[1] = r557[2]; lenv = r556; FUN_string_ref_336_0();
        } else {
          O r560 = (object *) 14;
          JUMP_21();
        }
      } else {
        O r566 = (object *) 14;
        IRK_RETURN(566);
      }
    } break;
  }
}
static void FUN_loop_3317_i280_i304_i386_63 (O rr) {
  O r554 = k[4]; lenv = k[2]; k = k[1];
  O r555 = rr;
  r554[2] = r555;
  O r553 = ((object*) lenv) [2];
  O r552 = TAG_INTEGER((irk_int)GET_CHAR(r553));
  lenv = lenv[1];
  O r551 = r552;
  ((object*****) lenv) [1][1][1][1][7] = r551;
  O r547 = ((object*****) lenv) [1][1][1][1][3];
  O r549 = r547; // reg varref
  O r550 = (object *) 3;
  O r548 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r549)+UNTAG_INTEGER(r550));
  O r546 = r548; // reg varref
  ((object*****) lenv) [1][1][1][1][3] = r546;
  O r545 = (object *) 15;
  ((object*****) lenv) [1][1][1][1][6] = r545;
  O r544 = (object *) TC_UNDEFINED;
  JUMP_21();
}
static void JUMP_21() {
  // env 108:
  //   int int int int bool
  O r531 = allocate (TC_ENV, 6);
  r531[1] = lenv; lenv = r531;
  O r532 = ((object******) lenv) [1][1][1][1][1][7];
  r531[2] = r532;
  O r533 = ((object******) lenv) [1][1][1][1][1][6];
  r531[3] = r533;
  O r541 = ((object*) lenv) [2];
  O r542 = ((object*) lenv) [3];
  O r534 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r541)>>UNTAG_INTEGER(r542));
  r531[4] = r534;
  O r539 = (object *) 3;
  O r540 = ((object*) lenv) [4];
  O r535 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r539)&UNTAG_INTEGER(r540));
  r531[5] = r535;
  O r537 = (object *) 3;
  O r538 = ((object*) lenv) [5];
  O r536 = IRK_TEST(UNTAG_INTEGER(r537)==UNTAG_INTEGER(r538));
  r531[6] = r536;
  O r527 = ((object******) lenv) [1][1][1][1][1][6];
  O r529 = r527; // reg varref
  O r530 = (object *) 3;
  O r528 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r529)-UNTAG_INTEGER(r530));
  O r526 = r528; // reg varref
  ((object******) lenv) [1][1][1][1][1][6] = r526;
  O r522 = ((object******) lenv) [1][1][1][1][1][5];
  O r524 = r522; // reg varref
  O r525 = (object *) 3;
  O r523 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r524)-UNTAG_INTEGER(r525));
  O r521 = r523; // reg varref
  ((object******) lenv) [1][1][1][1][1][5] = r521;
  O r520 = ((object*) lenv) [6];
  lenv = lenv[1];
  O r517 = r520;
  if IRK_IS_TRUE(r517) {
    O r518 = ((object*) lenv) [3];
    O r516 = r518;
    JUMP_20(r516);
  } else {
    O r519 = ((object*) lenv) [2];
    O r516 = r519;
    JUMP_20(r516);
  }
}
static void JUMP_20(O r516) {
  lenv = ((object *)lenv)[1];
  lenv[2] = r516;
  FUN_loop_3317_i280_i304_i386_0();
}
static void JUMP_16(O r466, O r461) {
  r461[6] = r466;
  // env 109:
  //   int
  O r458 = allocate (TC_ENV, 2);
  O r460 = ((object*) lenv) [5];
  r458[2] = r460;
  O r459 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_3317_i280_i304_i386_64; ; k = t;
  r458[1] = r459[2]; lenv = r458; FUN_forward_3405_i304_i386_0();
}
static void FUN_loop_3317_i280_i304_i386_64 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r457 = ((object*) lenv) [6];
  IRK_RETURN(457);
}
static void FUN_get_integer_3407_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 110:
  //   int int int
  O r691 = alloc_cleared (TC_ENV, 4);
  r691[1] = lenv; lenv = r691;
  O r697 = top[54];
  O r698 = ((object**) lenv) [1][2];
  range_check (GET_TUPLE_LENGTH(*(object*)r697), unbox(r698));
  O r692 = ((irk_vector*)r697)->val[unbox(r698)];
  r691[2] = r692;
  O r693 = ((object****) lenv) [1][1][1][4];
  r691[3] = r693;
  O r695 = ((object*) lenv) [2];
  O r696 = ((object*) lenv) [3];
  O r694 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r695)&UNTAG_INTEGER(r696));
  r691[4] = r694;
  O r687 = ((object*) lenv) [4];
  O r689 = r687; // reg varref
  O r690 = ((object*) lenv) [2];
  O r688 = IRK_TEST(UNTAG_INTEGER(r689)==UNTAG_INTEGER(r690));
  O r626 = r688; // reg varref
  if IRK_IS_TRUE(r626) {
    O r680 = (object *) 1;
    ((object*) lenv) [4] = r680;
    // env 111:
    //   (int -> undefined)
    O r642 = alloc_cleared (TC_ENV, 2);
    r642[1] = lenv; lenv = r642;
    O r643 = allocate (TC_CLOSURE, 2);
    r643[1] = FUN_loop_3416_i304_i386_0; r643[2] = lenv;
    r642[2] = r643;
    // env 112:
    //   int
    O r639 = allocate (TC_ENV, 2);
    O r641 = (object *) 1;
    r639[2] = r641;
    O r640 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_integer_3407_i304_i386_65; ; k = t;
    r639[1] = r640[2]; lenv = r639; FUN_loop_3416_i304_i386_0();
  } else {
    // env 113:
    //   int
    O r684 = allocate (TC_ENV, 2);
    O r686 = (object *) 3;
    r684[2] = r686;
    O r685 = ((object***) lenv) [1][1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_integer_3407_i304_i386_66; ; k = t;
    r684[1] = r685[2]; lenv = r684; FUN_forward_3405_i304_i386_0();
  }
}
static void FUN_get_integer_3407_i304_i386_66 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r683 = ((object*) lenv) [4];
  IRK_RETURN(683);
}
static void FUN_get_integer_3407_i304_i386_65 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r638 = rr;
  lenv = lenv[1];
  // env 114:
  //   int
  O r635 = allocate (TC_ENV, 2);
  O r637 = (object *) 3;
  r635[2] = r637;
  O r636 = ((object***) lenv) [1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_integer_3407_i304_i386_67; ; k = t;
  r635[1] = r636[2]; lenv = r635; FUN_forward_3405_i304_i386_0();
}
static void FUN_get_integer_3407_i304_i386_67 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r630 = ((object*) lenv) [4];
  O r633 = r630; // reg varref
  O r634 = ((object*) lenv) [2];
  O r632 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r633)+UNTAG_INTEGER(r634));
  IRK_RETURN(632);
}
static void FUN_loop_3416_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 115:
  //   int
  O r677 = allocate (TC_ENV, 2);
  O r679 = (object *) 3;
  r677[2] = r679;
  O r678 = ((object*****) lenv) [1][1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_3416_i304_i386_68; ; k = t;
  r677[1] = r678[2]; lenv = r677; FUN_forward_3405_i304_i386_0();
}
static void FUN_loop_3416_i304_i386_68 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r666 = ((object***) lenv) [1][1][4];
  O r667 = ((object******) lenv) [1][1][1][1][1][4];
  O r675 = (object *) 255;
  O r676 = r667; // reg varref
  O r668 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r675)&UNTAG_INTEGER(r676));
  O r673 = r668; // reg varref
  O r674 = ((object*) lenv) [2];
  O r669 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r673)<<UNTAG_INTEGER(r674));
  O r671 = r666; // reg varref
  O r672 = r669; // reg varref
  O r670 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r671)|UNTAG_INTEGER(r672));
  O r665 = r670; // reg varref
  ((object***) lenv) [1][1][4] = r665;
  O r655 = ((object******) lenv) [1][1][1][1][1][4];
  O r663 = (object *) 257;
  O r664 = r655; // reg varref
  O r656 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r663)&UNTAG_INTEGER(r664));
  O r661 = (object *) 1;
  O r662 = r656; // reg varref
  O r657 = IRK_TEST(UNTAG_INTEGER(r661)==UNTAG_INTEGER(r662));
  O r659 = r657; // reg varref
  O r660 = (object *) 6;
  O r658 = IRK_TEST(r659==r660);
  O r647 = r658; // reg varref
  if IRK_IS_TRUE(r647) {
    O r651 = ((object*) lenv) [2];
    O r652 = (object *) 15;
    O r650 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r651)+UNTAG_INTEGER(r652));
    lenv[2] = r650;
    FUN_loop_3416_i304_i386_0();
  } else {
    O r654 = (object *) 14;
    IRK_RETURN(654);
  }
}
static void FUN_forward_3405_i304_i386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r722 = ((object***) lenv) [1][1][3];
  O r724 = r722; // reg varref
  O r725 = ((object*) lenv) [2];
  O r723 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r724)+UNTAG_INTEGER(r725));
  O r721 = r723; // reg varref
  ((object***) lenv) [1][1][3] = r721;
  O r716 = ((object***) lenv) [1][1][3];
  O r717 = ((object***) lenv) [1][1][5];
  O r719 = r716; // reg varref
  O r720 = r717; // reg varref
  O r718 = IRK_TEST(UNTAG_INTEGER(r719)<UNTAG_INTEGER(r720));
  O r702 = r718; // reg varref
  if IRK_IS_TRUE(r702) {
    // env 116:
    //   char
    O r708 = alloc_cleared (TC_ENV, 2);
    r708[1] = lenv; lenv = r708;
    // env 117:
    //   string int
    O r710 = allocate (TC_ENV, 3);
    O r712 = ((object****) lenv) [1][1][1][2];
    r710[2] = r712;
    O r713 = ((object****) lenv) [1][1][1][3];
    r710[3] = r713;
    O r711 = top[18];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_forward_3405_i304_i386_69; t[4] = r708; k = t;
    r710[1] = r711[2]; lenv = r710; FUN_string_ref_336_0();
  } else {
    O r715 = (object *) 14;
    IRK_RETURN(715);
  }
}
static void FUN_forward_3405_i304_i386_69 (O rr) {
  O r708 = k[4]; lenv = k[2]; k = k[1];
  O r709 = rr;
  r708[2] = r709;
  O r707 = ((object*) lenv) [2];
  O r706 = TAG_INTEGER((irk_int)GET_CHAR(r707));
  lenv = lenv[1];
  O r705 = r706;
  ((object***) lenv) [1][1][4] = r705;
  O r704 = (object *) TC_UNDEFINED;
  IRK_RETURN(704);
}
static void FUN_get_index_3341_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 118:
  //   string (maybe int) |tuple=(product (maybe int) bool) |
  O r832 = alloc_cleared (TC_ENV, 4);
  r832[1] = lenv; lenv = r832;
  O r833 = ((object**) lenv) [1][2];
  r832[2] = r833;
  // env 119:
  //   (tree |tuple=(product string string) | int) (|tuple=(product string t2540) ...t2551| |tuple=(product string t2540) ...t2551| -> cmp) |tuple=(product string t2540) ...t2551|
  O r851 = allocate (TC_ENV, 4);
  O r853 = top[51];
  r851[2] = r853;
  O r854 = top[8];
  r851[3] = r854;
  O r855 = allocate (UOTAG(3), 2);
  O r856 = ((object*) lenv) [2];
  r855[1] = r856;
  O r857 = ((object**) lenv) [1][3];
  r855[2] = r857;
  r851[4] = r855;
  O r852 = top[38];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_70; t[4] = r832; k = t;
  r851[1] = r852[2]; lenv = r851; FUN_tree_2fmember_570_0();
}
static void FUN_get_index_3341_i2_70 (O rr) {
  O r832 = k[4]; lenv = k[2]; k = k[1];
  O r834 = rr;
  r832[3] = r834;
  O r836 = ((object*) lenv) [3];
  switch (get_case (r836)) {
    case (UOTAG(0)): { // yes
      O r843 = ((object*) lenv) [3];
      O r838 = UOBJ_GET(r843,0);
      O r839 = allocate (UOTAG(3), 2);
      O r842 = r838; // reg varref
      O r840 = alloc_no_clear (UOTAG(0),1);
      r840[1] = r842;
      r839[1] = r840;
      O r841 = (object *) 65542;
      r839[2] = r841;
      O r837 = r839; // reg varref
      O r835 = r837;
      JUMP_34(r835, r832);
    } break;
    default: {
      O r844 = alloc_cleared (UOTAG(3), 2);
      // env 120:
      //   (tree |tuple=(product string string) | int) string
      O r847 = allocate (TC_ENV, 3);
      O r849 = top[51];
      r847[2] = r849;
      O r850 = ((object*) lenv) [2];
      r847[3] = r850;
      O r848 = ((object***) lenv) [1][1][7];
      O t = allocate (TC_SAVE, 5);
      t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_71; t[4] = r832; t[5] = r844; k = t;
      r847[1] = r848[2]; lenv = r847; FUN_lookup_name_only_3336_i2_0();
    } break;
  }
}
static void FUN_get_index_3341_i2_71 (O rr) {
  O r832 = k[4]; O r844 = k[5]; lenv = k[2]; k = k[1];
  O r845 = rr;
  r844[1] = r845;
  O r846 = (object *) 6;
  r844[2] = r846;
  O r835 = r844;
  JUMP_34(r835, r832);
}
static void JUMP_34(O r835, O r832) {
  r832[4] = r835;
  // env 121:
  //   (maybe int) bool
  O r827 = alloc_cleared (TC_ENV, 3);
  r827[1] = lenv; lenv = r827;
  O r831 = ((object**) lenv) [1][4];
  O r828 = UOBJ_GET(r831,0);
  r827[2] = r828;
  O r830 = ((object**) lenv) [1][4];
  O r829 = UOBJ_GET(r830,1);
  r827[3] = r829;
  O r749 = ((object*) lenv) [3];
  switch (get_case (r749)) {
    case ((irk_int)IRK_TRUE): { // true
      O r751 = allocate (UOTAG(3), 2);
      O r752 = ((object*) lenv) [2];
      r751[1] = r752;
      O r753 = (object *) 65542;
      r751[2] = r753;
      IRK_RETURN(751);
    } break;
    default: {
      // env 122:
      //   string (maybe int) |tuple=(product (maybe int) bool) |
      O r801 = alloc_cleared (TC_ENV, 4);
      r801[1] = lenv; lenv = r801;
      O r802 = ((object****) lenv) [1][1][1][2];
      r801[2] = r802;
      // env 123:
      //   (tree |tuple=(product string string) | int) (|tuple=(product string t2540) ...t2606| |tuple=(product string t2540) ...t2606| -> cmp) |tuple=(product string t2540) ...t2606|
      O r820 = allocate (TC_ENV, 4);
      O r822 = ((object******) lenv) [1][1][1][1][1][7];
      r820[2] = r822;
      O r823 = top[8];
      r820[3] = r823;
      O r824 = allocate (UOTAG(3), 2);
      O r825 = ((object*) lenv) [2];
      r824[1] = r825;
      O r826 = ((object****) lenv) [1][1][1][3];
      r824[2] = r826;
      r820[4] = r824;
      O r821 = top[38];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_72; t[4] = r801; k = t;
      r820[1] = r821[2]; lenv = r820; FUN_tree_2fmember_570_0();
    } break;
  }
}
static void FUN_get_index_3341_i2_72 (O rr) {
  O r801 = k[4]; lenv = k[2]; k = k[1];
  O r803 = rr;
  r801[3] = r803;
  O r805 = ((object*) lenv) [3];
  switch (get_case (r805)) {
    case (UOTAG(0)): { // yes
      O r812 = ((object*) lenv) [3];
      O r807 = UOBJ_GET(r812,0);
      O r808 = allocate (UOTAG(3), 2);
      O r811 = r807; // reg varref
      O r809 = alloc_no_clear (UOTAG(0),1);
      r809[1] = r811;
      r808[1] = r809;
      O r810 = (object *) 65542;
      r808[2] = r810;
      O r806 = r808; // reg varref
      O r804 = r806;
      JUMP_33(r804, r801);
    } break;
    default: {
      O r813 = alloc_cleared (UOTAG(3), 2);
      // env 124:
      //   (tree |tuple=(product string string) | int) string
      O r816 = allocate (TC_ENV, 3);
      O r818 = ((object******) lenv) [1][1][1][1][1][7];
      r816[2] = r818;
      O r819 = ((object*) lenv) [2];
      r816[3] = r819;
      O r817 = ((object*****) lenv) [1][1][1][1][7];
      O t = allocate (TC_SAVE, 5);
      t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_73; t[4] = r801; t[5] = r813; k = t;
      r816[1] = r817[2]; lenv = r816; FUN_lookup_name_only_3336_i2_0();
    } break;
  }
}
static void FUN_get_index_3341_i2_73 (O rr) {
  O r801 = k[4]; O r813 = k[5]; lenv = k[2]; k = k[1];
  O r814 = rr;
  r813[1] = r814;
  O r815 = (object *) 6;
  r813[2] = r815;
  O r804 = r813;
  JUMP_33(r804, r801);
}
static void JUMP_33(O r804, O r801) {
  r801[4] = r804;
  // env 125:
  //   (maybe int) bool
  O r796 = alloc_cleared (TC_ENV, 3);
  r796[1] = lenv; lenv = r796;
  O r800 = ((object**) lenv) [1][4];
  O r797 = UOBJ_GET(r800,0);
  r796[2] = r797;
  O r799 = ((object**) lenv) [1][4];
  O r798 = UOBJ_GET(r799,1);
  r796[3] = r798;
  O r761 = ((object*) lenv) [3];
  if IRK_IS_TRUE(r761) {
    O r763 = alloc_cleared (UOTAG(3), 2);
    // env 126:
    //   (maybe int)
    O r766 = allocate (TC_ENV, 2);
    O r768 = ((object*) lenv) [2];
    r766[2] = r768;
    O r767 = ((object******) lenv) [1][1][1][1][1][9];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_74; t[4] = r763; k = t;
    r766[1] = r767[2]; lenv = r766; FUN_adjust_3340_i2_0();
  } else {
    O r793 = ((object***) lenv) [1][1][2];
    switch (get_case (r793)) {
      case (UOTAG(0)): { // yes
        O r794 = (object *) 65542;
        O r771 = r794;
        JUMP_32(r771);
      } break;
      default: {
        O r795 = (object *) 6;
        O r771 = r795;
        JUMP_32(r771);
      } break;
    }
  }
}
static void JUMP_32(O r771) {
  if IRK_IS_TRUE(r771) {
    O r773 = allocate (UOTAG(3), 2);
    O r774 = ((object***) lenv) [1][1][2];
    r773[1] = r774;
    O r775 = (object *) 6;
    r773[2] = r775;
    IRK_RETURN(773);
  } else {
    O r790 = ((object*) lenv) [2];
    switch (get_case (r790)) {
      case (UOTAG(0)): { // yes
        O r791 = (object *) 65542;
        O r778 = r791;
        JUMP_31(r778);
      } break;
      default: {
        O r792 = (object *) 6;
        O r778 = r792;
        JUMP_31(r778);
      } break;
    }
  }
}
static void JUMP_31(O r778) {
  if IRK_IS_TRUE(r778) {
    O r780 = alloc_cleared (UOTAG(3), 2);
    // env 127:
    //   (maybe int)
    O r783 = allocate (TC_ENV, 2);
    O r785 = ((object*) lenv) [2];
    r783[2] = r785;
    O r784 = ((object******) lenv) [1][1][1][1][1][9];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_index_3341_i2_75; t[4] = r780; k = t;
    r783[1] = r784[2]; lenv = r783; FUN_adjust_3340_i2_0();
  } else {
    O r787 = allocate (UOTAG(3), 2);
    O r788 = (object *) 65558;
    r787[1] = r788;
    O r789 = (object *) 6;
    r787[2] = r789;
    IRK_RETURN(787);
  }
}
static void FUN_get_index_3341_i2_75 (O rr) {
  O r780 = k[4]; lenv = k[2]; k = k[1];
  O r781 = rr;
  r780[1] = r781;
  O r782 = (object *) 6;
  r780[2] = r782;
  IRK_RETURN(780);
}
static void FUN_get_index_3341_i2_74 (O rr) {
  O r763 = k[4]; lenv = k[2]; k = k[1];
  O r764 = rr;
  r763[1] = r764;
  O r765 = (object *) 65542;
  r763[2] = r765;
  IRK_RETURN(763);
}
static void FUN_adjust_3340_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=36 label=35
  O r862 = ((object*) lenv) [2];
  switch (get_case (r862)) {
    case (UOTAG(0)): { // yes
      O r879 = ((object*) lenv) [2];
      O r865 = UOBJ_GET(r879,0);
      O r869 = ((object***) lenv) [1][1][9];
      O r877 = r865; // reg varref
      O r878 = (object *) 3;
      O r870 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r877)+UNTAG_INTEGER(r878));
      O r875 = r869; // reg varref
      O r876 = r870; // reg varref
      O r871 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r875)-UNTAG_INTEGER(r876));
      O r873 = top[50];
      O r874 = r871; // reg varref
      O r872 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r873)+UNTAG_INTEGER(r874));
      O r868 = r872; // reg varref
      O r867 = alloc_no_clear (UOTAG(0),1);
      r867[1] = r868;
      IRK_RETURN(867);
    } break;
    default: {
      FAIL_35();
    }
  }
}
static void FAIL_35() {
  O r882 = ((object*) lenv) [2];
  IRK_RETURN(882);
}
static void FUN_nvals_3337_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 128:
  //   (list |tuple=(product |tuple=(product string string) | int) ...t2497|)
  O r891 = alloc_cleared (TC_ENV, 2);
  r891[1] = lenv; lenv = r891;
  // env 129:
  //   (tree |tuple=(product string string) | int) string
  O r893 = allocate (TC_ENV, 3);
  O r895 = ((object****) lenv) [1][1][1][7];
  r893[2] = r895;
  O r896 = ((object**) lenv) [1][2];
  r893[3] = r896;
  O r894 = ((object***) lenv) [1][1][6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_nvals_3337_i2_76; t[4] = r891; k = t;
  r893[1] = r894[2]; lenv = r893; FUN_name_range_3335_i2_0();
}
static void FUN_nvals_3337_i2_76 (O rr) {
  O r891 = k[4]; lenv = k[2]; k = k[1];
  O r892 = rr;
  r891[2] = r892;
  // env 130:
  //   (list |tuple=(product |tuple=(product string string) | int) ...t2497|) int
  O r887 = allocate (TC_ENV, 3);
  O r889 = ((object*) lenv) [2];
  r887[2] = r889;
  O r890 = (object *) 1;
  r887[3] = r890;
  O r888 = top[10];
  r887[1] = r888[2]; lenv = r887; FUN_length_splat_272_0();
}
static void FUN_lookup_name_only_3336_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 131:
  //   (list |tuple=(product |tuple=(product t2505 string) ...t2508| t2507) |)
  O r918 = alloc_cleared (TC_ENV, 2);
  r918[1] = lenv; lenv = r918;
  // env 132:
  //   (tree |tuple=(product t2505 string) ...t2508| t2507) t2505
  O r920 = allocate (TC_ENV, 3);
  O r922 = ((object**) lenv) [1][2];
  r920[2] = r922;
  O r923 = ((object**) lenv) [1][3];
  r920[3] = r923;
  O r921 = ((object***) lenv) [1][1][6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lookup_name_only_3336_i2_77; t[4] = r918; k = t;
  r920[1] = r921[2]; lenv = r920; FUN_name_range_3335_i2_0();
}
static void FUN_lookup_name_only_3336_i2_77 (O rr) {
  O r918 = k[4]; lenv = k[2]; k = k[1];
  O r919 = rr;
  r918[2] = r919;
  O r901 = ((object*) lenv) [2];
  switch (get_case (r901)) {
    case (TC_NIL): { // nil
      O r903 = (object *) 65558;
      IRK_RETURN(903);
    } break;
    default: {
      O r917 = ((object*) lenv) [2];
      O r906 = UOBJ_GET(r917,0);
      O r916 = r906; // reg varref
      O r910 = UOBJ_GET(r916,0);
      O r915 = r906; // reg varref
      O r911 = UOBJ_GET(r915,1);
      O r914 = r911; // reg varref
      O r913 = alloc_no_clear (UOTAG(0),1);
      r913[1] = r914;
      IRK_RETURN(913);
    } break;
  }
}
static void FUN_name_range_3335_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 133:
  //   (list |tuple=(product |tuple=(product t2427 string) ...t2434| t2442) ...t2449|)
  O r1054 = allocate (TC_ENV, 2);
  r1054[1] = lenv; lenv = r1054;
  O r1055 = (object *) 10;
  r1054[2] = r1055;
  // env 134:
  //   |tuple=(product t2427 string) ...t2434| |tuple=(product t2427 string) ...t2434| (|tuple=(product t2427 string) ...t2434| t2442 -> undefined)
  O r1039 = alloc_cleared (TC_ENV, 4);
  r1039[1] = lenv; lenv = r1039;
  O r1040 = allocate (UOTAG(3), 2);
  O r1052 = ((object***) lenv) [1][1][3];
  r1040[1] = r1052;
  O r1053 = (O) &constructed_17;
  r1040[2] = r1053;
  r1039[2] = r1040;
  O r1041 = allocate (UOTAG(3), 2);
  O r1050 = ((object***) lenv) [1][1][3];
  r1041[1] = r1050;
  O r1051 = (O) &constructed_16;
  r1041[2] = r1051;
  r1039[3] = r1041;
  O r1042 = allocate (TC_CLOSURE, 2);
  r1042[1] = FUN_lambda_22340_i2_0; r1042[2] = lenv;
  r1039[4] = r1042;
  // env 135:
  //   ((tree |tuple=(product t2427 string) ...t2434| t2442) -> undefined) ((tree |tuple=(product t2427 string) ...t2434| t2442) (tree |tuple=(product t2427 string) ...t2434| t2442) |tuple=(product t2427 string) ...t2434| t2442 -> undefined)
  O r941 = alloc_cleared (TC_ENV, 3);
  r941[1] = lenv; lenv = r941;
  O r942 = allocate (TC_CLOSURE, 2);
  r942[1] = FUN_walk_2987_i244_i2_0; r942[2] = lenv;
  r941[2] = r942;
  O r943 = allocate (TC_CLOSURE, 2);
  r943[1] = FUN_visit_2986_i244_i2_0; r943[2] = lenv;
  r941[3] = r943;
  // env 136:
  //   (tree |tuple=(product t2427 string) ...t2434| t2442)
  O r938 = allocate (TC_ENV, 2);
  O r940 = ((object****) lenv) [1][1][1][2];
  r938[2] = r940;
  O r939 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_name_range_3335_i2_78; ; k = t;
  r938[1] = r939[2]; lenv = r938; FUN_walk_2987_i244_i2_0();
}
static void FUN_name_range_3335_i2_78 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r937 = rr;
  lenv = lenv[1];
  O r936 = r937;
  lenv = lenv[1];
  O r929 = ((object*) lenv) [2];
  // env 137:
  //   (list |tuple=(product |tuple=(product t2427 string) ...t2434| t2442) ...t2449|) (list |tuple=(product string string) |)
  O r932 = allocate (TC_ENV, 3);
  O r934 = r929; // reg varref
  r932[2] = r934;
  O r935 = (object *) 10;
  r932[3] = r935;
  O r933 = top[9];
  r932[1] = r933[2]; lenv = r932; FUN_reverse_onto_269_0();
}
static void FUN_visit_2986_i244_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r996 = ((object***) lenv) [1][1][2];
  O r997 = ((object*) lenv) [4];
  O r992 = (object*)UITAG(1+magic_cmp(r996, r997));
  O r994 = (object *) 22;
  O r995 = r992; // reg varref
  O r993 = IRK_TEST(r994==r995);
  O r986 = r993; // reg varref
  if IRK_IS_TRUE(r986) {
    // env 138:
    //   (tree |tuple=(product t2427 string) ...t2434| t2442)
    O r988 = allocate (TC_ENV, 2);
    O r990 = ((object*) lenv) [2];
    r988[2] = r990;
    O r989 = ((object**) lenv) [1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_visit_2986_i244_i2_79; ; k = t;
    r988[1] = r989[2]; lenv = r988; FUN_walk_2987_i244_i2_0();
  } else {
    O r991 = (object *) 14;
    JUMP_43();
  }
}
static void FUN_visit_2986_i244_i2_79 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r987 = rr;
  JUMP_43();
}
static void JUMP_43() {
  O r984 = ((object*) lenv) [4];
  O r985 = ((object***) lenv) [1][1][3];
  O r980 = (object*)UITAG(1+magic_cmp(r984, r985));
  O r982 = (object *) 22;
  O r983 = r980; // reg varref
  O r981 = IRK_TEST(r982==r983);
  O r968 = r981; // reg varref
  if IRK_IS_TRUE(r968) {
    O r977 = ((object*) lenv) [4];
    O r978 = ((object***) lenv) [1][1][2];
    O r970 = (object*)UITAG(1+magic_cmp(r977, r978));
    O r975 = (object *) 22;
    O r976 = r970; // reg varref
    O r971 = IRK_TEST(r975==r976);
    O r973 = r971; // reg varref
    O r974 = (object *) 6;
    O r972 = IRK_TEST(r973==r974);
    O r969 = r972; // reg varref
    O r961 = r969;
    JUMP_42(r961);
  } else {
    O r979 = (object *) 6;
    O r961 = r979;
    JUMP_42(r961);
  }
}
static void JUMP_42(O r961) {
  if IRK_IS_TRUE(r961) {
    // env 139:
    //   |tuple=(product t2427 string) ...t2434| t2442
    O r963 = allocate (TC_ENV, 3);
    O r965 = ((object*) lenv) [4];
    r963[2] = r965;
    O r966 = ((object*) lenv) [5];
    r963[3] = r966;
    O r964 = ((object***) lenv) [1][1][4];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_visit_2986_i244_i2_80; ; k = t;
    r963[1] = r964[2]; lenv = r963; ((pfun)(r964[1]))();
  } else {
    O r967 = (object *) 14;
    JUMP_41();
  }
}
static void FUN_visit_2986_i244_i2_80 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r962 = rr;
  JUMP_41();
}
static void JUMP_41() {
  O r959 = ((object*) lenv) [4];
  O r960 = ((object***) lenv) [1][1][3];
  O r955 = (object*)UITAG(1+magic_cmp(r959, r960));
  O r957 = (object *) 22;
  O r958 = r955; // reg varref
  O r956 = IRK_TEST(r957==r958);
  O r947 = r956; // reg varref
  if IRK_IS_TRUE(r947) {
    // env 140:
    //   (tree |tuple=(product t2427 string) ...t2434| t2442)
    O r950 = allocate (TC_ENV, 2);
    O r952 = ((object*) lenv) [3];
    r950[2] = r952;
    O r951 = ((object**) lenv) [1][2];
    r950[1] = r951[2]; lenv = r950; FUN_walk_2987_i244_i2_0();
  } else {
    O r954 = (object *) 14;
    IRK_RETURN(954);
  }
}
static void FUN_walk_2987_i244_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1000 = ((object*) lenv) [2];
  switch (get_case (r1000)) {
    case (UITAG(2)): { // empty
      O r1002 = (object *) 14;
      IRK_RETURN(1002);
    } break;
    case (UOTAG(0)): { // red
      O r1020 = ((object*) lenv) [2];
      O r1005 = UOBJ_GET(r1020,0);
      O r1019 = ((object*) lenv) [2];
      O r1006 = UOBJ_GET(r1019,1);
      O r1018 = ((object*) lenv) [2];
      O r1007 = UOBJ_GET(r1018,2);
      O r1017 = ((object*) lenv) [2];
      O r1008 = UOBJ_GET(r1017,3);
      // env 141:
      //   (tree |tuple=(product t2427 string) ...t2434| t2442) (tree |tuple=(product t2427 string) ...t2434| t2442) |tuple=(product t2427 string) ...t2434| t2442
      O r1011 = allocate (TC_ENV, 5);
      O r1013 = r1005; // reg varref
      r1011[2] = r1013;
      O r1014 = r1006; // reg varref
      r1011[3] = r1014;
      O r1015 = r1007; // reg varref
      r1011[4] = r1015;
      O r1016 = r1008; // reg varref
      r1011[5] = r1016;
      O r1012 = ((object**) lenv) [1][3];
      r1011[1] = r1012[2]; lenv = r1011; FUN_visit_2986_i244_i2_0();
    } break;
    default: {
      O r1038 = ((object*) lenv) [2];
      O r1023 = UOBJ_GET(r1038,0);
      O r1037 = ((object*) lenv) [2];
      O r1024 = UOBJ_GET(r1037,1);
      O r1036 = ((object*) lenv) [2];
      O r1025 = UOBJ_GET(r1036,2);
      O r1035 = ((object*) lenv) [2];
      O r1026 = UOBJ_GET(r1035,3);
      // env 142:
      //   (tree |tuple=(product t2427 string) ...t2434| t2442) (tree |tuple=(product t2427 string) ...t2434| t2442) |tuple=(product t2427 string) ...t2434| t2442
      O r1029 = allocate (TC_ENV, 5);
      O r1031 = r1023; // reg varref
      r1029[2] = r1031;
      O r1032 = r1024; // reg varref
      r1029[3] = r1032;
      O r1033 = r1025; // reg varref
      r1029[4] = r1033;
      O r1034 = r1026; // reg varref
      r1029[5] = r1034;
      O r1030 = ((object**) lenv) [1][3];
      r1029[1] = r1030[2]; lenv = r1029; FUN_visit_2986_i244_i2_0();
    } break;
  }
}
static void FUN_lambda_22340_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1046 = allocate (UOTAG(3), 2);
  O r1048 = ((object*) lenv) [2];
  r1046[1] = r1048;
  O r1049 = ((object*) lenv) [3];
  r1046[2] = r1049;
  O r1047 = ((object***) lenv) [1][1][2];
  O r1045 = alloc_no_clear (TC_PAIR,2);
  r1045[1] = r1046;
  r1045[2] = r1047;
  ((object***) lenv) [1][1][2] = r1045;
  O r1044 = (object *) TC_UNDEFINED;
  IRK_RETURN(1044);
}
static void FUN_set_size_3332_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 143:
  //   string
  O r1083 = alloc_cleared (TC_ENV, 2);
  r1083[1] = lenv; lenv = r1083;
  // env 144:
  //   (list string)
  O r1085 = alloc_cleared (TC_ENV, 2);
  O r1088 = (O) &constructed_18;
  // env 145:
  //   int
  O r1092 = allocate (TC_ENV, 2);
  O r1094 = ((object**) lenv) [1][2];
  r1092[2] = r1094;
  O r1093 = top[33];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_set_size_3332_i2_81; t[4] = r1083; t[5] = r1085; t[6] = r1088; k = t;
  r1092[1] = r1093[2]; lenv = r1092; FUN_int__3estring_363_0();
}
static void FUN_set_size_3332_i2_81 (O rr) {
  O r1083 = k[4]; O r1085 = k[5]; O r1088 = k[6]; lenv = k[2]; k = k[1];
  O r1090 = rr;
  O r1091 = (object*) constructed_3[0];
  O r1089 = alloc_no_clear (TC_PAIR,2);
  r1089[1] = r1090;
  r1089[2] = r1091;
  O r1087 = alloc_no_clear (TC_PAIR,2);
  r1087[1] = r1088;
  r1087[2] = r1089;
  r1085[2] = r1087;
  O r1086 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_set_size_3332_i2_82; t[4] = r1083; k = t;
  r1085[1] = r1086[2]; lenv = r1085; FUN_string_concat_338_0();
}
static void FUN_set_size_3332_i2_82 (O rr) {
  O r1083 = k[4]; lenv = k[2]; k = k[1];
  O r1084 = rr;
  r1083[2] = r1084;
  O r1080 = ((object*) lenv) [2];
  O r1082 = ((object*) lenv) [2];
  O r1081 = TAG_INTEGER((irk_int)((irk_string*)(r1082))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1080))->data, 1, UNTAG_INTEGER(r1081), stdout));
  O r1079 = (object *) 14;
  lenv = lenv[1];
  O r1078 = ((object*) lenv) [2];
  ((object***) lenv) [1][1][6] = r1078;
  // env 146:
  //   ( -> undefined)
  O r1062 = alloc_cleared (TC_ENV, 2);
  r1062[1] = lenv; lenv = r1062;
  O r1063 = allocate (TC_CLOSURE, 2);
  r1063[1] = FUN__24loop102_3365_i2_0; r1063[2] = lenv;
  r1062[2] = r1063;
  O r1061 = ((object*) lenv) [2];
  lenv = r1061[2]; FUN__24loop102_3365_i2_0();
}
static void FUN__24loop102_3365_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1073 = ((object****) lenv) [1][1][1][10];
  O r1074 = ((object****) lenv) [1][1][1][6];
  O r1076 = r1073; // reg varref
  O r1077 = r1074; // reg varref
  O r1075 = IRK_TEST(UNTAG_INTEGER(r1076)>UNTAG_INTEGER(r1077));
  O r1066 = r1075; // reg varref
  if IRK_IS_TRUE(r1066) {
    O r1070 = ((object***) lenv) [1][1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop102_3365_i2_83; ; k = t;
    lenv = r1070[2]; FUN_evict_one_3328_i2_0();
  } else {
    O r1072 = (object *) 14;
    IRK_RETURN(1072);
  }
}
static void FUN__24loop102_3365_i2_83 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop102_3365_i2_0();
}
static void FUN_get_3331_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 147:
  //   string
  O r1142 = alloc_cleared (TC_ENV, 2);
  r1142[1] = lenv; lenv = r1142;
  // env 148:
  //   (list string)
  O r1144 = alloc_cleared (TC_ENV, 2);
  O r1147 = (O) &constructed_20;
  // env 149:
  //   int
  O r1158 = allocate (TC_ENV, 2);
  O r1160 = ((object**) lenv) [1][2];
  r1158[2] = r1160;
  O r1159 = top[33];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_3331_i2_84; t[4] = r1142; t[5] = r1144; t[6] = r1147; k = t;
  r1158[1] = r1159[2]; lenv = r1158; FUN_int__3estring_363_0();
}
static void FUN_get_3331_i2_84 (O rr) {
  O r1142 = k[4]; O r1144 = k[5]; O r1147 = k[6]; lenv = k[2]; k = k[1];
  O r1149 = rr;
  O r1151 = (O) &constructed_19;
  // env 150:
  //   int
  O r1155 = allocate (TC_ENV, 2);
  O r1157 = top[50];
  r1155[2] = r1157;
  O r1156 = top[33];
  O t = allocate (TC_SAVE, 8);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_3331_i2_85; t[4] = r1142; t[5] = r1144; t[6] = r1147; t[7] = r1149; t[8] = r1151; k = t;
  r1155[1] = r1156[2]; lenv = r1155; FUN_int__3estring_363_0();
}
static void FUN_get_3331_i2_85 (O rr) {
  O r1142 = k[4]; O r1144 = k[5]; O r1147 = k[6]; O r1149 = k[7]; O r1151 = k[8]; lenv = k[2]; k = k[1];
  O r1153 = rr;
  O r1154 = (object*) constructed_3[0];
  O r1152 = alloc_no_clear (TC_PAIR,2);
  r1152[1] = r1153;
  r1152[2] = r1154;
  O r1150 = alloc_no_clear (TC_PAIR,2);
  r1150[1] = r1151;
  r1150[2] = r1152;
  O r1148 = alloc_no_clear (TC_PAIR,2);
  r1148[1] = r1149;
  r1148[2] = r1150;
  O r1146 = alloc_no_clear (TC_PAIR,2);
  r1146[1] = r1147;
  r1146[2] = r1148;
  r1144[2] = r1146;
  O r1145 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_3331_i2_86; t[4] = r1142; k = t;
  r1144[1] = r1145[2]; lenv = r1144; FUN_string_concat_338_0();
}
static void FUN_get_3331_i2_86 (O rr) {
  O r1142 = k[4]; lenv = k[2]; k = k[1];
  O r1143 = rr;
  r1142[2] = r1143;
  O r1139 = ((object*) lenv) [2];
  O r1141 = ((object*) lenv) [2];
  O r1140 = TAG_INTEGER((irk_int)((irk_string*)(r1141))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1139))->data, 1, UNTAG_INTEGER(r1140), stdout));
  O r1138 = (object *) 14;
  lenv = lenv[1];
  O r1098 = ((object*) lenv) [2];
  O r1099 = top[50];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1098)<UNTAG_INTEGER(r1099))) {
    O r1102 = top[49];
    O r1103 = ((object*) lenv) [2];
    range_check (GET_TUPLE_LENGTH(*(object*)r1102), unbox(r1103));
    O r1101 = ((irk_vector*)r1102)->val[unbox(r1103)];
    IRK_RETURN(1101);
  } else {
    // env 151:
    //   int int int int (maybe |tuple=(product string string) |)
    O r1121 = alloc_cleared (TC_ENV, 6);
    r1121[1] = lenv; lenv = r1121;
    O r1136 = ((object**) lenv) [1][2];
    O r1137 = top[50];
    O r1122 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1136)-UNTAG_INTEGER(r1137));
    r1121[2] = r1122;
    O r1123 = ((object****) lenv) [1][1][1][9];
    r1121[3] = r1123;
    O r1134 = ((object*) lenv) [2];
    O r1135 = (object *) 3;
    O r1124 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1134)+UNTAG_INTEGER(r1135));
    r1121[4] = r1124;
    O r1132 = ((object*) lenv) [3];
    O r1133 = ((object*) lenv) [4];
    O r1125 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1132)-UNTAG_INTEGER(r1133));
    r1121[5] = r1125;
    // env 152:
    //   (tree int |tuple=(product string string) |) (int int -> cmp) int
    O r1127 = allocate (TC_ENV, 4);
    O r1129 = ((object****) lenv) [1][1][1][8];
    r1127[2] = r1129;
    O r1130 = top[7];
    r1127[3] = r1130;
    O r1131 = ((object*) lenv) [5];
    r1127[4] = r1131;
    O r1128 = top[38];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_get_3331_i2_87; t[4] = r1121; k = t;
    r1127[1] = r1128[2]; lenv = r1127; FUN_tree_2fmember_570_0();
  }
}
static void FUN_get_3331_i2_87 (O rr) {
  O r1121 = k[4]; lenv = k[2]; k = k[1];
  O r1126 = rr;
  r1121[6] = r1126;
  O r1108 = ((object*) lenv) [6];
  switch (get_case (r1108)) {
    case (UOTAG(0)): { // yes
      O r1114 = ((object*) lenv) [6];
      O r1111 = UOBJ_GET(r1114,0);
      O r1113 = r1111; // reg varref
      IRK_RETURN(1113);
    } break;
    default: {
      // env 153:
      //   |Hpack/BadIndex=int ...t2410|
      O r1117 = alloc_cleared (TC_ENV, 2);
      O r1119 = allocate (UOTAG(9), 1);
      O r1120 = ((object**) lenv) [1][2];
      r1119[1] = r1120;
      r1117[2] = r1119;
      O r1118 = top[3];
      r1117[1] = r1118[2]; lenv = r1117; ((pfun)(r1118[1]))();
    } break;
  }
}
static void FUN_set_3330_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 154:
  //   |tuple=(product string string) | int int int int
  O r1211 = allocate (TC_ENV, 6);
  r1211[1] = lenv; lenv = r1211;
  O r1212 = allocate (UOTAG(3), 2);
  O r1223 = ((object**) lenv) [1][2];
  r1212[1] = r1223;
  O r1224 = ((object**) lenv) [1][3];
  r1212[2] = r1224;
  r1211[2] = r1212;
  O r1222 = ((object**) lenv) [1][2];
  O r1213 = TAG_INTEGER((irk_int)((irk_string*)(r1222))->len);
  r1211[3] = r1213;
  O r1221 = ((object**) lenv) [1][3];
  O r1214 = TAG_INTEGER((irk_int)((irk_string*)(r1221))->len);
  r1211[4] = r1214;
  O r1219 = ((object*) lenv) [3];
  O r1220 = ((object*) lenv) [4];
  O r1215 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1219)+UNTAG_INTEGER(r1220));
  r1211[5] = r1215;
  O r1217 = (object *) 65;
  O r1218 = ((object*) lenv) [5];
  O r1216 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1217)+UNTAG_INTEGER(r1218));
  r1211[6] = r1216;
  // env 155:
  //   ( -> undefined)
  O r1192 = alloc_cleared (TC_ENV, 2);
  r1192[1] = lenv; lenv = r1192;
  O r1193 = allocate (TC_CLOSURE, 2);
  r1193[1] = FUN__24loop101_3358_i2_0; r1193[2] = lenv;
  r1192[2] = r1193;
  O r1191 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_set_3330_i2_88; ; k = t;
  lenv = r1191[2]; FUN__24loop101_3358_i2_0();
}
static void FUN_set_3330_i2_88 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1190 = rr;
  lenv = lenv[1];
  // env 156:
  //   (tree |tuple=(product string string) | int) (|tuple=(product string string) | |tuple=(product string string) | -> cmp) |tuple=(product string string) | int
  O r1184 = allocate (TC_ENV, 5);
  O r1186 = ((object****) lenv) [1][1][1][7];
  r1184[2] = r1186;
  O r1187 = top[8];
  r1184[3] = r1187;
  O r1188 = ((object*) lenv) [2];
  r1184[4] = r1188;
  O r1189 = ((object****) lenv) [1][1][1][9];
  r1184[5] = r1189;
  O r1185 = top[36];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_set_3330_i2_89; ; k = t;
  r1184[1] = r1185[2]; lenv = r1184; FUN_tree_2finsert_565_0();
}
static void FUN_set_3330_i2_89 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1183 = rr;
  ((object****) lenv) [1][1][1][7] = r1183;
  // env 157:
  //   (tree int |tuple=(product string string) |) (int int -> cmp) int |tuple=(product string string) |
  O r1177 = allocate (TC_ENV, 5);
  O r1179 = ((object****) lenv) [1][1][1][8];
  r1177[2] = r1179;
  O r1180 = top[7];
  r1177[3] = r1180;
  O r1181 = ((object****) lenv) [1][1][1][9];
  r1177[4] = r1181;
  O r1182 = ((object*) lenv) [2];
  r1177[5] = r1182;
  O r1178 = top[36];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_set_3330_i2_90; ; k = t;
  r1177[1] = r1178[2]; lenv = r1177; FUN_tree_2finsert_565_0();
}
static void FUN_set_3330_i2_90 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1176 = rr;
  ((object****) lenv) [1][1][1][8] = r1176;
  O r1172 = ((object****) lenv) [1][1][1][9];
  O r1174 = r1172; // reg varref
  O r1175 = (object *) 3;
  O r1173 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1174)+UNTAG_INTEGER(r1175));
  O r1171 = r1173; // reg varref
  ((object****) lenv) [1][1][1][9] = r1171;
  O r1167 = ((object****) lenv) [1][1][1][10];
  O r1169 = r1167; // reg varref
  O r1170 = ((object*) lenv) [6];
  O r1168 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1169)+UNTAG_INTEGER(r1170));
  O r1166 = r1168; // reg varref
  ((object****) lenv) [1][1][1][10] = r1166;
  O r1165 = (object *) TC_UNDEFINED;
  IRK_RETURN(1165);
}
static void FUN__24loop101_3358_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1203 = ((object*****) lenv) [1][1][1][1][10];
  O r1209 = r1203; // reg varref
  O r1210 = ((object**) lenv) [1][6];
  O r1204 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1209)+UNTAG_INTEGER(r1210));
  O r1205 = ((object*****) lenv) [1][1][1][1][6];
  O r1207 = r1204; // reg varref
  O r1208 = r1205; // reg varref
  O r1206 = IRK_TEST(UNTAG_INTEGER(r1207)>UNTAG_INTEGER(r1208));
  O r1196 = r1206; // reg varref
  if IRK_IS_TRUE(r1196) {
    O r1200 = ((object****) lenv) [1][1][1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop101_3358_i2_91; ; k = t;
    lenv = r1200[2]; FUN_evict_one_3328_i2_0();
  } else {
    O r1202 = (object *) 14;
    IRK_RETURN(1202);
  }
}
static void FUN__24loop101_3358_i2_91 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop101_3358_i2_0();
}
static void FUN_evict_one_3328_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 158:
  //   |tuple=(product int |tuple=(product string string) |) |
  O r1289 = alloc_cleared (TC_ENV, 2);
  r1289[1] = lenv; lenv = r1289;
  // env 159:
  //   (tree int |tuple=(product string string) |)
  O r1291 = allocate (TC_ENV, 2);
  O r1293 = ((object***) lenv) [1][1][8];
  r1291[2] = r1293;
  O r1292 = top[39];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_evict_one_3328_i2_92; t[4] = r1289; k = t;
  r1291[1] = r1292[2]; lenv = r1291; FUN_tree_2fmin_572_0();
}
static void FUN_evict_one_3328_i2_92 (O rr) {
  O r1289 = k[4]; lenv = k[2]; k = k[1];
  O r1290 = rr;
  r1289[2] = r1290;
  // env 160:
  //   int |tuple=(product string string) |
  O r1284 = alloc_cleared (TC_ENV, 3);
  r1284[1] = lenv; lenv = r1284;
  O r1288 = ((object**) lenv) [1][2];
  O r1285 = UOBJ_GET(r1288,0);
  r1284[2] = r1285;
  O r1287 = ((object**) lenv) [1][2];
  O r1286 = UOBJ_GET(r1287,1);
  r1284[3] = r1286;
  // env 161:
  //   string string int int int int
  O r1269 = alloc_cleared (TC_ENV, 7);
  r1269[1] = lenv; lenv = r1269;
  O r1283 = ((object**) lenv) [1][3];
  O r1270 = UOBJ_GET(r1283,0);
  r1269[2] = r1270;
  O r1282 = ((object**) lenv) [1][3];
  O r1271 = UOBJ_GET(r1282,1);
  r1269[3] = r1271;
  O r1281 = ((object*) lenv) [2];
  O r1272 = TAG_INTEGER((irk_int)((irk_string*)(r1281))->len);
  r1269[4] = r1272;
  O r1280 = ((object*) lenv) [3];
  O r1273 = TAG_INTEGER((irk_int)((irk_string*)(r1280))->len);
  r1269[5] = r1273;
  O r1278 = ((object*) lenv) [4];
  O r1279 = ((object*) lenv) [5];
  O r1274 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1278)+UNTAG_INTEGER(r1279));
  r1269[6] = r1274;
  O r1276 = (object *) 65;
  O r1277 = ((object*) lenv) [6];
  O r1275 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1276)+UNTAG_INTEGER(r1277));
  r1269[7] = r1275;
  // env 162:
  //   string
  O r1257 = alloc_cleared (TC_ENV, 2);
  r1257[1] = lenv; lenv = r1257;
  // env 163:
  //   (list string)
  O r1259 = alloc_cleared (TC_ENV, 2);
  O r1262 = (O) &constructed_23;
  // env 164:
  //   int
  O r1266 = allocate (TC_ENV, 2);
  O r1268 = ((object**) lenv) [1][7];
  r1266[2] = r1268;
  O r1267 = top[33];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_evict_one_3328_i2_93; t[4] = r1257; t[5] = r1259; t[6] = r1262; k = t;
  r1266[1] = r1267[2]; lenv = r1266; FUN_int__3estring_363_0();
}
static void FUN_evict_one_3328_i2_93 (O rr) {
  O r1257 = k[4]; O r1259 = k[5]; O r1262 = k[6]; lenv = k[2]; k = k[1];
  O r1264 = rr;
  O r1265 = (object*) constructed_22[0];
  O r1263 = alloc_no_clear (TC_PAIR,2);
  r1263[1] = r1264;
  r1263[2] = r1265;
  O r1261 = alloc_no_clear (TC_PAIR,2);
  r1261[1] = r1262;
  r1261[2] = r1263;
  r1259[2] = r1261;
  O r1260 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_evict_one_3328_i2_94; t[4] = r1257; k = t;
  r1259[1] = r1260[2]; lenv = r1259; FUN_string_concat_338_0();
}
static void FUN_evict_one_3328_i2_94 (O rr) {
  O r1257 = k[4]; lenv = k[2]; k = k[1];
  O r1258 = rr;
  r1257[2] = r1258;
  O r1254 = ((object*) lenv) [2];
  O r1256 = ((object*) lenv) [2];
  O r1255 = TAG_INTEGER((irk_int)((irk_string*)(r1256))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1254))->data, 1, UNTAG_INTEGER(r1255), stdout));
  O r1253 = (object *) 14;
  lenv = lenv[1];
  // env 165:
  //   (tree |tuple=(product string string) | int) (|tuple=(product string string) | |tuple=(product string string) | -> cmp) |tuple=(product string string) |
  O r1248 = allocate (TC_ENV, 4);
  O r1250 = ((object*****) lenv) [1][1][1][1][7];
  r1248[2] = r1250;
  O r1251 = top[8];
  r1248[3] = r1251;
  O r1252 = ((object**) lenv) [1][3];
  r1248[4] = r1252;
  O r1249 = top[37];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_evict_one_3328_i2_95; ; k = t;
  r1248[1] = r1249[2]; lenv = r1248; FUN_tree_2fdelete_567_0();
}
static void FUN_evict_one_3328_i2_95 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1247 = rr;
  ((object*****) lenv) [1][1][1][1][7] = r1247;
  // env 166:
  //   (tree int |tuple=(product string string) |) (int int -> cmp) int
  O r1242 = allocate (TC_ENV, 4);
  O r1244 = ((object*****) lenv) [1][1][1][1][8];
  r1242[2] = r1244;
  O r1245 = top[7];
  r1242[3] = r1245;
  O r1246 = ((object**) lenv) [1][2];
  r1242[4] = r1246;
  O r1243 = top[37];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_evict_one_3328_i2_96; ; k = t;
  r1242[1] = r1243[2]; lenv = r1242; FUN_tree_2fdelete_567_0();
}
static void FUN_evict_one_3328_i2_96 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1241 = rr;
  ((object*****) lenv) [1][1][1][1][8] = r1241;
  O r1237 = ((object*****) lenv) [1][1][1][1][10];
  O r1239 = r1237; // reg varref
  O r1240 = ((object*) lenv) [7];
  O r1238 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1239)-UNTAG_INTEGER(r1240));
  O r1236 = r1238; // reg varref
  ((object*****) lenv) [1][1][1][1][10] = r1236;
  O r1235 = (object *) TC_UNDEFINED;
  IRK_RETURN(1235);
}
static void FUN__24loop128_3534_0 (void) {
  O r1371 = ((object*) lenv) [2];
  O r1372 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1371)==UNTAG_INTEGER(r1372))) {
    O r1374 = (object *) 14;
    IRK_RETURN(1374);
  } else {
    O r1382 = ((object***) lenv) [1][1][3];
    O r1394 = ((object*) lenv) [2];
    O r1395 = (object *) 3;
    O r1383 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1394)+UNTAG_INTEGER(r1395));
    O r1392 = (object *) 3;
    O r1393 = ((object*) lenv) [2];
    O r1385 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1392)+UNTAG_INTEGER(r1393));
    O r1390 = (object *) 3;
    O r1391 = r1385; // reg varref
    O r1386 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1390)<<UNTAG_INTEGER(r1391));
    O r1388 = r1386; // reg varref
    O r1389 = (object *) 3;
    O r1387 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1388)-UNTAG_INTEGER(r1389));
    O r1384 = r1387; // reg varref
    range_check (GET_TUPLE_LENGTH(*(object*)r1382), unbox(r1383));
    ((irk_vector*)r1382)->val[unbox(r1383)] = r1384;
    O r1380 = ((object*) lenv) [2];
    O r1381 = (object *) 3;
    O r1378 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1380)+UNTAG_INTEGER(r1381));
    O r1379 = ((object*) lenv) [3];
    lenv[2] = r1378;
    lenv[3] = r1379;
    FUN__24loop128_3534_0();
  }
}
static void FUN_recur_3281_i376_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 167:
  //   char
  O r1477 = alloc_cleared (TC_ENV, 2);
  r1477[1] = lenv; lenv = r1477;
  // env 168:
  //   string int
  O r1479 = allocate (TC_ENV, 3);
  O r1481 = ((object***) lenv) [1][1][2];
  r1479[2] = r1481;
  O r1482 = ((object***) lenv) [1][1][3];
  r1479[3] = r1482;
  O r1480 = top[18];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_97; t[4] = r1477; k = t;
  r1479[1] = r1480[2]; lenv = r1479; FUN_string_ref_336_0();
}
static void FUN_recur_3281_i376_97 (O rr) {
  O r1477 = k[4]; lenv = k[2]; k = k[1];
  O r1478 = rr;
  r1477[2] = r1478;
  // fatbar jn=56 label=55
  O r1413 = ((object*) lenv) [2];
  O r1414 = (object *) 5898242;
  if IRK_IS_TRUE(IRK_TEST(r1413==r1414)) {
    O r1416 = (object*) constructed_29[0];
    IRK_RETURN(1416);
  } else {
    FAIL_55();
  }
}
static void FAIL_55() {
  // fatbar jn=60 label=59
  O r1422 = ((object*) lenv) [2];
  O r1423 = (object *) 3014658;
  if IRK_IS_TRUE(IRK_TEST(r1422==r1423)) {
    O r1432 = ((object***) lenv) [1][1][3];
    O r1434 = (object *) 3;
    O r1435 = r1432; // reg varref
    O r1433 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1434)+UNTAG_INTEGER(r1435));
    O r1431 = r1433; // reg varref
    ((object***) lenv) [1][1][3] = r1431;
    O r1430 = ((object**) lenv) [1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_98; ; k = t;
    lenv = r1430[2]; FUN_recur_3281_i376_0();
  } else {
    FAIL_59();
  }
}
static void FUN_recur_3281_i376_98 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1427 = rr;
  O r1429 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_99; t[4] = r1427; k = t;
  lenv = r1429[2]; FUN_recur_3281_i376_0();
}
static void FUN_recur_3281_i376_99 (O rr) {
  O r1427 = k[4]; lenv = k[2]; k = k[1];
  O r1428 = rr;
  O r1426 = alloc_no_clear (UOTAG(0),2);
  r1426[1] = r1427;
  r1426[2] = r1428;
  IRK_RETURN(1426);
}
static void FAIL_59() {
  // env 169:
  //   hufftree
  O r1447 = alloc_cleared (TC_ENV, 2);
  r1447[1] = lenv; lenv = r1447;
  // env 170:
  //   string int int int int
  O r1453 = alloc_cleared (TC_ENV, 6);
  r1453[1] = lenv; lenv = r1453;
  O r1454 = ((object*****) lenv) [1][1][1][1][2];
  r1453[2] = r1454;
  O r1455 = ((object*****) lenv) [1][1][1][1][3];
  r1453[3] = r1455;
  // env 171:
  //   char
  O r1470 = alloc_cleared (TC_ENV, 2);
  // env 172:
  //   string int
  O r1473 = allocate (TC_ENV, 3);
  O r1475 = ((object*) lenv) [2];
  r1473[2] = r1475;
  O r1476 = ((object*) lenv) [3];
  r1473[3] = r1476;
  O r1474 = top[18];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_100; t[4] = r1447; t[5] = r1453; t[6] = r1470; k = t;
  r1473[1] = r1474[2]; lenv = r1473; FUN_string_ref_336_0();
}
static void FUN_recur_3281_i376_100 (O rr) {
  O r1447 = k[4]; O r1453 = k[5]; O r1470 = k[6]; lenv = k[2]; k = k[1];
  O r1472 = rr;
  r1470[2] = r1472;
  O r1471 = top[47];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_101; t[4] = r1447; t[5] = r1453; k = t;
  r1470[1] = r1471[2]; lenv = r1470; FUN_hexdig__3eint_652_0();
}
static void FUN_recur_3281_i376_101 (O rr) {
  O r1447 = k[4]; O r1453 = k[5]; lenv = k[2]; k = k[1];
  O r1456 = rr;
  r1453[4] = r1456;
  O r1468 = ((object*) lenv) [4];
  O r1469 = (object *) 9;
  O r1457 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1468)<<UNTAG_INTEGER(r1469));
  r1453[5] = r1457;
  // env 173:
  //   char
  O r1459 = alloc_cleared (TC_ENV, 2);
  // env 174:
  //   string int
  O r1462 = allocate (TC_ENV, 3);
  O r1464 = ((object*) lenv) [2];
  r1462[2] = r1464;
  O r1466 = ((object*) lenv) [3];
  O r1467 = (object *) 3;
  O r1465 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1466)+UNTAG_INTEGER(r1467));
  r1462[3] = r1465;
  O r1463 = top[18];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_102; t[4] = r1447; t[5] = r1453; t[6] = r1459; k = t;
  r1462[1] = r1463[2]; lenv = r1462; FUN_string_ref_336_0();
}
static void FUN_recur_3281_i376_102 (O rr) {
  O r1447 = k[4]; O r1453 = k[5]; O r1459 = k[6]; lenv = k[2]; k = k[1];
  O r1461 = rr;
  r1459[2] = r1461;
  O r1460 = top[47];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_recur_3281_i376_103; t[4] = r1447; t[5] = r1453; k = t;
  r1459[1] = r1460[2]; lenv = r1459; FUN_hexdig__3eint_652_0();
}
static void FUN_recur_3281_i376_103 (O rr) {
  O r1447 = k[4]; O r1453 = k[5]; lenv = k[2]; k = k[1];
  O r1458 = rr;
  r1453[6] = r1458;
  O r1451 = ((object*) lenv) [5];
  O r1452 = ((object*) lenv) [6];
  O r1450 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1451)|UNTAG_INTEGER(r1452));
  lenv = lenv[1];
  O r1449 = r1450;
  O r1448 = alloc_no_clear (UOTAG(1),1);
  r1448[1] = r1449;
  r1447[2] = r1448;
  O r1443 = ((object****) lenv) [1][1][1][3];
  O r1445 = r1443; // reg varref
  O r1446 = (object *) 5;
  O r1444 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1445)+UNTAG_INTEGER(r1446));
  O r1442 = r1444; // reg varref
  ((object****) lenv) [1][1][1][3] = r1442;
  O r1441 = ((object*) lenv) [2];
  IRK_RETURN(1441);
}
static void FUN__24loop126_3522_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1499 = ((object*) lenv) [2];
  O r1500 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1499)==UNTAG_INTEGER(r1500))) {
    O r1502 = (object *) 14;
    IRK_RETURN(1502);
  } else {
    // env 175:
    //   (tree |tuple=(product string string) | int) (|tuple=(product string string) | |tuple=(product string string) | -> cmp) |tuple=(product string string) | int
    O r1511 = alloc_cleared (TC_ENV, 5);
    O r1513 = ((object***) lenv) [1][1][2];
    r1511[2] = r1513;
    O r1514 = top[8];
    r1511[3] = r1514;
    O r1517 = top[49];
    O r1518 = ((object*) lenv) [2];
    range_check (GET_TUPLE_LENGTH(*(object*)r1517), unbox(r1518));
    O r1515 = ((irk_vector*)r1517)->val[unbox(r1518)];
    r1511[4] = r1515;
    O r1516 = ((object*) lenv) [2];
    r1511[5] = r1516;
    O r1512 = top[36];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop126_3522_104; ; k = t;
    r1511[1] = r1512[2]; lenv = r1511; FUN_tree_2finsert_565_0();
  }
}
static void FUN__24loop126_3522_104 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1510 = rr;
  ((object***) lenv) [1][1][2] = r1510;
  O r1508 = ((object*) lenv) [2];
  O r1509 = (object *) 3;
  O r1506 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1508)+UNTAG_INTEGER(r1509));
  O r1507 = ((object*) lenv) [3];
  lenv[2] = r1506;
  lenv[3] = r1507;
  FUN__24loop126_3522_0();
}
static void FUN_recur_1164_i2_0 (void) {
  O r1549 = ((object*) lenv) [4];
  switch (get_case (r1549)) {
    case (TC_NIL): { // nil
      O r1551 = ((object*) lenv) [2];
      IRK_RETURN(1551);
    } break;
    default: {
      O r1568 = ((object*) lenv) [4];
      O r1554 = UOBJ_GET(r1568,0);
      O r1567 = ((object*) lenv) [4];
      O r1555 = UOBJ_GET(r1567,1);
      O r1564 = ((object*) lenv) [2];
      O r1565 = ((object*) lenv) [3];
      O r1566 = r1554; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r1564), unbox(r1565));
      ((irk_vector*)r1564)->val[unbox(r1565)] = r1566;
      O r1559 = ((object*) lenv) [2];
      O r1562 = ((object*) lenv) [3];
      O r1563 = (object *) 3;
      O r1560 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1562)+UNTAG_INTEGER(r1563));
      O r1561 = r1555; // reg varref
      lenv[2] = r1559;
      lenv[3] = r1560;
      lenv[4] = r1561;
      FUN_recur_1164_i2_0();
    } break;
  }
}
static void FUN_hexdig__3eint_652_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=68 label=67
  O r1823 = ((object*) lenv) [2];
  O r1824 = (object *) 6684674;
  if IRK_IS_TRUE(IRK_TEST(r1823==r1824)) {
    O r1826 = (object *) 31;
    IRK_RETURN(1826);
  } else {
    FAIL_67();
  }
}
static void FAIL_67() {
  // fatbar jn=72 label=71
  O r1832 = ((object*) lenv) [2];
  O r1833 = (object *) 6619138;
  if IRK_IS_TRUE(IRK_TEST(r1832==r1833)) {
    O r1835 = (object *) 29;
    IRK_RETURN(1835);
  } else {
    FAIL_71();
  }
}
static void FAIL_71() {
  // fatbar jn=76 label=75
  O r1841 = ((object*) lenv) [2];
  O r1842 = (object *) 6553602;
  if IRK_IS_TRUE(IRK_TEST(r1841==r1842)) {
    O r1844 = (object *) 27;
    IRK_RETURN(1844);
  } else {
    FAIL_75();
  }
}
static void FAIL_75() {
  // fatbar jn=80 label=79
  O r1850 = ((object*) lenv) [2];
  O r1851 = (object *) 6488066;
  if IRK_IS_TRUE(IRK_TEST(r1850==r1851)) {
    O r1853 = (object *) 25;
    IRK_RETURN(1853);
  } else {
    FAIL_79();
  }
}
static void FAIL_79() {
  // fatbar jn=84 label=83
  O r1859 = ((object*) lenv) [2];
  O r1860 = (object *) 6422530;
  if IRK_IS_TRUE(IRK_TEST(r1859==r1860)) {
    O r1862 = (object *) 23;
    IRK_RETURN(1862);
  } else {
    FAIL_83();
  }
}
static void FAIL_83() {
  // fatbar jn=88 label=87
  O r1868 = ((object*) lenv) [2];
  O r1869 = (object *) 6356994;
  if IRK_IS_TRUE(IRK_TEST(r1868==r1869)) {
    O r1871 = (object *) 21;
    IRK_RETURN(1871);
  } else {
    FAIL_87();
  }
}
static void FAIL_87() {
  // fatbar jn=92 label=91
  O r1877 = ((object*) lenv) [2];
  O r1878 = (object *) 3735554;
  if IRK_IS_TRUE(IRK_TEST(r1877==r1878)) {
    O r1880 = (object *) 19;
    IRK_RETURN(1880);
  } else {
    FAIL_91();
  }
}
static void FAIL_91() {
  // fatbar jn=96 label=95
  O r1886 = ((object*) lenv) [2];
  O r1887 = (object *) 3670018;
  if IRK_IS_TRUE(IRK_TEST(r1886==r1887)) {
    O r1889 = (object *) 17;
    IRK_RETURN(1889);
  } else {
    FAIL_95();
  }
}
static void FAIL_95() {
  // fatbar jn=100 label=99
  O r1895 = ((object*) lenv) [2];
  O r1896 = (object *) 3604482;
  if IRK_IS_TRUE(IRK_TEST(r1895==r1896)) {
    O r1898 = (object *) 15;
    IRK_RETURN(1898);
  } else {
    FAIL_99();
  }
}
static void FAIL_99() {
  // fatbar jn=104 label=103
  O r1904 = ((object*) lenv) [2];
  O r1905 = (object *) 3538946;
  if IRK_IS_TRUE(IRK_TEST(r1904==r1905)) {
    O r1907 = (object *) 13;
    IRK_RETURN(1907);
  } else {
    FAIL_103();
  }
}
static void FAIL_103() {
  // fatbar jn=108 label=107
  O r1913 = ((object*) lenv) [2];
  O r1914 = (object *) 3473410;
  if IRK_IS_TRUE(IRK_TEST(r1913==r1914)) {
    O r1916 = (object *) 11;
    IRK_RETURN(1916);
  } else {
    FAIL_107();
  }
}
static void FAIL_107() {
  // fatbar jn=112 label=111
  O r1922 = ((object*) lenv) [2];
  O r1923 = (object *) 3407874;
  if IRK_IS_TRUE(IRK_TEST(r1922==r1923)) {
    O r1925 = (object *) 9;
    IRK_RETURN(1925);
  } else {
    FAIL_111();
  }
}
static void FAIL_111() {
  // fatbar jn=116 label=115
  O r1931 = ((object*) lenv) [2];
  O r1932 = (object *) 3342338;
  if IRK_IS_TRUE(IRK_TEST(r1931==r1932)) {
    O r1934 = (object *) 7;
    IRK_RETURN(1934);
  } else {
    FAIL_115();
  }
}
static void FAIL_115() {
  // fatbar jn=120 label=119
  O r1940 = ((object*) lenv) [2];
  O r1941 = (object *) 3276802;
  if IRK_IS_TRUE(IRK_TEST(r1940==r1941)) {
    O r1943 = (object *) 5;
    IRK_RETURN(1943);
  } else {
    FAIL_119();
  }
}
static void FAIL_119() {
  // fatbar jn=124 label=123
  O r1949 = ((object*) lenv) [2];
  O r1950 = (object *) 3211266;
  if IRK_IS_TRUE(IRK_TEST(r1949==r1950)) {
    O r1952 = (object *) 3;
    IRK_RETURN(1952);
  } else {
    FAIL_123();
  }
}
static void FAIL_123() {
  // fatbar jn=128 label=127
  O r1958 = ((object*) lenv) [2];
  O r1959 = (object *) 3145730;
  if IRK_IS_TRUE(IRK_TEST(r1958==r1959)) {
    O r1961 = (object *) 1;
    IRK_RETURN(1961);
  } else {
    FAIL_127();
  }
}
static void FAIL_127() {
  // env 176:
  //   |BadHexDigit=product ...t1096|
  O r1965 = alloc_cleared (TC_ENV, 2);
  O r1967 = (object*)UITAG(5);
  r1965[2] = r1967;
  O r1966 = top[3];
  r1965[1] = r1966[2]; lenv = r1965; ((pfun)(r1966[1]))();
}
static void FUN_hexdigit_649_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=132 label=131
  O r1972 = ((object*) lenv) [2];
  O r1973 = (object *) 4587522;
  if IRK_IS_TRUE(IRK_TEST(r1972==r1973)) {
    O r1975 = (object *) 31;
    IRK_RETURN(1975);
  } else {
    FAIL_131();
  }
}
static void FAIL_131() {
  // fatbar jn=136 label=135
  O r1981 = ((object*) lenv) [2];
  O r1982 = (object *) 4521986;
  if IRK_IS_TRUE(IRK_TEST(r1981==r1982)) {
    O r1984 = (object *) 29;
    IRK_RETURN(1984);
  } else {
    FAIL_135();
  }
}
static void FAIL_135() {
  // fatbar jn=140 label=139
  O r1990 = ((object*) lenv) [2];
  O r1991 = (object *) 4456450;
  if IRK_IS_TRUE(IRK_TEST(r1990==r1991)) {
    O r1993 = (object *) 27;
    IRK_RETURN(1993);
  } else {
    FAIL_139();
  }
}
static void FAIL_139() {
  // fatbar jn=144 label=143
  O r1999 = ((object*) lenv) [2];
  O r2000 = (object *) 4390914;
  if IRK_IS_TRUE(IRK_TEST(r1999==r2000)) {
    O r2002 = (object *) 25;
    IRK_RETURN(2002);
  } else {
    FAIL_143();
  }
}
static void FAIL_143() {
  // fatbar jn=148 label=147
  O r2008 = ((object*) lenv) [2];
  O r2009 = (object *) 4325378;
  if IRK_IS_TRUE(IRK_TEST(r2008==r2009)) {
    O r2011 = (object *) 23;
    IRK_RETURN(2011);
  } else {
    FAIL_147();
  }
}
static void FAIL_147() {
  // fatbar jn=152 label=151
  O r2017 = ((object*) lenv) [2];
  O r2018 = (object *) 4259842;
  if IRK_IS_TRUE(IRK_TEST(r2017==r2018)) {
    O r2020 = (object *) 21;
    IRK_RETURN(2020);
  } else {
    FAIL_151();
  }
}
static void FAIL_151() {
  // fatbar jn=156 label=155
  O r2026 = ((object*) lenv) [2];
  O r2027 = (object *) 6684674;
  if IRK_IS_TRUE(IRK_TEST(r2026==r2027)) {
    O r2029 = (object *) 31;
    IRK_RETURN(2029);
  } else {
    FAIL_155();
  }
}
static void FAIL_155() {
  // fatbar jn=160 label=159
  O r2035 = ((object*) lenv) [2];
  O r2036 = (object *) 6619138;
  if IRK_IS_TRUE(IRK_TEST(r2035==r2036)) {
    O r2038 = (object *) 29;
    IRK_RETURN(2038);
  } else {
    FAIL_159();
  }
}
static void FAIL_159() {
  // fatbar jn=164 label=163
  O r2044 = ((object*) lenv) [2];
  O r2045 = (object *) 6553602;
  if IRK_IS_TRUE(IRK_TEST(r2044==r2045)) {
    O r2047 = (object *) 27;
    IRK_RETURN(2047);
  } else {
    FAIL_163();
  }
}
static void FAIL_163() {
  // fatbar jn=168 label=167
  O r2053 = ((object*) lenv) [2];
  O r2054 = (object *) 6488066;
  if IRK_IS_TRUE(IRK_TEST(r2053==r2054)) {
    O r2056 = (object *) 25;
    IRK_RETURN(2056);
  } else {
    FAIL_167();
  }
}
static void FAIL_167() {
  // fatbar jn=172 label=171
  O r2062 = ((object*) lenv) [2];
  O r2063 = (object *) 6422530;
  if IRK_IS_TRUE(IRK_TEST(r2062==r2063)) {
    O r2065 = (object *) 23;
    IRK_RETURN(2065);
  } else {
    FAIL_171();
  }
}
static void FAIL_171() {
  // fatbar jn=176 label=175
  O r2071 = ((object*) lenv) [2];
  O r2072 = (object *) 6356994;
  if IRK_IS_TRUE(IRK_TEST(r2071==r2072)) {
    O r2074 = (object *) 21;
    IRK_RETURN(2074);
  } else {
    FAIL_175();
  }
}
static void FAIL_175() {
  // fatbar jn=180 label=179
  O r2080 = ((object*) lenv) [2];
  O r2081 = (object *) 3735554;
  if IRK_IS_TRUE(IRK_TEST(r2080==r2081)) {
    O r2083 = (object *) 19;
    IRK_RETURN(2083);
  } else {
    FAIL_179();
  }
}
static void FAIL_179() {
  // fatbar jn=184 label=183
  O r2089 = ((object*) lenv) [2];
  O r2090 = (object *) 3670018;
  if IRK_IS_TRUE(IRK_TEST(r2089==r2090)) {
    O r2092 = (object *) 17;
    IRK_RETURN(2092);
  } else {
    FAIL_183();
  }
}
static void FAIL_183() {
  // fatbar jn=188 label=187
  O r2098 = ((object*) lenv) [2];
  O r2099 = (object *) 3604482;
  if IRK_IS_TRUE(IRK_TEST(r2098==r2099)) {
    O r2101 = (object *) 15;
    IRK_RETURN(2101);
  } else {
    FAIL_187();
  }
}
static void FAIL_187() {
  // fatbar jn=192 label=191
  O r2107 = ((object*) lenv) [2];
  O r2108 = (object *) 3538946;
  if IRK_IS_TRUE(IRK_TEST(r2107==r2108)) {
    O r2110 = (object *) 13;
    IRK_RETURN(2110);
  } else {
    FAIL_191();
  }
}
static void FAIL_191() {
  // fatbar jn=196 label=195
  O r2116 = ((object*) lenv) [2];
  O r2117 = (object *) 3473410;
  if IRK_IS_TRUE(IRK_TEST(r2116==r2117)) {
    O r2119 = (object *) 11;
    IRK_RETURN(2119);
  } else {
    FAIL_195();
  }
}
static void FAIL_195() {
  // fatbar jn=200 label=199
  O r2125 = ((object*) lenv) [2];
  O r2126 = (object *) 3407874;
  if IRK_IS_TRUE(IRK_TEST(r2125==r2126)) {
    O r2128 = (object *) 9;
    IRK_RETURN(2128);
  } else {
    FAIL_199();
  }
}
static void FAIL_199() {
  // fatbar jn=204 label=203
  O r2134 = ((object*) lenv) [2];
  O r2135 = (object *) 3342338;
  if IRK_IS_TRUE(IRK_TEST(r2134==r2135)) {
    O r2137 = (object *) 7;
    IRK_RETURN(2137);
  } else {
    FAIL_203();
  }
}
static void FAIL_203() {
  // fatbar jn=208 label=207
  O r2143 = ((object*) lenv) [2];
  O r2144 = (object *) 3276802;
  if IRK_IS_TRUE(IRK_TEST(r2143==r2144)) {
    O r2146 = (object *) 5;
    IRK_RETURN(2146);
  } else {
    FAIL_207();
  }
}
static void FAIL_207() {
  // fatbar jn=212 label=211
  O r2152 = ((object*) lenv) [2];
  O r2153 = (object *) 3211266;
  if IRK_IS_TRUE(IRK_TEST(r2152==r2153)) {
    O r2155 = (object *) 3;
    IRK_RETURN(2155);
  } else {
    FAIL_211();
  }
}
static void FAIL_211() {
  // fatbar jn=216 label=215
  O r2161 = ((object*) lenv) [2];
  O r2162 = (object *) 3145730;
  if IRK_IS_TRUE(IRK_TEST(r2161==r2162)) {
    O r2164 = (object *) 1;
    IRK_RETURN(2164);
  } else {
    FAIL_215();
  }
}
static void FAIL_215() {
  // env 177:
  //   |Hex/BadDigit=(product string char) ...t1199|
  O r2168 = alloc_cleared (TC_ENV, 2);
  O r2170 = allocate (UOTAG(6), 2);
  O r2171 = (O) &constructed_109;
  r2170[1] = r2171;
  O r2172 = ((object*) lenv) [2];
  r2170[2] = r2172;
  r2168[2] = r2170;
  O r2169 = top[3];
  r2168[1] = r2169[2]; lenv = r2168; ((pfun)(r2169[1]))();
}
static void FUN_print_exception_exit_592_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 178:
  //   |...t29| symbol
  O r2201 = alloc_cleared (TC_ENV, 3);
  r2201[1] = lenv; lenv = r2201;
  O r2202 = ((object**) lenv) [1][2];
  r2201[2] = r2202;
  O r2552 = top[42];
  O r2557 = r2552; // reg varref
  O r2553 = TAG_INTEGER((irk_int)(r2557 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*r2557));
  O r2555 = (object *) 1;
  O r2556 = r2553; // reg varref
  O r2554 = IRK_TEST(UNTAG_INTEGER(r2555)==UNTAG_INTEGER(r2556));
  O r2347 = r2554; // reg varref
  if IRK_IS_TRUE(r2347) {
    // env 179:
    //   (list sexp)
    O r2447 = alloc_cleared (TC_ENV, 2);
    r2447[1] = lenv; lenv = r2447;
    // env 180:
    //   (symbol (list sexp) -> (list sexp))
    O r2489 = alloc_cleared (TC_ENV, 2);
    r2489[1] = lenv; lenv = r2489;
    O r2490 = allocate (TC_CLOSURE, 2);
    r2490[1] = FUN_search_sexp_list_3009_i112_i141_i142_i147_0; r2490[2] = lenv;
    r2489[2] = r2490;
    // env 181:
    //   sexp
    O r2487 = allocate (TC_ENV, 2);
    r2487[1] = lenv; lenv = r2487;
    O r2488 = irk_get_metadata();
    r2487[2] = r2488;
    // fatbar jn=244 label=243
    O r2452 = ((object*) lenv) [2];
    switch (get_case (r2452)) {
      case (UOTAG(0)): { // list
        // env 182:
        //   (list sexp)
        O r2479 = alloc_cleared (TC_ENV, 2);
        r2479[1] = lenv; lenv = r2479;
        O r2481 = ((object**) lenv) [1][2];
        O r2480 = UOBJ_GET(r2481,0);
        r2479[2] = r2480;
        O r2455 = ((object*) lenv) [2];
        switch (get_case (r2455)) {
          case (TC_PAIR): { // cons
            // env 183:
            //   sexp (list sexp)
            O r2473 = alloc_cleared (TC_ENV, 3);
            r2473[1] = lenv; lenv = r2473;
            O r2477 = ((object**) lenv) [1][2];
            O r2474 = UOBJ_GET(r2477,0);
            r2473[2] = r2474;
            O r2476 = ((object**) lenv) [1][2];
            O r2475 = UOBJ_GET(r2476,1);
            r2473[3] = r2475;
            O r2458 = ((object*) lenv) [2];
            switch (get_case (r2458)) {
              case (UOTAG(1)): { // symbol
                // env 184:
                //   symbol
                O r2469 = alloc_cleared (TC_ENV, 2);
                r2469[1] = lenv; lenv = r2469;
                O r2471 = ((object**) lenv) [1][2];
                O r2470 = UOBJ_GET(r2471,0);
                r2469[2] = r2470;
                O r2461 = ((object*) lenv) [2];
                O r2462 = (object*) constructed_121[0];
                if IRK_IS_TRUE(IRK_TEST(r2461==r2462)) {
                  // env 185:
                  //   symbol (list sexp)
                  O r2464 = allocate (TC_ENV, 3);
                  O r2466 = (object*) constructed_119[0];
                  r2464[2] = r2466;
                  O r2467 = ((object**) lenv) [1][3];
                  r2464[3] = r2467;
                  O r2465 = ((object*****) lenv) [1][1][1][1][2];
                  O t = allocate (TC_SAVE, 5);
                  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_592_105; t[4] = r2201; t[5] = r2447; k = t;
                  r2464[1] = r2465[2]; lenv = r2464; FUN_search_sexp_list_3009_i112_i141_i142_i147_0();
                } else {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_243(r2447, r2201);
                }
              } break;
              default: {
                lenv = ((object **)lenv)[1][1];
                FAIL_243(r2447, r2201);
              }
            }
          } break;
          default: {
            lenv = ((object *)lenv)[1];
            FAIL_243(r2447, r2201);
          }
        }
      } break;
      default: {
        FAIL_243(r2447, r2201);
      }
    }
  } else {
    O r2551 = (object *) 14;
    JUMP_233(r2201);
  }
}
static void FUN_print_exception_exit_592_105 (O rr) {
  O r2201 = k[4]; O r2447 = k[5]; lenv = k[2]; k = k[1];
  O r2463 = rr;
  O r2460 = r2463;
  JUMP_249(r2460, r2447, r2201);
}
static void JUMP_249(O r2460, O r2447, O r2201) {
  lenv = lenv[1];
  O r2459 = r2460;
  O r2457 = r2459;
  JUMP_247(r2457, r2447, r2201);
}
static void JUMP_247(O r2457, O r2447, O r2201) {
  lenv = lenv[1];
  O r2456 = r2457;
  O r2454 = r2456;
  JUMP_246(r2454, r2447, r2201);
}
static void JUMP_246(O r2454, O r2447, O r2201) {
  lenv = lenv[1];
  O r2453 = r2454;
  O r2451 = r2453;
  JUMP_245(r2451, r2447, r2201);
}
static void JUMP_245(O r2451, O r2447, O r2201) {
  O r2450 = r2451;
  JUMP_244(r2450, r2447, r2201);
}
static void JUMP_244(O r2450, O r2447, O r2201) {
  lenv = lenv[1];
  O r2449 = r2450;
  lenv = lenv[1];
  O r2448 = r2449;
  r2447[2] = r2448;
  // env 186:
  //   int
  O r2441 = alloc_cleared (TC_ENV, 2);
  r2441[1] = lenv; lenv = r2441;
  // env 187:
  //   (list sexp) int
  O r2443 = allocate (TC_ENV, 3);
  O r2445 = ((object**) lenv) [1][2];
  r2443[2] = r2445;
  O r2446 = (object *) 1;
  r2443[3] = r2446;
  O r2444 = top[10];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_592_106; t[4] = r2201; t[5] = r2441; k = t;
  r2443[1] = r2444[2]; lenv = r2443; FUN_length_splat_272_0();
}
static void FUN_print_exception_exit_592_106 (O rr) {
  O r2201 = k[4]; O r2441 = k[5]; lenv = k[2]; k = k[1];
  O r2442 = rr;
  r2441[2] = r2442;
  O r2440 = ((object*) lenv) [2];
  if (freep + unbox(r2440) >= limit) {
    heap_request = unbox(r2440);
    heap1[3] = r2201;
    gc_flip (1);
    r2201 = heap0[3];
  }
  O r2438 = ((object*) lenv) [2];
  O r2439 = (object*) constructed_117[0];
  O r2437;
  if (unbox(r2438) == 0) { r2437 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r2438));
    for (int i=0; i<unbox(r2438); i++) { t[i+1] = r2439; }
    r2437 = t;
  }
  lenv = lenv[1];
  O r2436 = r2437;
  top[42] = r2436;
  // env 188:
  //   ((list sexp) -> undefined)
  O r2354 = alloc_cleared (TC_ENV, 2);
  r2354[1] = lenv; lenv = r2354;
  O r2355 = allocate (TC_CLOSURE, 2);
  r2355[1] = FUN_loop_3024_i141_i142_i147_0; r2355[2] = lenv;
  r2354[2] = r2355;
  // env 189:
  //   (list sexp)
  O r2351 = allocate (TC_ENV, 2);
  O r2353 = ((object**) lenv) [1][2];
  r2351[2] = r2353;
  O r2352 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_592_107; t[4] = r2201; k = t;
  r2351[1] = r2352[2]; lenv = r2351; FUN_loop_3024_i141_i142_i147_0();
}
static void FUN_print_exception_exit_592_107 (O rr) {
  O r2201 = k[4]; lenv = k[2]; k = k[1];
  O r2350 = rr;
  lenv = lenv[1];
  O r2349 = r2350;
  lenv = lenv[1];
  O r2348 = r2349;
  JUMP_233(r2201);
}
static void FUN_loop_3024_i141_i142_i147_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=235 label=234
  O r2360 = ((object*) lenv) [2];
  switch (get_case (r2360)) {
    case (TC_PAIR): { // cons
      // env 190:
      //   sexp (list sexp)
      O r2428 = alloc_cleared (TC_ENV, 3);
      r2428[1] = lenv; lenv = r2428;
      O r2432 = ((object**) lenv) [1][2];
      O r2429 = UOBJ_GET(r2432,0);
      r2428[2] = r2429;
      O r2431 = ((object**) lenv) [1][2];
      O r2430 = UOBJ_GET(r2431,1);
      r2428[3] = r2430;
      O r2365 = ((object*) lenv) [2];
      switch (get_case (r2365)) {
        case (UOTAG(0)): { // list
          // env 191:
          //   (list sexp)
          O r2423 = alloc_cleared (TC_ENV, 2);
          r2423[1] = lenv; lenv = r2423;
          O r2425 = ((object**) lenv) [1][2];
          O r2424 = UOBJ_GET(r2425,0);
          r2423[2] = r2424;
          O r2370 = ((object*) lenv) [2];
          switch (get_case (r2370)) {
            case (TC_PAIR): { // cons
              // env 192:
              //   sexp (list sexp)
              O r2416 = alloc_cleared (TC_ENV, 3);
              r2416[1] = lenv; lenv = r2416;
              O r2420 = ((object**) lenv) [1][2];
              O r2417 = UOBJ_GET(r2420,0);
              r2416[2] = r2417;
              O r2419 = ((object**) lenv) [1][2];
              O r2418 = UOBJ_GET(r2419,1);
              r2416[3] = r2418;
              O r2375 = ((object*) lenv) [2];
              switch (get_case (r2375)) {
                case (UOTAG(1)): { // symbol
                  // env 193:
                  //   symbol
                  O r2411 = alloc_cleared (TC_ENV, 2);
                  r2411[1] = lenv; lenv = r2411;
                  O r2413 = ((object**) lenv) [1][2];
                  O r2412 = UOBJ_GET(r2413,0);
                  r2411[2] = r2412;
                  O r2380 = ((object**) lenv) [1][3];
                  switch (get_case (r2380)) {
                    case (TC_PAIR): { // cons
                      // env 194:
                      //   sexp (list sexp)
                      O r2404 = alloc_cleared (TC_ENV, 3);
                      r2404[1] = lenv; lenv = r2404;
                      O r2408 = ((object***) lenv) [1][1][3];
                      O r2405 = UOBJ_GET(r2408,0);
                      r2404[2] = r2405;
                      O r2407 = ((object***) lenv) [1][1][3];
                      O r2406 = UOBJ_GET(r2407,1);
                      r2404[3] = r2406;
                      O r2385 = ((object*) lenv) [2];
                      switch (get_case (r2385)) {
                        case (UOTAG(5)): { // int
                          O r2401 = ((object*) lenv) [2];
                          O r2388 = UOBJ_GET(r2401,0);
                          O r2391 = ((object*) lenv) [3];
                          switch (get_case (r2391)) {
                            case (TC_NIL): { // nil
                              O r2396 = top[42];
                              O r2397 = r2388; // reg varref
                              O r2398 = ((object**) lenv) [1][2];
                              range_check (GET_TUPLE_LENGTH(*(object*)r2396), unbox(r2397));
                              ((irk_vector*)r2396)->val[unbox(r2397)] = r2398;
                              O r2395 = ((object*****) lenv) [1][1][1][1][3];
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              lenv[2] = r2395;
                              FUN_loop_3024_i141_i142_i147_0();
                            } break;
                            default: {
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              FAIL_234();
                            }
                          }
                        } break;
                        default: {
                          lenv = ((object *****)lenv)[1][1][1][1][1];
                          FAIL_234();
                        }
                      }
                    } break;
                    default: {
                      lenv = ((object ****)lenv)[1][1][1][1];
                      FAIL_234();
                    }
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_234();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_234();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_234();
        }
      }
    } break;
    default: {
      FAIL_234();
    }
  }
}
static void FAIL_234() {
  O r2435 = (object *) 14;
  IRK_RETURN(2435);
}
static void FAIL_243(O r2447, O r2201) {
  // env 195:
  //   |Metadata/Malformed=sexp ...t216|
  O r2483 = alloc_cleared (TC_ENV, 2);
  O r2485 = allocate (UOTAG(2), 1);
  O r2486 = ((object*) lenv) [2];
  r2485[1] = r2486;
  r2483[2] = r2485;
  O r2484 = top[3];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_3024_i141_i142_i147_108; t[4] = r2201; t[5] = r2447; k = t;
  r2483[1] = r2484[2]; lenv = r2483; ((pfun)(r2484[1]))();
}
static void FUN_loop_3024_i141_i142_i147_108 (O rr) {
  O r2201 = k[4]; O r2447 = k[5]; lenv = k[2]; k = k[1];
  O r2451 = rr;
  O r2450 = r2451;
  JUMP_244(r2450, r2447, r2201);
}
static void FUN_search_sexp_list_3009_i112_i141_i142_i147_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=251 label=250
  O r2495 = ((object*) lenv) [3];
  switch (get_case (r2495)) {
    case (TC_PAIR): { // cons
      // env 196:
      //   sexp (list sexp)
      O r2539 = alloc_cleared (TC_ENV, 3);
      r2539[1] = lenv; lenv = r2539;
      O r2543 = ((object**) lenv) [1][3];
      O r2540 = UOBJ_GET(r2543,0);
      r2539[2] = r2540;
      O r2542 = ((object**) lenv) [1][3];
      O r2541 = UOBJ_GET(r2542,1);
      r2539[3] = r2541;
      O r2500 = ((object*) lenv) [2];
      switch (get_case (r2500)) {
        case (UOTAG(0)): { // list
          // env 197:
          //   (list sexp)
          O r2534 = alloc_cleared (TC_ENV, 2);
          r2534[1] = lenv; lenv = r2534;
          O r2536 = ((object**) lenv) [1][2];
          O r2535 = UOBJ_GET(r2536,0);
          r2534[2] = r2535;
          O r2505 = ((object*) lenv) [2];
          switch (get_case (r2505)) {
            case (TC_PAIR): { // cons
              // env 198:
              //   sexp (list sexp)
              O r2527 = alloc_cleared (TC_ENV, 3);
              r2527[1] = lenv; lenv = r2527;
              O r2531 = ((object**) lenv) [1][2];
              O r2528 = UOBJ_GET(r2531,0);
              r2527[2] = r2528;
              O r2530 = ((object**) lenv) [1][2];
              O r2529 = UOBJ_GET(r2530,1);
              r2527[3] = r2529;
              O r2510 = ((object*) lenv) [2];
              switch (get_case (r2510)) {
                case (UOTAG(1)): { // symbol
                  O r2524 = ((object*) lenv) [2];
                  O r2513 = UOBJ_GET(r2524,0);
                  O r2516 = r2513; // reg varref
                  O r2517 = ((object****) lenv) [1][1][1][2];
                  if IRK_IS_TRUE(IRK_TEST(r2516==r2517)) {
                    O r2519 = ((object*) lenv) [3];
                    IRK_RETURN(2519);
                  } else {
                    O r2522 = ((object****) lenv) [1][1][1][2];
                    O r2523 = ((object***) lenv) [1][1][3];
                    lenv = ((object ***)lenv)[1][1][1];
                    lenv[2] = r2522;
                    lenv[3] = r2523;
                    FUN_search_sexp_list_3009_i112_i141_i142_i147_0();
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_250();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_250();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_250();
        }
      }
    } break;
    default: {
      FAIL_250();
    }
  }
}
static void FAIL_250() {
  // env 199:
  //   |Metadata/KeyError=symbol ...t197|
  O r2547 = alloc_cleared (TC_ENV, 2);
  O r2549 = allocate (UOTAG(1), 1);
  O r2550 = ((object*) lenv) [2];
  r2549[1] = r2550;
  r2547[2] = r2549;
  O r2548 = top[3];
  r2547[1] = r2548[2]; lenv = r2547; ((pfun)(r2548[1]))();
}
static void JUMP_233(O r2201) {
  // env 200:
  //   |...t29| int |tuple=(product int int int) | ident
  O r2238 = alloc_cleared (TC_ENV, 5);
  r2238[1] = lenv; lenv = r2238;
  O r2239 = ((object**) lenv) [1][2];
  r2238[2] = r2239;
  O r2346 = ((object*) lenv) [2];
  O r2240 = TAG_INTEGER((irk_int)unbox(irk_object2int(r2346)));
  r2238[3] = r2240;
  O r2344 = (object *) 3;
  O r2345 = ((object*) lenv) [3];
  O r2340 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2344)&UNTAG_INTEGER(r2345));
  O r2342 = (object *) 3;
  O r2343 = r2340; // reg varref
  O r2341 = IRK_TEST(UNTAG_INTEGER(r2342)==UNTAG_INTEGER(r2343));
  O r2306 = r2341; // reg varref
  if IRK_IS_TRUE(r2306) {
    O r2307 = allocate (UOTAG(3), 3);
    O r2311 = ((object*) lenv) [3];
    O r2312 = (object *) 3;
    O r2308 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2311)>>UNTAG_INTEGER(r2312));
    r2307[1] = r2308;
    O r2309 = (object *) 1;
    r2307[2] = r2309;
    O r2310 = (object *) 1;
    r2307[3] = r2310;
    O r2241 = r2307;
    JUMP_231(r2241, r2238, r2201);
  } else {
    O r2338 = (object *) 7;
    O r2339 = ((object*) lenv) [3];
    O r2334 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2338)&UNTAG_INTEGER(r2339));
    O r2336 = r2334; // reg varref
    O r2337 = (object *) 1;
    O r2335 = IRK_TEST(UNTAG_INTEGER(r2336)>UNTAG_INTEGER(r2337));
    O r2314 = r2335; // reg varref
    if IRK_IS_TRUE(r2314) {
      O r2315 = allocate (UOTAG(3), 3);
      O r2321 = ((object*) lenv) [3];
      O r2322 = top[40];
      O r2316 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2321)>>UNTAG_INTEGER(r2322));
      r2315[1] = r2316;
      O r2317 = (object *) 1;
      r2315[2] = r2317;
      O r2319 = top[41];
      O r2320 = ((object*) lenv) [3];
      O r2318 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2319)&UNTAG_INTEGER(r2320));
      r2315[3] = r2318;
      O r2313 = r2315;
      JUMP_232(r2313, r2238, r2201);
    } else {
      O r2333 = ((object*) lenv) [2];
      O r2324 = TAG_INTEGER((irk_int)unbox(irk_objectptr2int(r2333)));
      O r2325 = allocate (UOTAG(3), 3);
      O r2326 = (object *) 1;
      r2325[1] = r2326;
      O r2331 = r2324; // reg varref
      O r2332 = top[40];
      O r2327 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2331)>>UNTAG_INTEGER(r2332));
      r2325[2] = r2327;
      O r2329 = top[41];
      O r2330 = r2324; // reg varref
      O r2328 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2329)&UNTAG_INTEGER(r2330));
      r2325[3] = r2328;
      O r2323 = r2325; // reg varref
      O r2313 = r2323;
      JUMP_232(r2313, r2238, r2201);
    }
  }
}
static void JUMP_232(O r2313, O r2238, O r2201) {
  O r2241 = r2313;
  JUMP_231(r2241, r2238, r2201);
}
static void JUMP_231(O r2241, O r2238, O r2201) {
  r2238[4] = r2241;
  // env 201:
  //   int int int
  O r2299 = alloc_cleared (TC_ENV, 4);
  r2299[1] = lenv; lenv = r2299;
  O r2305 = ((object**) lenv) [1][4];
  O r2300 = UOBJ_GET(r2305,0);
  r2299[2] = r2300;
  O r2304 = ((object**) lenv) [1][4];
  O r2301 = UOBJ_GET(r2304,1);
  r2299[3] = r2301;
  O r2303 = ((object**) lenv) [1][4];
  O r2302 = UOBJ_GET(r2303,2);
  r2299[4] = r2302;
  O r2293 = ((object*) lenv) [3];
  O r2294 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2293)==UNTAG_INTEGER(r2294))) {
    O r2296 = ((object*) lenv) [4];
    O r2297 = (object *) 1;
    O r2295 = IRK_TEST(UNTAG_INTEGER(r2296)==UNTAG_INTEGER(r2297));
    O r2244 = r2295;
    JUMP_230(r2244, r2238, r2201);
  } else {
    O r2298 = (object *) 6;
    O r2244 = r2298;
    JUMP_230(r2244, r2238, r2201);
  }
}
static void JUMP_230(O r2244, O r2238, O r2201) {
  if IRK_IS_TRUE(r2244) {
    O r2246 = ((object*) lenv) [2];
    O r2245 = alloc_no_clear (UOTAG(0),1);
    r2245[1] = r2246;
    O r2243 = r2245;
    JUMP_221(r2243, r2238, r2201);
  } else {
    O r2287 = ((object*) lenv) [3];
    O r2288 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2287)>UNTAG_INTEGER(r2288))) {
      O r2290 = ((object*) lenv) [2];
      O r2291 = (object *) 1;
      O r2289 = IRK_TEST(UNTAG_INTEGER(r2290)==UNTAG_INTEGER(r2291));
      O r2248 = r2289;
      JUMP_228(r2248, r2238, r2201);
    } else {
      O r2292 = (object *) 6;
      O r2248 = r2292;
      JUMP_228(r2248, r2238, r2201);
    }
  }
}
static void JUMP_228(O r2248, O r2238, O r2201) {
  if IRK_IS_TRUE(r2248) {
    O r2250 = ((object*) lenv) [4];
    O r2251 = (object *) 81;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2250)>=UNTAG_INTEGER(r2251))) {
      O r2259 = ((object*) lenv) [4];
      O r2260 = (object *) 5;
      O r2255 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2259)>>UNTAG_INTEGER(r2260));
      O r2257 = r2255; // reg varref
      O r2258 = (object *) 21;
      O r2256 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2257)-UNTAG_INTEGER(r2258));
      O r2253 = r2256; // reg varref
      O r2254 = ((object*) lenv) [3];
      O r2252 = alloc_no_clear (UOTAG(2),2);
      r2252[1] = r2253;
      r2252[2] = r2254;
      O r2249 = r2252;
      JUMP_224(r2249, r2238, r2201);
    } else {
      O r2264 = top[44];
      O r2266 = ((object*) lenv) [4];
      O r2267 = (object *) 5;
      O r2265 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2266)>>UNTAG_INTEGER(r2267));
      range_check (GET_TUPLE_LENGTH(*(object*)r2264), unbox(r2265));
      O r2262 = ((irk_vector*)r2264)->val[unbox(r2265)];
      O r2263 = ((object*) lenv) [3];
      O r2261 = alloc_no_clear (UOTAG(4),2);
      r2261[1] = r2262;
      r2261[2] = r2263;
      O r2249 = r2261;
      JUMP_224(r2249, r2238, r2201);
    }
  } else {
    O r2269 = ((object*) lenv) [4];
    O r2270 = (object *) 45;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2269)>=UNTAG_INTEGER(r2270))) {
      O r2278 = ((object*) lenv) [4];
      O r2279 = (object *) 5;
      O r2274 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2278)>>UNTAG_INTEGER(r2279));
      O r2276 = r2274; // reg varref
      O r2277 = (object *) 11;
      O r2275 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2276)-UNTAG_INTEGER(r2277));
      O r2272 = r2275; // reg varref
      O r2273 = ((object*) lenv) [2];
      O r2271 = alloc_no_clear (UOTAG(1),2);
      r2271[1] = r2272;
      r2271[2] = r2273;
      O r2268 = r2271;
      JUMP_226(r2268, r2238, r2201);
    } else {
      O r2283 = top[43];
      O r2285 = ((object*) lenv) [4];
      O r2286 = (object *) 5;
      O r2284 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2285)>>UNTAG_INTEGER(r2286));
      range_check (GET_TUPLE_LENGTH(*(object*)r2283), unbox(r2284));
      O r2281 = ((irk_vector*)r2283)->val[unbox(r2284)];
      O r2282 = ((object*) lenv) [2];
      O r2280 = alloc_no_clear (UOTAG(3),2);
      r2280[1] = r2281;
      r2280[2] = r2282;
      O r2268 = r2280;
      JUMP_226(r2268, r2238, r2201);
    }
  }
}
static void JUMP_226(O r2268, O r2238, O r2201) {
  O r2247 = r2268;
  JUMP_222(r2247, r2238, r2201);
}
static void JUMP_224(O r2249, O r2238, O r2201) {
  O r2247 = r2249;
  JUMP_222(r2247, r2238, r2201);
}
static void JUMP_222(O r2247, O r2238, O r2201) {
  O r2243 = r2247;
  JUMP_221(r2243, r2238, r2201);
}
static void JUMP_221(O r2243, O r2238, O r2201) {
  lenv = lenv[1];
  O r2242 = r2243;
  r2238[5] = r2242;
  O r2205 = ((object*) lenv) [5];
  switch (get_case (r2205)) {
    case (UOTAG(1)): { // userimm
      O r2213 = ((object*) lenv) [5];
      O r2207 = UOBJ_GET(r2213,0);
      O r2212 = ((object*) lenv) [5];
      O r2208 = UOBJ_GET(r2212,1);
      O r2210 = top[42];
      O r2211 = r2208; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r2210), unbox(r2211));
      O r2209 = ((irk_vector*)r2210)->val[unbox(r2211)];
      O r2206 = r2209; // reg varref
      O r2204 = r2206;
      JUMP_219(r2204, r2201);
    } break;
    case (UOTAG(2)): { // usertup
      O r2221 = ((object*) lenv) [5];
      O r2215 = UOBJ_GET(r2221,0);
      O r2220 = ((object*) lenv) [5];
      O r2216 = UOBJ_GET(r2220,1);
      O r2218 = top[42];
      O r2219 = r2215; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r2218), unbox(r2219));
      O r2217 = ((irk_vector*)r2218)->val[unbox(r2219)];
      O r2214 = r2217; // reg varref
      O r2204 = r2214;
      JUMP_219(r2204, r2201);
    } break;
    case (UOTAG(3)): { // sysimm
      O r2227 = ((object*) lenv) [5];
      O r2223 = UOBJ_GET(r2227,0);
      O r2226 = ((object*) lenv) [5];
      O r2224 = UOBJ_GET(r2226,1);
      O r2225 = r2223; // reg varref
      O r2222 = r2225; // reg varref
      O r2204 = r2222;
      JUMP_219(r2204, r2201);
    } break;
    case (UOTAG(4)): { // systup
      O r2233 = ((object*) lenv) [5];
      O r2229 = UOBJ_GET(r2233,0);
      O r2232 = ((object*) lenv) [5];
      O r2230 = UOBJ_GET(r2232,1);
      O r2231 = r2229; // reg varref
      O r2228 = r2231; // reg varref
      O r2204 = r2228;
      JUMP_219(r2204, r2201);
    } break;
    default: {
      O r2237 = ((object*) lenv) [5];
      O r2235 = UOBJ_GET(r2237,0);
      O r2236 = (object*) constructed_115[0];
      O r2234 = r2236; // reg varref
      O r2204 = r2234;
      JUMP_219(r2204, r2201);
    } break;
  }
}
static void JUMP_219(O r2204, O r2201) {
  lenv = lenv[1];
  O r2203 = r2204;
  r2201[3] = r2203;
  // env 202:
  //   string
  O r2189 = alloc_cleared (TC_ENV, 2);
  r2189[1] = lenv; lenv = r2189;
  // env 203:
  //   (list string)
  O r2191 = alloc_cleared (TC_ENV, 2);
  O r2194 = (O) &constructed_113;
  O r2200 = ((object**) lenv) [1][3];
  O r2198 = UOBJ_GET(r2200,0);
  O r2199 = r2198; // reg varref
  O r2196 = r2199; // reg varref
  O r2197 = (object*) constructed_112[0];
  O r2195 = alloc_no_clear (TC_PAIR,2);
  r2195[1] = r2196;
  r2195[2] = r2197;
  O r2193 = alloc_no_clear (TC_PAIR,2);
  r2193[1] = r2194;
  r2193[2] = r2195;
  r2191[2] = r2193;
  O r2192 = top[19];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_search_sexp_list_3009_i112_i141_i142_i147_109; t[4] = r2189; k = t;
  r2191[1] = r2192[2]; lenv = r2191; FUN_string_concat_338_0();
}
static void FUN_search_sexp_list_3009_i112_i141_i142_i147_109 (O rr) {
  O r2189 = k[4]; lenv = k[2]; k = k[1];
  O r2190 = rr;
  r2189[2] = r2190;
  O r2186 = ((object*) lenv) [2];
  O r2188 = ((object*) lenv) [2];
  O r2187 = TAG_INTEGER((irk_int)((irk_string*)(r2188))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r2186))->data, 1, UNTAG_INTEGER(r2187), stdout));
  O r2185 = (object *) 14;
  lenv = lenv[1];
  O r2180 = (O) &constructed_110;
  O r2182 = r2180; // reg varref
  O r2184 = r2180; // reg varref
  O r2183 = TAG_INTEGER((irk_int)((irk_string*)(r2184))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r2182))->data, 1, UNTAG_INTEGER(r2183), stdout));
  O r2181 = (object *) 14;
  O r2179 = ((object*) lenv) [2];
  dump_object (r2179, 0);
  O r2178 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r2178), stdout));
  // dead  = (object *) 14;
  O r2177 = (object *) 14;
  lenv = lenv[1];
  O r2176 = (object *) -1;
  exit_continuation(r2176);
  O r2175 = (object *) TC_UNDEFINED;
  IRK_RETURN(2175);
}
static void FUN_tree_2fmin_572_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2560 = ((object*) lenv) [2];
  switch (get_case (r2560)) {
    case (UITAG(2)): { // empty
      // env 204:
      //   |Tree/Empty=product ...t2189|
      O r2563 = alloc_cleared (TC_ENV, 2);
      O r2565 = (object*)UITAG(7);
      r2563[2] = r2565;
      O r2564 = top[3];
      r2563[1] = r2564[2]; lenv = r2563; ((pfun)(r2564[1]))();
    } break;
    case (UOTAG(1)): { // black
      O r2586 = ((object*) lenv) [2];
      O r2568 = UOBJ_GET(r2586,0);
      O r2585 = ((object*) lenv) [2];
      O r2569 = UOBJ_GET(r2585,2);
      O r2584 = ((object*) lenv) [2];
      O r2570 = UOBJ_GET(r2584,3);
      // fatbar jn=260 label=259
      O r2575 = r2568; // reg varref
      switch (get_case (r2575)) {
        case (UITAG(2)): { // empty
          O r2577 = allocate (UOTAG(3), 2);
          O r2578 = r2569; // reg varref
          r2577[1] = r2578;
          O r2579 = r2570; // reg varref
          r2577[2] = r2579;
          IRK_RETURN(2577);
        } break;
        default: {
          FAIL_259(r2568);
        }
      }
    } break;
    default: {
      O r2607 = ((object*) lenv) [2];
      O r2589 = UOBJ_GET(r2607,0);
      O r2606 = ((object*) lenv) [2];
      O r2590 = UOBJ_GET(r2606,2);
      O r2605 = ((object*) lenv) [2];
      O r2591 = UOBJ_GET(r2605,3);
      // fatbar jn=263 label=262
      O r2596 = r2589; // reg varref
      switch (get_case (r2596)) {
        case (UITAG(2)): { // empty
          O r2598 = allocate (UOTAG(3), 2);
          O r2599 = r2590; // reg varref
          r2598[1] = r2599;
          O r2600 = r2591; // reg varref
          r2598[2] = r2600;
          IRK_RETURN(2598);
        } break;
        default: {
          FAIL_262(r2589);
        }
      }
    } break;
  }
}
static void FAIL_262(O r2589) {
  O r2604 = r2589; // reg varref
  lenv[2] = r2604;
  FUN_tree_2fmin_572_0();
}
static void FAIL_259(O r2568) {
  O r2583 = r2568; // reg varref
  lenv[2] = r2583;
  FUN_tree_2fmin_572_0();
}
static void FUN_tree_2fmember_570_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 205:
  //   ((tree t2236 t2235) -> (maybe t2235))
  O r2615 = alloc_cleared (TC_ENV, 2);
  r2615[1] = lenv; lenv = r2615;
  O r2616 = allocate (TC_CLOSURE, 2);
  r2616[1] = FUN_member0_2890_0; r2616[2] = lenv;
  r2615[2] = r2616;
  // env 206:
  //   (tree t2267 t2268)
  O r2612 = allocate (TC_ENV, 2);
  O r2614 = ((object**) lenv) [1][2];
  r2612[2] = r2614;
  O r2613 = ((object*) lenv) [2];
  r2612[1] = r2613[2]; lenv = r2612; FUN_member0_2890_0();
}
static void FUN_member0_2890_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2619 = ((object*) lenv) [2];
  switch (get_case (r2619)) {
    case (UITAG(2)): { // empty
      O r2621 = (object *) 65558;
      IRK_RETURN(2621);
    } break;
    case (UOTAG(0)): { // red
      // env 207:
      //   (tree t2236 t2235) (tree t2236 t2235) t2236 t2235 cmp
      O r2636 = alloc_cleared (TC_ENV, 6);
      r2636[1] = lenv; lenv = r2636;
      O r2649 = ((object**) lenv) [1][2];
      O r2637 = UOBJ_GET(r2649,0);
      r2636[2] = r2637;
      O r2648 = ((object**) lenv) [1][2];
      O r2638 = UOBJ_GET(r2648,1);
      r2636[3] = r2638;
      O r2647 = ((object**) lenv) [1][2];
      O r2639 = UOBJ_GET(r2647,2);
      r2636[4] = r2639;
      O r2646 = ((object**) lenv) [1][2];
      O r2640 = UOBJ_GET(r2646,3);
      r2636[5] = r2640;
      // env 208:
      //   t2231 t2236
      O r2642 = allocate (TC_ENV, 3);
      O r2644 = ((object****) lenv) [1][1][1][4];
      r2642[2] = r2644;
      O r2645 = ((object*) lenv) [4];
      r2642[3] = r2645;
      O r2643 = ((object****) lenv) [1][1][1][3];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_member0_2890_110; t[4] = r2636; k = t;
      r2642[1] = r2643[2]; lenv = r2642; ((pfun)(r2643[1]))();
    } break;
    default: {
      // env 209:
      //   (tree t2236 t2235) (tree t2236 t2235) t2236 t2235 cmp
      O r2664 = alloc_cleared (TC_ENV, 6);
      r2664[1] = lenv; lenv = r2664;
      O r2677 = ((object**) lenv) [1][2];
      O r2665 = UOBJ_GET(r2677,0);
      r2664[2] = r2665;
      O r2676 = ((object**) lenv) [1][2];
      O r2666 = UOBJ_GET(r2676,1);
      r2664[3] = r2666;
      O r2675 = ((object**) lenv) [1][2];
      O r2667 = UOBJ_GET(r2675,2);
      r2664[4] = r2667;
      O r2674 = ((object**) lenv) [1][2];
      O r2668 = UOBJ_GET(r2674,3);
      r2664[5] = r2668;
      // env 210:
      //   t2231 t2236
      O r2670 = allocate (TC_ENV, 3);
      O r2672 = ((object****) lenv) [1][1][1][4];
      r2670[2] = r2672;
      O r2673 = ((object*) lenv) [4];
      r2670[3] = r2673;
      O r2671 = ((object****) lenv) [1][1][1][3];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_member0_2890_111; t[4] = r2664; k = t;
      r2670[1] = r2671[2]; lenv = r2670; ((pfun)(r2671[1]))();
    } break;
  }
}
static void FUN_member0_2890_111 (O rr) {
  O r2664 = k[4]; lenv = k[2]; k = k[1];
  O r2669 = rr;
  r2664[6] = r2669;
  O r2654 = ((object*) lenv) [6];
  switch (get_case (r2654)) {
    case (UITAG(0)): { // <
      O r2657 = ((object*) lenv) [2];
      lenv = ((object *)lenv)[1];
      lenv[2] = r2657;
      FUN_member0_2890_0();
    } break;
    case (UITAG(2)): { // >
      O r2660 = ((object*) lenv) [3];
      lenv = ((object *)lenv)[1];
      lenv[2] = r2660;
      FUN_member0_2890_0();
    } break;
    default: {
      O r2663 = ((object*) lenv) [5];
      O r2662 = alloc_no_clear (UOTAG(0),1);
      r2662[1] = r2663;
      IRK_RETURN(2662);
    } break;
  }
}
static void FUN_member0_2890_110 (O rr) {
  O r2636 = k[4]; lenv = k[2]; k = k[1];
  O r2641 = rr;
  r2636[6] = r2641;
  O r2626 = ((object*) lenv) [6];
  switch (get_case (r2626)) {
    case (UITAG(0)): { // <
      O r2629 = ((object*) lenv) [2];
      lenv = ((object *)lenv)[1];
      lenv[2] = r2629;
      FUN_member0_2890_0();
    } break;
    case (UITAG(2)): { // >
      O r2632 = ((object*) lenv) [3];
      lenv = ((object *)lenv)[1];
      lenv[2] = r2632;
      FUN_member0_2890_0();
    } break;
    default: {
      O r2635 = ((object*) lenv) [5];
      O r2634 = alloc_no_clear (UOTAG(0),1);
      r2634[1] = r2635;
      IRK_RETURN(2634);
    } break;
  }
}
static void FUN_tree_2fdelete_567_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 211:
  //   (t1389 t1862 -> cmp) t1389 |tuple=(product (tree t2171 t2172) bool) |
  O r2687 = alloc_cleared (TC_ENV, 4);
  r2687[1] = lenv; lenv = r2687;
  O r2688 = ((object**) lenv) [1][3];
  r2687[2] = r2688;
  O r2689 = ((object**) lenv) [1][4];
  r2687[3] = r2689;
  // env 212:
  //   ((tree t1398 t1397) -> |tuple=(product (tree t1398 t1397) bool) ...t1423|) ((tree t1502 t1501) -> |tuple=(product (tree t1502 t1501) bool) ...t1527|) ((tree t1605 t1604) -> |tuple=(product (tree t1605 t1604) t1605 t1604 bool) |) ((tree t1862 t1861) -> |tuple=(product (tree t1862 t1861) bool) |)
  O r2695 = alloc_cleared (TC_ENV, 5);
  r2695[1] = lenv; lenv = r2695;
  O r2696 = allocate (TC_CLOSURE, 2);
  r2696[1] = FUN_lunbalanced_2739_i102_0; r2696[2] = lenv;
  r2695[2] = r2696;
  O r2697 = allocate (TC_CLOSURE, 2);
  r2697[1] = FUN_runbalanced_2740_i102_0; r2697[2] = lenv;
  r2695[3] = r2697;
  O r2698 = allocate (TC_CLOSURE, 2);
  r2698[1] = FUN_remove_min_2741_i102_0; r2698[2] = lenv;
  r2695[4] = r2698;
  O r2699 = allocate (TC_CLOSURE, 2);
  r2699[1] = FUN_remove_aux_2743_i102_0; r2699[2] = lenv;
  r2695[5] = r2699;
  // env 213:
  //   (tree t2171 t2172)
  O r2692 = allocate (TC_ENV, 2);
  O r2694 = ((object***) lenv) [1][1][2];
  r2692[2] = r2694;
  O r2693 = ((object*) lenv) [5];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_tree_2fdelete_567_112; t[4] = r2687; k = t;
  r2692[1] = r2693[2]; lenv = r2692; FUN_remove_aux_2743_i102_0();
}
static void FUN_tree_2fdelete_567_112 (O rr) {
  O r2687 = k[4]; lenv = k[2]; k = k[1];
  O r2691 = rr;
  lenv = lenv[1];
  O r2690 = r2691;
  r2687[4] = r2690;
  O r2686 = ((object*) lenv) [4];
  O r2683 = UOBJ_GET(r2686,0);
  O r2685 = r2683; // reg varref
  IRK_RETURN(2685);
}
static void FUN_remove_aux_2743_i102_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2702 = ((object*) lenv) [2];
  switch (get_case (r2702)) {
    case (UITAG(2)): { // empty
      O r2704 = allocate (UOTAG(3), 2);
      O r2705 = (object *) 131094;
      r2704[1] = r2705;
      O r2706 = (object *) 6;
      r2704[2] = r2706;
      IRK_RETURN(2704);
    } break;
    case (UOTAG(1)): { // black
      // env 214:
      //   (tree t1862 t1861) (tree t1862 t1861) t1862 t1861 cmp
      O r2848 = alloc_cleared (TC_ENV, 6);
      r2848[1] = lenv; lenv = r2848;
      O r2861 = ((object**) lenv) [1][2];
      O r2849 = UOBJ_GET(r2861,0);
      r2848[2] = r2849;
      O r2860 = ((object**) lenv) [1][2];
      O r2850 = UOBJ_GET(r2860,1);
      r2848[3] = r2850;
      O r2859 = ((object**) lenv) [1][2];
      O r2851 = UOBJ_GET(r2859,2);
      r2848[4] = r2851;
      O r2858 = ((object**) lenv) [1][2];
      O r2852 = UOBJ_GET(r2858,3);
      r2848[5] = r2852;
      // env 215:
      //   t1389 t1862
      O r2854 = allocate (TC_ENV, 3);
      O r2856 = ((object****) lenv) [1][1][1][3];
      r2854[2] = r2856;
      O r2857 = ((object*) lenv) [4];
      r2854[3] = r2857;
      O r2855 = ((object****) lenv) [1][1][1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_113; t[4] = r2848; k = t;
      r2854[1] = r2855[2]; lenv = r2854; ((pfun)(r2855[1]))();
    } break;
    default: {
      // env 216:
      //   (tree t1862 t1861) (tree t1862 t1861) t1862 t1861 cmp
      O r2975 = alloc_cleared (TC_ENV, 6);
      r2975[1] = lenv; lenv = r2975;
      O r2988 = ((object**) lenv) [1][2];
      O r2976 = UOBJ_GET(r2988,0);
      r2975[2] = r2976;
      O r2987 = ((object**) lenv) [1][2];
      O r2977 = UOBJ_GET(r2987,1);
      r2975[3] = r2977;
      O r2986 = ((object**) lenv) [1][2];
      O r2978 = UOBJ_GET(r2986,2);
      r2975[4] = r2978;
      O r2985 = ((object**) lenv) [1][2];
      O r2979 = UOBJ_GET(r2985,3);
      r2975[5] = r2979;
      // env 217:
      //   t1389 t1862
      O r2981 = allocate (TC_ENV, 3);
      O r2983 = ((object****) lenv) [1][1][1][3];
      r2981[2] = r2983;
      O r2984 = ((object*) lenv) [4];
      r2981[3] = r2984;
      O r2982 = ((object****) lenv) [1][1][1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_114; t[4] = r2975; k = t;
      r2981[1] = r2982[2]; lenv = r2981; ((pfun)(r2982[1]))();
    } break;
  }
}
static void FUN_remove_aux_2743_i102_114 (O rr) {
  O r2975 = k[4]; lenv = k[2]; k = k[1];
  O r2980 = rr;
  r2975[6] = r2980;
  O r2866 = ((object*) lenv) [6];
  switch (get_case (r2866)) {
    case (UITAG(0)): { // <
      // env 218:
      //   |tuple=(product (tree t1862 t1861) bool) |
      O r2893 = alloc_cleared (TC_ENV, 2);
      r2893[1] = lenv; lenv = r2893;
      // env 219:
      //   (tree t1862 t1861)
      O r2895 = allocate (TC_ENV, 2);
      O r2897 = ((object**) lenv) [1][2];
      r2895[2] = r2897;
      O r2896 = ((object****) lenv) [1][1][1][5];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_115; t[4] = r2893; k = t;
      r2895[1] = r2896[2]; lenv = r2895; FUN_remove_aux_2743_i102_0();
    } break;
    case (UITAG(2)): { // >
      // env 220:
      //   |tuple=(product (tree t1862 t1861) bool) |
      O r2924 = alloc_cleared (TC_ENV, 2);
      r2924[1] = lenv; lenv = r2924;
      // env 221:
      //   (tree t1862 t1861)
      O r2926 = allocate (TC_ENV, 2);
      O r2928 = ((object**) lenv) [1][3];
      r2926[2] = r2928;
      O r2927 = ((object****) lenv) [1][1][1][5];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_116; t[4] = r2924; k = t;
      r2926[1] = r2927[2]; lenv = r2926; FUN_remove_aux_2743_i102_0();
    } break;
    default: {
      // fatbar jn=289 label=288
      O r2933 = ((object*) lenv) [3];
      switch (get_case (r2933)) {
        case (UITAG(2)): { // empty
          O r2935 = allocate (UOTAG(3), 2);
          O r2936 = ((object*) lenv) [2];
          r2935[1] = r2936;
          O r2937 = (object *) 6;
          r2935[2] = r2937;
          IRK_RETURN(2935);
        } break;
        default: {
          FAIL_288();
        }
      }
    } break;
  }
}
static void FAIL_288() {
  // env 222:
  //   |tuple=(product (tree t1862 t1861) t1862 t1861 bool) |
  O r2970 = alloc_cleared (TC_ENV, 2);
  r2970[1] = lenv; lenv = r2970;
  // env 223:
  //   (tree t1862 t1861)
  O r2972 = allocate (TC_ENV, 2);
  O r2974 = ((object**) lenv) [1][3];
  r2972[2] = r2974;
  O r2973 = ((object****) lenv) [1][1][1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_117; t[4] = r2970; k = t;
  r2972[1] = r2973[2]; lenv = r2972; FUN_remove_min_2741_i102_0();
}
static void FUN_remove_aux_2743_i102_117 (O rr) {
  O r2970 = k[4]; lenv = k[2]; k = k[1];
  O r2971 = rr;
  r2970[2] = r2971;
  // env 224:
  //   (tree t1862 t1861) t1862 t1861 bool (tree t1862 t1861)
  O r2956 = alloc_cleared (TC_ENV, 6);
  r2956[1] = lenv; lenv = r2956;
  O r2969 = ((object**) lenv) [1][2];
  O r2957 = UOBJ_GET(r2969,0);
  r2956[2] = r2957;
  O r2968 = ((object**) lenv) [1][2];
  O r2958 = UOBJ_GET(r2968,1);
  r2956[3] = r2958;
  O r2967 = ((object**) lenv) [1][2];
  O r2959 = UOBJ_GET(r2967,2);
  r2956[4] = r2959;
  O r2966 = ((object**) lenv) [1][2];
  O r2960 = UOBJ_GET(r2966,3);
  r2956[5] = r2960;
  O r2962 = ((object***) lenv) [1][1][2];
  O r2963 = ((object*) lenv) [2];
  O r2964 = ((object*) lenv) [3];
  O r2965 = ((object*) lenv) [4];
  O r2961 = alloc_no_clear (UOTAG(0),4);
  r2961[1] = r2962;
  r2961[2] = r2963;
  r2961[3] = r2964;
  r2961[4] = r2965;
  r2956[6] = r2961;
  O r2946 = ((object*) lenv) [5];
  if IRK_IS_TRUE(r2946) {
    // env 225:
    //   (tree t1862 t1861)
    O r2949 = allocate (TC_ENV, 2);
    O r2951 = ((object*) lenv) [6];
    r2949[2] = r2951;
    O r2950 = ((object*****) lenv) [1][1][1][1][2];
    r2949[1] = r2950[2]; lenv = r2949; FUN_lunbalanced_2739_i102_0();
  } else {
    O r2953 = allocate (UOTAG(3), 2);
    O r2954 = ((object*) lenv) [6];
    r2953[1] = r2954;
    O r2955 = (object *) 6;
    r2953[2] = r2955;
    IRK_RETURN(2953);
  }
}
static void FUN_remove_aux_2743_i102_116 (O rr) {
  O r2924 = k[4]; lenv = k[2]; k = k[1];
  O r2925 = rr;
  r2924[2] = r2925;
  O r2923 = ((object*) lenv) [2];
  O r2903 = UOBJ_GET(r2923,0);
  O r2922 = ((object*) lenv) [2];
  O r2904 = UOBJ_GET(r2922,1);
  O r2918 = ((object**) lenv) [1][2];
  O r2919 = r2903; // reg varref
  O r2920 = ((object**) lenv) [1][4];
  O r2921 = ((object**) lenv) [1][5];
  O r2905 = alloc_no_clear (UOTAG(0),4);
  r2905[1] = r2918;
  r2905[2] = r2919;
  r2905[3] = r2920;
  r2905[4] = r2921;
  O r2908 = r2904; // reg varref
  if IRK_IS_TRUE(r2908) {
    // env 226:
    //   (tree t1862 t1861)
    O r2911 = allocate (TC_ENV, 2);
    O r2913 = r2905; // reg varref
    r2911[2] = r2913;
    O r2912 = ((object****) lenv) [1][1][1][2];
    r2911[1] = r2912[2]; lenv = r2911; FUN_lunbalanced_2739_i102_0();
  } else {
    O r2915 = allocate (UOTAG(3), 2);
    O r2916 = r2905; // reg varref
    r2915[1] = r2916;
    O r2917 = (object *) 6;
    r2915[2] = r2917;
    IRK_RETURN(2915);
  }
}
static void FUN_remove_aux_2743_i102_115 (O rr) {
  O r2893 = k[4]; lenv = k[2]; k = k[1];
  O r2894 = rr;
  r2893[2] = r2894;
  O r2892 = ((object*) lenv) [2];
  O r2872 = UOBJ_GET(r2892,0);
  O r2891 = ((object*) lenv) [2];
  O r2873 = UOBJ_GET(r2891,1);
  O r2887 = r2872; // reg varref
  O r2888 = ((object**) lenv) [1][3];
  O r2889 = ((object**) lenv) [1][4];
  O r2890 = ((object**) lenv) [1][5];
  O r2874 = alloc_no_clear (UOTAG(0),4);
  r2874[1] = r2887;
  r2874[2] = r2888;
  r2874[3] = r2889;
  r2874[4] = r2890;
  O r2877 = r2873; // reg varref
  if IRK_IS_TRUE(r2877) {
    // env 227:
    //   (tree t1862 t1861)
    O r2880 = allocate (TC_ENV, 2);
    O r2882 = r2874; // reg varref
    r2880[2] = r2882;
    O r2881 = ((object****) lenv) [1][1][1][3];
    r2880[1] = r2881[2]; lenv = r2880; FUN_runbalanced_2740_i102_0();
  } else {
    O r2884 = allocate (UOTAG(3), 2);
    O r2885 = r2874; // reg varref
    r2884[1] = r2885;
    O r2886 = (object *) 6;
    r2884[2] = r2886;
    IRK_RETURN(2884);
  }
}
static void FUN_remove_aux_2743_i102_113 (O rr) {
  O r2848 = k[4]; lenv = k[2]; k = k[1];
  O r2853 = rr;
  r2848[6] = r2853;
  O r2711 = ((object*) lenv) [6];
  switch (get_case (r2711)) {
    case (UITAG(0)): { // <
      // env 228:
      //   |tuple=(product (tree t1862 t1861) bool) |
      O r2738 = alloc_cleared (TC_ENV, 2);
      r2738[1] = lenv; lenv = r2738;
      // env 229:
      //   (tree t1862 t1861)
      O r2740 = allocate (TC_ENV, 2);
      O r2742 = ((object**) lenv) [1][2];
      r2740[2] = r2742;
      O r2741 = ((object****) lenv) [1][1][1][5];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_118; t[4] = r2738; k = t;
      r2740[1] = r2741[2]; lenv = r2740; FUN_remove_aux_2743_i102_0();
    } break;
    case (UITAG(2)): { // >
      // env 230:
      //   |tuple=(product (tree t1862 t1861) bool) |
      O r2769 = alloc_cleared (TC_ENV, 2);
      r2769[1] = lenv; lenv = r2769;
      // env 231:
      //   (tree t1862 t1861)
      O r2771 = allocate (TC_ENV, 2);
      O r2773 = ((object**) lenv) [1][3];
      r2771[2] = r2773;
      O r2772 = ((object****) lenv) [1][1][1][5];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_119; t[4] = r2769; k = t;
      r2771[1] = r2772[2]; lenv = r2771; FUN_remove_aux_2743_i102_0();
    } break;
    default: {
      // fatbar jn=276 label=275
      O r2778 = ((object*) lenv) [3];
      switch (get_case (r2778)) {
        case (UITAG(2)): { // empty
          // env 232:
          //   (tree t1862 t1861)
          O r2809 = allocate (TC_ENV, 2);
          r2809[1] = lenv; lenv = r2809;
          O r2810 = ((object**) lenv) [1][2];
          r2809[2] = r2810;
          // fatbar jn=279 label=278
          O r2785 = ((object*) lenv) [2];
          switch (get_case (r2785)) {
            case (UOTAG(0)): { // red
              O r2803 = ((object*) lenv) [2];
              O r2788 = UOBJ_GET(r2803,0);
              O r2802 = ((object*) lenv) [2];
              O r2789 = UOBJ_GET(r2802,1);
              O r2801 = ((object*) lenv) [2];
              O r2790 = UOBJ_GET(r2801,2);
              O r2800 = ((object*) lenv) [2];
              O r2791 = UOBJ_GET(r2800,3);
              O r2793 = allocate (UOTAG(3), 2);
              O r2796 = r2788; // reg varref
              O r2797 = r2789; // reg varref
              O r2798 = r2790; // reg varref
              O r2799 = r2791; // reg varref
              O r2794 = alloc_no_clear (UOTAG(1),4);
              r2794[1] = r2796;
              r2794[2] = r2797;
              r2794[3] = r2798;
              r2794[4] = r2799;
              r2793[1] = r2794;
              O r2795 = (object *) 6;
              r2793[2] = r2795;
              IRK_RETURN(2793);
            } break;
            default: {
              FAIL_278();
            }
          }
        } break;
        default: {
          FAIL_275();
        }
      }
    } break;
  }
}
static void FAIL_278() {
  O r2806 = allocate (UOTAG(3), 2);
  O r2807 = ((object*) lenv) [2];
  r2806[1] = r2807;
  O r2808 = (object *) 65542;
  r2806[2] = r2808;
  IRK_RETURN(2806);
}
static void FAIL_275() {
  // env 233:
  //   |tuple=(product (tree t1862 t1861) t1862 t1861 bool) |
  O r2843 = alloc_cleared (TC_ENV, 2);
  r2843[1] = lenv; lenv = r2843;
  // env 234:
  //   (tree t1862 t1861)
  O r2845 = allocate (TC_ENV, 2);
  O r2847 = ((object**) lenv) [1][3];
  r2845[2] = r2847;
  O r2846 = ((object****) lenv) [1][1][1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_remove_aux_2743_i102_120; t[4] = r2843; k = t;
  r2845[1] = r2846[2]; lenv = r2845; FUN_remove_min_2741_i102_0();
}
static void FUN_remove_aux_2743_i102_120 (O rr) {
  O r2843 = k[4]; lenv = k[2]; k = k[1];
  O r2844 = rr;
  r2843[2] = r2844;
  // env 235:
  //   (tree t1862 t1861) t1862 t1861 bool (tree t1862 t1861)
  O r2829 = alloc_cleared (TC_ENV, 6);
  r2829[1] = lenv; lenv = r2829;
  O r2842 = ((object**) lenv) [1][2];
  O r2830 = UOBJ_GET(r2842,0);
  r2829[2] = r2830;
  O r2841 = ((object**) lenv) [1][2];
  O r2831 = UOBJ_GET(r2841,1);
  r2829[3] = r2831;
  O r2840 = ((object**) lenv) [1][2];
  O r2832 = UOBJ_GET(r2840,2);
  r2829[4] = r2832;
  O r2839 = ((object**) lenv) [1][2];
  O r2833 = UOBJ_GET(r2839,3);
  r2829[5] = r2833;
  O r2835 = ((object***) lenv) [1][1][2];
  O r2836 = ((object*) lenv) [2];
  O r2837 = ((object*) lenv) [3];
  O r2838 = ((object*) lenv) [4];
  O r2834 = alloc_no_clear (UOTAG(1),4);
  r2834[1] = r2835;
  r2834[2] = r2836;
  r2834[3] = r2837;
  r2834[4] = r2838;
  r2829[6] = r2834;
  O r2819 = ((object*) lenv) [5];
  if IRK_IS_TRUE(r2819) {
    // env 236:
    //   (tree t1862 t1861)
    O r2822 = allocate (TC_ENV, 2);
    O r2824 = ((object*) lenv) [6];
    r2822[2] = r2824;
    O r2823 = ((object*****) lenv) [1][1][1][1][2];
    r2822[1] = r2823[2]; lenv = r2822; FUN_lunbalanced_2739_i102_0();
  } else {
    O r2826 = allocate (UOTAG(3), 2);
    O r2827 = ((object*) lenv) [6];
    r2826[1] = r2827;
    O r2828 = (object *) 6;
    r2826[2] = r2828;
    IRK_RETURN(2826);
  }
}
static void FUN_remove_aux_2743_i102_119 (O rr) {
  O r2769 = k[4]; lenv = k[2]; k = k[1];
  O r2770 = rr;
  r2769[2] = r2770;
  O r2768 = ((object*) lenv) [2];
  O r2748 = UOBJ_GET(r2768,0);
  O r2767 = ((object*) lenv) [2];
  O r2749 = UOBJ_GET(r2767,1);
  O r2763 = ((object**) lenv) [1][2];
  O r2764 = r2748; // reg varref
  O r2765 = ((object**) lenv) [1][4];
  O r2766 = ((object**) lenv) [1][5];
  O r2750 = alloc_no_clear (UOTAG(1),4);
  r2750[1] = r2763;
  r2750[2] = r2764;
  r2750[3] = r2765;
  r2750[4] = r2766;
  O r2753 = r2749; // reg varref
  if IRK_IS_TRUE(r2753) {
    // env 237:
    //   (tree t1862 t1861)
    O r2756 = allocate (TC_ENV, 2);
    O r2758 = r2750; // reg varref
    r2756[2] = r2758;
    O r2757 = ((object****) lenv) [1][1][1][2];
    r2756[1] = r2757[2]; lenv = r2756; FUN_lunbalanced_2739_i102_0();
  } else {
    O r2760 = allocate (UOTAG(3), 2);
    O r2761 = r2750; // reg varref
    r2760[1] = r2761;
    O r2762 = (object *) 6;
    r2760[2] = r2762;
    IRK_RETURN(2760);
  }
}
static void FUN_remove_aux_2743_i102_118 (O rr) {
  O r2738 = k[4]; lenv = k[2]; k = k[1];
  O r2739 = rr;
  r2738[2] = r2739;
  O r2737 = ((object*) lenv) [2];
  O r2717 = UOBJ_GET(r2737,0);
  O r2736 = ((object*) lenv) [2];
  O r2718 = UOBJ_GET(r2736,1);
  O r2732 = r2717; // reg varref
  O r2733 = ((object**) lenv) [1][3];
  O r2734 = ((object**) lenv) [1][4];
  O r2735 = ((object**) lenv) [1][5];
  O r2719 = alloc_no_clear (UOTAG(1),4);
  r2719[1] = r2732;
  r2719[2] = r2733;
  r2719[3] = r2734;
  r2719[4] = r2735;
  O r2722 = r2718; // reg varref
  if IRK_IS_TRUE(r2722) {
    // env 238:
    //   (tree t1862 t1861)
    O r2725 = allocate (TC_ENV, 2);
    O r2727 = r2719; // reg varref
    r2725[2] = r2727;
    O r2726 = ((object****) lenv) [1][1][1][3];
    r2725[1] = r2726[2]; lenv = r2725; FUN_runbalanced_2740_i102_0();
  } else {
    O r2729 = allocate (UOTAG(3), 2);
    O r2730 = r2719; // reg varref
    r2729[1] = r2730;
    O r2731 = (object *) 6;
    r2729[2] = r2731;
    IRK_RETURN(2729);
  }
}
static void FUN_remove_min_2741_i102_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2991 = ((object*) lenv) [2];
  switch (get_case (r2991)) {
    case (UITAG(2)): { // empty
      O r2998 = (O) &constructed_155;
      O r3000 = r2998; // reg varref
      O r3002 = r2998; // reg varref
      O r3001 = TAG_INTEGER((irk_int)((irk_string*)(r3002))->len);
      TAG_INTEGER((irk_int)fwrite (((irk_string*)(r3000))->data, 1, UNTAG_INTEGER(r3001), stdout));
      O r2999 = (object *) 14;
      O r2997 = (O) &constructed_154;
      dump_object (r2997, 0);
      O r2996 = (object *) 655362;
      TAG_INTEGER((irk_int)fputc (GET_CHAR(r2996), stdout));
      // dead  = (object *) 14;
      O r2995 = (object *) -1;
      exit_continuation(r2995);
      O r2994 = (object *) TC_UNDEFINED;
      IRK_RETURN(2994);
    } break;
    case (UOTAG(1)): { // black
      // env 239:
      //   (tree t1605 t1604) (tree t1605 t1604) t1605 t1604
      O r3104 = alloc_cleared (TC_ENV, 5);
      r3104[1] = lenv; lenv = r3104;
      O r3112 = ((object**) lenv) [1][2];
      O r3105 = UOBJ_GET(r3112,0);
      r3104[2] = r3105;
      O r3111 = ((object**) lenv) [1][2];
      O r3106 = UOBJ_GET(r3111,1);
      r3104[3] = r3106;
      O r3110 = ((object**) lenv) [1][2];
      O r3107 = UOBJ_GET(r3110,2);
      r3104[4] = r3107;
      O r3109 = ((object**) lenv) [1][2];
      O r3108 = UOBJ_GET(r3109,3);
      r3104[5] = r3108;
      // fatbar jn=295 label=294
      O r3009 = ((object*) lenv) [2];
      switch (get_case (r3009)) {
        case (UITAG(2)): { // empty
          O r3012 = ((object*) lenv) [3];
          switch (get_case (r3012)) {
            case (UOTAG(1)): { // black
              O r3019 = (O) &constructed_155;
              O r3021 = r3019; // reg varref
              O r3023 = r3019; // reg varref
              O r3022 = TAG_INTEGER((irk_int)((irk_string*)(r3023))->len);
              TAG_INTEGER((irk_int)fwrite (((irk_string*)(r3021))->data, 1, UNTAG_INTEGER(r3022), stdout));
              O r3020 = (object *) 14;
              O r3018 = (O) &constructed_154;
              dump_object (r3018, 0);
              O r3017 = (object *) 655362;
              TAG_INTEGER((irk_int)fputc (GET_CHAR(r3017), stdout));
              // dead  = (object *) 14;
              O r3016 = (object *) -1;
              exit_continuation(r3016);
              O r3015 = (object *) TC_UNDEFINED;
              IRK_RETURN(3015);
            } break;
            case (UITAG(2)): { // empty
              O r3025 = allocate (UOTAG(3), 4);
              O r3026 = (object *) 131094;
              r3025[1] = r3026;
              O r3027 = ((object*) lenv) [4];
              r3025[2] = r3027;
              O r3028 = ((object*) lenv) [5];
              r3025[3] = r3028;
              O r3029 = (object *) 65542;
              r3025[4] = r3029;
              IRK_RETURN(3025);
            } break;
            default: {
              O r3049 = ((object*) lenv) [3];
              O r3032 = UOBJ_GET(r3049,0);
              O r3048 = ((object*) lenv) [3];
              O r3033 = UOBJ_GET(r3048,1);
              O r3047 = ((object*) lenv) [3];
              O r3034 = UOBJ_GET(r3047,2);
              O r3046 = ((object*) lenv) [3];
              O r3035 = UOBJ_GET(r3046,3);
              O r3037 = allocate (UOTAG(3), 4);
              O r3042 = r3032; // reg varref
              O r3043 = r3033; // reg varref
              O r3044 = r3034; // reg varref
              O r3045 = r3035; // reg varref
              O r3038 = alloc_no_clear (UOTAG(1),4);
              r3038[1] = r3042;
              r3038[2] = r3043;
              r3038[3] = r3044;
              r3038[4] = r3045;
              r3037[1] = r3038;
              O r3039 = ((object*) lenv) [4];
              r3037[2] = r3039;
              O r3040 = ((object*) lenv) [5];
              r3037[3] = r3040;
              O r3041 = (object *) 6;
              r3037[4] = r3041;
              IRK_RETURN(3037);
            } break;
          }
        } break;
        default: {
          FAIL_294();
        }
      }
    } break;
    default: {
      // env 240:
      //   (tree t1605 t1604) (tree t1605 t1604) t1605 t1604
      O r3180 = alloc_cleared (TC_ENV, 5);
      r3180[1] = lenv; lenv = r3180;
      O r3188 = ((object**) lenv) [1][2];
      O r3181 = UOBJ_GET(r3188,0);
      r3180[2] = r3181;
      O r3187 = ((object**) lenv) [1][2];
      O r3182 = UOBJ_GET(r3187,1);
      r3180[3] = r3182;
      O r3186 = ((object**) lenv) [1][2];
      O r3183 = UOBJ_GET(r3186,2);
      r3180[4] = r3183;
      O r3185 = ((object**) lenv) [1][2];
      O r3184 = UOBJ_GET(r3185,3);
      r3180[5] = r3184;
      // fatbar jn=302 label=301
      O r3119 = ((object*) lenv) [2];
      switch (get_case (r3119)) {
        case (UITAG(2)): { // empty
          O r3121 = allocate (UOTAG(3), 4);
          O r3122 = ((object*) lenv) [3];
          r3121[1] = r3122;
          O r3123 = ((object*) lenv) [4];
          r3121[2] = r3123;
          O r3124 = ((object*) lenv) [5];
          r3121[3] = r3124;
          O r3125 = (object *) 6;
          r3121[4] = r3125;
          IRK_RETURN(3121);
        } break;
        default: {
          FAIL_301();
        }
      }
    } break;
  }
}
static void FAIL_301() {
  // env 241:
  //   |tuple=(product (tree t1605 t1604) t1605 t1604 bool) |
  O r3175 = alloc_cleared (TC_ENV, 2);
  r3175[1] = lenv; lenv = r3175;
  // env 242:
  //   (tree t1605 t1604)
  O r3177 = allocate (TC_ENV, 2);
  O r3179 = ((object**) lenv) [1][2];
  r3177[2] = r3179;
  O r3178 = ((object****) lenv) [1][1][1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_remove_min_2741_i102_121; t[4] = r3175; k = t;
  r3177[1] = r3178[2]; lenv = r3177; FUN_remove_min_2741_i102_0();
}
static void FUN_remove_min_2741_i102_121 (O rr) {
  O r3175 = k[4]; lenv = k[2]; k = k[1];
  O r3176 = rr;
  r3175[2] = r3176;
  // env 243:
  //   (tree t1605 t1604) t1605 t1604 bool (tree t1605 t1604)
  O r3161 = alloc_cleared (TC_ENV, 6);
  r3161[1] = lenv; lenv = r3161;
  O r3174 = ((object**) lenv) [1][2];
  O r3162 = UOBJ_GET(r3174,0);
  r3161[2] = r3162;
  O r3173 = ((object**) lenv) [1][2];
  O r3163 = UOBJ_GET(r3173,1);
  r3161[3] = r3163;
  O r3172 = ((object**) lenv) [1][2];
  O r3164 = UOBJ_GET(r3172,2);
  r3161[4] = r3164;
  O r3171 = ((object**) lenv) [1][2];
  O r3165 = UOBJ_GET(r3171,3);
  r3161[5] = r3165;
  O r3167 = ((object*) lenv) [2];
  O r3168 = ((object***) lenv) [1][1][3];
  O r3169 = ((object***) lenv) [1][1][4];
  O r3170 = ((object***) lenv) [1][1][5];
  O r3166 = alloc_no_clear (UOTAG(0),4);
  r3166[1] = r3167;
  r3166[2] = r3168;
  r3166[3] = r3169;
  r3166[4] = r3170;
  r3161[6] = r3166;
  O r3134 = ((object*) lenv) [5];
  if IRK_IS_TRUE(r3134) {
    // env 244:
    //   |tuple=(product (tree t1605 t1604) bool) |
    O r3150 = alloc_cleared (TC_ENV, 2);
    r3150[1] = lenv; lenv = r3150;
    // env 245:
    //   (tree t1605 t1604)
    O r3152 = allocate (TC_ENV, 2);
    O r3154 = ((object**) lenv) [1][6];
    r3152[2] = r3154;
    O r3153 = ((object******) lenv) [1][1][1][1][1][3];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_remove_min_2741_i102_122; t[4] = r3150; k = t;
    r3152[1] = r3153[2]; lenv = r3152; FUN_runbalanced_2740_i102_0();
  } else {
    O r3156 = allocate (UOTAG(3), 4);
    O r3157 = ((object*) lenv) [6];
    r3156[1] = r3157;
    O r3158 = ((object*) lenv) [3];
    r3156[2] = r3158;
    O r3159 = ((object*) lenv) [4];
    r3156[3] = r3159;
    O r3160 = (object *) 6;
    r3156[4] = r3160;
    IRK_RETURN(3156);
  }
}
static void FUN_remove_min_2741_i102_122 (O rr) {
  O r3150 = k[4]; lenv = k[2]; k = k[1];
  O r3151 = rr;
  r3150[2] = r3151;
  O r3149 = ((object*) lenv) [2];
  O r3140 = UOBJ_GET(r3149,0);
  O r3148 = ((object*) lenv) [2];
  O r3141 = UOBJ_GET(r3148,1);
  O r3143 = allocate (UOTAG(3), 4);
  O r3144 = r3140; // reg varref
  r3143[1] = r3144;
  O r3145 = ((object**) lenv) [1][3];
  r3143[2] = r3145;
  O r3146 = ((object**) lenv) [1][4];
  r3143[3] = r3146;
  O r3147 = r3141; // reg varref
  r3143[4] = r3147;
  IRK_RETURN(3143);
}
static void FAIL_294() {
  // env 246:
  //   |tuple=(product (tree t1605 t1604) t1605 t1604 bool) |
  O r3099 = alloc_cleared (TC_ENV, 2);
  r3099[1] = lenv; lenv = r3099;
  // env 247:
  //   (tree t1605 t1604)
  O r3101 = allocate (TC_ENV, 2);
  O r3103 = ((object**) lenv) [1][2];
  r3101[2] = r3103;
  O r3102 = ((object****) lenv) [1][1][1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_remove_min_2741_i102_123; t[4] = r3099; k = t;
  r3101[1] = r3102[2]; lenv = r3101; FUN_remove_min_2741_i102_0();
}
static void FUN_remove_min_2741_i102_123 (O rr) {
  O r3099 = k[4]; lenv = k[2]; k = k[1];
  O r3100 = rr;
  r3099[2] = r3100;
  // env 248:
  //   (tree t1605 t1604) t1605 t1604 bool (tree t1605 t1604)
  O r3085 = alloc_cleared (TC_ENV, 6);
  r3085[1] = lenv; lenv = r3085;
  O r3098 = ((object**) lenv) [1][2];
  O r3086 = UOBJ_GET(r3098,0);
  r3085[2] = r3086;
  O r3097 = ((object**) lenv) [1][2];
  O r3087 = UOBJ_GET(r3097,1);
  r3085[3] = r3087;
  O r3096 = ((object**) lenv) [1][2];
  O r3088 = UOBJ_GET(r3096,2);
  r3085[4] = r3088;
  O r3095 = ((object**) lenv) [1][2];
  O r3089 = UOBJ_GET(r3095,3);
  r3085[5] = r3089;
  O r3091 = ((object*) lenv) [2];
  O r3092 = ((object***) lenv) [1][1][3];
  O r3093 = ((object***) lenv) [1][1][4];
  O r3094 = ((object***) lenv) [1][1][5];
  O r3090 = alloc_no_clear (UOTAG(1),4);
  r3090[1] = r3091;
  r3090[2] = r3092;
  r3090[3] = r3093;
  r3090[4] = r3094;
  r3085[6] = r3090;
  O r3058 = ((object*) lenv) [5];
  if IRK_IS_TRUE(r3058) {
    // env 249:
    //   |tuple=(product (tree t1605 t1604) bool) |
    O r3074 = alloc_cleared (TC_ENV, 2);
    r3074[1] = lenv; lenv = r3074;
    // env 250:
    //   (tree t1605 t1604)
    O r3076 = allocate (TC_ENV, 2);
    O r3078 = ((object**) lenv) [1][6];
    r3076[2] = r3078;
    O r3077 = ((object******) lenv) [1][1][1][1][1][3];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_remove_min_2741_i102_124; t[4] = r3074; k = t;
    r3076[1] = r3077[2]; lenv = r3076; FUN_runbalanced_2740_i102_0();
  } else {
    O r3080 = allocate (UOTAG(3), 4);
    O r3081 = ((object*) lenv) [6];
    r3080[1] = r3081;
    O r3082 = ((object*) lenv) [3];
    r3080[2] = r3082;
    O r3083 = ((object*) lenv) [4];
    r3080[3] = r3083;
    O r3084 = (object *) 6;
    r3080[4] = r3084;
    IRK_RETURN(3080);
  }
}
static void FUN_remove_min_2741_i102_124 (O rr) {
  O r3074 = k[4]; lenv = k[2]; k = k[1];
  O r3075 = rr;
  r3074[2] = r3075;
  O r3073 = ((object*) lenv) [2];
  O r3064 = UOBJ_GET(r3073,0);
  O r3072 = ((object*) lenv) [2];
  O r3065 = UOBJ_GET(r3072,1);
  O r3067 = allocate (UOTAG(3), 4);
  O r3068 = r3064; // reg varref
  r3067[1] = r3068;
  O r3069 = ((object**) lenv) [1][3];
  r3067[2] = r3069;
  O r3070 = ((object**) lenv) [1][4];
  r3067[3] = r3070;
  O r3071 = r3065; // reg varref
  r3067[4] = r3071;
  IRK_RETURN(3067);
}
static void FUN_runbalanced_2740_i102_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=308 label=307
  O r3193 = ((object*) lenv) [2];
  switch (get_case (r3193)) {
    case (UOTAG(0)): { // red
      // env 251:
      //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
      O r3226 = alloc_cleared (TC_ENV, 5);
      r3226[1] = lenv; lenv = r3226;
      O r3234 = ((object**) lenv) [1][2];
      O r3227 = UOBJ_GET(r3234,0);
      r3226[2] = r3227;
      O r3233 = ((object**) lenv) [1][2];
      O r3228 = UOBJ_GET(r3233,1);
      r3226[3] = r3228;
      O r3232 = ((object**) lenv) [1][2];
      O r3229 = UOBJ_GET(r3232,2);
      r3226[4] = r3229;
      O r3231 = ((object**) lenv) [1][2];
      O r3230 = UOBJ_GET(r3231,3);
      r3226[5] = r3230;
      O r3198 = ((object*) lenv) [3];
      switch (get_case (r3198)) {
        case (UOTAG(1)): { // black
          // env 252:
          //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
          O r3215 = alloc_cleared (TC_ENV, 5);
          r3215[1] = lenv; lenv = r3215;
          O r3223 = ((object**) lenv) [1][3];
          O r3216 = UOBJ_GET(r3223,0);
          r3215[2] = r3216;
          O r3222 = ((object**) lenv) [1][3];
          O r3217 = UOBJ_GET(r3222,1);
          r3215[3] = r3217;
          O r3221 = ((object**) lenv) [1][3];
          O r3218 = UOBJ_GET(r3221,2);
          r3215[4] = r3218;
          O r3220 = ((object**) lenv) [1][3];
          O r3219 = UOBJ_GET(r3220,3);
          r3215[5] = r3219;
          O r3202 = alloc_cleared (UOTAG(3), 2);
          // env 253:
          //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
          O r3205 = allocate (TC_ENV, 5);
          O r3207 = ((object**) lenv) [1][2];
          r3205[2] = r3207;
          O r3211 = ((object*) lenv) [2];
          O r3212 = ((object*) lenv) [3];
          O r3213 = ((object*) lenv) [4];
          O r3214 = ((object*) lenv) [5];
          O r3208 = alloc_no_clear (UOTAG(0),4);
          r3208[1] = r3211;
          r3208[2] = r3212;
          r3208[3] = r3213;
          r3208[4] = r3214;
          r3205[3] = r3208;
          O r3209 = ((object**) lenv) [1][4];
          r3205[4] = r3209;
          O r3210 = ((object**) lenv) [1][5];
          r3205[5] = r3210;
          O r3206 = top[35];
          O t = allocate (TC_SAVE, 4);
          t[1] = k; t[2] = lenv; t[3] = FUN_runbalanced_2740_i102_125; t[4] = r3202; k = t;
          r3205[1] = r3206[2]; lenv = r3205; FUN_rbalance_564_0();
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_307();
        }
      }
    } break;
    case (UOTAG(1)): { // black
      // env 254:
      //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
      O r3312 = alloc_cleared (TC_ENV, 5);
      r3312[1] = lenv; lenv = r3312;
      O r3320 = ((object**) lenv) [1][2];
      O r3313 = UOBJ_GET(r3320,0);
      r3312[2] = r3313;
      O r3319 = ((object**) lenv) [1][2];
      O r3314 = UOBJ_GET(r3319,1);
      r3312[3] = r3314;
      O r3318 = ((object**) lenv) [1][2];
      O r3315 = UOBJ_GET(r3318,2);
      r3312[4] = r3315;
      O r3317 = ((object**) lenv) [1][2];
      O r3316 = UOBJ_GET(r3317,3);
      r3312[5] = r3316;
      O r3239 = ((object*) lenv) [3];
      switch (get_case (r3239)) {
        case (UOTAG(1)): { // black
          // env 255:
          //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
          O r3256 = alloc_cleared (TC_ENV, 5);
          r3256[1] = lenv; lenv = r3256;
          O r3264 = ((object**) lenv) [1][3];
          O r3257 = UOBJ_GET(r3264,0);
          r3256[2] = r3257;
          O r3263 = ((object**) lenv) [1][3];
          O r3258 = UOBJ_GET(r3263,1);
          r3256[3] = r3258;
          O r3262 = ((object**) lenv) [1][3];
          O r3259 = UOBJ_GET(r3262,2);
          r3256[4] = r3259;
          O r3261 = ((object**) lenv) [1][3];
          O r3260 = UOBJ_GET(r3261,3);
          r3256[5] = r3260;
          O r3243 = alloc_cleared (UOTAG(3), 2);
          // env 256:
          //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
          O r3246 = allocate (TC_ENV, 5);
          O r3248 = ((object**) lenv) [1][2];
          r3246[2] = r3248;
          O r3252 = ((object*) lenv) [2];
          O r3253 = ((object*) lenv) [3];
          O r3254 = ((object*) lenv) [4];
          O r3255 = ((object*) lenv) [5];
          O r3249 = alloc_no_clear (UOTAG(0),4);
          r3249[1] = r3252;
          r3249[2] = r3253;
          r3249[3] = r3254;
          r3249[4] = r3255;
          r3246[3] = r3249;
          O r3250 = ((object**) lenv) [1][4];
          r3246[4] = r3250;
          O r3251 = ((object**) lenv) [1][5];
          r3246[5] = r3251;
          O r3247 = top[35];
          O t = allocate (TC_SAVE, 4);
          t[1] = k; t[2] = lenv; t[3] = FUN_runbalanced_2740_i102_126; t[4] = r3243; k = t;
          r3246[1] = r3247[2]; lenv = r3246; FUN_rbalance_564_0();
        } break;
        case (UOTAG(0)): { // red
          // env 257:
          //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
          O r3301 = alloc_cleared (TC_ENV, 5);
          r3301[1] = lenv; lenv = r3301;
          O r3309 = ((object**) lenv) [1][3];
          O r3302 = UOBJ_GET(r3309,0);
          r3301[2] = r3302;
          O r3308 = ((object**) lenv) [1][3];
          O r3303 = UOBJ_GET(r3308,1);
          r3301[3] = r3303;
          O r3307 = ((object**) lenv) [1][3];
          O r3304 = UOBJ_GET(r3307,2);
          r3301[4] = r3304;
          O r3306 = ((object**) lenv) [1][3];
          O r3305 = UOBJ_GET(r3306,3);
          r3301[5] = r3305;
          O r3269 = ((object*) lenv) [2];
          switch (get_case (r3269)) {
            case (UOTAG(1)): { // black
              // env 258:
              //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
              O r3290 = alloc_cleared (TC_ENV, 5);
              r3290[1] = lenv; lenv = r3290;
              O r3298 = ((object**) lenv) [1][2];
              O r3291 = UOBJ_GET(r3298,0);
              r3290[2] = r3291;
              O r3297 = ((object**) lenv) [1][2];
              O r3292 = UOBJ_GET(r3297,1);
              r3290[3] = r3292;
              O r3296 = ((object**) lenv) [1][2];
              O r3293 = UOBJ_GET(r3296,2);
              r3290[4] = r3293;
              O r3295 = ((object**) lenv) [1][2];
              O r3294 = UOBJ_GET(r3295,3);
              r3290[5] = r3294;
              O r3273 = alloc_cleared (UOTAG(3), 2);
              // env 259:
              //   (tree t1502 t1501) (tree t1502 t1501) t1502 t1501
              O r3280 = allocate (TC_ENV, 5);
              O r3282 = ((object***) lenv) [1][1][2];
              r3280[2] = r3282;
              O r3286 = ((object*) lenv) [2];
              O r3287 = ((object*) lenv) [3];
              O r3288 = ((object*) lenv) [4];
              O r3289 = ((object*) lenv) [5];
              O r3283 = alloc_no_clear (UOTAG(0),4);
              r3283[1] = r3286;
              r3283[2] = r3287;
              r3283[3] = r3288;
              r3283[4] = r3289;
              r3280[3] = r3283;
              O r3284 = ((object***) lenv) [1][1][4];
              r3280[4] = r3284;
              O r3285 = ((object***) lenv) [1][1][5];
              r3280[5] = r3285;
              O r3281 = top[35];
              O t = allocate (TC_SAVE, 4);
              t[1] = k; t[2] = lenv; t[3] = FUN_runbalanced_2740_i102_127; t[4] = r3273; k = t;
              r3280[1] = r3281[2]; lenv = r3280; FUN_rbalance_564_0();
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_307();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_307();
        }
      }
    } break;
    default: {
      FAIL_307();
    }
  }
}
static void FUN_runbalanced_2740_i102_127 (O rr) {
  O r3273 = k[4]; lenv = k[2]; k = k[1];
  O r3276 = rr;
  O r3277 = ((object**) lenv) [1][3];
  O r3278 = ((object**) lenv) [1][4];
  O r3279 = ((object**) lenv) [1][5];
  O r3274 = alloc_no_clear (UOTAG(1),4);
  r3274[1] = r3276;
  r3274[2] = r3277;
  r3274[3] = r3278;
  r3274[4] = r3279;
  r3273[1] = r3274;
  O r3275 = (object *) 6;
  r3273[2] = r3275;
  IRK_RETURN(3273);
}
static void FUN_runbalanced_2740_i102_126 (O rr) {
  O r3243 = k[4]; lenv = k[2]; k = k[1];
  O r3244 = rr;
  r3243[1] = r3244;
  O r3245 = (object *) 65542;
  r3243[2] = r3245;
  IRK_RETURN(3243);
}
static void FUN_runbalanced_2740_i102_125 (O rr) {
  O r3202 = k[4]; lenv = k[2]; k = k[1];
  O r3203 = rr;
  r3202[1] = r3203;
  O r3204 = (object *) 6;
  r3202[2] = r3204;
  IRK_RETURN(3202);
}
static void FAIL_307() {
  O r3328 = (O) &constructed_155;
  O r3330 = r3328; // reg varref
  O r3332 = r3328; // reg varref
  O r3331 = TAG_INTEGER((irk_int)((irk_string*)(r3332))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r3330))->data, 1, UNTAG_INTEGER(r3331), stdout));
  O r3329 = (object *) 14;
  O r3327 = (O) &constructed_154;
  dump_object (r3327, 0);
  O r3326 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r3326), stdout));
  // dead  = (object *) 14;
  O r3325 = (object *) -1;
  exit_continuation(r3325);
  O r3324 = (object *) TC_UNDEFINED;
  IRK_RETURN(3324);
}
static void FUN_lunbalanced_2739_i102_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=314 label=313
  O r3337 = ((object*) lenv) [2];
  switch (get_case (r3337)) {
    case (UOTAG(0)): { // red
      // env 260:
      //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
      O r3370 = alloc_cleared (TC_ENV, 5);
      r3370[1] = lenv; lenv = r3370;
      O r3378 = ((object**) lenv) [1][2];
      O r3371 = UOBJ_GET(r3378,0);
      r3370[2] = r3371;
      O r3377 = ((object**) lenv) [1][2];
      O r3372 = UOBJ_GET(r3377,1);
      r3370[3] = r3372;
      O r3376 = ((object**) lenv) [1][2];
      O r3373 = UOBJ_GET(r3376,2);
      r3370[4] = r3373;
      O r3375 = ((object**) lenv) [1][2];
      O r3374 = UOBJ_GET(r3375,3);
      r3370[5] = r3374;
      O r3342 = ((object*) lenv) [2];
      switch (get_case (r3342)) {
        case (UOTAG(1)): { // black
          // env 261:
          //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
          O r3359 = alloc_cleared (TC_ENV, 5);
          r3359[1] = lenv; lenv = r3359;
          O r3367 = ((object**) lenv) [1][2];
          O r3360 = UOBJ_GET(r3367,0);
          r3359[2] = r3360;
          O r3366 = ((object**) lenv) [1][2];
          O r3361 = UOBJ_GET(r3366,1);
          r3359[3] = r3361;
          O r3365 = ((object**) lenv) [1][2];
          O r3362 = UOBJ_GET(r3365,2);
          r3359[4] = r3362;
          O r3364 = ((object**) lenv) [1][2];
          O r3363 = UOBJ_GET(r3364,3);
          r3359[5] = r3363;
          O r3346 = alloc_cleared (UOTAG(3), 2);
          // env 262:
          //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
          O r3349 = allocate (TC_ENV, 5);
          O r3355 = ((object*) lenv) [2];
          O r3356 = ((object*) lenv) [3];
          O r3357 = ((object*) lenv) [4];
          O r3358 = ((object*) lenv) [5];
          O r3351 = alloc_no_clear (UOTAG(0),4);
          r3351[1] = r3355;
          r3351[2] = r3356;
          r3351[3] = r3357;
          r3351[4] = r3358;
          r3349[2] = r3351;
          O r3352 = ((object**) lenv) [1][3];
          r3349[3] = r3352;
          O r3353 = ((object**) lenv) [1][4];
          r3349[4] = r3353;
          O r3354 = ((object**) lenv) [1][5];
          r3349[5] = r3354;
          O r3350 = top[34];
          O t = allocate (TC_SAVE, 4);
          t[1] = k; t[2] = lenv; t[3] = FUN_lunbalanced_2739_i102_128; t[4] = r3346; k = t;
          r3349[1] = r3350[2]; lenv = r3349; FUN_lbalance_563_0();
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_313();
        }
      }
    } break;
    case (UOTAG(1)): { // black
      // env 263:
      //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
      O r3456 = alloc_cleared (TC_ENV, 5);
      r3456[1] = lenv; lenv = r3456;
      O r3464 = ((object**) lenv) [1][2];
      O r3457 = UOBJ_GET(r3464,0);
      r3456[2] = r3457;
      O r3463 = ((object**) lenv) [1][2];
      O r3458 = UOBJ_GET(r3463,1);
      r3456[3] = r3458;
      O r3462 = ((object**) lenv) [1][2];
      O r3459 = UOBJ_GET(r3462,2);
      r3456[4] = r3459;
      O r3461 = ((object**) lenv) [1][2];
      O r3460 = UOBJ_GET(r3461,3);
      r3456[5] = r3460;
      O r3383 = ((object*) lenv) [2];
      switch (get_case (r3383)) {
        case (UOTAG(1)): { // black
          // env 264:
          //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
          O r3400 = alloc_cleared (TC_ENV, 5);
          r3400[1] = lenv; lenv = r3400;
          O r3408 = ((object**) lenv) [1][2];
          O r3401 = UOBJ_GET(r3408,0);
          r3400[2] = r3401;
          O r3407 = ((object**) lenv) [1][2];
          O r3402 = UOBJ_GET(r3407,1);
          r3400[3] = r3402;
          O r3406 = ((object**) lenv) [1][2];
          O r3403 = UOBJ_GET(r3406,2);
          r3400[4] = r3403;
          O r3405 = ((object**) lenv) [1][2];
          O r3404 = UOBJ_GET(r3405,3);
          r3400[5] = r3404;
          O r3387 = alloc_cleared (UOTAG(3), 2);
          // env 265:
          //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
          O r3390 = allocate (TC_ENV, 5);
          O r3396 = ((object*) lenv) [2];
          O r3397 = ((object*) lenv) [3];
          O r3398 = ((object*) lenv) [4];
          O r3399 = ((object*) lenv) [5];
          O r3392 = alloc_no_clear (UOTAG(0),4);
          r3392[1] = r3396;
          r3392[2] = r3397;
          r3392[3] = r3398;
          r3392[4] = r3399;
          r3390[2] = r3392;
          O r3393 = ((object**) lenv) [1][3];
          r3390[3] = r3393;
          O r3394 = ((object**) lenv) [1][4];
          r3390[4] = r3394;
          O r3395 = ((object**) lenv) [1][5];
          r3390[5] = r3395;
          O r3391 = top[34];
          O t = allocate (TC_SAVE, 4);
          t[1] = k; t[2] = lenv; t[3] = FUN_lunbalanced_2739_i102_129; t[4] = r3387; k = t;
          r3390[1] = r3391[2]; lenv = r3390; FUN_lbalance_563_0();
        } break;
        case (UOTAG(0)): { // red
          // env 266:
          //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
          O r3445 = alloc_cleared (TC_ENV, 5);
          r3445[1] = lenv; lenv = r3445;
          O r3453 = ((object**) lenv) [1][2];
          O r3446 = UOBJ_GET(r3453,0);
          r3445[2] = r3446;
          O r3452 = ((object**) lenv) [1][2];
          O r3447 = UOBJ_GET(r3452,1);
          r3445[3] = r3447;
          O r3451 = ((object**) lenv) [1][2];
          O r3448 = UOBJ_GET(r3451,2);
          r3445[4] = r3448;
          O r3450 = ((object**) lenv) [1][2];
          O r3449 = UOBJ_GET(r3450,3);
          r3445[5] = r3449;
          O r3413 = ((object*) lenv) [3];
          switch (get_case (r3413)) {
            case (UOTAG(1)): { // black
              // env 267:
              //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
              O r3434 = alloc_cleared (TC_ENV, 5);
              r3434[1] = lenv; lenv = r3434;
              O r3442 = ((object**) lenv) [1][3];
              O r3435 = UOBJ_GET(r3442,0);
              r3434[2] = r3435;
              O r3441 = ((object**) lenv) [1][3];
              O r3436 = UOBJ_GET(r3441,1);
              r3434[3] = r3436;
              O r3440 = ((object**) lenv) [1][3];
              O r3437 = UOBJ_GET(r3440,2);
              r3434[4] = r3437;
              O r3439 = ((object**) lenv) [1][3];
              O r3438 = UOBJ_GET(r3439,3);
              r3434[5] = r3438;
              O r3417 = alloc_cleared (UOTAG(3), 2);
              O r3420 = ((object**) lenv) [1][2];
              // env 268:
              //   (tree t1398 t1397) (tree t1398 t1397) t1398 t1397
              O r3424 = allocate (TC_ENV, 5);
              O r3430 = ((object*) lenv) [2];
              O r3431 = ((object*) lenv) [3];
              O r3432 = ((object*) lenv) [4];
              O r3433 = ((object*) lenv) [5];
              O r3426 = alloc_no_clear (UOTAG(0),4);
              r3426[1] = r3430;
              r3426[2] = r3431;
              r3426[3] = r3432;
              r3426[4] = r3433;
              r3424[2] = r3426;
              O r3427 = ((object***) lenv) [1][1][3];
              r3424[3] = r3427;
              O r3428 = ((object***) lenv) [1][1][4];
              r3424[4] = r3428;
              O r3429 = ((object***) lenv) [1][1][5];
              r3424[5] = r3429;
              O r3425 = top[34];
              O t = allocate (TC_SAVE, 5);
              t[1] = k; t[2] = lenv; t[3] = FUN_lunbalanced_2739_i102_130; t[4] = r3417; t[5] = r3420; k = t;
              r3424[1] = r3425[2]; lenv = r3424; FUN_lbalance_563_0();
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_313();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_313();
        }
      }
    } break;
    default: {
      FAIL_313();
    }
  }
}
static void FUN_lunbalanced_2739_i102_130 (O rr) {
  O r3417 = k[4]; O r3420 = k[5]; lenv = k[2]; k = k[1];
  O r3421 = rr;
  O r3422 = ((object**) lenv) [1][4];
  O r3423 = ((object**) lenv) [1][5];
  O r3418 = alloc_no_clear (UOTAG(1),4);
  r3418[1] = r3420;
  r3418[2] = r3421;
  r3418[3] = r3422;
  r3418[4] = r3423;
  r3417[1] = r3418;
  O r3419 = (object *) 6;
  r3417[2] = r3419;
  IRK_RETURN(3417);
}
static void FUN_lunbalanced_2739_i102_129 (O rr) {
  O r3387 = k[4]; lenv = k[2]; k = k[1];
  O r3388 = rr;
  r3387[1] = r3388;
  O r3389 = (object *) 65542;
  r3387[2] = r3389;
  IRK_RETURN(3387);
}
static void FUN_lunbalanced_2739_i102_128 (O rr) {
  O r3346 = k[4]; lenv = k[2]; k = k[1];
  O r3347 = rr;
  r3346[1] = r3347;
  O r3348 = (object *) 6;
  r3346[2] = r3348;
  IRK_RETURN(3346);
}
static void FAIL_313() {
  O r3472 = (O) &constructed_155;
  O r3474 = r3472; // reg varref
  O r3476 = r3472; // reg varref
  O r3475 = TAG_INTEGER((irk_int)((irk_string*)(r3476))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r3474))->data, 1, UNTAG_INTEGER(r3475), stdout));
  O r3473 = (object *) 14;
  O r3471 = (O) &constructed_154;
  dump_object (r3471, 0);
  O r3470 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r3470), stdout));
  // dead  = (object *) 14;
  O r3469 = (object *) -1;
  exit_continuation(r3469);
  O r3468 = (object *) TC_UNDEFINED;
  IRK_RETURN(3468);
}
static void FUN_tree_2finsert_565_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 269:
  //   ((tree t1321 t1322) -> (tree t1321 t1322))
  O r3510 = alloc_cleared (TC_ENV, 2);
  r3510[1] = lenv; lenv = r3510;
  O r3511 = allocate (TC_CLOSURE, 2);
  r3511[1] = FUN_ins_2719_0; r3511[2] = lenv;
  r3510[2] = r3511;
  // env 270:
  //   (tree t1321 t1322)
  O r3505 = alloc_cleared (TC_ENV, 2);
  r3505[1] = lenv; lenv = r3505;
  // env 271:
  //   (tree t1321 t1322)
  O r3507 = allocate (TC_ENV, 2);
  O r3509 = ((object***) lenv) [1][1][2];
  r3507[2] = r3509;
  O r3508 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_tree_2finsert_565_131; t[4] = r3505; k = t;
  r3507[1] = r3508[2]; lenv = r3507; FUN_ins_2719_0();
}
static void FUN_tree_2finsert_565_131 (O rr) {
  O r3505 = k[4]; lenv = k[2]; k = k[1];
  O r3506 = rr;
  r3505[2] = r3506;
  // fatbar jn=320 label=319
  O r3485 = ((object*) lenv) [2];
  switch (get_case (r3485)) {
    case (UOTAG(0)): { // red
      O r3501 = ((object*) lenv) [2];
      O r3488 = UOBJ_GET(r3501,0);
      O r3500 = ((object*) lenv) [2];
      O r3489 = UOBJ_GET(r3500,1);
      O r3499 = ((object*) lenv) [2];
      O r3490 = UOBJ_GET(r3499,2);
      O r3498 = ((object*) lenv) [2];
      O r3491 = UOBJ_GET(r3498,3);
      O r3494 = r3488; // reg varref
      O r3495 = r3489; // reg varref
      O r3496 = r3490; // reg varref
      O r3497 = r3491; // reg varref
      O r3493 = alloc_no_clear (UOTAG(1),4);
      r3493[1] = r3494;
      r3493[2] = r3495;
      r3493[3] = r3496;
      r3493[4] = r3497;
      IRK_RETURN(3493);
    } break;
    default: {
      FAIL_319();
    }
  }
}
static void FAIL_319() {
  O r3504 = ((object*) lenv) [2];
  IRK_RETURN(3504);
}
static void FUN_ins_2719_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r3514 = ((object*) lenv) [2];
  switch (get_case (r3514)) {
    case (UITAG(2)): { // empty
      O r3517 = (object *) 131094;
      O r3518 = (object *) 131094;
      O r3519 = ((object***) lenv) [1][1][4];
      O r3520 = ((object***) lenv) [1][1][5];
      O r3516 = alloc_no_clear (UOTAG(0),4);
      r3516[1] = r3517;
      r3516[2] = r3518;
      r3516[3] = r3519;
      r3516[4] = r3520;
      IRK_RETURN(3516);
    } break;
    case (UOTAG(0)): { // red
      // env 272:
      //   (tree t1321 t1322) (tree t1321 t1322) t1321 t1322 cmp
      O r3546 = alloc_cleared (TC_ENV, 6);
      r3546[1] = lenv; lenv = r3546;
      O r3559 = ((object**) lenv) [1][2];
      O r3547 = UOBJ_GET(r3559,0);
      r3546[2] = r3547;
      O r3558 = ((object**) lenv) [1][2];
      O r3548 = UOBJ_GET(r3558,1);
      r3546[3] = r3548;
      O r3557 = ((object**) lenv) [1][2];
      O r3549 = UOBJ_GET(r3557,2);
      r3546[4] = r3549;
      O r3556 = ((object**) lenv) [1][2];
      O r3550 = UOBJ_GET(r3556,3);
      r3546[5] = r3550;
      // env 273:
      //   t1321 t1321
      O r3552 = allocate (TC_ENV, 3);
      O r3554 = ((object****) lenv) [1][1][1][4];
      r3552[2] = r3554;
      O r3555 = ((object*) lenv) [4];
      r3552[3] = r3555;
      O r3553 = ((object****) lenv) [1][1][1][3];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_132; t[4] = r3546; k = t;
      r3552[1] = r3553[2]; lenv = r3552; ((pfun)(r3553[1]))();
    } break;
    default: {
      // env 274:
      //   (tree t1321 t1322) (tree t1321 t1322) t1321 t1322 cmp
      O r3589 = alloc_cleared (TC_ENV, 6);
      r3589[1] = lenv; lenv = r3589;
      O r3602 = ((object**) lenv) [1][2];
      O r3590 = UOBJ_GET(r3602,0);
      r3589[2] = r3590;
      O r3601 = ((object**) lenv) [1][2];
      O r3591 = UOBJ_GET(r3601,1);
      r3589[3] = r3591;
      O r3600 = ((object**) lenv) [1][2];
      O r3592 = UOBJ_GET(r3600,2);
      r3589[4] = r3592;
      O r3599 = ((object**) lenv) [1][2];
      O r3593 = UOBJ_GET(r3599,3);
      r3589[5] = r3593;
      // env 275:
      //   t1321 t1321
      O r3595 = allocate (TC_ENV, 3);
      O r3597 = ((object****) lenv) [1][1][1][4];
      r3595[2] = r3597;
      O r3598 = ((object*) lenv) [4];
      r3595[3] = r3598;
      O r3596 = ((object****) lenv) [1][1][1][3];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_133; t[4] = r3589; k = t;
      r3595[1] = r3596[2]; lenv = r3595; ((pfun)(r3596[1]))();
    } break;
  }
}
static void FUN_ins_2719_133 (O rr) {
  O r3589 = k[4]; lenv = k[2]; k = k[1];
  O r3594 = rr;
  r3589[6] = r3594;
  O r3564 = ((object*) lenv) [6];
  switch (get_case (r3564)) {
    case (UITAG(0)): { // <
      // env 276:
      //   (tree t1321 t1322) (tree t1321 t1322) t1321 t1322
      O r3567 = alloc_cleared (TC_ENV, 5);
      // env 277:
      //   (tree t1321 t1322)
      O r3573 = allocate (TC_ENV, 2);
      O r3575 = ((object*) lenv) [2];
      r3573[2] = r3575;
      O r3574 = ((object***) lenv) [1][1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_134; t[4] = r3567; k = t;
      r3573[1] = r3574[2]; lenv = r3573; FUN_ins_2719_0();
    } break;
    case (UITAG(2)): { // >
      // env 278:
      //   (tree t1321 t1322) (tree t1321 t1322) t1321 t1322
      O r3578 = alloc_cleared (TC_ENV, 5);
      O r3580 = ((object*) lenv) [2];
      r3578[2] = r3580;
      // env 279:
      //   (tree t1321 t1322)
      O r3584 = allocate (TC_ENV, 2);
      O r3586 = ((object*) lenv) [3];
      r3584[2] = r3586;
      O r3585 = ((object***) lenv) [1][1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_135; t[4] = r3578; k = t;
      r3584[1] = r3585[2]; lenv = r3584; FUN_ins_2719_0();
    } break;
    default: {
      O r3588 = ((object**) lenv) [1][2];
      IRK_RETURN(3588);
    } break;
  }
}
static void FUN_ins_2719_135 (O rr) {
  O r3578 = k[4]; lenv = k[2]; k = k[1];
  O r3581 = rr;
  r3578[3] = r3581;
  O r3582 = ((object*) lenv) [4];
  r3578[4] = r3582;
  O r3583 = ((object*) lenv) [5];
  r3578[5] = r3583;
  O r3579 = top[35];
  r3578[1] = r3579[2]; lenv = r3578; FUN_rbalance_564_0();
}
static void FUN_ins_2719_134 (O rr) {
  O r3567 = k[4]; lenv = k[2]; k = k[1];
  O r3569 = rr;
  r3567[2] = r3569;
  O r3570 = ((object*) lenv) [3];
  r3567[3] = r3570;
  O r3571 = ((object*) lenv) [4];
  r3567[4] = r3571;
  O r3572 = ((object*) lenv) [5];
  r3567[5] = r3572;
  O r3568 = top[34];
  r3567[1] = r3568[2]; lenv = r3567; FUN_lbalance_563_0();
}
static void FUN_ins_2719_132 (O rr) {
  O r3546 = k[4]; lenv = k[2]; k = k[1];
  O r3551 = rr;
  r3546[6] = r3551;
  O r3525 = ((object*) lenv) [6];
  switch (get_case (r3525)) {
    case (UITAG(0)): { // <
      // env 280:
      //   (tree t1321 t1322)
      O r3532 = allocate (TC_ENV, 2);
      O r3534 = ((object*) lenv) [2];
      r3532[2] = r3534;
      O r3533 = ((object***) lenv) [1][1][2];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_136; ; k = t;
      r3532[1] = r3533[2]; lenv = r3532; FUN_ins_2719_0();
    } break;
    case (UITAG(2)): { // >
      O r3537 = ((object*) lenv) [2];
      // env 281:
      //   (tree t1321 t1322)
      O r3541 = allocate (TC_ENV, 2);
      O r3543 = ((object*) lenv) [3];
      r3541[2] = r3543;
      O r3542 = ((object***) lenv) [1][1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_ins_2719_137; t[4] = r3537; k = t;
      r3541[1] = r3542[2]; lenv = r3541; FUN_ins_2719_0();
    } break;
    default: {
      O r3545 = ((object**) lenv) [1][2];
      IRK_RETURN(3545);
    } break;
  }
}
static void FUN_ins_2719_137 (O rr) {
  O r3537 = k[4]; lenv = k[2]; k = k[1];
  O r3538 = rr;
  O r3539 = ((object*) lenv) [4];
  O r3540 = ((object*) lenv) [5];
  O r3536 = alloc_no_clear (UOTAG(0),4);
  r3536[1] = r3537;
  r3536[2] = r3538;
  r3536[3] = r3539;
  r3536[4] = r3540;
  IRK_RETURN(3536);
}
static void FUN_ins_2719_136 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r3528 = rr;
  O r3529 = ((object*) lenv) [3];
  O r3530 = ((object*) lenv) [4];
  O r3531 = ((object*) lenv) [5];
  O r3527 = alloc_no_clear (UOTAG(0),4);
  r3527[1] = r3528;
  r3527[2] = r3529;
  r3527[3] = r3530;
  r3527[4] = r3531;
  IRK_RETURN(3527);
}
static void FUN_rbalance_564_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=326 label=325
  O r3607 = ((object*) lenv) [3];
  switch (get_case (r3607)) {
    case (UOTAG(0)): { // red
      // env 282:
      //   (tree t1265 t1266) (tree t1265 t1266) t1265 t1266
      O r3671 = alloc_cleared (TC_ENV, 5);
      r3671[1] = lenv; lenv = r3671;
      O r3679 = ((object**) lenv) [1][3];
      O r3672 = UOBJ_GET(r3679,0);
      r3671[2] = r3672;
      O r3678 = ((object**) lenv) [1][3];
      O r3673 = UOBJ_GET(r3678,1);
      r3671[3] = r3673;
      O r3677 = ((object**) lenv) [1][3];
      O r3674 = UOBJ_GET(r3677,2);
      r3671[4] = r3674;
      O r3676 = ((object**) lenv) [1][3];
      O r3675 = UOBJ_GET(r3676,3);
      r3671[5] = r3675;
      // fatbar jn=329 label=328
      O r3614 = ((object*) lenv) [2];
      switch (get_case (r3614)) {
        case (UOTAG(0)): { // red
          // env 283:
          //   (tree t1265 t1266) (tree t1265 t1266) t1265 t1266
          O r3631 = alloc_cleared (TC_ENV, 5);
          r3631[1] = lenv; lenv = r3631;
          O r3639 = ((object**) lenv) [1][2];
          O r3632 = UOBJ_GET(r3639,0);
          r3631[2] = r3632;
          O r3638 = ((object**) lenv) [1][2];
          O r3633 = UOBJ_GET(r3638,1);
          r3631[3] = r3633;
          O r3637 = ((object**) lenv) [1][2];
          O r3634 = UOBJ_GET(r3637,2);
          r3631[4] = r3634;
          O r3636 = ((object**) lenv) [1][2];
          O r3635 = UOBJ_GET(r3636,3);
          r3631[5] = r3635;
          O r3627 = ((object***) lenv) [1][1][2];
          O r3628 = ((object*) lenv) [2];
          O r3629 = ((object***) lenv) [1][1][4];
          O r3630 = ((object***) lenv) [1][1][5];
          O r3619 = alloc_no_clear (UOTAG(1),4);
          r3619[1] = r3627;
          r3619[2] = r3628;
          r3619[3] = r3629;
          r3619[4] = r3630;
          O r3623 = ((object*) lenv) [3];
          O r3624 = ((object**) lenv) [1][3];
          O r3625 = ((object**) lenv) [1][4];
          O r3626 = ((object**) lenv) [1][5];
          O r3620 = alloc_no_clear (UOTAG(1),4);
          r3620[1] = r3623;
          r3620[2] = r3624;
          r3620[3] = r3625;
          r3620[4] = r3626;
          O r3621 = ((object*) lenv) [4];
          O r3622 = ((object*) lenv) [5];
          O r3618 = alloc_no_clear (UOTAG(0),4);
          r3618[1] = r3619;
          r3618[2] = r3620;
          r3618[3] = r3621;
          r3618[4] = r3622;
          IRK_RETURN(3618);
        } break;
        default: {
          FAIL_328();
        }
      }
    } break;
    default: {
      FAIL_325();
    }
  }
}
static void FAIL_328() {
  O r3643 = ((object*) lenv) [3];
  switch (get_case (r3643)) {
    case (UOTAG(0)): { // red
      // env 284:
      //   (tree t1265 t1266) (tree t1265 t1266) t1265 t1266
      O r3660 = alloc_cleared (TC_ENV, 5);
      r3660[1] = lenv; lenv = r3660;
      O r3668 = ((object**) lenv) [1][3];
      O r3661 = UOBJ_GET(r3668,0);
      r3660[2] = r3661;
      O r3667 = ((object**) lenv) [1][3];
      O r3662 = UOBJ_GET(r3667,1);
      r3660[3] = r3662;
      O r3666 = ((object**) lenv) [1][3];
      O r3663 = UOBJ_GET(r3666,2);
      r3660[4] = r3663;
      O r3665 = ((object**) lenv) [1][3];
      O r3664 = UOBJ_GET(r3665,3);
      r3660[5] = r3664;
      O r3656 = ((object***) lenv) [1][1][2];
      O r3657 = ((object**) lenv) [1][2];
      O r3658 = ((object***) lenv) [1][1][4];
      O r3659 = ((object***) lenv) [1][1][5];
      O r3648 = alloc_no_clear (UOTAG(1),4);
      r3648[1] = r3656;
      r3648[2] = r3657;
      r3648[3] = r3658;
      r3648[4] = r3659;
      O r3652 = ((object*) lenv) [2];
      O r3653 = ((object*) lenv) [3];
      O r3654 = ((object*) lenv) [4];
      O r3655 = ((object*) lenv) [5];
      O r3649 = alloc_no_clear (UOTAG(1),4);
      r3649[1] = r3652;
      r3649[2] = r3653;
      r3649[3] = r3654;
      r3649[4] = r3655;
      O r3650 = ((object**) lenv) [1][4];
      O r3651 = ((object**) lenv) [1][5];
      O r3647 = alloc_no_clear (UOTAG(0),4);
      r3647[1] = r3648;
      r3647[2] = r3649;
      r3647[3] = r3650;
      r3647[4] = r3651;
      IRK_RETURN(3647);
    } break;
    default: {
      lenv = ((object *)lenv)[1];
      FAIL_325();
    }
  }
}
static void FAIL_325() {
  O r3683 = ((object*) lenv) [2];
  O r3684 = ((object*) lenv) [3];
  O r3685 = ((object*) lenv) [4];
  O r3686 = ((object*) lenv) [5];
  O r3682 = alloc_no_clear (UOTAG(1),4);
  r3682[1] = r3683;
  r3682[2] = r3684;
  r3682[3] = r3685;
  r3682[4] = r3686;
  IRK_RETURN(3682);
}
static void FUN_lbalance_563_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=333 label=332
  O r3691 = ((object*) lenv) [2];
  switch (get_case (r3691)) {
    case (UOTAG(0)): { // red
      // env 285:
      //   (tree t1209 t1210) (tree t1209 t1210) t1209 t1210
      O r3755 = alloc_cleared (TC_ENV, 5);
      r3755[1] = lenv; lenv = r3755;
      O r3763 = ((object**) lenv) [1][2];
      O r3756 = UOBJ_GET(r3763,0);
      r3755[2] = r3756;
      O r3762 = ((object**) lenv) [1][2];
      O r3757 = UOBJ_GET(r3762,1);
      r3755[3] = r3757;
      O r3761 = ((object**) lenv) [1][2];
      O r3758 = UOBJ_GET(r3761,2);
      r3755[4] = r3758;
      O r3760 = ((object**) lenv) [1][2];
      O r3759 = UOBJ_GET(r3760,3);
      r3755[5] = r3759;
      // fatbar jn=336 label=335
      O r3698 = ((object*) lenv) [2];
      switch (get_case (r3698)) {
        case (UOTAG(0)): { // red
          // env 286:
          //   (tree t1209 t1210) (tree t1209 t1210) t1209 t1210
          O r3715 = alloc_cleared (TC_ENV, 5);
          r3715[1] = lenv; lenv = r3715;
          O r3723 = ((object**) lenv) [1][2];
          O r3716 = UOBJ_GET(r3723,0);
          r3715[2] = r3716;
          O r3722 = ((object**) lenv) [1][2];
          O r3717 = UOBJ_GET(r3722,1);
          r3715[3] = r3717;
          O r3721 = ((object**) lenv) [1][2];
          O r3718 = UOBJ_GET(r3721,2);
          r3715[4] = r3718;
          O r3720 = ((object**) lenv) [1][2];
          O r3719 = UOBJ_GET(r3720,3);
          r3715[5] = r3719;
          O r3711 = ((object*) lenv) [2];
          O r3712 = ((object*) lenv) [3];
          O r3713 = ((object*) lenv) [4];
          O r3714 = ((object*) lenv) [5];
          O r3703 = alloc_no_clear (UOTAG(1),4);
          r3703[1] = r3711;
          r3703[2] = r3712;
          r3703[3] = r3713;
          r3703[4] = r3714;
          O r3707 = ((object**) lenv) [1][3];
          O r3708 = ((object***) lenv) [1][1][3];
          O r3709 = ((object***) lenv) [1][1][4];
          O r3710 = ((object***) lenv) [1][1][5];
          O r3704 = alloc_no_clear (UOTAG(1),4);
          r3704[1] = r3707;
          r3704[2] = r3708;
          r3704[3] = r3709;
          r3704[4] = r3710;
          O r3705 = ((object**) lenv) [1][4];
          O r3706 = ((object**) lenv) [1][5];
          O r3702 = alloc_no_clear (UOTAG(0),4);
          r3702[1] = r3703;
          r3702[2] = r3704;
          r3702[3] = r3705;
          r3702[4] = r3706;
          IRK_RETURN(3702);
        } break;
        default: {
          FAIL_335();
        }
      }
    } break;
    default: {
      FAIL_332();
    }
  }
}
static void FAIL_335() {
  O r3727 = ((object*) lenv) [3];
  switch (get_case (r3727)) {
    case (UOTAG(0)): { // red
      // env 287:
      //   (tree t1209 t1210) (tree t1209 t1210) t1209 t1210
      O r3744 = alloc_cleared (TC_ENV, 5);
      r3744[1] = lenv; lenv = r3744;
      O r3752 = ((object**) lenv) [1][3];
      O r3745 = UOBJ_GET(r3752,0);
      r3744[2] = r3745;
      O r3751 = ((object**) lenv) [1][3];
      O r3746 = UOBJ_GET(r3751,1);
      r3744[3] = r3746;
      O r3750 = ((object**) lenv) [1][3];
      O r3747 = UOBJ_GET(r3750,2);
      r3744[4] = r3747;
      O r3749 = ((object**) lenv) [1][3];
      O r3748 = UOBJ_GET(r3749,3);
      r3744[5] = r3748;
      O r3740 = ((object**) lenv) [1][2];
      O r3741 = ((object*) lenv) [2];
      O r3742 = ((object**) lenv) [1][4];
      O r3743 = ((object**) lenv) [1][5];
      O r3732 = alloc_no_clear (UOTAG(1),4);
      r3732[1] = r3740;
      r3732[2] = r3741;
      r3732[3] = r3742;
      r3732[4] = r3743;
      O r3736 = ((object*) lenv) [3];
      O r3737 = ((object***) lenv) [1][1][3];
      O r3738 = ((object***) lenv) [1][1][4];
      O r3739 = ((object***) lenv) [1][1][5];
      O r3733 = alloc_no_clear (UOTAG(1),4);
      r3733[1] = r3736;
      r3733[2] = r3737;
      r3733[3] = r3738;
      r3733[4] = r3739;
      O r3734 = ((object*) lenv) [4];
      O r3735 = ((object*) lenv) [5];
      O r3731 = alloc_no_clear (UOTAG(0),4);
      r3731[1] = r3732;
      r3731[2] = r3733;
      r3731[3] = r3734;
      r3731[4] = r3735;
      IRK_RETURN(3731);
    } break;
    default: {
      lenv = ((object *)lenv)[1];
      FAIL_332();
    }
  }
}
static void FAIL_332() {
  O r3767 = ((object*) lenv) [2];
  O r3768 = ((object*) lenv) [3];
  O r3769 = ((object*) lenv) [4];
  O r3770 = ((object*) lenv) [5];
  O r3766 = alloc_no_clear (UOTAG(1),4);
  r3766[1] = r3767;
  r3766[2] = r3768;
  r3766[3] = r3769;
  r3766[4] = r3770;
  IRK_RETURN(3766);
}
static void FUN_int__3estring_363_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r3773 = (object *) 1;
  O r3774 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3773)==UNTAG_INTEGER(r3774))) {
    // env 288:
    //   char
    O r3777 = allocate (TC_ENV, 2);
    O r3779 = (object *) 3145730;
    r3777[2] = r3779;
    O r3778 = top[17];
    r3777[1] = r3778[2]; lenv = r3777; FUN_char__3estring_333_0();
  } else {
    // env 289:
    //   (int (list char) -> string)
    O r3796 = alloc_cleared (TC_ENV, 2);
    r3796[1] = lenv; lenv = r3796;
    O r3797 = allocate (TC_CLOSURE, 2);
    r3797[1] = FUN_loop_1410_0; r3797[2] = lenv;
    r3796[2] = r3797;
    // env 290:
    //   int (list char)
    O r3784 = alloc_cleared (TC_ENV, 3);
    O r3788 = ((object**) lenv) [1][2];
    O r3790 = r3788; // reg varref
    O r3791 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3790)<UNTAG_INTEGER(r3791))) {
      O r3793 = (object *) 1;
      O r3794 = r3788; // reg varref
      O r3792 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r3793)-UNTAG_INTEGER(r3794));
      O r3789 = r3792;
      JUMP_342(r3789, r3784);
    } else {
      O r3795 = r3788; // reg varref
      O r3789 = r3795;
      JUMP_342(r3789, r3784);
    }
  }
}
static void JUMP_342(O r3789, O r3784) {
  O r3786 = r3789; // reg varref
  r3784[2] = r3786;
  O r3787 = (object *) 10;
  r3784[3] = r3787;
  O r3785 = ((object*) lenv) [2];
  r3784[1] = r3785[2]; lenv = r3784; FUN_loop_1410_0();
}
static void FUN_loop_1410_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r3800 = (object *) 1;
  O r3801 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3800)==UNTAG_INTEGER(r3801))) {
    // env 291:
    //   (list char)
    O r3804 = alloc_cleared (TC_ENV, 2);
    O r3807 = ((object***) lenv) [1][1][2];
    O r3808 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3807)<UNTAG_INTEGER(r3808))) {
      O r3810 = (object *) 2949122;
      O r3811 = ((object*) lenv) [3];
      O r3809 = alloc_no_clear (TC_PAIR,2);
      r3809[1] = r3810;
      r3809[2] = r3811;
      O r3806 = r3809;
      JUMP_346(r3806, r3804);
    } else {
      O r3812 = ((object*) lenv) [3];
      O r3806 = r3812;
      JUMP_346(r3806, r3804);
    }
  } else {
    // env 292:
    //   int int
    O r3830 = allocate (TC_ENV, 3);
    O r3832 = ((object*) lenv) [2];
    r3830[2] = r3832;
    O r3833 = (object *) 21;
    r3830[3] = r3833;
    O r3831 = top[31];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1410_138; ; k = t;
    r3830[1] = r3831[2]; lenv = r3830; ((pfun)(r3831[1]))();
  }
}
static void FUN_loop_1410_138 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r3815 = rr;
  // env 293:
  //   int int
  O r3821 = alloc_cleared (TC_ENV, 3);
  r3821[1] = lenv; lenv = r3821;
  // env 294:
  //   int int
  O r3826 = allocate (TC_ENV, 3);
  O r3828 = ((object**) lenv) [1][2];
  r3826[2] = r3828;
  O r3829 = (object *) 21;
  r3826[3] = r3829;
  O r3827 = top[32];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1410_139; t[4] = r3815; t[5] = r3821; k = t;
  r3826[1] = r3827[2]; lenv = r3826; ((pfun)(r3827[1]))();
}
static void FUN_loop_1410_139 (O rr) {
  O r3815 = k[4]; O r3821 = k[5]; lenv = k[2]; k = k[1];
  O r3822 = rr;
  r3821[2] = r3822;
  O r3824 = (object *) 97;
  O r3825 = ((object*) lenv) [2];
  O r3823 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r3824)+UNTAG_INTEGER(r3825));
  r3821[3] = r3823;
  O r3820 = ((object*) lenv) [3];
  O r3819 = TO_CHAR(UNTAG_INTEGER(r3820));
  lenv = lenv[1];
  O r3817 = r3819;
  O r3818 = ((object*) lenv) [3];
  O r3816 = alloc_no_clear (TC_PAIR,2);
  r3816[1] = r3817;
  r3816[2] = r3818;
  lenv[2] = r3815;
  lenv[3] = r3816;
  FUN_loop_1410_0();
}
static void JUMP_346(O r3806, O r3804) {
  r3804[2] = r3806;
  O r3805 = top[20];
  r3804[1] = r3805[2]; lenv = r3804; FUN_list__3estring_352_0();
}
static void FUN_safe_char_359_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=348 label=347
  O r3838 = ((object*) lenv) [2];
  O r3839 = (object *) 6029314;
  if IRK_IS_TRUE(IRK_TEST(r3838==r3839)) {
    O r3841 = (O) &constructed_156;
    IRK_RETURN(3841);
  } else {
    FAIL_347();
  }
}
static void FAIL_347() {
  // fatbar jn=352 label=351
  O r3847 = ((object*) lenv) [2];
  O r3848 = (object *) 2228226;
  if IRK_IS_TRUE(IRK_TEST(r3847==r3848)) {
    O r3850 = (O) &constructed_157;
    IRK_RETURN(3850);
  } else {
    FAIL_351();
  }
}
static void FAIL_351() {
  // fatbar jn=356 label=355
  O r3856 = ((object*) lenv) [2];
  O r3857 = (object *) 851970;
  if IRK_IS_TRUE(IRK_TEST(r3856==r3857)) {
    O r3859 = (O) &constructed_158;
    IRK_RETURN(3859);
  } else {
    FAIL_355();
  }
}
static void FAIL_355() {
  // fatbar jn=360 label=359
  O r3865 = ((object*) lenv) [2];
  O r3866 = (object *) 589826;
  if IRK_IS_TRUE(IRK_TEST(r3865==r3866)) {
    O r3868 = (O) &constructed_159;
    IRK_RETURN(3868);
  } else {
    FAIL_359();
  }
}
static void FAIL_359() {
  // fatbar jn=364 label=363
  O r3874 = ((object*) lenv) [2];
  O r3875 = (object *) 655362;
  if IRK_IS_TRUE(IRK_TEST(r3874==r3875)) {
    O r3877 = (O) &constructed_160;
    IRK_RETURN(3877);
  } else {
    FAIL_363();
  }
}
static void FAIL_363() {
  // fatbar jn=368 label=367
  O r3883 = ((object*) lenv) [2];
  O r3884 = (object *) 2097154;
  if IRK_IS_TRUE(IRK_TEST(r3883==r3884)) {
    O r3886 = (O) &constructed_161;
    IRK_RETURN(3886);
  } else {
    FAIL_367();
  }
}
static void FAIL_367() {
  // env 295:
  //   char
  O r3957 = allocate (TC_ENV, 2);
  O r3959 = ((object*) lenv) [2];
  r3957[2] = r3959;
  O r3958 = top[29];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_359_140; ; k = t;
  r3957[1] = r3958[2]; lenv = r3957; ((pfun)(r3958[1]))();
}
static void FUN_safe_char_359_140 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r3890 = rr;
  if IRK_IS_TRUE(r3890) {
    // env 296:
    //   char
    O r3893 = allocate (TC_ENV, 2);
    O r3895 = ((object*) lenv) [2];
    r3893[2] = r3895;
    O r3894 = top[17];
    r3893[1] = r3894[2]; lenv = r3893; FUN_char__3estring_333_0();
  } else {
    // env 297:
    //   (list string)
    O r3898 = alloc_cleared (TC_ENV, 2);
    O r3901 = (O) &constructed_162;
    // env 298:
    //   string int string char int
    O r3946 = alloc_cleared (TC_ENV, 6);
    r3946[1] = lenv; lenv = r3946;
    // env 299:
    //   int
    O r3953 = allocate (TC_ENV, 2);
    O r3956 = ((object**) lenv) [1][2];
    O r3955 = TAG_INTEGER((irk_int)GET_CHAR(r3956));
    r3953[2] = r3955;
    O r3954 = top[23];
    O t = allocate (TC_SAVE, 6);
    t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_359_141; t[4] = r3898; t[5] = r3901; t[6] = r3946; k = t;
    r3953[1] = r3954[2]; lenv = r3953; FUN_int__3ehex_string_372_0();
  }
}
static void FUN_safe_char_359_141 (O rr) {
  O r3898 = k[4]; O r3901 = k[5]; O r3946 = k[6]; lenv = k[2]; k = k[1];
  O r3947 = rr;
  r3946[2] = r3947;
  O r3948 = (object *) 5;
  r3946[3] = r3948;
  O r3949 = ((object*) lenv) [2];
  r3946[4] = r3949;
  O r3950 = (object *) 3145730;
  r3946[5] = r3950;
  O r3952 = ((object*) lenv) [4];
  O r3951 = TAG_INTEGER((irk_int)((irk_string*)(r3952))->len);
  r3946[6] = r3951;
  O r3906 = ((object*) lenv) [6];
  O r3907 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3906)>UNTAG_INTEGER(r3907))) {
    O r3908 = ((object*) lenv) [4];
    O r3905 = r3908;
    JUMP_373(r3905, r3901, r3898);
  } else {
    // env 300:
    //   int
    O r3942 = allocate (TC_ENV, 2);
    r3942[1] = lenv; lenv = r3942;
    O r3944 = ((object**) lenv) [1][3];
    O r3945 = ((object**) lenv) [1][6];
    O r3943 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r3944)-UNTAG_INTEGER(r3945));
    r3942[2] = r3943;
    // env 301:
    //   (list string)
    O r3911 = alloc_cleared (TC_ENV, 2);
    // env 302:
    //   (list char)
    O r3918 = alloc_cleared (TC_ENV, 2);
    // env 303:
    //   (int (list char) -> (list char))
    O r3926 = alloc_cleared (TC_ENV, 2);
    r3926[1] = lenv; lenv = r3926;
    O r3927 = allocate (TC_CLOSURE, 2);
    r3927[1] = FUN_loop_1044_i0_0; r3927[2] = lenv;
    r3926[2] = r3927;
    // env 304:
    //   int (list t2283)
    O r3922 = allocate (TC_ENV, 3);
    O r3924 = ((object**) lenv) [1][2];
    r3922[2] = r3924;
    O r3925 = (object *) 10;
    r3922[3] = r3925;
    O r3923 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 7);
    t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_359_142; t[4] = r3898; t[5] = r3901; t[6] = r3911; t[7] = r3918; k = t;
    r3922[1] = r3923[2]; lenv = r3922; ((pfun)(r3923[1]))();
  }
}
static void FUN_safe_char_359_142 (O rr) {
  O r3898 = k[4]; O r3901 = k[5]; O r3911 = k[6]; O r3918 = k[7]; lenv = k[2]; k = k[1];
  O r3921 = rr;
  lenv = lenv[1];
  O r3920 = r3921;
  r3918[2] = r3920;
  O r3919 = top[20];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_359_143; t[4] = r3898; t[5] = r3901; t[6] = r3911; k = t;
  r3918[1] = r3919[2]; lenv = r3918; FUN_list__3estring_352_0();
}
static void FUN_safe_char_359_143 (O rr) {
  O r3898 = k[4]; O r3901 = k[5]; O r3911 = k[6]; lenv = k[2]; k = k[1];
  O r3914 = rr;
  O r3916 = ((object**) lenv) [1][4];
  O r3917 = (object *) 10;
  O r3915 = alloc_no_clear (TC_PAIR,2);
  r3915[1] = r3916;
  r3915[2] = r3917;
  O r3913 = alloc_no_clear (TC_PAIR,2);
  r3913[1] = r3914;
  r3913[2] = r3915;
  r3911[2] = r3913;
  O r3912 = top[19];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_359_144; t[4] = r3898; t[5] = r3901; k = t;
  r3911[1] = r3912[2]; lenv = r3911; FUN_string_concat_338_0();
}
static void FUN_safe_char_359_144 (O rr) {
  O r3898 = k[4]; O r3901 = k[5]; lenv = k[2]; k = k[1];
  O r3910 = rr;
  lenv = lenv[1];
  O r3909 = r3910;
  O r3905 = r3909;
  JUMP_373(r3905, r3901, r3898);
}
static void FUN_loop_1044_i0_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r3930 = ((object*) lenv) [2];
  O r3931 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r3930)<=UNTAG_INTEGER(r3931))) {
    O r3933 = ((object*) lenv) [3];
    IRK_RETURN(3933);
  } else {
    O r3940 = ((object*) lenv) [2];
    O r3941 = (object *) 3;
    O r3936 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r3940)-UNTAG_INTEGER(r3941));
    O r3938 = ((object****) lenv) [1][1][1][5];
    O r3939 = ((object*) lenv) [3];
    O r3937 = alloc_no_clear (TC_PAIR,2);
    r3937[1] = r3938;
    r3937[2] = r3939;
    lenv[2] = r3936;
    lenv[3] = r3937;
    FUN_loop_1044_i0_0();
  }
}
static void JUMP_373(O r3905, O r3901, O r3898) {
  lenv = lenv[1];
  O r3903 = r3905;
  O r3904 = (object *) 10;
  O r3902 = alloc_no_clear (TC_PAIR,2);
  r3902[1] = r3903;
  r3902[2] = r3904;
  O r3900 = alloc_no_clear (TC_PAIR,2);
  r3900[1] = r3901;
  r3900[2] = r3902;
  r3898[2] = r3900;
  O r3899 = top[19];
  r3898[1] = r3899[2]; lenv = r3898; FUN_string_concat_338_0();
}
static void FUN__24loop31_1374_i1_0 (void) {
  O r3970 = ((object*) lenv) [2];
  switch (get_case (r3970)) {
    case (TC_NIL): { // nil
      O r3972 = (object *) 14;
      IRK_RETURN(3972);
    } break;
    default: {
      O r3986 = ((object*) lenv) [2];
      O r3975 = UOBJ_GET(r3986,0);
      O r3985 = ((object*) lenv) [2];
      O r3976 = UOBJ_GET(r3985,1);
      O r3981 = ((object****) lenv) [1][1][1][3];
      O r3984 = r3975; // reg varref
      O r3982 = TAG_INTEGER((irk_int)GET_CHAR(r3984));
      O r3983 = (object *) 65542;
      range_check (GET_TUPLE_LENGTH(*(object*)r3981), unbox(r3982));
      ((irk_vector*)r3981)->val[unbox(r3982)] = r3983;
      O r3980 = r3976; // reg varref
      lenv[2] = r3980;
      FUN__24loop31_1374_i1_0();
    } break;
  }
}
static void FUN_in_class_question_1372_i1_0 (void) {
  O r3991 = ((object***) lenv) [1][1][3];
  O r3993 = ((object*) lenv) [2];
  O r3992 = TAG_INTEGER((irk_int)GET_CHAR(r3993));
  range_check (GET_TUPLE_LENGTH(*(object*)r3991), unbox(r3992));
  O r3990 = ((irk_vector*)r3991)->val[unbox(r3992)];
  IRK_RETURN(3990);
}
static void FUN_int__3ehex_string_372_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 305:
  //   (int (list char) -> string)
  O r4056 = alloc_cleared (TC_ENV, 2);
  r4056[1] = lenv; lenv = r4056;
  O r4057 = allocate (TC_CLOSURE, 2);
  r4057[1] = FUN_loop_1446_0; r4057[2] = lenv;
  r4056[2] = r4057;
  // env 306:
  //   int (list char)
  O r4044 = alloc_cleared (TC_ENV, 3);
  O r4048 = ((object**) lenv) [1][2];
  O r4050 = r4048; // reg varref
  O r4051 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4050)<UNTAG_INTEGER(r4051))) {
    O r4053 = (object *) 1;
    O r4054 = r4048; // reg varref
    O r4052 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4053)-UNTAG_INTEGER(r4054));
    O r4049 = r4052;
    JUMP_378(r4049, r4044);
  } else {
    O r4055 = r4048; // reg varref
    O r4049 = r4055;
    JUMP_378(r4049, r4044);
  }
}
static void JUMP_378(O r4049, O r4044) {
  O r4046 = r4049; // reg varref
  r4044[2] = r4046;
  O r4047 = (object *) 10;
  r4044[3] = r4047;
  O r4045 = ((object*) lenv) [2];
  r4044[1] = r4045[2]; lenv = r4044; FUN_loop_1446_0();
}
static void FUN_loop_1446_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4060 = (object *) 1;
  O r4061 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4060)==UNTAG_INTEGER(r4061))) {
    // env 307:
    //   (list char)
    O r4064 = alloc_cleared (TC_ENV, 2);
    O r4067 = ((object***) lenv) [1][1][2];
    O r4068 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4067)<UNTAG_INTEGER(r4068))) {
      O r4070 = (object *) 2949122;
      O r4071 = ((object*) lenv) [3];
      O r4069 = alloc_no_clear (TC_PAIR,2);
      r4069[1] = r4070;
      r4069[2] = r4071;
      O r4066 = r4069;
      JUMP_382(r4066, r4064);
    } else {
      O r4072 = ((object*) lenv) [3];
      O r4066 = r4072;
      JUMP_382(r4066, r4064);
    }
  } else {
    O r4083 = ((object*) lenv) [2];
    O r4084 = (object *) 9;
    O r4075 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4083)>>UNTAG_INTEGER(r4084));
    O r4079 = top[22];
    O r4081 = ((object*) lenv) [2];
    O r4082 = (object *) 31;
    O r4080 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4081)&UNTAG_INTEGER(r4082));
    range_check (GET_TUPLE_LENGTH(*(object*)r4079), unbox(r4080));
    O r4077 = ((irk_vector*)r4079)->val[unbox(r4080)];
    O r4078 = ((object*) lenv) [3];
    O r4076 = alloc_no_clear (TC_PAIR,2);
    r4076[1] = r4077;
    r4076[2] = r4078;
    lenv[2] = r4075;
    lenv[3] = r4076;
    FUN_loop_1446_0();
  }
}
static void JUMP_382(O r4066, O r4064) {
  r4064[2] = r4066;
  O r4065 = top[20];
  r4064[1] = r4065[2]; lenv = r4064; FUN_list__3estring_352_0();
}
static void FUN_string__3elist_353_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 308:
  //   ((list char) int -> (list char))
  O r4094 = alloc_cleared (TC_ENV, 2);
  r4094[1] = lenv; lenv = r4094;
  O r4095 = allocate (TC_CLOSURE, 2);
  r4095[1] = FUN_loop_1367_0; r4095[2] = lenv;
  r4094[2] = r4095;
  // env 309:
  //   (list char) int
  O r4089 = allocate (TC_ENV, 3);
  O r4091 = (object *) 10;
  r4089[2] = r4091;
  O r4093 = ((object**) lenv) [1][2];
  O r4092 = TAG_INTEGER((irk_int)((irk_string*)(r4093))->len);
  r4089[3] = r4092;
  O r4090 = ((object*) lenv) [2];
  r4089[1] = r4090[2]; lenv = r4089; FUN_loop_1367_0();
}
static void FUN_loop_1367_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4098 = ((object*) lenv) [3];
  O r4099 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4098)==UNTAG_INTEGER(r4099))) {
    O r4101 = ((object*) lenv) [2];
    IRK_RETURN(4101);
  } else {
    // env 310:
    //   string int
    O r4110 = allocate (TC_ENV, 3);
    O r4112 = ((object***) lenv) [1][1][2];
    r4110[2] = r4112;
    O r4114 = ((object*) lenv) [3];
    O r4115 = (object *) 3;
    O r4113 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4114)-UNTAG_INTEGER(r4115));
    r4110[3] = r4113;
    O r4111 = top[18];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1367_145; ; k = t;
    r4110[1] = r4111[2]; lenv = r4110; FUN_string_ref_336_0();
  }
}
static void FUN_loop_1367_145 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4108 = rr;
  O r4109 = ((object*) lenv) [2];
  O r4104 = alloc_no_clear (TC_PAIR,2);
  r4104[1] = r4108;
  r4104[2] = r4109;
  O r4106 = ((object*) lenv) [3];
  O r4107 = (object *) 3;
  O r4105 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4106)-UNTAG_INTEGER(r4107));
  lenv[2] = r4104;
  lenv[3] = r4105;
  FUN_loop_1367_0();
}
static void FUN_list__3estring_352_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 311:
  //   int int string int
  O r4153 = alloc_cleared (TC_ENV, 5);
  r4153[1] = lenv; lenv = r4153;
  // env 312:
  //   (list char) int
  O r4161 = allocate (TC_ENV, 3);
  O r4163 = ((object**) lenv) [1][2];
  r4161[2] = r4163;
  O r4164 = (object *) 1;
  r4161[3] = r4164;
  O r4162 = top[10];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_list__3estring_352_146; t[4] = r4153; k = t;
  r4161[1] = r4162[2]; lenv = r4161; FUN_length_splat_272_0();
}
static void FUN_list__3estring_352_146 (O rr) {
  O r4153 = k[4]; lenv = k[2]; k = k[1];
  O r4154 = rr;
  r4153[2] = r4154;
  O r4155 = ((object*) lenv) [2];
  r4153[3] = r4155;
  O r4160 = ((object*) lenv) [3];
  O r4159 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r4160)));
  if (freep + unbox(r4159) >= limit) {
    heap_request = unbox(r4159);
    heap1[3] = r4153;
    gc_flip (1);
    r4153 = heap0[3];
  }
  O r4158 = ((object*) lenv) [3];
  O r4156 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r4158))), ((irk_string*)(t))->len = UNTAG_INTEGER(r4158), t);
  r4153[4] = r4156;
  O r4157 = (object *) 1;
  r4153[5] = r4157;
  // env 313:
  //   ((list char) -> undefined)
  O r4125 = alloc_cleared (TC_ENV, 2);
  r4125[1] = lenv; lenv = r4125;
  O r4126 = allocate (TC_CLOSURE, 2);
  r4126[1] = FUN__24loop28_1362_0; r4126[2] = lenv;
  r4125[2] = r4126;
  // env 314:
  //   (list char)
  O r4122 = allocate (TC_ENV, 2);
  O r4124 = ((object***) lenv) [1][1][2];
  r4122[2] = r4124;
  O r4123 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_list__3estring_352_147; ; k = t;
  r4122[1] = r4123[2]; lenv = r4122; FUN__24loop28_1362_0();
}
static void FUN_list__3estring_352_147 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4121 = rr;
  lenv = lenv[1];
  O r4120 = ((object*) lenv) [4];
  IRK_RETURN(4120);
}
static void FUN__24loop28_1362_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4129 = ((object*) lenv) [2];
  switch (get_case (r4129)) {
    case (TC_NIL): { // nil
      O r4131 = (object *) 14;
      IRK_RETURN(4131);
    } break;
    default: {
      // env 315:
      //   char (list char)
      O r4148 = alloc_cleared (TC_ENV, 3);
      r4148[1] = lenv; lenv = r4148;
      O r4152 = ((object**) lenv) [1][2];
      O r4149 = UOBJ_GET(r4152,0);
      r4148[2] = r4149;
      O r4151 = ((object**) lenv) [1][2];
      O r4150 = UOBJ_GET(r4151,1);
      r4148[3] = r4150;
      // env 316:
      //   string int char
      O r4143 = allocate (TC_ENV, 4);
      O r4145 = ((object****) lenv) [1][1][1][4];
      r4143[2] = r4145;
      O r4146 = ((object****) lenv) [1][1][1][5];
      r4143[3] = r4146;
      O r4147 = ((object*) lenv) [2];
      r4143[4] = r4147;
      O r4144 = top[16];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop28_1362_148; ; k = t;
      r4143[1] = r4144[2]; lenv = r4143; FUN_string_set_bang_337_0();
    } break;
  }
}
static void FUN__24loop28_1362_148 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4139 = ((object****) lenv) [1][1][1][5];
  O r4141 = r4139; // reg varref
  O r4142 = (object *) 3;
  O r4140 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4141)+UNTAG_INTEGER(r4142));
  O r4138 = r4140; // reg varref
  ((object****) lenv) [1][1][1][5] = r4138;
  O r4137 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r4137;
  FUN__24loop28_1362_0();
}
static void FUN_string_concat_338_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=387 label=386
  O r4169 = ((object*) lenv) [2];
  switch (get_case (r4169)) {
    case (TC_NIL): { // nil
      O r4171 = (O) &constructed_17;
      IRK_RETURN(4171);
    } break;
    default: {
      O r4184 = ((object*) lenv) [2];
      O r4174 = UOBJ_GET(r4184,0);
      O r4183 = ((object*) lenv) [2];
      O r4175 = UOBJ_GET(r4183,1);
      O r4178 = r4175; // reg varref
      switch (get_case (r4178)) {
        case (TC_NIL): { // nil
          O r4180 = r4174; // reg varref
          IRK_RETURN(4180);
        } break;
        default: {
          FAIL_386();
        }
      }
    } break;
  }
}
static void FAIL_386() {
  // env 317:
  //   int int string
  O r4226 = alloc_cleared (TC_ENV, 4);
  r4226[1] = lenv; lenv = r4226;
  // env 318:
  //   (string int -> int) int (list string)
  O r4233 = alloc_cleared (TC_ENV, 4);
  O r4235 = allocate (TC_CLOSURE, 2);
  r4235[1] = FUN_lambda_3907_0; r4235[2] = lenv;
  r4233[2] = r4235;
  O r4236 = (object *) 1;
  r4233[3] = r4236;
  O r4237 = ((object**) lenv) [1][2];
  r4233[4] = r4237;
  O r4234 = top[14];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_string_concat_338_149; t[4] = r4226; k = t;
  r4233[1] = r4234[2]; lenv = r4233; FUN_fold_299_0();
}
static void FUN_string_concat_338_149 (O rr) {
  O r4226 = k[4]; lenv = k[2]; k = k[1];
  O r4227 = rr;
  r4226[2] = r4227;
  O r4228 = ((object*) lenv) [2];
  r4226[3] = r4228;
  O r4232 = ((object*) lenv) [3];
  O r4231 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r4232)));
  if (freep + unbox(r4231) >= limit) {
    heap_request = unbox(r4231);
    heap1[3] = r4226;
    gc_flip (1);
    r4226 = heap0[3];
  }
  O r4230 = ((object*) lenv) [3];
  O r4229 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r4230))), ((irk_string*)(t))->len = UNTAG_INTEGER(r4230), t);
  r4226[4] = r4229;
  // env 319:
  //   ((list string) int -> string)
  O r4194 = alloc_cleared (TC_ENV, 2);
  r4194[1] = lenv; lenv = r4194;
  O r4195 = allocate (TC_CLOSURE, 2);
  r4195[1] = FUN_loop_1295_0; r4195[2] = lenv;
  r4194[2] = r4195;
  // env 320:
  //   (list string) int
  O r4190 = allocate (TC_ENV, 3);
  O r4192 = ((object***) lenv) [1][1][2];
  r4190[2] = r4192;
  O r4193 = (object *) 1;
  r4190[3] = r4193;
  O r4191 = ((object*) lenv) [2];
  r4190[1] = r4191[2]; lenv = r4190; FUN_loop_1295_0();
}
static void FUN_loop_1295_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4198 = ((object*) lenv) [2];
  switch (get_case (r4198)) {
    case (TC_NIL): { // nil
      O r4200 = ((object***) lenv) [1][1][4];
      IRK_RETURN(4200);
    } break;
    default: {
      // env 321:
      //   string (list string)
      O r4221 = alloc_cleared (TC_ENV, 3);
      r4221[1] = lenv; lenv = r4221;
      O r4225 = ((object**) lenv) [1][2];
      O r4222 = UOBJ_GET(r4225,0);
      r4221[2] = r4222;
      O r4224 = ((object**) lenv) [1][2];
      O r4223 = UOBJ_GET(r4224,1);
      r4221[3] = r4223;
      // env 322:
      //   string int int string int
      O r4213 = allocate (TC_ENV, 6);
      O r4215 = ((object*) lenv) [2];
      r4213[2] = r4215;
      O r4216 = (object *) 1;
      r4213[3] = r4216;
      O r4220 = ((object*) lenv) [2];
      O r4217 = TAG_INTEGER((irk_int)((irk_string*)(r4220))->len);
      r4213[4] = r4217;
      O r4218 = ((object****) lenv) [1][1][1][4];
      r4213[5] = r4218;
      O r4219 = ((object**) lenv) [1][3];
      r4213[6] = r4219;
      O r4214 = top[15];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_loop_1295_150; ; k = t;
      r4213[1] = r4214[2]; lenv = r4213; FUN_buffer_copy_330_0();
    } break;
  }
}
static void FUN_loop_1295_150 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4206 = ((object*) lenv) [3];
  O r4212 = ((object*) lenv) [2];
  O r4208 = TAG_INTEGER((irk_int)((irk_string*)(r4212))->len);
  O r4210 = ((object**) lenv) [1][3];
  O r4211 = r4208; // reg varref
  O r4209 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4210)+UNTAG_INTEGER(r4211));
  O r4207 = r4209; // reg varref
  lenv = ((object *)lenv)[1];
  lenv[2] = r4206;
  lenv[3] = r4207;
  FUN_loop_1295_0();
}
static void FUN_lambda_3907_0 (void) {
  O r4245 = ((object*) lenv) [2];
  O r4240 = TAG_INTEGER((irk_int)((irk_string*)(r4245))->len);
  O r4243 = r4240; // reg varref
  O r4244 = ((object*) lenv) [3];
  O r4242 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4243)+UNTAG_INTEGER(r4244));
  IRK_RETURN(4242);
}
static void FUN_string_ref_336_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4264 = (object *) 1;
  O r4265 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4264)<=UNTAG_INTEGER(r4265))) {
    O r4271 = ((object*) lenv) [2];
    O r4267 = TAG_INTEGER((irk_int)((irk_string*)(r4271))->len);
    O r4269 = ((object*) lenv) [3];
    O r4270 = r4267; // reg varref
    O r4268 = IRK_TEST(UNTAG_INTEGER(r4269)<UNTAG_INTEGER(r4270));
    O r4266 = r4268; // reg varref
    O r4260 = r4266;
    JUMP_393(r4260);
  } else {
    O r4272 = (object *) 6;
    O r4260 = r4272;
    JUMP_393(r4260);
  }
}
static void JUMP_393(O r4260) {
  O r4262 = r4260; // reg varref
  O r4263 = (object *) 6;
  O r4261 = IRK_TEST(r4262==r4263);
  O r4251 = r4261; // reg varref
  if IRK_IS_TRUE(r4251) {
    // env 323:
    //   |String/Range=(product string int int) ...t809|
    O r4253 = alloc_cleared (TC_ENV, 2);
    O r4255 = allocate (UOTAG(0), 3);
    O r4256 = ((object*) lenv) [2];
    r4255[1] = r4256;
    O r4257 = ((object*) lenv) [3];
    r4255[2] = r4257;
    O r4258 = ((object*) lenv) [3];
    r4255[3] = r4258;
    r4253[2] = r4255;
    O r4254 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_string_ref_336_151; ; k = t;
    r4253[1] = r4254[2]; lenv = r4253; ((pfun)(r4254[1]))();
  } else {
    O r4259 = (object *) 14;
    JUMP_391();
  }
}
static void FUN_string_ref_336_151 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4252 = rr;
  JUMP_391();
}
static void JUMP_391() {
  O r4249 = ((object*) lenv) [2];
  O r4250 = ((object*) lenv) [3];
  O r4248 = TO_CHAR(((unsigned char *)((irk_string*)(r4249))->data)[UNTAG_INTEGER(r4250)]);
  IRK_RETURN(4248);
}
static void FUN_char__3estring_333_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 324:
  //   int string
  O r4283 = alloc_cleared (TC_ENV, 3);
  r4283[1] = lenv; lenv = r4283;
  O r4284 = (object *) 3;
  r4283[2] = r4284;
  O r4288 = ((object*) lenv) [2];
  O r4287 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r4288)));
  if (freep + unbox(r4287) >= limit) {
    heap_request = unbox(r4287);
    heap1[3] = r4283;
    gc_flip (1);
    r4283 = heap0[3];
  }
  O r4286 = ((object*) lenv) [2];
  O r4285 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r4286))), ((irk_string*)(t))->len = UNTAG_INTEGER(r4286), t);
  r4283[3] = r4285;
  // env 325:
  //   string int char
  O r4278 = allocate (TC_ENV, 4);
  O r4280 = ((object*) lenv) [3];
  r4278[2] = r4280;
  O r4281 = (object *) 1;
  r4278[3] = r4281;
  O r4282 = ((object**) lenv) [1][2];
  r4278[4] = r4282;
  O r4279 = top[16];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_char__3estring_333_152; ; k = t;
  r4278[1] = r4279[2]; lenv = r4278; FUN_string_set_bang_337_0();
}
static void FUN_char__3estring_333_152 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4277 = ((object*) lenv) [3];
  IRK_RETURN(4277);
}
static void FUN_string_set_bang_337_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4308 = (object *) 1;
  O r4309 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4308)<=UNTAG_INTEGER(r4309))) {
    O r4315 = ((object*) lenv) [2];
    O r4311 = TAG_INTEGER((irk_int)((irk_string*)(r4315))->len);
    O r4313 = ((object*) lenv) [3];
    O r4314 = r4311; // reg varref
    O r4312 = IRK_TEST(UNTAG_INTEGER(r4313)<UNTAG_INTEGER(r4314));
    O r4310 = r4312; // reg varref
    O r4304 = r4310;
    JUMP_396(r4304);
  } else {
    O r4316 = (object *) 6;
    O r4304 = r4316;
    JUMP_396(r4304);
  }
}
static void JUMP_396(O r4304) {
  O r4306 = r4304; // reg varref
  O r4307 = (object *) 6;
  O r4305 = IRK_TEST(r4306==r4307);
  O r4295 = r4305; // reg varref
  if IRK_IS_TRUE(r4295) {
    // env 326:
    //   |String/Range=(product string int int) ...t844|
    O r4297 = alloc_cleared (TC_ENV, 2);
    O r4299 = allocate (UOTAG(0), 3);
    O r4300 = ((object*) lenv) [2];
    r4299[1] = r4300;
    O r4301 = ((object*) lenv) [3];
    r4299[2] = r4301;
    O r4302 = ((object*) lenv) [3];
    r4299[3] = r4302;
    r4297[2] = r4299;
    O r4298 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_string_set_bang_337_153; ; k = t;
    r4297[1] = r4298[2]; lenv = r4297; ((pfun)(r4298[1]))();
  } else {
    O r4303 = (object *) 14;
    JUMP_394();
  }
}
static void FUN_string_set_bang_337_153 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4296 = rr;
  JUMP_394();
}
static void JUMP_394() {
  O r4292 = ((object*) lenv) [2];
  O r4293 = ((object*) lenv) [3];
  O r4294 = ((object*) lenv) [4];
  ((irk_string*)(r4292))->data[UNTAG_INTEGER(r4293)] = GET_CHAR (r4294);
  O r4291 = (object *) 14;
  IRK_RETURN(4291);
}
static void FUN_buffer_copy_330_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 327:
  //   bool
  O r4376 = alloc_cleared (TC_ENV, 2);
  r4376[1] = lenv; lenv = r4376;
  O r4378 = (object *) 1;
  O r4379 = ((object**) lenv) [1][3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4378)<=UNTAG_INTEGER(r4379))) {
    O r4396 = ((object**) lenv) [1][3];
    O r4397 = ((object**) lenv) [1][4];
    O r4392 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4396)+UNTAG_INTEGER(r4397));
    O r4394 = ((object**) lenv) [1][3];
    O r4395 = r4392; // reg varref
    O r4393 = IRK_TEST(UNTAG_INTEGER(r4394)<=UNTAG_INTEGER(r4395));
    O r4381 = r4393; // reg varref
    if IRK_IS_TRUE(r4381) {
      O r4389 = ((object**) lenv) [1][3];
      O r4390 = ((object**) lenv) [1][4];
      O r4383 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4389)+UNTAG_INTEGER(r4390));
      O r4388 = ((object**) lenv) [1][2];
      O r4384 = TAG_INTEGER((irk_int)((irk_string*)(r4388))->len);
      O r4386 = r4383; // reg varref
      O r4387 = r4384; // reg varref
      O r4385 = IRK_TEST(UNTAG_INTEGER(r4386)<=UNTAG_INTEGER(r4387));
      O r4382 = r4385; // reg varref
      O r4380 = r4382;
      JUMP_404(r4380, r4376);
    } else {
      O r4391 = (object *) 6;
      O r4380 = r4391;
      JUMP_404(r4380, r4376);
    }
  } else {
    O r4398 = (object *) 6;
    O r4377 = r4398;
    JUMP_403(r4377, r4376);
  }
}
static void JUMP_404(O r4380, O r4376) {
  O r4377 = r4380;
  JUMP_403(r4377, r4376);
}
static void JUMP_403(O r4377, O r4376) {
  r4376[2] = r4377;
  O r4374 = ((object*) lenv) [2];
  O r4375 = (object *) 6;
  O r4373 = IRK_TEST(r4374==r4375);
  lenv = lenv[1];
  O r4362 = r4373;
  if IRK_IS_TRUE(r4362) {
    // env 328:
    //   |String/Range=(product string int int) ...t127|
    O r4364 = alloc_cleared (TC_ENV, 2);
    O r4366 = allocate (UOTAG(0), 3);
    O r4367 = ((object*) lenv) [2];
    r4366[1] = r4367;
    O r4368 = ((object*) lenv) [3];
    r4366[2] = r4368;
    O r4370 = ((object*) lenv) [3];
    O r4371 = ((object*) lenv) [4];
    O r4369 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4370)+UNTAG_INTEGER(r4371));
    r4366[3] = r4369;
    r4364[2] = r4366;
    O r4365 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_buffer_copy_330_154; ; k = t;
    r4364[1] = r4365[2]; lenv = r4364; ((pfun)(r4365[1]))();
  } else {
    O r4372 = (object *) 14;
    JUMP_401();
  }
}
static void FUN_buffer_copy_330_154 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4363 = rr;
  JUMP_401();
}
static void JUMP_401() {
  // env 329:
  //   bool
  O r4339 = alloc_cleared (TC_ENV, 2);
  r4339[1] = lenv; lenv = r4339;
  O r4341 = (object *) 1;
  O r4342 = ((object**) lenv) [1][6];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4341)<=UNTAG_INTEGER(r4342))) {
    O r4359 = ((object**) lenv) [1][6];
    O r4360 = ((object**) lenv) [1][4];
    O r4355 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4359)+UNTAG_INTEGER(r4360));
    O r4357 = ((object**) lenv) [1][6];
    O r4358 = r4355; // reg varref
    O r4356 = IRK_TEST(UNTAG_INTEGER(r4357)<=UNTAG_INTEGER(r4358));
    O r4344 = r4356; // reg varref
    if IRK_IS_TRUE(r4344) {
      O r4352 = ((object**) lenv) [1][6];
      O r4353 = ((object**) lenv) [1][4];
      O r4346 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4352)+UNTAG_INTEGER(r4353));
      O r4351 = ((object**) lenv) [1][5];
      O r4347 = TAG_INTEGER((irk_int)((irk_string*)(r4351))->len);
      O r4349 = r4346; // reg varref
      O r4350 = r4347; // reg varref
      O r4348 = IRK_TEST(UNTAG_INTEGER(r4349)<=UNTAG_INTEGER(r4350));
      O r4345 = r4348; // reg varref
      O r4343 = r4345;
      JUMP_400(r4343, r4339);
    } else {
      O r4354 = (object *) 6;
      O r4343 = r4354;
      JUMP_400(r4343, r4339);
    }
  } else {
    O r4361 = (object *) 6;
    O r4340 = r4361;
    JUMP_399(r4340, r4339);
  }
}
static void JUMP_400(O r4343, O r4339) {
  O r4340 = r4343;
  JUMP_399(r4340, r4339);
}
static void JUMP_399(O r4340, O r4339) {
  r4339[2] = r4340;
  O r4337 = ((object*) lenv) [2];
  O r4338 = (object *) 6;
  O r4336 = IRK_TEST(r4337==r4338);
  lenv = lenv[1];
  O r4325 = r4336;
  if IRK_IS_TRUE(r4325) {
    // env 330:
    //   |String/Range=(product string int int) ...t140|
    O r4327 = alloc_cleared (TC_ENV, 2);
    O r4329 = allocate (UOTAG(0), 3);
    O r4330 = ((object*) lenv) [5];
    r4329[1] = r4330;
    O r4331 = ((object*) lenv) [6];
    r4329[2] = r4331;
    O r4333 = ((object*) lenv) [6];
    O r4334 = ((object*) lenv) [4];
    O r4332 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4333)+UNTAG_INTEGER(r4334));
    r4329[3] = r4332;
    r4327[2] = r4329;
    O r4328 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_buffer_copy_330_155; ; k = t;
    r4327[1] = r4328[2]; lenv = r4327; ((pfun)(r4328[1]))();
  } else {
    O r4335 = (object *) 14;
    JUMP_397();
  }
}
static void FUN_buffer_copy_330_155 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4326 = rr;
  JUMP_397();
}
static void JUMP_397() {
  O r4320 = ((object*) lenv) [5];
  O r4321 = ((object*) lenv) [6];
  O r4322 = ((object*) lenv) [2];
  O r4323 = ((object*) lenv) [3];
  O r4324 = ((object*) lenv) [4];
  O r4319 = memcpy (((irk_string*)(r4320))->data+UNTAG_INTEGER(r4321), ((irk_string*)(r4322))->data+UNTAG_INTEGER(r4323), UNTAG_INTEGER(r4324));
  IRK_RETURN(4319);
}
static void FUN_fold_299_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4401 = ((object*) lenv) [4];
  switch (get_case (r4401)) {
    case (TC_NIL): { // nil
      O r4403 = ((object*) lenv) [3];
      IRK_RETURN(4403);
    } break;
    default: {
      // env 331:
      //   t98 (list t98)
      O r4415 = alloc_cleared (TC_ENV, 3);
      r4415[1] = lenv; lenv = r4415;
      O r4419 = ((object**) lenv) [1][4];
      O r4416 = UOBJ_GET(r4419,0);
      r4415[2] = r4416;
      O r4418 = ((object**) lenv) [1][4];
      O r4417 = UOBJ_GET(r4418,1);
      r4415[3] = r4417;
      O r4408 = ((object**) lenv) [1][2];
      // env 332:
      //   t98 t95
      O r4411 = allocate (TC_ENV, 3);
      O r4413 = ((object*) lenv) [2];
      r4411[2] = r4413;
      O r4414 = ((object**) lenv) [1][3];
      r4411[3] = r4414;
      O r4412 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_fold_299_156; t[4] = r4408; k = t;
      r4411[1] = r4412[2]; lenv = r4411; ((pfun)(r4412[1]))();
    } break;
  }
}
static void FUN_fold_299_156 (O rr) {
  O r4408 = k[4]; lenv = k[2]; k = k[1];
  O r4409 = rr;
  O r4410 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r4408;
  lenv[3] = r4409;
  lenv[4] = r4410;
  FUN_fold_299_0();
}
static void FUN_for_each_297_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4422 = ((object*) lenv) [3];
  switch (get_case (r4422)) {
    case (TC_NIL): { // nil
      O r4424 = (object *) 14;
      IRK_RETURN(4424);
    } break;
    default: {
      // env 333:
      //   t1118 (list t1118)
      O r4435 = alloc_cleared (TC_ENV, 3);
      r4435[1] = lenv; lenv = r4435;
      O r4439 = ((object**) lenv) [1][3];
      O r4436 = UOBJ_GET(r4439,0);
      r4435[2] = r4436;
      O r4438 = ((object**) lenv) [1][3];
      O r4437 = UOBJ_GET(r4438,1);
      r4435[3] = r4437;
      // env 334:
      //   t1118
      O r4432 = allocate (TC_ENV, 2);
      O r4434 = ((object*) lenv) [2];
      r4432[2] = r4434;
      O r4433 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_for_each_297_157; ; k = t;
      r4432[1] = r4433[2]; lenv = r4432; ((pfun)(r4433[1]))();
    } break;
  }
}
static void FUN_for_each_297_157 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4430 = ((object**) lenv) [1][2];
  O r4431 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r4430;
  lenv[3] = r4431;
  FUN_for_each_297_0();
}
static void FUN_map_292_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4442 = ((object*) lenv) [3];
  switch (get_case (r4442)) {
    case (TC_NIL): { // nil
      O r4444 = (object *) 10;
      IRK_RETURN(4444);
    } break;
    default: {
      // env 335:
      //   t814 (list t814)
      O r4458 = alloc_cleared (TC_ENV, 3);
      r4458[1] = lenv; lenv = r4458;
      O r4462 = ((object**) lenv) [1][3];
      O r4459 = UOBJ_GET(r4462,0);
      r4458[2] = r4459;
      O r4461 = ((object**) lenv) [1][3];
      O r4460 = UOBJ_GET(r4461,1);
      r4458[3] = r4460;
      // env 336:
      //   t814
      O r4455 = allocate (TC_ENV, 2);
      O r4457 = ((object*) lenv) [2];
      r4455[2] = r4457;
      O r4456 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_map_292_158; ; k = t;
      r4455[1] = r4456[2]; lenv = r4455; ((pfun)(r4456[1]))();
    } break;
  }
}
static void FUN_map_292_158 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r4449 = rr;
  // env 337:
  //   (t814 -> t815) (list t814)
  O r4451 = allocate (TC_ENV, 3);
  O r4453 = ((object**) lenv) [1][2];
  r4451[2] = r4453;
  O r4454 = ((object*) lenv) [3];
  r4451[3] = r4454;
  O r4452 = top[12];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_map_292_159; t[4] = r4449; k = t;
  r4451[1] = r4452[2]; lenv = r4451; FUN_map_292_0();
}
static void FUN_map_292_159 (O rr) {
  O r4449 = k[4]; lenv = k[2]; k = k[1];
  O r4450 = rr;
  O r4448 = alloc_no_clear (TC_PAIR,2);
  r4448[1] = r4449;
  r4448[2] = r4450;
  IRK_RETURN(4448);
}
static void FUN_n_of_291_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 338:
  //   (int (list t2283) -> (list t2283))
  O r4471 = alloc_cleared (TC_ENV, 2);
  r4471[1] = lenv; lenv = r4471;
  O r4472 = allocate (TC_CLOSURE, 2);
  r4472[1] = FUN_loop_1044_0; r4472[2] = lenv;
  r4471[2] = r4472;
  // env 339:
  //   int (list t2283)
  O r4467 = allocate (TC_ENV, 3);
  O r4469 = ((object**) lenv) [1][2];
  r4467[2] = r4469;
  O r4470 = (object *) 10;
  r4467[3] = r4470;
  O r4468 = ((object*) lenv) [2];
  r4467[1] = r4468[2]; lenv = r4467; FUN_loop_1044_0();
}
static void FUN_loop_1044_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4475 = ((object*) lenv) [2];
  O r4476 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4475)<=UNTAG_INTEGER(r4476))) {
    O r4478 = ((object*) lenv) [3];
    IRK_RETURN(4478);
  } else {
    O r4485 = ((object*) lenv) [2];
    O r4486 = (object *) 3;
    O r4481 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4485)-UNTAG_INTEGER(r4486));
    O r4483 = ((object***) lenv) [1][1][3];
    O r4484 = ((object*) lenv) [3];
    O r4482 = alloc_no_clear (TC_PAIR,2);
    r4482[1] = r4483;
    r4482[2] = r4484;
    lenv[2] = r4481;
    lenv[3] = r4482;
    FUN_loop_1044_0();
  }
}
static void FUN_length_splat_272_0 (void) {
  O r4489 = ((object*) lenv) [2];
  switch (get_case (r4489)) {
    case (TC_NIL): { // nil
      O r4491 = ((object*) lenv) [3];
      IRK_RETURN(4491);
    } break;
    default: {
      O r4501 = ((object*) lenv) [2];
      O r4494 = UOBJ_GET(r4501,1);
      O r4497 = r4494; // reg varref
      O r4499 = (object *) 3;
      O r4500 = ((object*) lenv) [3];
      O r4498 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4499)+UNTAG_INTEGER(r4500));
      lenv[2] = r4497;
      lenv[3] = r4498;
      FUN_length_splat_272_0();
    } break;
  }
}
static void FUN_reverse_onto_269_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4504 = ((object*) lenv) [2];
  switch (get_case (r4504)) {
    case (TC_NIL): { // nil
      O r4506 = ((object*) lenv) [3];
      IRK_RETURN(4506);
    } break;
    default: {
      O r4518 = ((object*) lenv) [2];
      O r4509 = UOBJ_GET(r4518,0);
      O r4517 = ((object*) lenv) [2];
      O r4510 = UOBJ_GET(r4517,1);
      O r4513 = r4510; // reg varref
      O r4515 = r4509; // reg varref
      O r4516 = ((object*) lenv) [3];
      O r4514 = alloc_no_clear (TC_PAIR,2);
      r4514[1] = r4515;
      r4514[2] = r4516;
      lenv[2] = r4513;
      lenv[3] = r4514;
      FUN_reverse_onto_269_0();
    } break;
  }
}
static void FUN_magic_cmp_221_0 (void) {
  O r4521 = ((object*) lenv) [2];
  O r4522 = ((object*) lenv) [3];
  O r4520 = (object*)UITAG(1+magic_cmp(r4521, r4522));
  IRK_RETURN(4520);
}
static void FUN_int_cmp_220_0 (void) {
  O r4525 = ((object*) lenv) [2];
  O r4526 = ((object*) lenv) [3];
  O r4524 = (object*)(irk_int)UITAG((UNTAG_INTEGER(r4525) < UNTAG_INTEGER(r4526)) ? 0 : ((UNTAG_INTEGER(r4526) < UNTAG_INTEGER(r4525)) ? 2 : 1));
  IRK_RETURN(4524);
}
static void FUN_euclidean_quotient_201_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 340:
  //   |tuple=(product int int) |
  O r4538 = alloc_cleared (TC_ENV, 2);
  r4538[1] = lenv; lenv = r4538;
  // env 341:
  //   int int
  O r4540 = allocate (TC_ENV, 3);
  O r4542 = ((object**) lenv) [1][2];
  r4540[2] = r4542;
  O r4543 = ((object**) lenv) [1][3];
  r4540[3] = r4543;
  O r4541 = top[5];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_euclidean_quotient_201_160; t[4] = r4538; k = t;
  r4540[1] = r4541[2]; lenv = r4540; FUN_euclidean_2f_200_0();
}
static void FUN_euclidean_quotient_201_160 (O rr) {
  O r4538 = k[4]; lenv = k[2]; k = k[1];
  O r4539 = rr;
  r4538[2] = r4539;
  O r4537 = ((object*) lenv) [2];
  O r4532 = UOBJ_GET(r4537,0);
  O r4536 = ((object*) lenv) [2];
  O r4533 = UOBJ_GET(r4536,1);
  O r4535 = r4532; // reg varref
  IRK_RETURN(4535);
}
static void FUN_euclidean_2f_200_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4546 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4546)==0)) {
    // env 342:
    //   |DivideByZero=int ...t955|
    O r4549 = alloc_cleared (TC_ENV, 2);
    O r4551 = allocate (UOTAG(4), 1);
    O r4552 = ((object*) lenv) [2];
    r4551[1] = r4552;
    r4549[2] = r4551;
    O r4550 = top[3];
    r4549[1] = r4550[2]; lenv = r4549; ((pfun)(r4550[1]))();
  } else {
    // env 343:
    //   int int cmp cmp
    O r4587 = allocate (TC_ENV, 5);
    r4587[1] = lenv; lenv = r4587;
    O r4598 = ((object**) lenv) [1][2];
    O r4599 = ((object**) lenv) [1][3];
    O r4588 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4598) / UNTAG_INTEGER(r4599));
    r4587[2] = r4588;
    O r4596 = ((object**) lenv) [1][2];
    O r4597 = ((object**) lenv) [1][3];
    O r4589 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4596) % UNTAG_INTEGER(r4597));
    r4587[3] = r4589;
    O r4594 = ((object**) lenv) [1][3];
    O r4595 = (object *) 1;
    O r4590 = (object*)(irk_int)UITAG((UNTAG_INTEGER(r4594) < UNTAG_INTEGER(r4595)) ? 0 : ((UNTAG_INTEGER(r4595) < UNTAG_INTEGER(r4594)) ? 2 : 1));
    r4587[4] = r4590;
    O r4592 = ((object*) lenv) [3];
    O r4593 = (object *) 1;
    O r4591 = (object*)(irk_int)UITAG((UNTAG_INTEGER(r4592) < UNTAG_INTEGER(r4593)) ? 0 : ((UNTAG_INTEGER(r4593) < UNTAG_INTEGER(r4592)) ? 2 : 1));
    r4587[5] = r4591;
    // fatbar jn=416 label=415
    O r4559 = ((object*) lenv) [5];
    switch (get_case (r4559)) {
      case (UITAG(0)): { // <
        // fatbar jn=419 label=418
        O r4564 = ((object*) lenv) [4];
        switch (get_case (r4564)) {
          case (UITAG(2)): { // >
            O r4566 = allocate (UOTAG(3), 2);
            O r4571 = ((object*) lenv) [2];
            O r4572 = (object *) 3;
            O r4567 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4571)-UNTAG_INTEGER(r4572));
            r4566[1] = r4567;
            O r4569 = ((object*) lenv) [3];
            O r4570 = ((object**) lenv) [1][3];
            O r4568 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4569)+UNTAG_INTEGER(r4570));
            r4566[2] = r4568;
            IRK_RETURN(4566);
          } break;
          default: {
            FAIL_418();
          }
        }
      } break;
      default: {
        FAIL_415();
      }
    }
  }
}
static void FAIL_418() {
  O r4575 = allocate (UOTAG(3), 2);
  O r4580 = ((object*) lenv) [2];
  O r4581 = (object *) 3;
  O r4576 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4580)+UNTAG_INTEGER(r4581));
  r4575[1] = r4576;
  O r4578 = ((object*) lenv) [3];
  O r4579 = ((object**) lenv) [1][3];
  O r4577 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4578)-UNTAG_INTEGER(r4579));
  r4575[2] = r4577;
  IRK_RETURN(4575);
}
static void FAIL_415() {
  O r4584 = allocate (UOTAG(3), 2);
  O r4585 = ((object*) lenv) [2];
  r4584[1] = r4585;
  O r4586 = ((object*) lenv) [3];
  r4584[2] = r4586;
  IRK_RETURN(4584);
}
static void FUN_truncate_remainder_196_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r4602 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r4602)==0)) {
    // env 344:
    //   |DivideByZero=int ...t1006|
    O r4605 = alloc_cleared (TC_ENV, 2);
    O r4607 = allocate (UOTAG(4), 1);
    O r4608 = ((object*) lenv) [2];
    r4607[1] = r4608;
    r4605[2] = r4607;
    O r4606 = top[3];
    r4605[1] = r4606[2]; lenv = r4605; ((pfun)(r4606[1]))();
  } else {
    O r4611 = ((object*) lenv) [2];
    O r4612 = ((object*) lenv) [3];
    O r4610 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r4611) % UNTAG_INTEGER(r4612));
    IRK_RETURN(4610);
  }
}
static void FUN_base_exception_handler_255_0 (void) {
  O r4634 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r4634), stdout));
  // dead  = (object *) 14;
  O r4629 = (O) &constructed_168;
  O r4631 = r4629; // reg varref
  O r4633 = r4629; // reg varref
  O r4632 = TAG_INTEGER((irk_int)((irk_string*)(r4633))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r4631))->data, 1, UNTAG_INTEGER(r4632), stdout));
  O r4630 = (object *) 14;
  O r4624 = (O) &constructed_161;
  O r4626 = r4624; // reg varref
  O r4628 = r4624; // reg varref
  O r4627 = TAG_INTEGER((irk_int)((irk_string*)(r4628))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r4626))->data, 1, UNTAG_INTEGER(r4627), stdout));
  O r4625 = (object *) 14;
  O r4619 = (O) &constructed_155;
  O r4621 = r4619; // reg varref
  O r4623 = r4619; // reg varref
  O r4622 = TAG_INTEGER((irk_int)((irk_string*)(r4623))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r4621))->data, 1, UNTAG_INTEGER(r4622), stdout));
  O r4620 = (object *) 14;
  O r4618 = ((object*) lenv) [2];
  dump_object (r4618, 0);
  O r4617 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r4617), stdout));
  // dead  = (object *) 14;
  O r4616 = (object *) -1;
  exit_continuation(r4616);
  O r4615 = (object *) TC_UNDEFINED;
  IRK_RETURN(4615);
}
object * irk_get_metadata (void) {
  return (object *) constructed_361[0];
}

uint32_t irk_ambig_size = 17;
int32_t G[] = {3, 0, 0, 0, 1, 0, 6, 0, -5, -14, -8, -16, 1, 0, -13, -17, -4, 0};
int32_t V[] = {1014, 1011, 1012, 1018, 1020, 1024, 1021, 1013, 1010, 1017, 1015, 1019, 1026, 1022, 1025, 1023, 1016, 0};
