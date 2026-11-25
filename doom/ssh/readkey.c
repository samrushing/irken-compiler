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
irk_string constructed_1 = {STRING_HEADER(1), 1, "\n" };
irk_int constructed_2[] = {UPTR(2,1),CONS_HEADER,UPTR0(1),TC_NIL};
irk_string constructed_3 = {STRING_HEADER(6), 6, "  prv " };
irk_string constructed_4 = {STRING_HEADER(6), 6, "  pub " };
irk_string constructed_5 = {STRING_HEADER(11), 11, "ssh-ed25519" };
irk_string constructed_6 = {STRING_HEADER(14), 14, "  [encrypted]\n" };
irk_string constructed_7 = {STRING_HEADER(4), 4, "none" };
irk_string constructed_8 = {STRING_HEADER(9), 9, "skeys: {\n" };
irk_string constructed_9 = {STRING_HEADER(1), 1, " " };
irk_string constructed_10 = {STRING_HEADER(2), 2, "  " };
irk_string constructed_11 = {STRING_HEADER(6), 6, "] : {\n" };
irk_int constructed_12[] = {UPTR(12,1),CONS_HEADER,UPTR0(11),TC_NIL};
irk_string constructed_13 = {STRING_HEADER(9), 9, "pubkeys [" };
irk_string constructed_14 = {STRING_HEADER(11), 11, "kdfoptions " };
irk_string constructed_15 = {STRING_HEADER(11), 11, "kdfname    " };
irk_string constructed_16 = {STRING_HEADER(11), 11, "ciphername " };
irk_string constructed_17 = {STRING_HEADER(0), 0, "" };
irk_string constructed_18 = {STRING_HEADER(15), 15, "openssh-key-v1\000" };
irk_string constructed_19 = {STRING_HEADER(19), 19, "OPENSSH PRIVATE KEY" };
irk_string constructed_20 = {STRING_HEADER(5), 5, "-----" };
irk_string constructed_21 = {STRING_HEADER(11), 11, "-----BEGIN " };
irk_string constructed_22 = {STRING_HEADER(4), 4, "null" };
irk_string constructed_23 = {STRING_HEADER(9), 9, "-----END " };
irk_string constructed_24 = {STRING_HEADER(9), 9, ";; path: " };
irk_string constructed_25 = {STRING_HEADER(65), 65, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=" };
irk_string constructed_26 = {STRING_HEADER(11), 11, "   values: " };
irk_string constructed_27 = {STRING_HEADER(10), 10, "\" raised.\n" };
irk_int constructed_28[] = {UPTR(28,1),CONS_HEADER,UPTR0(27),TC_NIL};
irk_string constructed_29 = {STRING_HEADER(11), 11, "Exception \"" };
irk_string constructed_30 = {STRING_HEADER(3), 3, "int" };
// symbol int
irk_int constructed_31[] = {UPTR(31,1), SYMBOL_HEADER, UPTR0(30), INTCON(0)};
irk_string constructed_32 = {STRING_HEADER(7), 7, "unknown" };
// symbol unknown
irk_int constructed_33[] = {UPTR(33,1), SYMBOL_HEADER, UPTR0(32), INTCON(1)};
irk_string constructed_34 = {STRING_HEADER(8), 8, "variants" };
// symbol variants
irk_int constructed_35[] = {UPTR(35,1), SYMBOL_HEADER, UPTR0(34), INTCON(2)};
irk_string constructed_36 = {STRING_HEADER(8), 8, "metadata" };
// symbol metadata
irk_int constructed_37[] = {UPTR(37,1), SYMBOL_HEADER, UPTR0(36), INTCON(3)};
// symbol none
irk_int constructed_38[] = {UPTR(38,1), SYMBOL_HEADER, UPTR0(7), INTCON(4)};
irk_string constructed_39 = {STRING_HEADER(4), 4, "save" };
// symbol save
irk_int constructed_40[] = {UPTR(40,1), SYMBOL_HEADER, UPTR0(39), INTCON(5)};
irk_string constructed_41 = {STRING_HEADER(7), 7, "closure" };
// symbol closure
irk_int constructed_42[] = {UPTR(42,1), SYMBOL_HEADER, UPTR0(41), INTCON(6)};
irk_string constructed_43 = {STRING_HEADER(3), 3, "env" };
// symbol env
irk_int constructed_44[] = {UPTR(44,1), SYMBOL_HEADER, UPTR0(43), INTCON(7)};
irk_string constructed_45 = {STRING_HEADER(6), 6, "string" };
// symbol string
irk_int constructed_46[] = {UPTR(46,1), SYMBOL_HEADER, UPTR0(45), INTCON(8)};
irk_string constructed_47 = {STRING_HEADER(6), 6, "vector" };
// symbol vector
irk_int constructed_48[] = {UPTR(48,1), SYMBOL_HEADER, UPTR0(47), INTCON(9)};
irk_string constructed_49 = {STRING_HEADER(4), 4, "pair" };
// symbol pair
irk_int constructed_50[] = {UPTR(50,1), SYMBOL_HEADER, UPTR0(49), INTCON(10)};
irk_string constructed_51 = {STRING_HEADER(6), 6, "symbol" };
// symbol symbol
irk_int constructed_52[] = {UPTR(52,1), SYMBOL_HEADER, UPTR0(51), INTCON(11)};
irk_string constructed_53 = {STRING_HEADER(6), 6, "buffer" };
// symbol buffer
irk_int constructed_54[] = {UPTR(54,1), SYMBOL_HEADER, UPTR0(53), INTCON(12)};
irk_string constructed_55 = {STRING_HEADER(7), 7, "foreign" };
// symbol foreign
irk_int constructed_56[] = {UPTR(56,1), SYMBOL_HEADER, UPTR0(55), INTCON(13)};
irk_int constructed_57[] = {UPTR(57,1),(10<<TAGSIZE)|TC_VECTOR,UPTR(38,1),UPTR(40,1),UPTR(42,1),UPTR(44,1),UPTR(46,1),UPTR(48,1),UPTR(50,1),UPTR(52,1),UPTR(54,1),
	UPTR(56,1)};
irk_string constructed_58 = {STRING_HEADER(4), 4, "char" };
// symbol char
irk_int constructed_59[] = {UPTR(59,1), SYMBOL_HEADER, UPTR0(58), INTCON(14)};
irk_string constructed_60 = {STRING_HEADER(4), 4, "bool" };
// symbol bool
irk_int constructed_61[] = {UPTR(61,1), SYMBOL_HEADER, UPTR0(60), INTCON(15)};
irk_string constructed_62 = {STRING_HEADER(3), 3, "nil" };
// symbol nil
irk_int constructed_63[] = {UPTR(63,1), SYMBOL_HEADER, UPTR0(62), INTCON(16)};
irk_string constructed_64 = {STRING_HEADER(9), 9, "undefined" };
// symbol undefined
irk_int constructed_65[] = {UPTR(65,1), SYMBOL_HEADER, UPTR0(64), INTCON(17)};
irk_string constructed_66 = {STRING_HEADER(12), 12, "empty-vector" };
// symbol empty-vector
irk_int constructed_67[] = {UPTR(67,1), SYMBOL_HEADER, UPTR0(66), INTCON(18)};
irk_int constructed_68[] = {UPTR(68,1),(5<<TAGSIZE)|TC_VECTOR,UPTR(59,1),UPTR(61,1),UPTR(63,1),UPTR(65,1),UPTR(67,1)};
irk_string constructed_69 = {STRING_HEADER(1), 1, "\"" };
irk_int constructed_70[] = {UPTR(70,1),CONS_HEADER,UPTR0(69),TC_NIL};
irk_string constructed_71 = {STRING_HEADER(2), 2, "\\\\" };
irk_string constructed_72 = {STRING_HEADER(2), 2, "\\\"" };
irk_string constructed_73 = {STRING_HEADER(2), 2, "\\r" };
irk_string constructed_74 = {STRING_HEADER(2), 2, "\\t" };
irk_string constructed_75 = {STRING_HEADER(2), 2, "\\n" };
irk_string constructed_76 = {STRING_HEADER(2), 2, "\\x" };
irk_int constructed_77[] = {UPTR(77,1),(16<<TAGSIZE)|TC_VECTOR,3145730,3211266,3276802,3342338,3407874,3473410,3538946,3604482,3670018,
	3735554,6356994,6422530,6488066,6553602,6619138,6684674};
irk_string constructed_78 = {STRING_HEADER(32), 32, "!\"#$%&'*+,-./:;<=>?@\\^_`[({|})]~" };
irk_string constructed_79 = {STRING_HEADER(10), 10, "0123456789" };
irk_string constructed_80 = {STRING_HEADER(26), 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ" };
irk_string constructed_81 = {STRING_HEADER(26), 26, "abcdefghijklmnopqrstuvwxyz" };
irk_string constructed_82 = {STRING_HEADER(29), 29, "\n***\nRuntime Error, halting: " };
irk_string constructed_83 = {STRING_HEADER(18), 18, "uncaught exception" };
irk_string constructed_84 = {STRING_HEADER(9), 9, "datatypes" };
// symbol datatypes
irk_int constructed_85[] = {UPTR(85,1), SYMBOL_HEADER, UPTR0(84), INTCON(19)};
irk_string constructed_86 = {STRING_HEADER(1), 1, "t" };
// symbol t
irk_int constructed_87[] = {UPTR(87,1), SYMBOL_HEADER, UPTR0(86), INTCON(20)};
irk_string constructed_88 = {STRING_HEADER(3), 3, "cmp" };
// symbol cmp
irk_int constructed_89[] = {UPTR(89,1), SYMBOL_HEADER, UPTR0(88), INTCON(21)};
irk_string constructed_90 = {STRING_HEADER(1), 1, "<" };
// symbol <
irk_int constructed_91[] = {UPTR(91,1), SYMBOL_HEADER, UPTR0(90), INTCON(22)};
irk_string constructed_92 = {STRING_HEADER(1), 1, "=" };
// symbol =
irk_int constructed_93[] = {UPTR(93,1), SYMBOL_HEADER, UPTR0(92), INTCON(23)};
irk_string constructed_94 = {STRING_HEADER(1), 1, ">" };
// symbol >
irk_int constructed_95[] = {UPTR(95,1), SYMBOL_HEADER, UPTR0(94), INTCON(24)};
irk_string constructed_96 = {STRING_HEADER(5), 5, "maybe" };
// symbol maybe
irk_int constructed_97[] = {UPTR(97,1), SYMBOL_HEADER, UPTR0(96), INTCON(25)};
irk_string constructed_98 = {STRING_HEADER(3), 3, "yes" };
// symbol yes
irk_int constructed_99[] = {UPTR(99,1), SYMBOL_HEADER, UPTR0(98), INTCON(26)};
irk_string constructed_100 = {STRING_HEADER(5), 5, "quote" };
// symbol quote
irk_int constructed_101[] = {UPTR(101,1), SYMBOL_HEADER, UPTR0(100), INTCON(27)};
irk_string constructed_102 = {STRING_HEADER(1), 1, "a" };
// symbol a
irk_int constructed_103[] = {UPTR(103,1), SYMBOL_HEADER, UPTR0(102), INTCON(28)};
irk_string constructed_104 = {STRING_HEADER(2), 2, "no" };
// symbol no
irk_int constructed_105[] = {UPTR(105,1), SYMBOL_HEADER, UPTR0(104), INTCON(29)};
irk_string constructed_106 = {STRING_HEADER(4), 4, "true" };
// symbol true
irk_int constructed_107[] = {UPTR(107,1), SYMBOL_HEADER, UPTR0(106), INTCON(30)};
irk_string constructed_108 = {STRING_HEADER(5), 5, "false" };
// symbol false
irk_int constructed_109[] = {UPTR(109,1), SYMBOL_HEADER, UPTR0(108), INTCON(31)};
irk_string constructed_110 = {STRING_HEADER(6), 6, "result" };
// symbol result
irk_int constructed_111[] = {UPTR(111,1), SYMBOL_HEADER, UPTR0(110), INTCON(32)};
irk_string constructed_112 = {STRING_HEADER(2), 2, "ok" };
// symbol ok
irk_int constructed_113[] = {UPTR(113,1), SYMBOL_HEADER, UPTR0(112), INTCON(33)};
irk_string constructed_114 = {STRING_HEADER(3), 3, "err" };
// symbol err
irk_int constructed_115[] = {UPTR(115,1), SYMBOL_HEADER, UPTR0(114), INTCON(34)};
irk_string constructed_116 = {STRING_HEADER(1), 1, "b" };
// symbol b
irk_int constructed_117[] = {UPTR(117,1), SYMBOL_HEADER, UPTR0(116), INTCON(35)};
irk_string constructed_118 = {STRING_HEADER(4), 4, "list" };
// symbol list
irk_int constructed_119[] = {UPTR(119,1), SYMBOL_HEADER, UPTR0(118), INTCON(36)};
irk_string constructed_120 = {STRING_HEADER(4), 4, "cons" };
// symbol cons
irk_int constructed_121[] = {UPTR(121,1), SYMBOL_HEADER, UPTR0(120), INTCON(37)};
irk_string constructed_122 = {STRING_HEADER(4), 4, "sexp" };
// symbol sexp
irk_int constructed_123[] = {UPTR(123,1), SYMBOL_HEADER, UPTR0(122), INTCON(38)};
irk_string constructed_124 = {STRING_HEADER(3), 3, "src" };
// symbol src
irk_int constructed_125[] = {UPTR(125,1), SYMBOL_HEADER, UPTR0(124), INTCON(39)};
irk_string constructed_126 = {STRING_HEADER(5), 5, "undef" };
// symbol undef
irk_int constructed_127[] = {UPTR(127,1), SYMBOL_HEADER, UPTR0(126), INTCON(40)};
irk_string constructed_128 = {STRING_HEADER(6), 6, "record" };
// symbol record
irk_int constructed_129[] = {UPTR(129,1), SYMBOL_HEADER, UPTR0(128), INTCON(41)};
irk_string constructed_130 = {STRING_HEADER(5), 5, "field" };
// symbol field
irk_int constructed_131[] = {UPTR(131,1), SYMBOL_HEADER, UPTR0(130), INTCON(42)};
irk_string constructed_132 = {STRING_HEADER(4), 4, "attr" };
// symbol attr
irk_int constructed_133[] = {UPTR(133,1), SYMBOL_HEADER, UPTR0(132), INTCON(43)};
irk_string constructed_134 = {STRING_HEADER(4), 4, "base" };
// symbol base
irk_int constructed_135[] = {UPTR(135,1), SYMBOL_HEADER, UPTR0(134), INTCON(44)};
irk_string constructed_136 = {STRING_HEADER(8), 8, "rproduct" };
// symbol rproduct
irk_int constructed_137[] = {UPTR(137,1), SYMBOL_HEADER, UPTR0(136), INTCON(45)};
irk_string constructed_138 = {STRING_HEADER(6), 6, "rlabel" };
// symbol rlabel
irk_int constructed_139[] = {UPTR(139,1), SYMBOL_HEADER, UPTR0(138), INTCON(46)};
irk_string constructed_140 = {STRING_HEADER(4), 4, "path" };
// symbol path
irk_int constructed_141[] = {UPTR(141,1), SYMBOL_HEADER, UPTR0(140), INTCON(47)};
irk_string constructed_142 = {STRING_HEADER(3), 3, "pre" };
// symbol pre
irk_int constructed_143[] = {UPTR(143,1), SYMBOL_HEADER, UPTR0(142), INTCON(48)};
irk_string constructed_144 = {STRING_HEADER(4), 4, "line" };
// symbol line
irk_int constructed_145[] = {UPTR(145,1), SYMBOL_HEADER, UPTR0(144), INTCON(49)};
irk_string constructed_146 = {STRING_HEADER(3), 3, "pos" };
// symbol pos
irk_int constructed_147[] = {UPTR(147,1), SYMBOL_HEADER, UPTR0(146), INTCON(50)};
irk_string constructed_148 = {STRING_HEADER(8), 8, "rdefault" };
// symbol rdefault
irk_int constructed_149[] = {UPTR(149,1), SYMBOL_HEADER, UPTR0(148), INTCON(51)};
irk_string constructed_150 = {STRING_HEADER(3), 3, "abs" };
// symbol abs
irk_int constructed_151[] = {UPTR(151,1), SYMBOL_HEADER, UPTR0(150), INTCON(52)};
irk_string constructed_152 = {STRING_HEADER(6), 6, "inline" };
// symbol inline
irk_int constructed_153[] = {UPTR(153,1), SYMBOL_HEADER, UPTR0(152), INTCON(53)};
irk_string constructed_154 = {STRING_HEADER(5), 5, "macro" };
// symbol macro
irk_int constructed_155[] = {UPTR(155,1), SYMBOL_HEADER, UPTR0(154), INTCON(54)};
irk_string constructed_156 = {STRING_HEADER(8), 8, "src-step" };
// symbol src-step
irk_int constructed_157[] = {UPTR(157,1), SYMBOL_HEADER, UPTR0(156), INTCON(55)};
irk_string constructed_158 = {STRING_HEADER(2), 2, "at" };
// symbol at
irk_int constructed_159[] = {UPTR(159,1), SYMBOL_HEADER, UPTR0(158), INTCON(56)};
irk_string constructed_160 = {STRING_HEADER(7), 7, "inlined" };
// symbol inlined
irk_int constructed_161[] = {UPTR(161,1), SYMBOL_HEADER, UPTR0(160), INTCON(57)};
irk_string constructed_162 = {STRING_HEADER(6), 6, "fqueue" };
// symbol fqueue
irk_int constructed_163[] = {UPTR(163,1), SYMBOL_HEADER, UPTR0(162), INTCON(58)};
irk_string constructed_164 = {STRING_HEADER(3), 3, "set" };
// symbol set
irk_int constructed_165[] = {UPTR(165,1), SYMBOL_HEADER, UPTR0(164), INTCON(59)};
irk_string constructed_166 = {STRING_HEADER(3), 3, "red" };
// symbol red
irk_int constructed_167[] = {UPTR(167,1), SYMBOL_HEADER, UPTR0(166), INTCON(60)};
irk_string constructed_168 = {STRING_HEADER(5), 5, "black" };
// symbol black
irk_int constructed_169[] = {UPTR(169,1), SYMBOL_HEADER, UPTR0(168), INTCON(61)};
irk_string constructed_170 = {STRING_HEADER(5), 5, "empty" };
// symbol empty
irk_int constructed_171[] = {UPTR(171,1), SYMBOL_HEADER, UPTR0(170), INTCON(62)};
irk_string constructed_172 = {STRING_HEADER(5), 5, "alist" };
// symbol alist
irk_int constructed_173[] = {UPTR(173,1), SYMBOL_HEADER, UPTR0(172), INTCON(63)};
irk_string constructed_174 = {STRING_HEADER(5), 5, "entry" };
// symbol entry
irk_int constructed_175[] = {UPTR(175,1), SYMBOL_HEADER, UPTR0(174), INTCON(64)};
irk_string constructed_176 = {STRING_HEADER(8), 8, "alist-ob" };
// symbol alist-ob
irk_int constructed_177[] = {UPTR(177,1), SYMBOL_HEADER, UPTR0(176), INTCON(65)};
irk_string constructed_178 = {STRING_HEADER(4), 4, "cint" };
// symbol cint
irk_int constructed_179[] = {UPTR(179,1), SYMBOL_HEADER, UPTR0(178), INTCON(66)};
irk_string constructed_180 = {STRING_HEADER(5), 5, "short" };
// symbol short
irk_int constructed_181[] = {UPTR(181,1), SYMBOL_HEADER, UPTR0(180), INTCON(67)};
irk_string constructed_182 = {STRING_HEADER(4), 4, "long" };
// symbol long
irk_int constructed_183[] = {UPTR(183,1), SYMBOL_HEADER, UPTR0(182), INTCON(68)};
irk_string constructed_184 = {STRING_HEADER(8), 8, "longlong" };
// symbol longlong
irk_int constructed_185[] = {UPTR(185,1), SYMBOL_HEADER, UPTR0(184), INTCON(69)};
irk_string constructed_186 = {STRING_HEADER(5), 5, "width" };
// symbol width
irk_int constructed_187[] = {UPTR(187,1), SYMBOL_HEADER, UPTR0(186), INTCON(70)};
irk_string constructed_188 = {STRING_HEADER(5), 5, "ctype" };
// symbol ctype
irk_int constructed_189[] = {UPTR(189,1), SYMBOL_HEADER, UPTR0(188), INTCON(71)};
irk_string constructed_190 = {STRING_HEADER(4), 4, "name" };
// symbol name
irk_int constructed_191[] = {UPTR(191,1), SYMBOL_HEADER, UPTR0(190), INTCON(72)};
irk_string constructed_192 = {STRING_HEADER(5), 5, "array" };
// symbol array
irk_int constructed_193[] = {UPTR(193,1), SYMBOL_HEADER, UPTR0(192), INTCON(73)};
irk_string constructed_194 = {STRING_HEADER(7), 7, "pointer" };
// symbol pointer
irk_int constructed_195[] = {UPTR(195,1), SYMBOL_HEADER, UPTR0(194), INTCON(74)};
irk_string constructed_196 = {STRING_HEADER(6), 6, "struct" };
// symbol struct
irk_int constructed_197[] = {UPTR(197,1), SYMBOL_HEADER, UPTR0(196), INTCON(75)};
irk_string constructed_198 = {STRING_HEADER(5), 5, "union" };
// symbol union
irk_int constructed_199[] = {UPTR(199,1), SYMBOL_HEADER, UPTR0(198), INTCON(76)};
irk_string constructed_200 = {STRING_HEADER(6), 6, "cfield" };
// symbol cfield
irk_int constructed_201[] = {UPTR(201,1), SYMBOL_HEADER, UPTR0(200), INTCON(77)};
irk_string constructed_202 = {STRING_HEADER(4), 4, "cdef" };
// symbol cdef
irk_int constructed_203[] = {UPTR(203,1), SYMBOL_HEADER, UPTR0(202), INTCON(78)};
irk_string constructed_204 = {STRING_HEADER(4), 4, "csig" };
// symbol csig
irk_int constructed_205[] = {UPTR(205,1), SYMBOL_HEADER, UPTR0(204), INTCON(79)};
irk_string constructed_206 = {STRING_HEADER(3), 3, "fun" };
// symbol fun
irk_int constructed_207[] = {UPTR(207,1), SYMBOL_HEADER, UPTR0(206), INTCON(80)};
irk_string constructed_208 = {STRING_HEADER(3), 3, "obj" };
// symbol obj
irk_int constructed_209[] = {UPTR(209,1), SYMBOL_HEADER, UPTR0(208), INTCON(81)};
irk_string constructed_210 = {STRING_HEADER(4), 4, "tree" };
// symbol tree
irk_int constructed_211[] = {UPTR(211,1), SYMBOL_HEADER, UPTR0(210), INTCON(82)};
irk_string constructed_212 = {STRING_HEADER(5), 5, "ident" };
// symbol ident
irk_int constructed_213[] = {UPTR(213,1), SYMBOL_HEADER, UPTR0(212), INTCON(83)};
irk_string constructed_214 = {STRING_HEADER(7), 7, "userimm" };
// symbol userimm
irk_int constructed_215[] = {UPTR(215,1), SYMBOL_HEADER, UPTR0(214), INTCON(84)};
irk_string constructed_216 = {STRING_HEADER(7), 7, "usertup" };
// symbol usertup
irk_int constructed_217[] = {UPTR(217,1), SYMBOL_HEADER, UPTR0(216), INTCON(85)};
irk_string constructed_218 = {STRING_HEADER(6), 6, "sysimm" };
// symbol sysimm
irk_int constructed_219[] = {UPTR(219,1), SYMBOL_HEADER, UPTR0(218), INTCON(86)};
irk_string constructed_220 = {STRING_HEADER(6), 6, "systup" };
// symbol systup
irk_int constructed_221[] = {UPTR(221,1), SYMBOL_HEADER, UPTR0(220), INTCON(87)};
irk_string constructed_222 = {STRING_HEADER(6), 6, "map-ob" };
// symbol map-ob
irk_int constructed_223[] = {UPTR(223,1), SYMBOL_HEADER, UPTR0(222), INTCON(88)};
irk_string constructed_224 = {STRING_HEADER(5), 5, "arrow" };
// symbol arrow
irk_int constructed_225[] = {UPTR(225,1), SYMBOL_HEADER, UPTR0(224), INTCON(89)};
irk_string constructed_226 = {STRING_HEADER(7), 7, "set2-ob" };
// symbol set2-ob
irk_int constructed_227[] = {UPTR(227,1), SYMBOL_HEADER, UPTR0(226), INTCON(90)};
irk_string constructed_228 = {STRING_HEADER(16), 16, "dirent-file-type" };
// symbol dirent-file-type
irk_int constructed_229[] = {UPTR(229,1), SYMBOL_HEADER, UPTR0(228), INTCON(91)};
irk_string constructed_230 = {STRING_HEADER(7), 7, "regular" };
// symbol regular
irk_int constructed_231[] = {UPTR(231,1), SYMBOL_HEADER, UPTR0(230), INTCON(92)};
irk_string constructed_232 = {STRING_HEADER(9), 9, "directory" };
// symbol directory
irk_int constructed_233[] = {UPTR(233,1), SYMBOL_HEADER, UPTR0(232), INTCON(93)};
irk_string constructed_234 = {STRING_HEADER(4), 4, "fifo" };
// symbol fifo
irk_int constructed_235[] = {UPTR(235,1), SYMBOL_HEADER, UPTR0(234), INTCON(94)};
irk_string constructed_236 = {STRING_HEADER(6), 6, "socket" };
// symbol socket
irk_int constructed_237[] = {UPTR(237,1), SYMBOL_HEADER, UPTR0(236), INTCON(95)};
irk_string constructed_238 = {STRING_HEADER(5), 5, "block" };
// symbol block
irk_int constructed_239[] = {UPTR(239,1), SYMBOL_HEADER, UPTR0(238), INTCON(96)};
irk_string constructed_240 = {STRING_HEADER(7), 7, "symlink" };
// symbol symlink
irk_int constructed_241[] = {UPTR(241,1), SYMBOL_HEADER, UPTR0(240), INTCON(97)};
irk_string constructed_242 = {STRING_HEADER(10), 10, "exceptions" };
// symbol exceptions
irk_int constructed_243[] = {UPTR(243,1), SYMBOL_HEADER, UPTR0(242), INTCON(98)};
irk_string constructed_244 = {STRING_HEADER(12), 12, "String/Range" };
// symbol String/Range
irk_int constructed_245[] = {UPTR(245,1), SYMBOL_HEADER, UPTR0(244), INTCON(99)};
irk_string constructed_246 = {STRING_HEADER(7), 7, "product" };
// symbol product
irk_int constructed_247[] = {UPTR(247,1), SYMBOL_HEADER, UPTR0(246), INTCON(100)};
irk_string constructed_248 = {STRING_HEADER(7), 7, "OSError" };
// symbol OSError
irk_int constructed_249[] = {UPTR(249,1), SYMBOL_HEADER, UPTR0(248), INTCON(101)};
irk_string constructed_250 = {STRING_HEADER(17), 17, "Metadata/KeyError" };
// symbol Metadata/KeyError
irk_int constructed_251[] = {UPTR(251,1), SYMBOL_HEADER, UPTR0(250), INTCON(102)};
irk_string constructed_252 = {STRING_HEADER(18), 18, "Metadata/Malformed" };
// symbol Metadata/Malformed
irk_int constructed_253[] = {UPTR(253,1), SYMBOL_HEADER, UPTR0(252), INTCON(103)};
irk_string constructed_254 = {STRING_HEADER(12), 12, "DivideByZero" };
// symbol DivideByZero
irk_int constructed_255[] = {UPTR(255,1), SYMBOL_HEADER, UPTR0(254), INTCON(104)};
irk_string constructed_256 = {STRING_HEADER(14), 14, "AssertionError" };
// symbol AssertionError
irk_int constructed_257[] = {UPTR(257,1), SYMBOL_HEADER, UPTR0(256), INTCON(105)};
irk_string constructed_258 = {STRING_HEADER(22), 22, "Readkey/UnknownKeyType" };
// symbol Readkey/UnknownKeyType
irk_int constructed_259[] = {UPTR(259,1), SYMBOL_HEADER, UPTR0(258), INTCON(106)};
irk_string constructed_260 = {STRING_HEADER(15), 15, "SSH/NotAKeyFile" };
// symbol SSH/NotAKeyFile
irk_int constructed_261[] = {UPTR(261,1), SYMBOL_HEADER, UPTR0(260), INTCON(107)};
irk_string constructed_262 = {STRING_HEADER(5), 5, "tuple" };
// symbol tuple
irk_int constructed_263[] = {UPTR(263,1), SYMBOL_HEADER, UPTR0(262), INTCON(108)};
irk_int constructed_264[] = {UPTR(264,5167),UOHEAD(2,1),UPTR(37,1),UITAG(0),UOHEAD(2,1),UPTR(85,1),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),UOHEAD(2,1),
	UPTR(87,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,UPTR(264,16),
	TC_NIL,UOHEAD(2,0),UPTR(264,19),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,25),TC_NIL,
	UOHEAD(2,0),UPTR(264,28),UITAG(0),CONS_HEADER,UPTR(264,31),TC_NIL,CONS_HEADER,UPTR(264,22),UPTR(264,34),UOHEAD(2,0),
	UPTR(264,37),UITAG(0),CONS_HEADER,UPTR(264,40),TC_NIL,CONS_HEADER,UPTR(264,13),UPTR(264,43),CONS_HEADER,UPTR(264,10),
	UPTR(264,46),UOHEAD(2,0),UPTR(264,49),UITAG(0),CONS_HEADER,UPTR(264,52),TC_NIL,UOHEAD(2,0),UPTR(264,55),UITAG(0),
	CONS_HEADER,UPTR(264,58),TC_NIL,CONS_HEADER,UPTR(264,7),UPTR(264,61),UOHEAD(2,0),UPTR(264,64),UITAG(0),UOHEAD(2,1),
	UPTR(89,1),UITAG(0),UOHEAD(2,1),UPTR(91,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(264,79),TC_NIL,CONS_HEADER,UPTR(264,76),UPTR(264,82),CONS_HEADER,UPTR(264,73),UPTR(264,85),
	UOHEAD(2,0),UPTR(264,88),UITAG(0),UOHEAD(2,1),UPTR(93,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,100),TC_NIL,CONS_HEADER,UPTR(264,97),UPTR(264,103),CONS_HEADER,UPTR(264,94),
	UPTR(264,106),UOHEAD(2,0),UPTR(264,109),UITAG(0),UOHEAD(2,1),UPTR(95,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,121),TC_NIL,CONS_HEADER,UPTR(264,118),UPTR(264,124),CONS_HEADER,
	UPTR(264,115),UPTR(264,127),UOHEAD(2,0),UPTR(264,130),UITAG(0),CONS_HEADER,UPTR(264,133),TC_NIL,CONS_HEADER,UPTR(264,112),
	UPTR(264,136),CONS_HEADER,UPTR(264,91),UPTR(264,139),UOHEAD(2,0),UPTR(264,142),UITAG(0),CONS_HEADER,UPTR(264,145),TC_NIL,
	CONS_HEADER,UPTR(264,70),UPTR(264,148),UOHEAD(2,0),UPTR(264,151),UITAG(0),UOHEAD(2,1),UPTR(97,1),UITAG(0),UOHEAD(2,1),
	UPTR(99,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),
	UITAG(0),CONS_HEADER,UPTR(264,169),TC_NIL,CONS_HEADER,UPTR(264,166),UPTR(264,172),UOHEAD(2,0),UPTR(264,175),UITAG(0),
	CONS_HEADER,UPTR(264,178),TC_NIL,UOHEAD(2,0),UPTR(264,181),UITAG(0),CONS_HEADER,UPTR(264,184),TC_NIL,CONS_HEADER,
	UPTR(264,163),UPTR(264,187),CONS_HEADER,UPTR(264,160),UPTR(264,190),UOHEAD(2,0),UPTR(264,193),UITAG(0),UOHEAD(2,1),UPTR(105,1),
	UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,205),TC_NIL,
	CONS_HEADER,UPTR(264,202),UPTR(264,208),CONS_HEADER,UPTR(264,199),UPTR(264,211),UOHEAD(2,0),UPTR(264,214),UITAG(0),CONS_HEADER,
	UPTR(264,217),TC_NIL,CONS_HEADER,UPTR(264,196),UPTR(264,220),UOHEAD(2,0),UPTR(264,223),UITAG(0),CONS_HEADER,UPTR(264,226),
	TC_NIL,CONS_HEADER,UPTR(264,157),UPTR(264,229),UOHEAD(2,0),UPTR(264,232),UITAG(0),UOHEAD(2,1),UPTR(61,1),UITAG(0),
	UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(264,247),TC_NIL,CONS_HEADER,UPTR(264,244),UPTR(264,250),CONS_HEADER,UPTR(264,241),UPTR(264,253),UOHEAD(2,0),UPTR(264,256),
	UITAG(0),UOHEAD(2,1),UPTR(109,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(264,268),TC_NIL,CONS_HEADER,UPTR(264,265),UPTR(264,271),CONS_HEADER,UPTR(264,262),UPTR(264,274),UOHEAD(2,0),
	UPTR(264,277),UITAG(0),CONS_HEADER,UPTR(264,280),TC_NIL,CONS_HEADER,UPTR(264,259),UPTR(264,283),UOHEAD(2,0),UPTR(264,286),
	UITAG(0),CONS_HEADER,UPTR(264,289),TC_NIL,CONS_HEADER,UPTR(264,238),UPTR(264,292),UOHEAD(2,0),UPTR(264,295),UITAG(0),
	UOHEAD(2,1),UPTR(111,1),UITAG(0),UOHEAD(2,1),UPTR(113,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,313),TC_NIL,CONS_HEADER,UPTR(264,310),
	UPTR(264,316),UOHEAD(2,0),UPTR(264,319),UITAG(0),CONS_HEADER,UPTR(264,322),TC_NIL,UOHEAD(2,0),UPTR(264,325),UITAG(0),
	CONS_HEADER,UPTR(264,328),TC_NIL,CONS_HEADER,UPTR(264,307),UPTR(264,331),CONS_HEADER,UPTR(264,304),UPTR(264,334),UOHEAD(2,0),
	UPTR(264,337),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(101,1),
	UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,352),TC_NIL,CONS_HEADER,UPTR(264,349),UPTR(264,355),
	UOHEAD(2,0),UPTR(264,358),UITAG(0),CONS_HEADER,UPTR(264,361),TC_NIL,UOHEAD(2,0),UPTR(264,364),UITAG(0),CONS_HEADER,
	UPTR(264,367),TC_NIL,CONS_HEADER,UPTR(264,346),UPTR(264,370),CONS_HEADER,UPTR(264,343),UPTR(264,373),UOHEAD(2,0),UPTR(264,376),
	UITAG(0),CONS_HEADER,UPTR(264,379),TC_NIL,CONS_HEADER,UPTR(264,340),UPTR(264,382),UOHEAD(2,0),UPTR(264,385),UITAG(0),
	CONS_HEADER,UPTR(264,388),TC_NIL,CONS_HEADER,UPTR(264,301),UPTR(264,391),UOHEAD(2,0),UPTR(264,394),UITAG(0),UOHEAD(2,1),
	UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(63,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(264,409),TC_NIL,CONS_HEADER,UPTR(264,406),UPTR(264,412),CONS_HEADER,UPTR(264,403),UPTR(264,415),
	UOHEAD(2,0),UPTR(264,418),UITAG(0),UOHEAD(2,1),UPTR(121,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,433),TC_NIL,CONS_HEADER,UPTR(264,430),
	UPTR(264,436),UOHEAD(2,0),UPTR(264,439),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,451),TC_NIL,CONS_HEADER,UPTR(264,448),UPTR(264,454),UOHEAD(2,0),
	UPTR(264,457),UITAG(0),CONS_HEADER,UPTR(264,460),TC_NIL,CONS_HEADER,UPTR(264,445),UPTR(264,463),UOHEAD(2,0),UPTR(264,466),
	UITAG(0),CONS_HEADER,UPTR(264,469),TC_NIL,CONS_HEADER,UPTR(264,442),UPTR(264,472),UOHEAD(2,0),UPTR(264,475),UITAG(0),
	CONS_HEADER,UPTR(264,478),TC_NIL,CONS_HEADER,UPTR(264,427),UPTR(264,481),CONS_HEADER,UPTR(264,424),UPTR(264,484),UOHEAD(2,0),
	UPTR(264,487),UITAG(0),CONS_HEADER,UPTR(264,490),TC_NIL,CONS_HEADER,UPTR(264,421),UPTR(264,493),UOHEAD(2,0),UPTR(264,496),
	UITAG(0),CONS_HEADER,UPTR(264,499),TC_NIL,CONS_HEADER,UPTR(264,400),UPTR(264,502),UOHEAD(2,0),UPTR(264,505),UITAG(0),
	UOHEAD(2,1),UPTR(123,1),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(123,1),UITAG(0),CONS_HEADER,UPTR(264,523),TC_NIL,UOHEAD(2,0),UPTR(264,526),
	UITAG(0),CONS_HEADER,UPTR(264,529),TC_NIL,CONS_HEADER,UPTR(264,520),UPTR(264,532),UOHEAD(2,0),UPTR(264,535),UITAG(0),
	UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,541),TC_NIL,UOHEAD(2,0),UPTR(264,544),UITAG(0),CONS_HEADER,
	UPTR(264,547),TC_NIL,CONS_HEADER,UPTR(264,538),UPTR(264,550),UOHEAD(2,0),UPTR(264,553),UITAG(0),CONS_HEADER,UPTR(264,556),
	TC_NIL,CONS_HEADER,UPTR(264,517),UPTR(264,559),CONS_HEADER,UPTR(264,514),UPTR(264,562),UOHEAD(2,0),UPTR(264,565),UITAG(0),
	UOHEAD(2,1),UPTR(52,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,
	UPTR(264,577),TC_NIL,UOHEAD(2,0),UPTR(264,580),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,586),
	TC_NIL,UOHEAD(2,0),UPTR(264,589),UITAG(0),CONS_HEADER,UPTR(264,592),TC_NIL,CONS_HEADER,UPTR(264,583),UPTR(264,595),
	UOHEAD(2,0),UPTR(264,598),UITAG(0),CONS_HEADER,UPTR(264,601),TC_NIL,CONS_HEADER,UPTR(264,574),UPTR(264,604),CONS_HEADER,
	UPTR(264,571),UPTR(264,607),UOHEAD(2,0),UPTR(264,610),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),UOHEAD(2,5),5,
	UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,UPTR(264,622),TC_NIL,UOHEAD(2,0),UPTR(264,625),UITAG(0),
	UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,631),TC_NIL,UOHEAD(2,0),UPTR(264,634),UITAG(0),CONS_HEADER,
	UPTR(264,637),TC_NIL,CONS_HEADER,UPTR(264,628),UPTR(264,640),UOHEAD(2,0),UPTR(264,643),UITAG(0),CONS_HEADER,UPTR(264,646),
	TC_NIL,CONS_HEADER,UPTR(264,619),UPTR(264,649),CONS_HEADER,UPTR(264,616),UPTR(264,652),UOHEAD(2,0),UPTR(264,655),UITAG(0),
	UOHEAD(2,1),UPTR(59,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(59,1),UITAG(0),CONS_HEADER,
	UPTR(264,667),TC_NIL,UOHEAD(2,0),UPTR(264,670),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,676),
	TC_NIL,UOHEAD(2,0),UPTR(264,679),UITAG(0),CONS_HEADER,UPTR(264,682),TC_NIL,CONS_HEADER,UPTR(264,673),UPTR(264,685),
	UOHEAD(2,0),UPTR(264,688),UITAG(0),CONS_HEADER,UPTR(264,691),TC_NIL,CONS_HEADER,UPTR(264,664),UPTR(264,694),CONS_HEADER,
	UPTR(264,661),UPTR(264,697),UOHEAD(2,0),UPTR(264,700),UITAG(0),UOHEAD(2,1),UPTR(61,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,1),UPTR(61,1),UITAG(0),CONS_HEADER,UPTR(264,712),TC_NIL,UOHEAD(2,0),UPTR(264,715),UITAG(0),
	UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,721),TC_NIL,UOHEAD(2,0),UPTR(264,724),UITAG(0),CONS_HEADER,
	UPTR(264,727),TC_NIL,CONS_HEADER,UPTR(264,718),UPTR(264,730),UOHEAD(2,0),UPTR(264,733),UITAG(0),CONS_HEADER,UPTR(264,736),
	TC_NIL,CONS_HEADER,UPTR(264,709),UPTR(264,739),CONS_HEADER,UPTR(264,706),UPTR(264,742),UOHEAD(2,0),UPTR(264,745),UITAG(0),
	UOHEAD(2,1),UPTR(31,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,
	UPTR(264,757),TC_NIL,UOHEAD(2,0),UPTR(264,760),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,766),
	TC_NIL,UOHEAD(2,0),UPTR(264,769),UITAG(0),CONS_HEADER,UPTR(264,772),TC_NIL,CONS_HEADER,UPTR(264,763),UPTR(264,775),
	UOHEAD(2,0),UPTR(264,778),UITAG(0),CONS_HEADER,UPTR(264,781),TC_NIL,CONS_HEADER,UPTR(264,754),UPTR(264,784),CONS_HEADER,
	UPTR(264,751),UPTR(264,787),UOHEAD(2,0),UPTR(264,790),UITAG(0),UOHEAD(2,1),UPTR(127,1),UITAG(0),UOHEAD(2,5),13,
	UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,802),TC_NIL,UOHEAD(2,0),UPTR(264,805),UITAG(0),
	CONS_HEADER,UPTR(264,808),TC_NIL,UOHEAD(2,0),UPTR(264,811),UITAG(0),CONS_HEADER,UPTR(264,814),TC_NIL,CONS_HEADER,
	UPTR(264,799),UPTR(264,817),CONS_HEADER,UPTR(264,796),UPTR(264,820),UOHEAD(2,0),UPTR(264,823),UITAG(0),UOHEAD(2,1),UPTR(48,1),
	UITAG(0),UOHEAD(2,5),15,UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(123,1),UITAG(0),
	CONS_HEADER,UPTR(264,838),TC_NIL,UOHEAD(2,0),UPTR(264,841),UITAG(0),CONS_HEADER,UPTR(264,844),TC_NIL,CONS_HEADER,
	UPTR(264,835),UPTR(264,847),UOHEAD(2,0),UPTR(264,850),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,856),
	TC_NIL,UOHEAD(2,0),UPTR(264,859),UITAG(0),CONS_HEADER,UPTR(264,862),TC_NIL,CONS_HEADER,UPTR(264,853),UPTR(264,865),
	UOHEAD(2,0),UPTR(264,868),UITAG(0),CONS_HEADER,UPTR(264,871),TC_NIL,CONS_HEADER,UPTR(264,832),UPTR(264,874),CONS_HEADER,
	UPTR(264,829),UPTR(264,877),UOHEAD(2,0),UPTR(264,880),UITAG(0),UOHEAD(2,1),UPTR(129,1),UITAG(0),UOHEAD(2,5),17,
	UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),CONS_HEADER,UPTR(264,895),TC_NIL,
	UOHEAD(2,0),UPTR(264,898),UITAG(0),CONS_HEADER,UPTR(264,901),TC_NIL,CONS_HEADER,UPTR(264,892),UPTR(264,904),UOHEAD(2,0),
	UPTR(264,907),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,913),TC_NIL,UOHEAD(2,0),UPTR(264,916),
	UITAG(0),CONS_HEADER,UPTR(264,919),TC_NIL,CONS_HEADER,UPTR(264,910),UPTR(264,922),UOHEAD(2,0),UPTR(264,925),UITAG(0),
	CONS_HEADER,UPTR(264,928),TC_NIL,CONS_HEADER,UPTR(264,889),UPTR(264,931),CONS_HEADER,UPTR(264,886),UPTR(264,934),UOHEAD(2,0),
	UPTR(264,937),UITAG(0),UOHEAD(2,1),UPTR(121,1),UITAG(0),UOHEAD(2,5),19,UITAG(0),UOHEAD(2,1),UPTR(52,1),
	UITAG(0),CONS_HEADER,UPTR(264,949),TC_NIL,UOHEAD(2,0),UPTR(264,952),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),
	CONS_HEADER,UPTR(264,958),TC_NIL,UOHEAD(2,0),UPTR(264,961),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,
	UPTR(264,967),TC_NIL,UOHEAD(2,0),UPTR(264,970),UITAG(0),CONS_HEADER,UPTR(264,973),TC_NIL,CONS_HEADER,UPTR(264,964),
	UPTR(264,976),CONS_HEADER,UPTR(264,955),UPTR(264,979),UOHEAD(2,0),UPTR(264,982),UITAG(0),CONS_HEADER,UPTR(264,985),TC_NIL,
	CONS_HEADER,UPTR(264,946),UPTR(264,988),CONS_HEADER,UPTR(264,943),UPTR(264,991),UOHEAD(2,0),UPTR(264,994),UITAG(0),UOHEAD(2,1),
	UPTR(133,1),UITAG(0),UOHEAD(2,5),21,UITAG(0),UOHEAD(2,1),UPTR(123,1),UITAG(0),CONS_HEADER,UPTR(264,1006),
	TC_NIL,UOHEAD(2,0),UPTR(264,1009),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,1015),TC_NIL,
	UOHEAD(2,0),UPTR(264,1018),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,1024),TC_NIL,UOHEAD(2,0),
	UPTR(264,1027),UITAG(0),CONS_HEADER,UPTR(264,1030),TC_NIL,CONS_HEADER,UPTR(264,1021),UPTR(264,1033),CONS_HEADER,UPTR(264,1012),
	UPTR(264,1036),UOHEAD(2,0),UPTR(264,1039),UITAG(0),CONS_HEADER,UPTR(264,1042),TC_NIL,CONS_HEADER,UPTR(264,1003),UPTR(264,1045),
	CONS_HEADER,UPTR(264,1000),UPTR(264,1048),UOHEAD(2,0),UPTR(264,1051),UITAG(0),CONS_HEADER,UPTR(264,1054),TC_NIL,CONS_HEADER,
	UPTR(264,997),UPTR(264,1057),CONS_HEADER,UPTR(264,940),UPTR(264,1060),CONS_HEADER,UPTR(264,883),UPTR(264,1063),CONS_HEADER,UPTR(264,826),
	UPTR(264,1066),CONS_HEADER,UPTR(264,793),UPTR(264,1069),CONS_HEADER,UPTR(264,748),UPTR(264,1072),CONS_HEADER,UPTR(264,703),UPTR(264,1075),
	CONS_HEADER,UPTR(264,658),UPTR(264,1078),CONS_HEADER,UPTR(264,613),UPTR(264,1081),CONS_HEADER,UPTR(264,568),UPTR(264,1084),UOHEAD(2,0),
	UPTR(264,1087),UITAG(0),CONS_HEADER,UPTR(264,1090),TC_NIL,CONS_HEADER,UPTR(264,511),UPTR(264,1093),UOHEAD(2,0),UPTR(264,1096),
	UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),
	UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,1111),TC_NIL,UOHEAD(2,0),UPTR(264,1114),UITAG(0),UOHEAD(2,1),
	UPTR(123,1),UITAG(0),CONS_HEADER,UPTR(264,1120),TC_NIL,UOHEAD(2,0),UPTR(264,1123),UITAG(0),UOHEAD(2,1),UPTR(125,1),
	UITAG(0),CONS_HEADER,UPTR(264,1129),TC_NIL,UOHEAD(2,0),UPTR(264,1132),UITAG(0),CONS_HEADER,UPTR(264,1135),TC_NIL,
	CONS_HEADER,UPTR(264,1126),UPTR(264,1138),CONS_HEADER,UPTR(264,1117),UPTR(264,1141),UOHEAD(2,0),UPTR(264,1144),UITAG(0),CONS_HEADER,
	UPTR(264,1147),TC_NIL,CONS_HEADER,UPTR(264,1108),UPTR(264,1150),CONS_HEADER,UPTR(264,1105),UPTR(264,1153),UOHEAD(2,0),UPTR(264,1156),
	UITAG(0),CONS_HEADER,UPTR(264,1159),TC_NIL,UOHEAD(2,0),UPTR(264,1162),UITAG(0),CONS_HEADER,UPTR(264,1165),TC_NIL,
	CONS_HEADER,UPTR(264,1102),UPTR(264,1168),UOHEAD(2,0),UPTR(264,1171),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),UOHEAD(2,1),
	UPTR(63,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,1186),
	TC_NIL,CONS_HEADER,UPTR(264,1183),UPTR(264,1189),CONS_HEADER,UPTR(264,1180),UPTR(264,1192),UOHEAD(2,0),UPTR(264,1195),UITAG(0),
	UOHEAD(2,1),UPTR(135,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),
	UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(141,1),UITAG(0),CONS_HEADER,UPTR(264,1213),TC_NIL,UOHEAD(2,0),UPTR(264,1216),
	UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,UPTR(264,1225),TC_NIL,
	UOHEAD(2,0),UPTR(264,1228),UITAG(0),CONS_HEADER,UPTR(264,1231),TC_NIL,CONS_HEADER,UPTR(264,1222),UPTR(264,1234),UOHEAD(2,0),
	UPTR(264,1237),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(145,1),UITAG(0),CONS_HEADER,UPTR(264,1246),
	TC_NIL,UOHEAD(2,0),UPTR(264,1249),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),
	CONS_HEADER,UPTR(264,1258),TC_NIL,UOHEAD(2,0),UPTR(264,1261),UITAG(0),CONS_HEADER,UPTR(264,1264),TC_NIL,CONS_HEADER,
	UPTR(264,1255),UPTR(264,1267),UOHEAD(2,0),UPTR(264,1270),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(147,1),
	UITAG(0),CONS_HEADER,UPTR(264,1279),TC_NIL,UOHEAD(2,0),UPTR(264,1282),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),
	UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,1291),TC_NIL,UOHEAD(2,0),UPTR(264,1294),UITAG(0),CONS_HEADER,
	UPTR(264,1297),TC_NIL,CONS_HEADER,UPTR(264,1288),UPTR(264,1300),UOHEAD(2,0),UPTR(264,1303),UITAG(0),UOHEAD(2,1),UPTR(149,1),
	UITAG(0),UOHEAD(2,1),UPTR(151,1),UITAG(0),CONS_HEADER,UPTR(264,1312),TC_NIL,UOHEAD(2,0),UPTR(264,1315),UITAG(0),
	CONS_HEADER,UPTR(264,1318),TC_NIL,CONS_HEADER,UPTR(264,1309),UPTR(264,1321),UOHEAD(2,0),UPTR(264,1324),UITAG(0),CONS_HEADER,
	UPTR(264,1327),TC_NIL,CONS_HEADER,UPTR(264,1306),UPTR(264,1330),CONS_HEADER,UPTR(264,1285),UPTR(264,1333),CONS_HEADER,UPTR(264,1276),
	UPTR(264,1336),UOHEAD(2,0),UPTR(264,1339),UITAG(0),CONS_HEADER,UPTR(264,1342),TC_NIL,CONS_HEADER,UPTR(264,1273),UPTR(264,1345),
	CONS_HEADER,UPTR(264,1252),UPTR(264,1348),CONS_HEADER,UPTR(264,1243),UPTR(264,1351),UOHEAD(2,0),UPTR(264,1354),UITAG(0),CONS_HEADER,
	UPTR(264,1357),TC_NIL,CONS_HEADER,UPTR(264,1240),UPTR(264,1360),CONS_HEADER,UPTR(264,1219),UPTR(264,1363),CONS_HEADER,UPTR(264,1210),
	UPTR(264,1366),UOHEAD(2,0),UPTR(264,1369),UITAG(0),CONS_HEADER,UPTR(264,1372),TC_NIL,CONS_HEADER,UPTR(264,1207),UPTR(264,1375),
	UOHEAD(2,0),UPTR(264,1378),UITAG(0),CONS_HEADER,UPTR(264,1381),TC_NIL,UOHEAD(2,0),UPTR(264,1384),UITAG(0),CONS_HEADER,
	UPTR(264,1387),TC_NIL,CONS_HEADER,UPTR(264,1204),UPTR(264,1390),CONS_HEADER,UPTR(264,1201),UPTR(264,1393),UOHEAD(2,0),UPTR(264,1396),
	UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),
	CONS_HEADER,UPTR(264,1408),TC_NIL,UOHEAD(2,0),UPTR(264,1411),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,
	UPTR(264,1417),TC_NIL,UOHEAD(2,0),UPTR(264,1420),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,1426),
	TC_NIL,UOHEAD(2,0),UPTR(264,1429),UITAG(0),CONS_HEADER,UPTR(264,1432),TC_NIL,CONS_HEADER,UPTR(264,1423),UPTR(264,1435),
	CONS_HEADER,UPTR(264,1414),UPTR(264,1438),UOHEAD(2,0),UPTR(264,1441),UITAG(0),CONS_HEADER,UPTR(264,1444),TC_NIL,CONS_HEADER,
	UPTR(264,1405),UPTR(264,1447),CONS_HEADER,UPTR(264,1402),UPTR(264,1450),UOHEAD(2,0),UPTR(264,1453),UITAG(0),UOHEAD(2,1),UPTR(155,1),
	UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,1465),TC_NIL,
	UOHEAD(2,0),UPTR(264,1468),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,1474),TC_NIL,UOHEAD(2,0),
	UPTR(264,1477),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),CONS_HEADER,UPTR(264,1483),TC_NIL,UOHEAD(2,0),UPTR(264,1486),
	UITAG(0),CONS_HEADER,UPTR(264,1489),TC_NIL,CONS_HEADER,UPTR(264,1480),UPTR(264,1492),CONS_HEADER,UPTR(264,1471),UPTR(264,1495),
	UOHEAD(2,0),UPTR(264,1498),UITAG(0),CONS_HEADER,UPTR(264,1501),TC_NIL,CONS_HEADER,UPTR(264,1462),UPTR(264,1504),CONS_HEADER,
	UPTR(264,1459),UPTR(264,1507),UOHEAD(2,0),UPTR(264,1510),UITAG(0),CONS_HEADER,UPTR(264,1513),TC_NIL,CONS_HEADER,UPTR(264,1456),
	UPTR(264,1516),CONS_HEADER,UPTR(264,1399),UPTR(264,1519),CONS_HEADER,UPTR(264,1198),UPTR(264,1522),UOHEAD(2,0),UPTR(264,1525),UITAG(0),
	CONS_HEADER,UPTR(264,1528),TC_NIL,CONS_HEADER,UPTR(264,1177),UPTR(264,1531),UOHEAD(2,0),UPTR(264,1534),UITAG(0),UOHEAD(2,1),
	UPTR(157,1),UITAG(0),UOHEAD(2,1),UPTR(159,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(141,1),UITAG(0),CONS_HEADER,UPTR(264,1555),TC_NIL,
	UOHEAD(2,0),UPTR(264,1558),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,
	UPTR(264,1567),TC_NIL,UOHEAD(2,0),UPTR(264,1570),UITAG(0),CONS_HEADER,UPTR(264,1573),TC_NIL,CONS_HEADER,UPTR(264,1564),
	UPTR(264,1576),UOHEAD(2,0),UPTR(264,1579),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(145,1),UITAG(0),
	CONS_HEADER,UPTR(264,1588),TC_NIL,UOHEAD(2,0),UPTR(264,1591),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),
	UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,1600),TC_NIL,UOHEAD(2,0),UPTR(264,1603),UITAG(0),CONS_HEADER,UPTR(264,1606),
	TC_NIL,CONS_HEADER,UPTR(264,1597),UPTR(264,1609),UOHEAD(2,0),UPTR(264,1612),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),
	UOHEAD(2,1),UPTR(147,1),UITAG(0),CONS_HEADER,UPTR(264,1621),TC_NIL,UOHEAD(2,0),UPTR(264,1624),UITAG(0),UOHEAD(2,1),
	UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,1633),TC_NIL,UOHEAD(2,0),UPTR(264,1636),
	UITAG(0),CONS_HEADER,UPTR(264,1639),TC_NIL,CONS_HEADER,UPTR(264,1630),UPTR(264,1642),UOHEAD(2,0),UPTR(264,1645),UITAG(0),
	UOHEAD(2,1),UPTR(149,1),UITAG(0),UOHEAD(2,1),UPTR(151,1),UITAG(0),CONS_HEADER,UPTR(264,1654),TC_NIL,UOHEAD(2,0),
	UPTR(264,1657),UITAG(0),CONS_HEADER,UPTR(264,1660),TC_NIL,CONS_HEADER,UPTR(264,1651),UPTR(264,1663),UOHEAD(2,0),UPTR(264,1666),
	UITAG(0),CONS_HEADER,UPTR(264,1669),TC_NIL,CONS_HEADER,UPTR(264,1648),UPTR(264,1672),CONS_HEADER,UPTR(264,1627),UPTR(264,1675),
	CONS_HEADER,UPTR(264,1618),UPTR(264,1678),UOHEAD(2,0),UPTR(264,1681),UITAG(0),CONS_HEADER,UPTR(264,1684),TC_NIL,CONS_HEADER,
	UPTR(264,1615),UPTR(264,1687),CONS_HEADER,UPTR(264,1594),UPTR(264,1690),CONS_HEADER,UPTR(264,1585),UPTR(264,1693),UOHEAD(2,0),UPTR(264,1696),
	UITAG(0),CONS_HEADER,UPTR(264,1699),TC_NIL,CONS_HEADER,UPTR(264,1582),UPTR(264,1702),CONS_HEADER,UPTR(264,1561),UPTR(264,1705),
	CONS_HEADER,UPTR(264,1552),UPTR(264,1708),UOHEAD(2,0),UPTR(264,1711),UITAG(0),CONS_HEADER,UPTR(264,1714),TC_NIL,CONS_HEADER,
	UPTR(264,1549),UPTR(264,1717),UOHEAD(2,0),UPTR(264,1720),UITAG(0),CONS_HEADER,UPTR(264,1723),TC_NIL,UOHEAD(2,0),UPTR(264,1726),
	UITAG(0),CONS_HEADER,UPTR(264,1729),TC_NIL,CONS_HEADER,UPTR(264,1546),UPTR(264,1732),CONS_HEADER,UPTR(264,1543),UPTR(264,1735),
	UOHEAD(2,0),UPTR(264,1738),UITAG(0),UOHEAD(2,1),UPTR(155,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,1750),TC_NIL,UOHEAD(2,0),UPTR(264,1753),UITAG(0),CONS_HEADER,UPTR(264,1756),
	TC_NIL,UOHEAD(2,0),UPTR(264,1759),UITAG(0),CONS_HEADER,UPTR(264,1762),TC_NIL,CONS_HEADER,UPTR(264,1747),UPTR(264,1765),
	CONS_HEADER,UPTR(264,1744),UPTR(264,1768),UOHEAD(2,0),UPTR(264,1771),UITAG(0),UOHEAD(2,1),UPTR(161,1),UITAG(0),UOHEAD(2,5),
	5,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,1783),TC_NIL,UOHEAD(2,0),UPTR(264,1786),
	UITAG(0),CONS_HEADER,UPTR(264,1789),TC_NIL,UOHEAD(2,0),UPTR(264,1792),UITAG(0),CONS_HEADER,UPTR(264,1795),TC_NIL,
	CONS_HEADER,UPTR(264,1780),UPTR(264,1798),CONS_HEADER,UPTR(264,1777),UPTR(264,1801),UOHEAD(2,0),UPTR(264,1804),UITAG(0),CONS_HEADER,
	UPTR(264,1807),TC_NIL,CONS_HEADER,UPTR(264,1774),UPTR(264,1810),CONS_HEADER,UPTR(264,1741),UPTR(264,1813),UOHEAD(2,0),UPTR(264,1816),
	UITAG(0),CONS_HEADER,UPTR(264,1819),TC_NIL,CONS_HEADER,UPTR(264,1540),UPTR(264,1822),UOHEAD(2,0),UPTR(264,1825),UITAG(0),
	UOHEAD(2,1),UPTR(163,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,1846),
	TC_NIL,CONS_HEADER,UPTR(264,1843),UPTR(264,1849),UOHEAD(2,0),UPTR(264,1852),UITAG(0),CONS_HEADER,UPTR(264,1855),TC_NIL,
	CONS_HEADER,UPTR(264,1840),UPTR(264,1858),UOHEAD(2,0),UPTR(264,1861),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,1873),TC_NIL,CONS_HEADER,UPTR(264,1870),
	UPTR(264,1876),UOHEAD(2,0),UPTR(264,1879),UITAG(0),CONS_HEADER,UPTR(264,1882),TC_NIL,CONS_HEADER,UPTR(264,1867),UPTR(264,1885),
	UOHEAD(2,0),UPTR(264,1888),UITAG(0),CONS_HEADER,UPTR(264,1891),TC_NIL,CONS_HEADER,UPTR(264,1864),UPTR(264,1894),UOHEAD(2,0),
	UPTR(264,1897),UITAG(0),CONS_HEADER,UPTR(264,1900),TC_NIL,CONS_HEADER,UPTR(264,1837),UPTR(264,1903),CONS_HEADER,UPTR(264,1834),
	UPTR(264,1906),UOHEAD(2,0),UPTR(264,1909),UITAG(0),CONS_HEADER,UPTR(264,1912),TC_NIL,UOHEAD(2,0),UPTR(264,1915),UITAG(0),
	CONS_HEADER,UPTR(264,1918),TC_NIL,CONS_HEADER,UPTR(264,1831),UPTR(264,1921),UOHEAD(2,0),UPTR(264,1924),UITAG(0),UOHEAD(2,1),
	UPTR(165,1),UITAG(0),UOHEAD(2,1),UPTR(167,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(165,1),
	UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,1945),TC_NIL,
	CONS_HEADER,UPTR(264,1942),UPTR(264,1948),UOHEAD(2,0),UPTR(264,1951),UITAG(0),CONS_HEADER,UPTR(264,1954),TC_NIL,CONS_HEADER,
	UPTR(264,1939),UPTR(264,1957),UOHEAD(2,0),UPTR(264,1960),UITAG(0),UOHEAD(2,1),UPTR(165,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),
	UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,1972),TC_NIL,CONS_HEADER,UPTR(264,1969),UPTR(264,1975),
	UOHEAD(2,0),UPTR(264,1978),UITAG(0),CONS_HEADER,UPTR(264,1981),TC_NIL,CONS_HEADER,UPTR(264,1966),UPTR(264,1984),UOHEAD(2,0),
	UPTR(264,1987),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,1996),
	TC_NIL,CONS_HEADER,UPTR(264,1993),UPTR(264,1999),UOHEAD(2,0),UPTR(264,2002),UITAG(0),CONS_HEADER,UPTR(264,2005),TC_NIL,
	CONS_HEADER,UPTR(264,1990),UPTR(264,2008),CONS_HEADER,UPTR(264,1963),UPTR(264,2011),UOHEAD(2,0),UPTR(264,2014),UITAG(0),CONS_HEADER,
	UPTR(264,2017),TC_NIL,CONS_HEADER,UPTR(264,1936),UPTR(264,2020),CONS_HEADER,UPTR(264,1933),UPTR(264,2023),UOHEAD(2,0),UPTR(264,2026),
	UITAG(0),UOHEAD(2,1),UPTR(169,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(165,1),UITAG(0),
	UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,2044),TC_NIL,CONS_HEADER,
	UPTR(264,2041),UPTR(264,2047),UOHEAD(2,0),UPTR(264,2050),UITAG(0),CONS_HEADER,UPTR(264,2053),TC_NIL,CONS_HEADER,UPTR(264,2038),
	UPTR(264,2056),UOHEAD(2,0),UPTR(264,2059),UITAG(0),UOHEAD(2,1),UPTR(165,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,2071),TC_NIL,CONS_HEADER,UPTR(264,2068),UPTR(264,2074),UOHEAD(2,0),
	UPTR(264,2077),UITAG(0),CONS_HEADER,UPTR(264,2080),TC_NIL,CONS_HEADER,UPTR(264,2065),UPTR(264,2083),UOHEAD(2,0),UPTR(264,2086),
	UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,2095),TC_NIL,
	CONS_HEADER,UPTR(264,2092),UPTR(264,2098),UOHEAD(2,0),UPTR(264,2101),UITAG(0),CONS_HEADER,UPTR(264,2104),TC_NIL,CONS_HEADER,
	UPTR(264,2089),UPTR(264,2107),CONS_HEADER,UPTR(264,2062),UPTR(264,2110),UOHEAD(2,0),UPTR(264,2113),UITAG(0),CONS_HEADER,UPTR(264,2116),
	TC_NIL,CONS_HEADER,UPTR(264,2035),UPTR(264,2119),CONS_HEADER,UPTR(264,2032),UPTR(264,2122),UOHEAD(2,0),UPTR(264,2125),UITAG(0),
	UOHEAD(2,1),UPTR(171,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(264,2137),TC_NIL,CONS_HEADER,UPTR(264,2134),UPTR(264,2140),CONS_HEADER,UPTR(264,2131),UPTR(264,2143),UOHEAD(2,0),UPTR(264,2146),
	UITAG(0),CONS_HEADER,UPTR(264,2149),TC_NIL,CONS_HEADER,UPTR(264,2128),UPTR(264,2152),CONS_HEADER,UPTR(264,2029),UPTR(264,2155),
	UOHEAD(2,0),UPTR(264,2158),UITAG(0),CONS_HEADER,UPTR(264,2161),TC_NIL,CONS_HEADER,UPTR(264,1930),UPTR(264,2164),UOHEAD(2,0),
	UPTR(264,2167),UITAG(0),UOHEAD(2,1),UPTR(173,1),UITAG(0),UOHEAD(2,1),UPTR(63,1),UITAG(0),UOHEAD(2,5),1,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,2182),TC_NIL,CONS_HEADER,UPTR(264,2179),UPTR(264,2185),
	CONS_HEADER,UPTR(264,2176),UPTR(264,2188),UOHEAD(2,0),UPTR(264,2191),UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,5),
	3,UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,2206),
	TC_NIL,CONS_HEADER,UPTR(264,2203),UPTR(264,2209),UOHEAD(2,0),UPTR(264,2212),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,2221),TC_NIL,CONS_HEADER,UPTR(264,2218),UPTR(264,2224),UOHEAD(2,0),
	UPTR(264,2227),UITAG(0),UOHEAD(2,1),UPTR(173,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),
	UITAG(0),CONS_HEADER,UPTR(264,2239),TC_NIL,CONS_HEADER,UPTR(264,2236),UPTR(264,2242),UOHEAD(2,0),UPTR(264,2245),UITAG(0),
	UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,2254),TC_NIL,CONS_HEADER,
	UPTR(264,2251),UPTR(264,2257),UOHEAD(2,0),UPTR(264,2260),UITAG(0),CONS_HEADER,UPTR(264,2263),TC_NIL,CONS_HEADER,UPTR(264,2248),
	UPTR(264,2266),CONS_HEADER,UPTR(264,2233),UPTR(264,2269),UOHEAD(2,0),UPTR(264,2272),UITAG(0),CONS_HEADER,UPTR(264,2275),TC_NIL,
	CONS_HEADER,UPTR(264,2230),UPTR(264,2278),CONS_HEADER,UPTR(264,2215),UPTR(264,2281),UOHEAD(2,0),UPTR(264,2284),UITAG(0),CONS_HEADER,
	UPTR(264,2287),TC_NIL,CONS_HEADER,UPTR(264,2200),UPTR(264,2290),CONS_HEADER,UPTR(264,2197),UPTR(264,2293),UOHEAD(2,0),UPTR(264,2296),
	UITAG(0),CONS_HEADER,UPTR(264,2299),TC_NIL,CONS_HEADER,UPTR(264,2194),UPTR(264,2302),UOHEAD(2,0),UPTR(264,2305),UITAG(0),
	CONS_HEADER,UPTR(264,2308),TC_NIL,CONS_HEADER,UPTR(264,2173),UPTR(264,2311),UOHEAD(2,0),UPTR(264,2314),UITAG(0),UOHEAD(2,1),
	UPTR(177,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(173,1),UITAG(0),CONS_HEADER,UPTR(264,2335),TC_NIL,
	UOHEAD(2,0),UPTR(264,2338),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(173,1),UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,2353),TC_NIL,CONS_HEADER,UPTR(264,2350),
	UPTR(264,2356),UOHEAD(2,0),UPTR(264,2359),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),
	CONS_HEADER,UPTR(264,2368),TC_NIL,CONS_HEADER,UPTR(264,2365),UPTR(264,2371),UOHEAD(2,0),UPTR(264,2374),UITAG(0),CONS_HEADER,
	UPTR(264,2377),TC_NIL,CONS_HEADER,UPTR(264,2362),UPTR(264,2380),CONS_HEADER,UPTR(264,2347),UPTR(264,2383),UOHEAD(2,0),UPTR(264,2386),
	UITAG(0),CONS_HEADER,UPTR(264,2389),TC_NIL,CONS_HEADER,UPTR(264,2344),UPTR(264,2392),UOHEAD(2,0),UPTR(264,2395),UITAG(0),
	UOHEAD(2,1),UPTR(149,1),UITAG(0),UOHEAD(2,1),UPTR(151,1),UITAG(0),CONS_HEADER,UPTR(264,2404),TC_NIL,UOHEAD(2,0),
	UPTR(264,2407),UITAG(0),CONS_HEADER,UPTR(264,2410),TC_NIL,CONS_HEADER,UPTR(264,2401),UPTR(264,2413),UOHEAD(2,0),UPTR(264,2416),
	UITAG(0),CONS_HEADER,UPTR(264,2419),TC_NIL,CONS_HEADER,UPTR(264,2398),UPTR(264,2422),CONS_HEADER,UPTR(264,2341),UPTR(264,2425),
	CONS_HEADER,UPTR(264,2332),UPTR(264,2428),UOHEAD(2,0),UPTR(264,2431),UITAG(0),CONS_HEADER,UPTR(264,2434),TC_NIL,CONS_HEADER,
	UPTR(264,2329),UPTR(264,2437),UOHEAD(2,0),UPTR(264,2440),UITAG(0),CONS_HEADER,UPTR(264,2443),TC_NIL,UOHEAD(2,0),UPTR(264,2446),
	UITAG(0),CONS_HEADER,UPTR(264,2449),TC_NIL,CONS_HEADER,UPTR(264,2326),UPTR(264,2452),CONS_HEADER,UPTR(264,2323),UPTR(264,2455),
	UOHEAD(2,0),UPTR(264,2458),UITAG(0),CONS_HEADER,UPTR(264,2461),TC_NIL,UOHEAD(2,0),UPTR(264,2464),UITAG(0),CONS_HEADER,
	UPTR(264,2467),TC_NIL,CONS_HEADER,UPTR(264,2320),UPTR(264,2470),UOHEAD(2,0),UPTR(264,2473),UITAG(0),UOHEAD(2,1),UPTR(179,1),
	UITAG(0),UOHEAD(2,1),UPTR(59,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(264,2488),TC_NIL,CONS_HEADER,UPTR(264,2485),UPTR(264,2491),CONS_HEADER,UPTR(264,2482),UPTR(264,2494),UOHEAD(2,0),
	UPTR(264,2497),UITAG(0),UOHEAD(2,1),UPTR(181,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(264,2509),TC_NIL,CONS_HEADER,UPTR(264,2506),UPTR(264,2512),CONS_HEADER,UPTR(264,2503),UPTR(264,2515),
	UOHEAD(2,0),UPTR(264,2518),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,2530),TC_NIL,CONS_HEADER,UPTR(264,2527),UPTR(264,2533),CONS_HEADER,UPTR(264,2524),
	UPTR(264,2536),UOHEAD(2,0),UPTR(264,2539),UITAG(0),UOHEAD(2,1),UPTR(183,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,2551),TC_NIL,CONS_HEADER,UPTR(264,2548),UPTR(264,2554),CONS_HEADER,
	UPTR(264,2545),UPTR(264,2557),UOHEAD(2,0),UPTR(264,2560),UITAG(0),UOHEAD(2,1),UPTR(185,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,2572),TC_NIL,CONS_HEADER,UPTR(264,2569),UPTR(264,2575),
	CONS_HEADER,UPTR(264,2566),UPTR(264,2578),UOHEAD(2,0),UPTR(264,2581),UITAG(0),UOHEAD(2,1),UPTR(187,1),UITAG(0),UOHEAD(2,5),
	11,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,2593),TC_NIL,UOHEAD(2,0),UPTR(264,2596),
	UITAG(0),CONS_HEADER,UPTR(264,2599),TC_NIL,UOHEAD(2,0),UPTR(264,2602),UITAG(0),CONS_HEADER,UPTR(264,2605),TC_NIL,
	CONS_HEADER,UPTR(264,2590),UPTR(264,2608),CONS_HEADER,UPTR(264,2587),UPTR(264,2611),UOHEAD(2,0),UPTR(264,2614),UITAG(0),CONS_HEADER,
	UPTR(264,2617),TC_NIL,CONS_HEADER,UPTR(264,2584),UPTR(264,2620),CONS_HEADER,UPTR(264,2563),UPTR(264,2623),CONS_HEADER,UPTR(264,2542),
	UPTR(264,2626),CONS_HEADER,UPTR(264,2521),UPTR(264,2629),CONS_HEADER,UPTR(264,2500),UPTR(264,2632),UOHEAD(2,0),UPTR(264,2635),UITAG(0),
	CONS_HEADER,UPTR(264,2638),TC_NIL,CONS_HEADER,UPTR(264,2479),UPTR(264,2641),UOHEAD(2,0),UPTR(264,2644),UITAG(0),UOHEAD(2,1),
	UPTR(189,1),UITAG(0),UOHEAD(2,1),UPTR(191,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(52,1),
	UITAG(0),CONS_HEADER,UPTR(264,2659),TC_NIL,UOHEAD(2,0),UPTR(264,2662),UITAG(0),CONS_HEADER,UPTR(264,2665),TC_NIL,
	UOHEAD(2,0),UPTR(264,2668),UITAG(0),CONS_HEADER,UPTR(264,2671),TC_NIL,CONS_HEADER,UPTR(264,2656),UPTR(264,2674),CONS_HEADER,
	UPTR(264,2653),UPTR(264,2677),UOHEAD(2,0),UPTR(264,2680),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),UOHEAD(2,5),3,
	UITAG(0),UOHEAD(2,1),UPTR(179,1),UITAG(0),CONS_HEADER,UPTR(264,2692),TC_NIL,UOHEAD(2,0),UPTR(264,2695),UITAG(0),
	UOHEAD(2,1),UPTR(61,1),UITAG(0),CONS_HEADER,UPTR(264,2701),TC_NIL,UOHEAD(2,0),UPTR(264,2704),UITAG(0),CONS_HEADER,
	UPTR(264,2707),TC_NIL,CONS_HEADER,UPTR(264,2698),UPTR(264,2710),UOHEAD(2,0),UPTR(264,2713),UITAG(0),CONS_HEADER,UPTR(264,2716),
	TC_NIL,CONS_HEADER,UPTR(264,2689),UPTR(264,2719),CONS_HEADER,UPTR(264,2686),UPTR(264,2722),UOHEAD(2,0),UPTR(264,2725),UITAG(0),
	UOHEAD(2,1),UPTR(193,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,
	UPTR(264,2737),TC_NIL,UOHEAD(2,0),UPTR(264,2740),UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),CONS_HEADER,UPTR(264,2746),
	TC_NIL,UOHEAD(2,0),UPTR(264,2749),UITAG(0),CONS_HEADER,UPTR(264,2752),TC_NIL,CONS_HEADER,UPTR(264,2743),UPTR(264,2755),
	UOHEAD(2,0),UPTR(264,2758),UITAG(0),CONS_HEADER,UPTR(264,2761),TC_NIL,CONS_HEADER,UPTR(264,2734),UPTR(264,2764),CONS_HEADER,
	UPTR(264,2731),UPTR(264,2767),UOHEAD(2,0),UPTR(264,2770),UITAG(0),UOHEAD(2,1),UPTR(195,1),UITAG(0),UOHEAD(2,5),7,
	UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),CONS_HEADER,UPTR(264,2782),TC_NIL,UOHEAD(2,0),UPTR(264,2785),UITAG(0),
	CONS_HEADER,UPTR(264,2788),TC_NIL,UOHEAD(2,0),UPTR(264,2791),UITAG(0),CONS_HEADER,UPTR(264,2794),TC_NIL,CONS_HEADER,
	UPTR(264,2779),UPTR(264,2797),CONS_HEADER,UPTR(264,2776),UPTR(264,2800),UOHEAD(2,0),UPTR(264,2803),UITAG(0),UOHEAD(2,1),UPTR(197,1),
	UITAG(0),UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,2815),TC_NIL,
	UOHEAD(2,0),UPTR(264,2818),UITAG(0),CONS_HEADER,UPTR(264,2821),TC_NIL,UOHEAD(2,0),UPTR(264,2824),UITAG(0),CONS_HEADER,
	UPTR(264,2827),TC_NIL,CONS_HEADER,UPTR(264,2812),UPTR(264,2830),CONS_HEADER,UPTR(264,2809),UPTR(264,2833),UOHEAD(2,0),UPTR(264,2836),
	UITAG(0),UOHEAD(2,1),UPTR(199,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),
	CONS_HEADER,UPTR(264,2848),TC_NIL,UOHEAD(2,0),UPTR(264,2851),UITAG(0),CONS_HEADER,UPTR(264,2854),TC_NIL,UOHEAD(2,0),
	UPTR(264,2857),UITAG(0),CONS_HEADER,UPTR(264,2860),TC_NIL,CONS_HEADER,UPTR(264,2845),UPTR(264,2863),CONS_HEADER,UPTR(264,2842),
	UPTR(264,2866),UOHEAD(2,0),UPTR(264,2869),UITAG(0),CONS_HEADER,UPTR(264,2872),TC_NIL,CONS_HEADER,UPTR(264,2839),UPTR(264,2875),
	CONS_HEADER,UPTR(264,2806),UPTR(264,2878),CONS_HEADER,UPTR(264,2773),UPTR(264,2881),CONS_HEADER,UPTR(264,2728),UPTR(264,2884),CONS_HEADER,
	UPTR(264,2683),UPTR(264,2887),UOHEAD(2,0),UPTR(264,2890),UITAG(0),CONS_HEADER,UPTR(264,2893),TC_NIL,CONS_HEADER,UPTR(264,2650),
	UPTR(264,2896),UOHEAD(2,0),UPTR(264,2899),UITAG(0),UOHEAD(2,1),UPTR(201,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,2914),TC_NIL,UOHEAD(2,0),
	UPTR(264,2917),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,2923),TC_NIL,UOHEAD(2,0),UPTR(264,2926),
	UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),CONS_HEADER,UPTR(264,2932),TC_NIL,UOHEAD(2,0),UPTR(264,2935),UITAG(0),
	CONS_HEADER,UPTR(264,2938),TC_NIL,CONS_HEADER,UPTR(264,2929),UPTR(264,2941),CONS_HEADER,UPTR(264,2920),UPTR(264,2944),UOHEAD(2,0),
	UPTR(264,2947),UITAG(0),CONS_HEADER,UPTR(264,2950),TC_NIL,CONS_HEADER,UPTR(264,2911),UPTR(264,2953),CONS_HEADER,UPTR(264,2908),
	UPTR(264,2956),UOHEAD(2,0),UPTR(264,2959),UITAG(0),CONS_HEADER,UPTR(264,2962),TC_NIL,UOHEAD(2,0),UPTR(264,2965),UITAG(0),
	CONS_HEADER,UPTR(264,2968),TC_NIL,CONS_HEADER,UPTR(264,2905),UPTR(264,2971),UOHEAD(2,0),UPTR(264,2974),UITAG(0),UOHEAD(2,1),
	UPTR(203,1),UITAG(0),UOHEAD(2,1),UPTR(197,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(31,1),
	UITAG(0),CONS_HEADER,UPTR(264,2989),TC_NIL,UOHEAD(2,0),UPTR(264,2992),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),
	CONS_HEADER,UPTR(264,2998),TC_NIL,UOHEAD(2,0),UPTR(264,3001),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),
	UPTR(201,1),UITAG(0),CONS_HEADER,UPTR(264,3010),TC_NIL,UOHEAD(2,0),UPTR(264,3013),UITAG(0),CONS_HEADER,UPTR(264,3016),
	TC_NIL,CONS_HEADER,UPTR(264,3007),UPTR(264,3019),UOHEAD(2,0),UPTR(264,3022),UITAG(0),CONS_HEADER,UPTR(264,3025),TC_NIL,
	CONS_HEADER,UPTR(264,3004),UPTR(264,3028),CONS_HEADER,UPTR(264,2995),UPTR(264,3031),UOHEAD(2,0),UPTR(264,3034),UITAG(0),CONS_HEADER,
	UPTR(264,3037),TC_NIL,CONS_HEADER,UPTR(264,2986),UPTR(264,3040),CONS_HEADER,UPTR(264,2983),UPTR(264,3043),UOHEAD(2,0),UPTR(264,3046),
	UITAG(0),UOHEAD(2,1),UPTR(199,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),
	CONS_HEADER,UPTR(264,3058),TC_NIL,UOHEAD(2,0),UPTR(264,3061),UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,
	UPTR(264,3067),TC_NIL,UOHEAD(2,0),UPTR(264,3070),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(201,1),
	UITAG(0),CONS_HEADER,UPTR(264,3079),TC_NIL,UOHEAD(2,0),UPTR(264,3082),UITAG(0),CONS_HEADER,UPTR(264,3085),TC_NIL,
	CONS_HEADER,UPTR(264,3076),UPTR(264,3088),UOHEAD(2,0),UPTR(264,3091),UITAG(0),CONS_HEADER,UPTR(264,3094),TC_NIL,CONS_HEADER,
	UPTR(264,3073),UPTR(264,3097),CONS_HEADER,UPTR(264,3064),UPTR(264,3100),UOHEAD(2,0),UPTR(264,3103),UITAG(0),CONS_HEADER,UPTR(264,3106),
	TC_NIL,CONS_HEADER,UPTR(264,3055),UPTR(264,3109),CONS_HEADER,UPTR(264,3052),UPTR(264,3112),UOHEAD(2,0),UPTR(264,3115),UITAG(0),
	CONS_HEADER,UPTR(264,3118),TC_NIL,CONS_HEADER,UPTR(264,3049),UPTR(264,3121),UOHEAD(2,0),UPTR(264,3124),UITAG(0),CONS_HEADER,
	UPTR(264,3127),TC_NIL,CONS_HEADER,UPTR(264,2980),UPTR(264,3130),UOHEAD(2,0),UPTR(264,3133),UITAG(0),UOHEAD(2,1),UPTR(205,1),
	UITAG(0),UOHEAD(2,1),UPTR(207,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),
	CONS_HEADER,UPTR(264,3148),TC_NIL,UOHEAD(2,0),UPTR(264,3151),UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),CONS_HEADER,
	UPTR(264,3157),TC_NIL,UOHEAD(2,0),UPTR(264,3160),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,1),UPTR(189,1),
	UITAG(0),CONS_HEADER,UPTR(264,3169),TC_NIL,UOHEAD(2,0),UPTR(264,3172),UITAG(0),CONS_HEADER,UPTR(264,3175),TC_NIL,
	CONS_HEADER,UPTR(264,3166),UPTR(264,3178),UOHEAD(2,0),UPTR(264,3181),UITAG(0),UOHEAD(2,1),UPTR(61,1),UITAG(0),CONS_HEADER,
	UPTR(264,3187),TC_NIL,UOHEAD(2,0),UPTR(264,3190),UITAG(0),CONS_HEADER,UPTR(264,3193),TC_NIL,CONS_HEADER,UPTR(264,3184),
	UPTR(264,3196),CONS_HEADER,UPTR(264,3163),UPTR(264,3199),CONS_HEADER,UPTR(264,3154),UPTR(264,3202),UOHEAD(2,0),UPTR(264,3205),UITAG(0),
	CONS_HEADER,UPTR(264,3208),TC_NIL,CONS_HEADER,UPTR(264,3145),UPTR(264,3211),CONS_HEADER,UPTR(264,3142),UPTR(264,3214),UOHEAD(2,0),
	UPTR(264,3217),UITAG(0),UOHEAD(2,1),UPTR(209,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(52,1),
	UITAG(0),CONS_HEADER,UPTR(264,3229),TC_NIL,UOHEAD(2,0),UPTR(264,3232),UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),
	CONS_HEADER,UPTR(264,3238),TC_NIL,UOHEAD(2,0),UPTR(264,3241),UITAG(0),CONS_HEADER,UPTR(264,3244),TC_NIL,CONS_HEADER,
	UPTR(264,3235),UPTR(264,3247),UOHEAD(2,0),UPTR(264,3250),UITAG(0),CONS_HEADER,UPTR(264,3253),TC_NIL,CONS_HEADER,UPTR(264,3226),
	UPTR(264,3256),CONS_HEADER,UPTR(264,3223),UPTR(264,3259),UOHEAD(2,0),UPTR(264,3262),UITAG(0),CONS_HEADER,UPTR(264,3265),TC_NIL,
	CONS_HEADER,UPTR(264,3220),UPTR(264,3268),UOHEAD(2,0),UPTR(264,3271),UITAG(0),CONS_HEADER,UPTR(264,3274),TC_NIL,CONS_HEADER,
	UPTR(264,3139),UPTR(264,3277),UOHEAD(2,0),UPTR(264,3280),UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),UOHEAD(2,1),UPTR(167,1),
	UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3301),TC_NIL,CONS_HEADER,UPTR(264,3298),UPTR(264,3304),UOHEAD(2,0),
	UPTR(264,3307),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,3316),
	TC_NIL,CONS_HEADER,UPTR(264,3313),UPTR(264,3319),UOHEAD(2,0),UPTR(264,3322),UITAG(0),CONS_HEADER,UPTR(264,3325),TC_NIL,
	CONS_HEADER,UPTR(264,3310),UPTR(264,3328),CONS_HEADER,UPTR(264,3295),UPTR(264,3331),UOHEAD(2,0),UPTR(264,3334),UITAG(0),UOHEAD(2,1),
	UPTR(211,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3346),
	TC_NIL,CONS_HEADER,UPTR(264,3343),UPTR(264,3349),UOHEAD(2,0),UPTR(264,3352),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,3361),TC_NIL,CONS_HEADER,UPTR(264,3358),UPTR(264,3364),UOHEAD(2,0),
	UPTR(264,3367),UITAG(0),CONS_HEADER,UPTR(264,3370),TC_NIL,CONS_HEADER,UPTR(264,3355),UPTR(264,3373),CONS_HEADER,UPTR(264,3340),
	UPTR(264,3376),UOHEAD(2,0),UPTR(264,3379),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),
	CONS_HEADER,UPTR(264,3388),TC_NIL,CONS_HEADER,UPTR(264,3385),UPTR(264,3391),UOHEAD(2,0),UPTR(264,3394),UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,3403),TC_NIL,CONS_HEADER,UPTR(264,3400),
	UPTR(264,3406),UOHEAD(2,0),UPTR(264,3409),UITAG(0),CONS_HEADER,UPTR(264,3412),TC_NIL,CONS_HEADER,UPTR(264,3397),UPTR(264,3415),
	CONS_HEADER,UPTR(264,3382),UPTR(264,3418),CONS_HEADER,UPTR(264,3337),UPTR(264,3421),UOHEAD(2,0),UPTR(264,3424),UITAG(0),CONS_HEADER,
	UPTR(264,3427),TC_NIL,CONS_HEADER,UPTR(264,3292),UPTR(264,3430),CONS_HEADER,UPTR(264,3289),UPTR(264,3433),UOHEAD(2,0),UPTR(264,3436),
	UITAG(0),UOHEAD(2,1),UPTR(169,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),
	UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3454),TC_NIL,CONS_HEADER,
	UPTR(264,3451),UPTR(264,3457),UOHEAD(2,0),UPTR(264,3460),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),
	UITAG(0),CONS_HEADER,UPTR(264,3469),TC_NIL,CONS_HEADER,UPTR(264,3466),UPTR(264,3472),UOHEAD(2,0),UPTR(264,3475),UITAG(0),
	CONS_HEADER,UPTR(264,3478),TC_NIL,CONS_HEADER,UPTR(264,3463),UPTR(264,3481),CONS_HEADER,UPTR(264,3448),UPTR(264,3484),UOHEAD(2,0),
	UPTR(264,3487),UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),
	UITAG(0),CONS_HEADER,UPTR(264,3499),TC_NIL,CONS_HEADER,UPTR(264,3496),UPTR(264,3502),UOHEAD(2,0),UPTR(264,3505),UITAG(0),
	UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,3514),TC_NIL,CONS_HEADER,
	UPTR(264,3511),UPTR(264,3517),UOHEAD(2,0),UPTR(264,3520),UITAG(0),CONS_HEADER,UPTR(264,3523),TC_NIL,CONS_HEADER,UPTR(264,3508),
	UPTR(264,3526),CONS_HEADER,UPTR(264,3493),UPTR(264,3529),UOHEAD(2,0),UPTR(264,3532),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3541),TC_NIL,CONS_HEADER,UPTR(264,3538),UPTR(264,3544),UOHEAD(2,0),
	UPTR(264,3547),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,3556),
	TC_NIL,CONS_HEADER,UPTR(264,3553),UPTR(264,3559),UOHEAD(2,0),UPTR(264,3562),UITAG(0),CONS_HEADER,UPTR(264,3565),TC_NIL,
	CONS_HEADER,UPTR(264,3550),UPTR(264,3568),CONS_HEADER,UPTR(264,3535),UPTR(264,3571),CONS_HEADER,UPTR(264,3490),UPTR(264,3574),UOHEAD(2,0),
	UPTR(264,3577),UITAG(0),CONS_HEADER,UPTR(264,3580),TC_NIL,CONS_HEADER,UPTR(264,3445),UPTR(264,3583),CONS_HEADER,UPTR(264,3442),
	UPTR(264,3586),UOHEAD(2,0),UPTR(264,3589),UITAG(0),UOHEAD(2,1),UPTR(171,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,3601),TC_NIL,CONS_HEADER,UPTR(264,3598),UPTR(264,3604),CONS_HEADER,
	UPTR(264,3595),UPTR(264,3607),UOHEAD(2,0),UPTR(264,3610),UITAG(0),CONS_HEADER,UPTR(264,3613),TC_NIL,CONS_HEADER,UPTR(264,3592),
	UPTR(264,3616),CONS_HEADER,UPTR(264,3439),UPTR(264,3619),UOHEAD(2,0),UPTR(264,3622),UITAG(0),CONS_HEADER,UPTR(264,3625),TC_NIL,
	CONS_HEADER,UPTR(264,3286),UPTR(264,3628),UOHEAD(2,0),UPTR(264,3631),UITAG(0),UOHEAD(2,1),UPTR(213,1),UITAG(0),UOHEAD(2,1),
	UPTR(31,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,3646),
	TC_NIL,UOHEAD(2,0),UPTR(264,3649),UITAG(0),CONS_HEADER,UPTR(264,3652),TC_NIL,UOHEAD(2,0),UPTR(264,3655),UITAG(0),
	CONS_HEADER,UPTR(264,3658),TC_NIL,CONS_HEADER,UPTR(264,3643),UPTR(264,3661),CONS_HEADER,UPTR(264,3640),UPTR(264,3664),UOHEAD(2,0),
	UPTR(264,3667),UITAG(0),UOHEAD(2,1),UPTR(215,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(31,1),
	UITAG(0),CONS_HEADER,UPTR(264,3679),TC_NIL,UOHEAD(2,0),UPTR(264,3682),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),
	CONS_HEADER,UPTR(264,3688),TC_NIL,UOHEAD(2,0),UPTR(264,3691),UITAG(0),CONS_HEADER,UPTR(264,3694),TC_NIL,CONS_HEADER,
	UPTR(264,3685),UPTR(264,3697),UOHEAD(2,0),UPTR(264,3700),UITAG(0),CONS_HEADER,UPTR(264,3703),TC_NIL,CONS_HEADER,UPTR(264,3676),
	UPTR(264,3706),CONS_HEADER,UPTR(264,3673),UPTR(264,3709),UOHEAD(2,0),UPTR(264,3712),UITAG(0),UOHEAD(2,1),UPTR(217,1),UITAG(0),
	UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,3724),TC_NIL,UOHEAD(2,0),
	UPTR(264,3727),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,3733),TC_NIL,UOHEAD(2,0),UPTR(264,3736),
	UITAG(0),CONS_HEADER,UPTR(264,3739),TC_NIL,CONS_HEADER,UPTR(264,3730),UPTR(264,3742),UOHEAD(2,0),UPTR(264,3745),UITAG(0),
	CONS_HEADER,UPTR(264,3748),TC_NIL,CONS_HEADER,UPTR(264,3721),UPTR(264,3751),CONS_HEADER,UPTR(264,3718),UPTR(264,3754),UOHEAD(2,0),
	UPTR(264,3757),UITAG(0),UOHEAD(2,1),UPTR(219,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(52,1),
	UITAG(0),CONS_HEADER,UPTR(264,3769),TC_NIL,UOHEAD(2,0),UPTR(264,3772),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),
	CONS_HEADER,UPTR(264,3778),TC_NIL,UOHEAD(2,0),UPTR(264,3781),UITAG(0),CONS_HEADER,UPTR(264,3784),TC_NIL,CONS_HEADER,
	UPTR(264,3775),UPTR(264,3787),UOHEAD(2,0),UPTR(264,3790),UITAG(0),CONS_HEADER,UPTR(264,3793),TC_NIL,CONS_HEADER,UPTR(264,3766),
	UPTR(264,3796),CONS_HEADER,UPTR(264,3763),UPTR(264,3799),UOHEAD(2,0),UPTR(264,3802),UITAG(0),UOHEAD(2,1),UPTR(221,1),UITAG(0),
	UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,3814),TC_NIL,UOHEAD(2,0),
	UPTR(264,3817),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,3823),TC_NIL,UOHEAD(2,0),UPTR(264,3826),
	UITAG(0),CONS_HEADER,UPTR(264,3829),TC_NIL,CONS_HEADER,UPTR(264,3820),UPTR(264,3832),UOHEAD(2,0),UPTR(264,3835),UITAG(0),
	CONS_HEADER,UPTR(264,3838),TC_NIL,CONS_HEADER,UPTR(264,3811),UPTR(264,3841),CONS_HEADER,UPTR(264,3808),UPTR(264,3844),UOHEAD(2,0),
	UPTR(264,3847),UITAG(0),CONS_HEADER,UPTR(264,3850),TC_NIL,CONS_HEADER,UPTR(264,3805),UPTR(264,3853),CONS_HEADER,UPTR(264,3760),
	UPTR(264,3856),CONS_HEADER,UPTR(264,3715),UPTR(264,3859),CONS_HEADER,UPTR(264,3670),UPTR(264,3862),UOHEAD(2,0),UPTR(264,3865),UITAG(0),
	CONS_HEADER,UPTR(264,3868),TC_NIL,CONS_HEADER,UPTR(264,3637),UPTR(264,3871),UOHEAD(2,0),UPTR(264,3874),UITAG(0),UOHEAD(2,1),
	UPTR(223,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),
	UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(264,3895),TC_NIL,
	UOHEAD(2,0),UPTR(264,3898),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3913),TC_NIL,CONS_HEADER,UPTR(264,3910),
	UPTR(264,3916),UOHEAD(2,0),UPTR(264,3919),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),
	CONS_HEADER,UPTR(264,3928),TC_NIL,CONS_HEADER,UPTR(264,3925),UPTR(264,3931),UOHEAD(2,0),UPTR(264,3934),UITAG(0),CONS_HEADER,
	UPTR(264,3937),TC_NIL,CONS_HEADER,UPTR(264,3922),UPTR(264,3940),CONS_HEADER,UPTR(264,3907),UPTR(264,3943),UOHEAD(2,0),UPTR(264,3946),
	UITAG(0),CONS_HEADER,UPTR(264,3949),TC_NIL,CONS_HEADER,UPTR(264,3904),UPTR(264,3952),UOHEAD(2,0),UPTR(264,3955),UITAG(0),
	UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(264,3964),TC_NIL,UOHEAD(2,0),
	UPTR(264,3967),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(225,1),UITAG(0),UOHEAD(2,1),UPTR(89,1),
	UITAG(0),CONS_HEADER,UPTR(264,3979),TC_NIL,UOHEAD(2,0),UPTR(264,3982),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,3991),TC_NIL,CONS_HEADER,UPTR(264,3988),UPTR(264,3994),UOHEAD(2,0),
	UPTR(264,3997),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,4006),
	TC_NIL,CONS_HEADER,UPTR(264,4003),UPTR(264,4009),UOHEAD(2,0),UPTR(264,4012),UITAG(0),CONS_HEADER,UPTR(264,4015),TC_NIL,
	CONS_HEADER,UPTR(264,4000),UPTR(264,4018),CONS_HEADER,UPTR(264,3985),UPTR(264,4021),CONS_HEADER,UPTR(264,3976),UPTR(264,4024),UOHEAD(2,0),
	UPTR(264,4027),UITAG(0),CONS_HEADER,UPTR(264,4030),TC_NIL,CONS_HEADER,UPTR(264,3973),UPTR(264,4033),UOHEAD(2,0),UPTR(264,4036),
	UITAG(0),UOHEAD(2,1),UPTR(149,1),UITAG(0),UOHEAD(2,1),UPTR(151,1),UITAG(0),CONS_HEADER,UPTR(264,4045),TC_NIL,
	UOHEAD(2,0),UPTR(264,4048),UITAG(0),CONS_HEADER,UPTR(264,4051),TC_NIL,CONS_HEADER,UPTR(264,4042),UPTR(264,4054),UOHEAD(2,0),
	UPTR(264,4057),UITAG(0),CONS_HEADER,UPTR(264,4060),TC_NIL,CONS_HEADER,UPTR(264,4039),UPTR(264,4063),CONS_HEADER,UPTR(264,3970),
	UPTR(264,4066),CONS_HEADER,UPTR(264,3961),UPTR(264,4069),UOHEAD(2,0),UPTR(264,4072),UITAG(0),CONS_HEADER,UPTR(264,4075),TC_NIL,
	CONS_HEADER,UPTR(264,3958),UPTR(264,4078),CONS_HEADER,UPTR(264,3901),UPTR(264,4081),CONS_HEADER,UPTR(264,3892),UPTR(264,4084),UOHEAD(2,0),
	UPTR(264,4087),UITAG(0),CONS_HEADER,UPTR(264,4090),TC_NIL,CONS_HEADER,UPTR(264,3889),UPTR(264,4093),UOHEAD(2,0),UPTR(264,4096),
	UITAG(0),CONS_HEADER,UPTR(264,4099),TC_NIL,UOHEAD(2,0),UPTR(264,4102),UITAG(0),CONS_HEADER,UPTR(264,4105),TC_NIL,
	CONS_HEADER,UPTR(264,3886),UPTR(264,4108),CONS_HEADER,UPTR(264,3883),UPTR(264,4111),UOHEAD(2,0),UPTR(264,4114),UITAG(0),CONS_HEADER,
	UPTR(264,4117),TC_NIL,UOHEAD(2,0),UPTR(264,4120),UITAG(0),CONS_HEADER,UPTR(264,4123),TC_NIL,CONS_HEADER,UPTR(264,3880),
	UPTR(264,4126),UOHEAD(2,0),UPTR(264,4129),UITAG(0),UOHEAD(2,1),UPTR(227,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),
	UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(264,4150),TC_NIL,UOHEAD(2,0),UPTR(264,4153),UITAG(0),UOHEAD(2,1),UPTR(143,1),
	UITAG(0),UOHEAD(2,1),UPTR(211,1),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),
	CONS_HEADER,UPTR(264,4168),TC_NIL,CONS_HEADER,UPTR(264,4165),UPTR(264,4171),UOHEAD(2,0),UPTR(264,4174),UITAG(0),UOHEAD(2,1),
	UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),CONS_HEADER,UPTR(264,4183),TC_NIL,CONS_HEADER,UPTR(264,4180),
	UPTR(264,4186),UOHEAD(2,0),UPTR(264,4189),UITAG(0),CONS_HEADER,UPTR(264,4192),TC_NIL,CONS_HEADER,UPTR(264,4177),UPTR(264,4195),
	CONS_HEADER,UPTR(264,4162),UPTR(264,4198),UOHEAD(2,0),UPTR(264,4201),UITAG(0),CONS_HEADER,UPTR(264,4204),TC_NIL,CONS_HEADER,
	UPTR(264,4159),UPTR(264,4207),UOHEAD(2,0),UPTR(264,4210),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,1),UPTR(89,1),
	UITAG(0),CONS_HEADER,UPTR(264,4219),TC_NIL,UOHEAD(2,0),UPTR(264,4222),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),
	UOHEAD(2,1),UPTR(225,1),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(264,4234),TC_NIL,UOHEAD(2,0),
	UPTR(264,4237),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,4246),
	TC_NIL,CONS_HEADER,UPTR(264,4243),UPTR(264,4249),UOHEAD(2,0),UPTR(264,4252),UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),CONS_HEADER,UPTR(264,4261),TC_NIL,CONS_HEADER,UPTR(264,4258),UPTR(264,4264),UOHEAD(2,0),
	UPTR(264,4267),UITAG(0),CONS_HEADER,UPTR(264,4270),TC_NIL,CONS_HEADER,UPTR(264,4255),UPTR(264,4273),CONS_HEADER,UPTR(264,4240),
	UPTR(264,4276),CONS_HEADER,UPTR(264,4231),UPTR(264,4279),UOHEAD(2,0),UPTR(264,4282),UITAG(0),CONS_HEADER,UPTR(264,4285),TC_NIL,
	CONS_HEADER,UPTR(264,4228),UPTR(264,4288),UOHEAD(2,0),UPTR(264,4291),UITAG(0),UOHEAD(2,1),UPTR(149,1),UITAG(0),UOHEAD(2,1),
	UPTR(151,1),UITAG(0),CONS_HEADER,UPTR(264,4300),TC_NIL,UOHEAD(2,0),UPTR(264,4303),UITAG(0),CONS_HEADER,UPTR(264,4306),
	TC_NIL,CONS_HEADER,UPTR(264,4297),UPTR(264,4309),UOHEAD(2,0),UPTR(264,4312),UITAG(0),CONS_HEADER,UPTR(264,4315),TC_NIL,
	CONS_HEADER,UPTR(264,4294),UPTR(264,4318),CONS_HEADER,UPTR(264,4225),UPTR(264,4321),CONS_HEADER,UPTR(264,4216),UPTR(264,4324),UOHEAD(2,0),
	UPTR(264,4327),UITAG(0),CONS_HEADER,UPTR(264,4330),TC_NIL,CONS_HEADER,UPTR(264,4213),UPTR(264,4333),CONS_HEADER,UPTR(264,4156),
	UPTR(264,4336),CONS_HEADER,UPTR(264,4147),UPTR(264,4339),UOHEAD(2,0),UPTR(264,4342),UITAG(0),CONS_HEADER,UPTR(264,4345),TC_NIL,
	CONS_HEADER,UPTR(264,4144),UPTR(264,4348),UOHEAD(2,0),UPTR(264,4351),UITAG(0),CONS_HEADER,UPTR(264,4354),TC_NIL,UOHEAD(2,0),
	UPTR(264,4357),UITAG(0),CONS_HEADER,UPTR(264,4360),TC_NIL,CONS_HEADER,UPTR(264,4141),UPTR(264,4363),CONS_HEADER,UPTR(264,4138),
	UPTR(264,4366),UOHEAD(2,0),UPTR(264,4369),UITAG(0),CONS_HEADER,UPTR(264,4372),TC_NIL,UOHEAD(2,0),UPTR(264,4375),UITAG(0),
	CONS_HEADER,UPTR(264,4378),TC_NIL,CONS_HEADER,UPTR(264,4135),UPTR(264,4381),UOHEAD(2,0),UPTR(264,4384),UITAG(0),UOHEAD(2,1),
	UPTR(229,1),UITAG(0),UOHEAD(2,1),UPTR(231,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(264,4399),TC_NIL,CONS_HEADER,UPTR(264,4396),UPTR(264,4402),CONS_HEADER,UPTR(264,4393),UPTR(264,4405),
	UOHEAD(2,0),UPTR(264,4408),UITAG(0),UOHEAD(2,1),UPTR(233,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4420),TC_NIL,CONS_HEADER,UPTR(264,4417),UPTR(264,4423),CONS_HEADER,UPTR(264,4414),
	UPTR(264,4426),UOHEAD(2,0),UPTR(264,4429),UITAG(0),UOHEAD(2,1),UPTR(235,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4441),TC_NIL,CONS_HEADER,UPTR(264,4438),UPTR(264,4444),CONS_HEADER,
	UPTR(264,4435),UPTR(264,4447),UOHEAD(2,0),UPTR(264,4450),UITAG(0),UOHEAD(2,1),UPTR(237,1),UITAG(0),UOHEAD(2,5),7,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4462),TC_NIL,CONS_HEADER,UPTR(264,4459),UPTR(264,4465),
	CONS_HEADER,UPTR(264,4456),UPTR(264,4468),UOHEAD(2,0),UPTR(264,4471),UITAG(0),UOHEAD(2,1),UPTR(59,1),UITAG(0),UOHEAD(2,5),
	9,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4483),TC_NIL,CONS_HEADER,UPTR(264,4480),
	UPTR(264,4486),CONS_HEADER,UPTR(264,4477),UPTR(264,4489),UOHEAD(2,0),UPTR(264,4492),UITAG(0),UOHEAD(2,1),UPTR(239,1),UITAG(0),
	UOHEAD(2,5),11,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4504),TC_NIL,CONS_HEADER,
	UPTR(264,4501),UPTR(264,4507),CONS_HEADER,UPTR(264,4498),UPTR(264,4510),UOHEAD(2,0),UPTR(264,4513),UITAG(0),UOHEAD(2,1),UPTR(241,1),
	UITAG(0),UOHEAD(2,5),13,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(264,4525),TC_NIL,
	CONS_HEADER,UPTR(264,4522),UPTR(264,4528),CONS_HEADER,UPTR(264,4519),UPTR(264,4531),UOHEAD(2,0),UPTR(264,4534),UITAG(0),CONS_HEADER,
	UPTR(264,4537),TC_NIL,CONS_HEADER,UPTR(264,4516),UPTR(264,4540),CONS_HEADER,UPTR(264,4495),UPTR(264,4543),CONS_HEADER,UPTR(264,4474),
	UPTR(264,4546),CONS_HEADER,UPTR(264,4453),UPTR(264,4549),CONS_HEADER,UPTR(264,4432),UPTR(264,4552),CONS_HEADER,UPTR(264,4411),UPTR(264,4555),
	UOHEAD(2,0),UPTR(264,4558),UITAG(0),CONS_HEADER,UPTR(264,4561),TC_NIL,CONS_HEADER,UPTR(264,4390),UPTR(264,4564),UOHEAD(2,0),
	UPTR(264,4567),UITAG(0),CONS_HEADER,UPTR(264,4570),TC_NIL,CONS_HEADER,UPTR(264,4387),UPTR(264,4573),CONS_HEADER,UPTR(264,4132),
	UPTR(264,4576),CONS_HEADER,UPTR(264,3877),UPTR(264,4579),CONS_HEADER,UPTR(264,3634),UPTR(264,4582),CONS_HEADER,UPTR(264,3283),UPTR(264,4585),
	CONS_HEADER,UPTR(264,3136),UPTR(264,4588),CONS_HEADER,UPTR(264,2977),UPTR(264,4591),CONS_HEADER,UPTR(264,2902),UPTR(264,4594),CONS_HEADER,
	UPTR(264,2647),UPTR(264,4597),CONS_HEADER,UPTR(264,2476),UPTR(264,4600),CONS_HEADER,UPTR(264,2317),UPTR(264,4603),CONS_HEADER,UPTR(264,2170),
	UPTR(264,4606),CONS_HEADER,UPTR(264,1927),UPTR(264,4609),CONS_HEADER,UPTR(264,1828),UPTR(264,4612),CONS_HEADER,UPTR(264,1537),UPTR(264,4615),
	CONS_HEADER,UPTR(264,1174),UPTR(264,4618),CONS_HEADER,UPTR(264,1099),UPTR(264,4621),CONS_HEADER,UPTR(264,508),UPTR(264,4624),CONS_HEADER,
	UPTR(264,397),UPTR(264,4627),CONS_HEADER,UPTR(264,298),UPTR(264,4630),CONS_HEADER,UPTR(264,235),UPTR(264,4633),CONS_HEADER,UPTR(264,154),
	UPTR(264,4636),CONS_HEADER,UPTR(264,67),UPTR(264,4639),CONS_HEADER,UPTR(264,4),UPTR(264,4642),UOHEAD(2,0),UPTR(264,4645),UITAG(0),
	UOHEAD(2,1),UPTR(243,1),UITAG(0),UOHEAD(2,1),UPTR(245,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),
	UPTR(247,1),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,UPTR(264,4663),TC_NIL,UOHEAD(2,0),UPTR(264,4666),
	UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,4672),TC_NIL,UOHEAD(2,0),UPTR(264,4675),UITAG(0),
	UOHEAD(2,1),UPTR(31,1),UITAG(0),CONS_HEADER,UPTR(264,4681),TC_NIL,UOHEAD(2,0),UPTR(264,4684),UITAG(0),CONS_HEADER,
	UPTR(264,4687),TC_NIL,CONS_HEADER,UPTR(264,4678),UPTR(264,4690),CONS_HEADER,UPTR(264,4669),UPTR(264,4693),CONS_HEADER,UPTR(264,4660),
	UPTR(264,4696),UOHEAD(2,0),UPTR(264,4699),UITAG(0),CONS_HEADER,UPTR(264,4702),TC_NIL,CONS_HEADER,UPTR(264,4657),UPTR(264,4705),
	UOHEAD(2,0),UPTR(264,4708),UITAG(0),CONS_HEADER,UPTR(264,4711),TC_NIL,CONS_HEADER,UPTR(264,4654),UPTR(264,4714),UOHEAD(2,0),
	UPTR(264,4717),UITAG(0),UOHEAD(2,1),UPTR(249,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(31,1),
	UITAG(0),CONS_HEADER,UPTR(264,4729),TC_NIL,UOHEAD(2,0),UPTR(264,4732),UITAG(0),CONS_HEADER,UPTR(264,4735),TC_NIL,
	CONS_HEADER,UPTR(264,4726),UPTR(264,4738),UOHEAD(2,0),UPTR(264,4741),UITAG(0),CONS_HEADER,UPTR(264,4744),TC_NIL,CONS_HEADER,
	UPTR(264,4723),UPTR(264,4747),UOHEAD(2,0),UPTR(264,4750),UITAG(0),UOHEAD(2,1),UPTR(251,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),
	UITAG(0),UOHEAD(2,1),UPTR(52,1),UITAG(0),CONS_HEADER,UPTR(264,4762),TC_NIL,UOHEAD(2,0),UPTR(264,4765),UITAG(0),
	CONS_HEADER,UPTR(264,4768),TC_NIL,CONS_HEADER,UPTR(264,4759),UPTR(264,4771),UOHEAD(2,0),UPTR(264,4774),UITAG(0),CONS_HEADER,
	UPTR(264,4777),TC_NIL,CONS_HEADER,UPTR(264,4756),UPTR(264,4780),UOHEAD(2,0),UPTR(264,4783),UITAG(0),UOHEAD(2,1),UPTR(253,1),
	UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(123,1),UITAG(0),CONS_HEADER,UPTR(264,4795),TC_NIL,
	UOHEAD(2,0),UPTR(264,4798),UITAG(0),CONS_HEADER,UPTR(264,4801),TC_NIL,CONS_HEADER,UPTR(264,4792),UPTR(264,4804),UOHEAD(2,0),
	UPTR(264,4807),UITAG(0),CONS_HEADER,UPTR(264,4810),TC_NIL,CONS_HEADER,UPTR(264,4789),UPTR(264,4813),UOHEAD(2,0),UPTR(264,4816),
	UITAG(0),UOHEAD(2,1),UPTR(255,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(31,1),UITAG(0),
	CONS_HEADER,UPTR(264,4828),TC_NIL,UOHEAD(2,0),UPTR(264,4831),UITAG(0),CONS_HEADER,UPTR(264,4834),TC_NIL,CONS_HEADER,
	UPTR(264,4825),UPTR(264,4837),UOHEAD(2,0),UPTR(264,4840),UITAG(0),CONS_HEADER,UPTR(264,4843),TC_NIL,CONS_HEADER,UPTR(264,4822),
	UPTR(264,4846),UOHEAD(2,0),UPTR(264,4849),UITAG(0),UOHEAD(2,1),UPTR(257,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),
	UOHEAD(2,1),UPTR(247,1),UITAG(0),CONS_HEADER,UPTR(264,4861),TC_NIL,UOHEAD(2,0),UPTR(264,4864),UITAG(0),CONS_HEADER,
	UPTR(264,4867),TC_NIL,CONS_HEADER,UPTR(264,4858),UPTR(264,4870),UOHEAD(2,0),UPTR(264,4873),UITAG(0),CONS_HEADER,UPTR(264,4876),
	TC_NIL,CONS_HEADER,UPTR(264,4855),UPTR(264,4879),UOHEAD(2,0),UPTR(264,4882),UITAG(0),UOHEAD(2,1),UPTR(259,1),UITAG(0),
	UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,UPTR(264,4894),TC_NIL,UOHEAD(2,0),
	UPTR(264,4897),UITAG(0),CONS_HEADER,UPTR(264,4900),TC_NIL,CONS_HEADER,UPTR(264,4891),UPTR(264,4903),UOHEAD(2,0),UPTR(264,4906),
	UITAG(0),CONS_HEADER,UPTR(264,4909),TC_NIL,CONS_HEADER,UPTR(264,4888),UPTR(264,4912),UOHEAD(2,0),UPTR(264,4915),UITAG(0),
	UOHEAD(2,1),UPTR(261,1),UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),UOHEAD(2,1),UPTR(46,1),UITAG(0),CONS_HEADER,
	UPTR(264,4927),TC_NIL,UOHEAD(2,0),UPTR(264,4930),UITAG(0),CONS_HEADER,UPTR(264,4933),TC_NIL,CONS_HEADER,UPTR(264,4924),
	UPTR(264,4936),UOHEAD(2,0),UPTR(264,4939),UITAG(0),CONS_HEADER,UPTR(264,4942),TC_NIL,CONS_HEADER,UPTR(264,4921),UPTR(264,4945),
	UOHEAD(2,0),UPTR(264,4948),UITAG(0),CONS_HEADER,UPTR(264,4951),TC_NIL,CONS_HEADER,UPTR(264,4918),UPTR(264,4954),CONS_HEADER,
	UPTR(264,4885),UPTR(264,4957),CONS_HEADER,UPTR(264,4852),UPTR(264,4960),CONS_HEADER,UPTR(264,4819),UPTR(264,4963),CONS_HEADER,UPTR(264,4786),
	UPTR(264,4966),CONS_HEADER,UPTR(264,4753),UPTR(264,4969),CONS_HEADER,UPTR(264,4720),UPTR(264,4972),CONS_HEADER,UPTR(264,4651),UPTR(264,4975),
	UOHEAD(2,0),UPTR(264,4978),UITAG(0),UOHEAD(2,1),UPTR(35,1),UITAG(0),UOHEAD(2,1),UPTR(245,1),UITAG(0),UOHEAD(2,5),
	1,UITAG(0),CONS_HEADER,UPTR(264,4990),TC_NIL,CONS_HEADER,UPTR(264,4987),UPTR(264,4993),UOHEAD(2,0),UPTR(264,4996),
	UITAG(0),UOHEAD(2,1),UPTR(249,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),CONS_HEADER,UPTR(264,5005),TC_NIL,
	CONS_HEADER,UPTR(264,5002),UPTR(264,5008),UOHEAD(2,0),UPTR(264,5011),UITAG(0),UOHEAD(2,1),UPTR(251,1),UITAG(0),UOHEAD(2,5),
	5,UITAG(0),CONS_HEADER,UPTR(264,5020),TC_NIL,CONS_HEADER,UPTR(264,5017),UPTR(264,5023),UOHEAD(2,0),UPTR(264,5026),
	UITAG(0),UOHEAD(2,1),UPTR(253,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),CONS_HEADER,UPTR(264,5035),TC_NIL,
	CONS_HEADER,UPTR(264,5032),UPTR(264,5038),UOHEAD(2,0),UPTR(264,5041),UITAG(0),UOHEAD(2,1),UPTR(263,1),UITAG(0),UOHEAD(2,5),
	9,UITAG(0),CONS_HEADER,UPTR(264,5050),TC_NIL,CONS_HEADER,UPTR(264,5047),UPTR(264,5053),UOHEAD(2,0),UPTR(264,5056),
	UITAG(0),UOHEAD(2,1),UPTR(255,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),CONS_HEADER,UPTR(264,5065),TC_NIL,
	CONS_HEADER,UPTR(264,5062),UPTR(264,5068),UOHEAD(2,0),UPTR(264,5071),UITAG(0),UOHEAD(2,1),UPTR(257,1),UITAG(0),UOHEAD(2,5),
	13,UITAG(0),CONS_HEADER,UPTR(264,5080),TC_NIL,CONS_HEADER,UPTR(264,5077),UPTR(264,5083),UOHEAD(2,0),UPTR(264,5086),
	UITAG(0),UOHEAD(2,1),UPTR(259,1),UITAG(0),UOHEAD(2,5),15,UITAG(0),CONS_HEADER,UPTR(264,5095),TC_NIL,
	CONS_HEADER,UPTR(264,5092),UPTR(264,5098),UOHEAD(2,0),UPTR(264,5101),UITAG(0),UOHEAD(2,1),UPTR(261,1),UITAG(0),UOHEAD(2,5),
	17,UITAG(0),CONS_HEADER,UPTR(264,5110),TC_NIL,CONS_HEADER,UPTR(264,5107),UPTR(264,5113),UOHEAD(2,0),UPTR(264,5116),
	UITAG(0),CONS_HEADER,UPTR(264,5119),TC_NIL,CONS_HEADER,UPTR(264,5104),UPTR(264,5122),CONS_HEADER,UPTR(264,5089),UPTR(264,5125),
	CONS_HEADER,UPTR(264,5074),UPTR(264,5128),CONS_HEADER,UPTR(264,5059),UPTR(264,5131),CONS_HEADER,UPTR(264,5044),UPTR(264,5134),CONS_HEADER,
	UPTR(264,5029),UPTR(264,5137),CONS_HEADER,UPTR(264,5014),UPTR(264,5140),CONS_HEADER,UPTR(264,4999),UPTR(264,5143),CONS_HEADER,UPTR(264,4984),
	UPTR(264,5146),UOHEAD(2,0),UPTR(264,5149),UITAG(0),CONS_HEADER,UPTR(264,5152),TC_NIL,CONS_HEADER,UPTR(264,4981),UPTR(264,5155),
	CONS_HEADER,UPTR(264,4648),UPTR(264,5158),CONS_HEADER,UPTR(264,1),UPTR(264,5161),UOHEAD(2,0),UPTR(264,5164),UITAG(0)};
irk_int irk_internal_symbols[] = {(109<<TAGSIZE)|TC_VECTOR,
	UPTR(241,1),UPTR(239,1),UPTR(237,1),UPTR(235,1),UPTR(233,1),UPTR(231,1),UPTR(229,1),UPTR(261,1),UPTR(259,1),UPTR(257,1),
	UPTR(253,1),UPTR(251,1),UPTR(249,1),UPTR(255,1),UPTR(245,1),UPTR(227,1),UPTR(223,1),UPTR(221,1),UPTR(219,1),UPTR(217,1),
	UPTR(215,1),UPTR(213,1),UPTR(211,1),UPTR(209,1),UPTR(205,1),UPTR(203,1),UPTR(201,1),UPTR(195,1),UPTR(191,1),UPTR(189,1),
	UPTR(187,1),UPTR(179,1),UPTR(177,1),UPTR(175,1),UPTR(173,1),UPTR(171,1),UPTR(169,1),UPTR(167,1),UPTR(163,1),UPTR(161,1),
	UPTR(159,1),UPTR(157,1),UPTR(155,1),UPTR(153,1),UPTR(147,1),UPTR(145,1),UPTR(141,1),UPTR(135,1),UPTR(131,1),UPTR(115,1),
	UPTR(113,1),UPTR(111,1),UPTR(105,1),UPTR(99,1),UPTR(97,1),UPTR(95,1),UPTR(93,1),UPTR(91,1),UPTR(67,1),UPTR(56,1),
	UPTR(50,1),UPTR(42,1),UPTR(40,1),UPTR(38,1),UPTR(33,1),UPTR(65,1),UPTR(133,1),UPTR(127,1),UPTR(199,1),UPTR(101,1),
	UPTR(137,1),UPTR(54,1),UPTR(193,1),UPTR(139,1),UPTR(151,1),UPTR(149,1),UPTR(247,1),UPTR(143,1),UPTR(35,1),UPTR(243,1),
	UPTR(85,1),UPTR(37,1),UPTR(103,1),UPTR(89,1),UPTR(263,1),UPTR(117,1),UPTR(59,1),UPTR(181,1),UPTR(31,1),UPTR(183,1),
	UPTR(185,1),UPTR(165,1),UPTR(44,1),UPTR(125,1),UPTR(123,1),UPTR(207,1),UPTR(121,1),UPTR(52,1),UPTR(87,1),UPTR(119,1),
	UPTR(63,1),UPTR(61,1),UPTR(107,1),UPTR(109,1),UPTR(197,1),UPTR(225,1),UPTR(46,1),UPTR(129,1),UPTR(48,1)};
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
static void FUN_base_exception_handler_250_0(void);
static void FUN_truncate_remainder_191_0(void);
static void FUN_euclidean_quotient_196_0(void);
static void FUN_make_generator_246_0(void);
static void FUN_reverse_onto_264_0(void);
static void FUN_length_splat_267_0(void);
static void FUN_n_of_286_0(void);
static void FUN_map_287_0(void);
static void FUN_fold_294_0(void);
static void FUN_buffer_copy_325_0(void);
static void FUN_substring_327_0(void);
static void FUN_string_set_bang_332_0(void);
static void FUN_char__3estring_328_0(void);
static void FUN_string_ref_331_0(void);
static void FUN_string_concat_333_0(void);
static void FUN_starts_with_341_0(void);
static void FUN_ends_with_342_0(void);
static void FUN_list__3estring_347_0(void);
static void FUN_string__3elist_348_0(void);
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
static void FUN_in_class_question_1369_i6_0(void);
static void FUN__24loop31_1371_i6_0(void);
static void FUN_toplevel_11(O);
static void FUN_safe_char_354_0(void);
static void FUN_repr_string_355_0(void);
static void FUN_raise_system_error_519_0(void);
static void FUN_print_exception_exit_587_0(void);
static void FUN_file_2fread_char_634_0(void);
static void FUN_string__3ehex_657_0(void);
static void FUN_decoder_660_0(void);
static void FUN__24loop180_3483_0(void);
static void FUN_toplevel_12(O);
static void FUN_toplevel_13(O);
static void JUMP_65(O, O, O, O);
static void FUN_toplevel_14(O);
static void FUN_toplevel_15(O);
static void JUMP_64(O, O, O);
static void FUN_toplevel_16(O);
static void FUN_lambda_20521_i275_i289_0(void);
static void FUN_toplevel_17(O);
static void FUN_toplevel_18(O);
static void FUN_lambda_21756_i277_i289_0(void);
static void FUN_toplevel_19(O);
static void FUN__24genloop174_3458_i289_0(void);
static void FUN_toplevel_20(O);
static void JUMP_3();
static void JUMP_4(O);
static void JUMP_5(O);
static void FUN__24genloop174_3458_i289_21(O);
static void FAIL_2();
static void FAIL_2();
static void FUN__24genloop174_3458_i289_22(O);
static void JUMP_29();
static void FUN__24genloop174_3458_i289_23(O);
static void FUN__24genloop174_3458_i289_24(O);
static void FUN__24genloop174_3458_i289_25(O);
static void FUN__24genloop174_3458_i289_26(O);
static void FUN__24genloop174_3458_i289_27(O);
static void FUN__24loop171_3445_i279_i289_0(void);
static void FUN__24genloop174_3458_i289_28(O);
static void FUN__24genloop174_3458_i289_29(O);
static void FUN__24genloop174_3458_i289_30(O);
static void FUN__24genloop174_3458_i289_31(O);
static void FUN__24genloop174_3458_i289_32(O);
static void FUN__24genloop174_3458_i289_33(O);
static void JUMP_20(O, O, O, O);
static void FUN__24genloop174_3458_i289_34(O);
static void FUN_loop_1407_i4_0(void);
static void JUMP_22(O, O, O, O, O);
static void FUN__24genloop174_3458_i289_35(O);
static void JUMP_26(O, O);
static void FUN_loop_1407_i4_36(O);
static void FUN_loop_1407_i4_37(O);
static void FUN_loop_1407_i4_38(O);
static void FUN__24loop168_3448_i279_i289_0(void);
static void FUN_loop_1407_i4_39(O);
static void FUN_loop_1407_i4_40(O);
static void JUMP_6();
static void FUN_loop_1407_i4_41(O);
static void FUN_loop_1407_i4_42(O);
static void FUN_loop_1407_i4_43(O);
static void FUN__24loop166_3426_i271_i279_i289_0(void);
static void FUN_loop_1407_i4_44(O);
static void FUN__24loop163_3430_i271_i279_i289_0(void);
static void FUN_loop_1407_i4_45(O);
static void FUN__24loop163_3430_i271_i279_i289_46(O);
static void FUN__24loop163_3430_i271_i279_i289_47(O);
static void FUN__24loop163_3430_i271_i279_i289_48(O);
static void FUN__24loop166_3426_i271_i279_i289_49(O);
static void JUMP_12(O);
static void JUMP_13(O);
static void FUN__24loop166_3426_i271_i279_i289_50(O);
static void FAIL_11();
static void FUN__24loop166_3426_i271_i279_i289_51(O);
static void FUN__24loop166_3426_i271_i279_i289_52(O);
static void FUN__24loop168_3448_i279_i289_53(O);
static void FUN__24loop168_3448_i279_i289_54(O);
static void JUMP_17(O, O);
static void JUMP_18(O, O);
static void FUN__24loop168_3448_i279_i289_55(O);
static void FAIL_16(O);
static void FUN__24loop168_3448_i279_i289_56(O);
static void FUN__24loop168_3448_i279_i289_57(O);
static void FUN__24loop168_3448_i279_i289_58(O);
static void FUN__24loop171_3445_i279_i289_59(O);
static void FUN__24loop171_3445_i279_i289_60(O);
static void FUN__24loop171_3445_i279_i289_61(O);
static void FUN_lambda_21562_i225_i277_i289_0(void);
static void FUN_lambda_21756_i277_i289_62(O);
static void FUN_lambda_21680_i226_i277_i289_0(void);
static void FUN_lambda_21756_i277_i289_63(O);
static void FUN__24genloop151_3365_i277_i289_0(void);
static void FUN_lambda_21756_i277_i289_64(O);
static void FUN_lambda_21756_i277_i289_65(O);
static void FUN__24loop149_3370_i277_i289_0(void);
static void FUN__24loop149_3370_i277_i289_66(O);
static void FUN__24genloop151_3365_i277_i289_67(O);
static void FUN_lambda_5310_i5_i3_0(void);
static void FUN__24genloop151_3365_i277_i289_68(O);
static void FUN_lambda_21157_i207_i213_i3_0(void);
static void FUN__24genloop151_3365_i277_i289_69(O);
static void FUN_lambda_20878_i208_i213_i3_0(void);
static void FUN__24genloop151_3365_i277_i289_70(O);
static void FUN__24genloop120_3312_i3_0(void);
static void FUN__24genloop151_3365_i277_i289_71(O);
static void FUN__24genloop151_3365_i277_i289_72(O);
static void JUMP_32(O, O, O);
static void FUN__24genloop151_3365_i277_i289_73(O);
static void FUN__24genloop151_3365_i277_i289_74(O);
static void FUN__24genloop151_3365_i277_i289_75(O);
static void FUN__24genloop120_3312_i3_76(O);
static void FUN__24genloop120_3312_i3_77(O);
static void FUN__24genloop101_3270_i208_i213_i3_0(void);
static void FUN_lambda_20878_i208_i213_i3_78(O);
static void FUN_lambda_20878_i208_i213_i3_79(O);
static void FUN__24loop99_3278_i208_i213_i3_0(void);
static void FUN__24loop99_3278_i208_i213_i3_80(O);
static void FUN__24loop103_3275_i208_i213_i3_0(void);
static void FUN__24genloop101_3270_i208_i213_i3_81(O);
static void FUN__24genloop101_3270_i208_i213_i3_82(O);
static void FUN__24loop103_3275_i208_i213_i3_83(O);
static void FUN__24genloop117_3301_i207_i213_i3_0(void);
static void FUN_lambda_21157_i207_i213_i3_84(O);
static void FUN_lambda_21157_i207_i213_i3_85(O);
static void JUMP_38();
static void JUMP_39(O);
static void FUN_lambda_21157_i207_i213_i3_86(O);
static void FAIL_37();
static void FUN__24loop115_3305_i207_i213_i3_0(void);
static void FUN__24loop115_3305_i207_i213_i3_87(O);
static void JUMP_41(O);
static void JUMP_42(O);
static void FUN__24loop115_3305_i207_i213_i3_88(O);
static void FAIL_40();
static void JUMP_45(O);
static void JUMP_46(O);
static void FUN__24genloop117_3301_i207_i213_i3_89(O);
static void FUN__24genloop117_3301_i207_i213_i3_90(O);
static void FUN__24loop41_1437_i5_i3_0(void);
static void FUN_lambda_5310_i5_i3_91(O);
static void FUN__24loop38_1441_i5_i3_0(void);
static void FUN__24loop38_1441_i5_i3_92(O);
static void FUN__24loop41_1437_i5_i3_93(O);
static void FUN__24loop41_1437_i5_i3_94(O);
static void FUN__24genloop145_3356_i226_i277_i289_0(void);
static void FUN_lambda_21680_i226_i277_i289_95(O);
static void FUN_lambda_21680_i226_i277_i289_96(O);
static void FUN__24loop143_3359_i226_i277_i289_0(void);
static void FUN__24loop143_3359_i226_i277_i289_97(O);
static void JUMP_51();
static void JUMP_52(O);
static void FUN__24genloop145_3356_i226_i277_i289_98(O);
static void FAIL_50();
static void JUMP_54(O);
static void FUN__24genloop145_3356_i226_i277_i289_99(O);
static void JUMP_53(O);
static void FUN__24genloop145_3356_i226_i277_i289_100(O);
static void FUN__24genloop145_3356_i226_i277_i289_101(O);
static void FUN__24genloop145_3356_i226_i277_i289_102(O);
static void JUMP_56(O);
static void FUN__24genloop145_3356_i226_i277_i289_103(O);
static void JUMP_55(O);
static void FUN__24genloop145_3356_i226_i277_i289_104(O);
static void FUN__24genloop145_3356_i226_i277_i289_105(O);
static void FUN__24genloop145_3356_i226_i277_i289_106(O);
static void FUN__24genloop139_3345_i225_i277_i289_0(void);
static void FUN_lambda_21562_i225_i277_i289_107(O);
static void FUN_lambda_21562_i225_i277_i289_108(O);
static void FUN__24loop137_3348_i225_i277_i289_0(void);
static void FUN__24loop137_3348_i225_i277_i289_109(O);
static void JUMP_59();
static void FUN__24genloop139_3345_i225_i277_i289_110(O);
static void FUN__24genloop139_3345_i225_i277_i289_111(O);
static void FUN__24genloop139_3345_i225_i277_i289_112(O);
static void FUN__24genloop139_3345_i225_i277_i289_113(O);
static void FUN__24loop90_3236_i275_i289_0(void);
static void FUN_lambda_20521_i275_i289_114(O);
static void FUN__24loop89_3238_i275_i289_0(void);
static void FUN__24loop89_3238_i275_i289_115(O);
static void FUN__24loop90_3236_i275_i289_116(O);
static void JUMP_62(O);
static void FUN__24loop90_3236_i275_i289_117(O);
static void FUN__24loop180_3483_118(O);
static void FUN_get_3406_0(void);
static void FUN_get_u32_3408_0(void);
static void FUN_get_string_3409_0(void);
static void FUN_left_3410_0(void);
static void FUN_get_string_3409_119(O);
static void FUN_get_u32_3408_120(O);
static void FUN_get_u32_3408_121(O);
static void FUN_get_u32_3408_122(O);
static void FUN_get_u32_3408_123(O);
static void FUN_get_u32_3408_124(O);
static void FUN_get_3406_125(O);
static void FUN__24loop159_3386_0(void);
static void FUN_string__3ehex_657_126(O);
static void FUN__24loop159_3386_127(O);
static void FUN__24loop159_3386_128(O);
static void FUN__24loop159_3386_129(O);
static void JUMP_73(O, O);
static void FUN_file_2fread_char_634_130(O);
static void JUMP_88(O);
static void FUN_search_sexp_list_3006_i130_i159_i160_i165_0(void);
static void JUMP_99(O, O, O);
static void JUMP_100(O, O, O);
static void JUMP_101(O, O, O);
static void JUMP_102(O, O, O);
static void JUMP_104(O, O, O);
static void FUN_print_exception_exit_587_131(O);
static void FAIL_98(O, O);
static void FAIL_98(O, O);
static void FAIL_98(O, O);
static void FAIL_98(O, O);
static void FUN_print_exception_exit_587_132(O);
static void FUN_loop_3021_i159_i160_i165_0(void);
static void FUN_print_exception_exit_587_133(O);
static void FAIL_89();
static void FAIL_89();
static void FAIL_89();
static void FAIL_89();
static void FAIL_89();
static void FAIL_89();
static void FAIL_89();
static void FUN_loop_3021_i159_i160_i165_134(O);
static void FAIL_105();
static void FAIL_105();
static void FAIL_105();
static void FAIL_105();
static void JUMP_86(O, O, O);
static void JUMP_87(O, O, O);
static void JUMP_85(O, O, O);
static void JUMP_76(O, O, O);
static void JUMP_83(O, O, O);
static void JUMP_77(O, O, O);
static void JUMP_79(O, O, O);
static void JUMP_81(O, O, O);
static void JUMP_74(O, O);
static void FUN_search_sexp_list_3006_i130_i159_i160_i165_135(O);
static void FUN_repr_string_355_136(O);
static void FUN_repr_string_355_137(O);
static void FUN_repr_string_355_138(O);
static void FAIL_113();
static void FAIL_117();
static void FAIL_121();
static void FAIL_125();
static void FAIL_129();
static void FAIL_133();
static void FUN_safe_char_354_139(O);
static void FUN_loop_1443_i2_0(void);
static void JUMP_143(O, O, O, O, O);
static void FUN_safe_char_354_140(O);
static void JUMP_139(O, O, O);
static void FUN_loop_1041_i0_0(void);
static void FUN_safe_char_354_141(O);
static void FUN_safe_char_354_142(O);
static void FUN_safe_char_354_143(O);
static void JUMP_147(O, O);
static void FUN_loop_1364_0(void);
static void FUN_loop_1364_144(O);
static void FUN_list__3estring_347_145(O);
static void FUN__24loop28_1359_0(void);
static void FUN_list__3estring_347_146(O);
static void FUN__24loop28_1359_147(O);
static void FUN_loop_1344_0(void);
static void FUN_loop_1344_148(O);
static void FUN_loop_1344_149(O);
static void FUN_loop_1338_0(void);
static void FUN_loop_1338_150(O);
static void FUN_loop_1338_151(O);
static void FAIL_162();
static void FUN_lambda_3898_0(void);
static void FUN_string_concat_333_152(O);
static void FUN_loop_1292_0(void);
static void FUN_loop_1292_153(O);
static void JUMP_169(O);
static void JUMP_167();
static void FUN_string_ref_331_154(O);
static void FUN_char__3estring_328_155(O);
static void JUMP_172(O);
static void JUMP_170();
static void FUN_string_set_bang_332_156(O);
static void FUN_substring_327_157(O);
static void JUMP_179(O, O);
static void JUMP_180(O, O);
static void JUMP_177();
static void FUN_buffer_copy_325_158(O);
static void JUMP_175(O, O);
static void JUMP_176(O, O);
static void JUMP_173();
static void FUN_buffer_copy_325_159(O);
static void FUN_fold_294_160(O);
static void FUN_map_287_161(O);
static void FUN_map_287_162(O);
static void FUN_loop_1041_0(void);
static void FUN_yield_917_0(void);
static void FUN_entry_point_916_0(void);
static void JUMP_190(O, O);
static void FUN_euclidean_quotient_196_163(O);
static void JUMP_192(O, O);
static void JUMP_193(O, O);
static void JUMP_195(O, O);
static void JUMP_196(O, O);
static void FAIL_194(O);
static void FAIL_191(O);
void toplevel (void) {
  clear_tospace = 0;
  // env 0:
  //   (|...t24| -> t25) (|...t102| -> t101) (int int -> int) (int int -> int) ((((maybe t23) -> undefined) -> undefined) -> ( -> (maybe t23))) ((list t414) (list t414) -> (list t414)) ((list t208) int -> int) (int t683 -> (list t683)) ((t523 -> t524) (list t523) -> (list t524)) ((t166 t163 -> t163) t163 (list t166) -> t163) (string int int string int -> undefined) (string int int -> string) (string int char -> undefined) (char -> string) (string int -> char) ((list string) -> string) (string string -> bool) (string string -> bool) ((list char) -> string) (string -> (list char)) (list char) (list char) (list char) (list char) (list char) (char -> bool) (vector char) (char -> string) (string -> string) ( -> t200) int int (vector symbol) (vector symbol) (vector symbol) (|...t29| -> t30) ({pos=int end=int rbuf=string fd=int ...t394} -> char) (string -> string) (string -> {get=(int -> string) u32=( -> int) string=( -> string) left=( -> int) }) (int int -> int) (int int -> int) {argc=int argv=(vector string) } int string (vector int)
  O r1188 = alloc_cleared (TC_ENV, 46);
  top = r1188;
  r1188[1] = lenv; lenv = r1188;
  O r1189 = allocate (TC_CLOSURE, 2);
  r1189[1] = FUN_base_exception_handler_250_0; r1189[2] = lenv;
  r1188[2] = r1189;
  O r1190 = top[2];
  r1188[3] = r1190;
  O r1191 = allocate (TC_CLOSURE, 2);
  r1191[1] = FUN_truncate_remainder_191_0; r1191[2] = lenv;
  r1188[4] = r1191;
  O r1192 = allocate (TC_CLOSURE, 2);
  r1192[1] = FUN_euclidean_quotient_196_0; r1192[2] = lenv;
  r1188[5] = r1192;
  O r1193 = allocate (TC_CLOSURE, 2);
  r1193[1] = FUN_make_generator_246_0; r1193[2] = lenv;
  r1188[6] = r1193;
  O r1194 = allocate (TC_CLOSURE, 2);
  r1194[1] = FUN_reverse_onto_264_0; r1194[2] = lenv;
  r1188[7] = r1194;
  O r1195 = allocate (TC_CLOSURE, 2);
  r1195[1] = FUN_length_splat_267_0; r1195[2] = lenv;
  r1188[8] = r1195;
  O r1196 = allocate (TC_CLOSURE, 2);
  r1196[1] = FUN_n_of_286_0; r1196[2] = lenv;
  r1188[9] = r1196;
  O r1197 = allocate (TC_CLOSURE, 2);
  r1197[1] = FUN_map_287_0; r1197[2] = lenv;
  r1188[10] = r1197;
  O r1198 = allocate (TC_CLOSURE, 2);
  r1198[1] = FUN_fold_294_0; r1198[2] = lenv;
  r1188[11] = r1198;
  O r1199 = allocate (TC_CLOSURE, 2);
  r1199[1] = FUN_buffer_copy_325_0; r1199[2] = lenv;
  r1188[12] = r1199;
  O r1200 = allocate (TC_CLOSURE, 2);
  r1200[1] = FUN_substring_327_0; r1200[2] = lenv;
  r1188[13] = r1200;
  O r1201 = allocate (TC_CLOSURE, 2);
  r1201[1] = FUN_string_set_bang_332_0; r1201[2] = lenv;
  r1188[14] = r1201;
  O r1202 = allocate (TC_CLOSURE, 2);
  r1202[1] = FUN_char__3estring_328_0; r1202[2] = lenv;
  r1188[15] = r1202;
  O r1203 = allocate (TC_CLOSURE, 2);
  r1203[1] = FUN_string_ref_331_0; r1203[2] = lenv;
  r1188[16] = r1203;
  O r1204 = allocate (TC_CLOSURE, 2);
  r1204[1] = FUN_string_concat_333_0; r1204[2] = lenv;
  r1188[17] = r1204;
  O r1205 = allocate (TC_CLOSURE, 2);
  r1205[1] = FUN_starts_with_341_0; r1205[2] = lenv;
  r1188[18] = r1205;
  O r1206 = allocate (TC_CLOSURE, 2);
  r1206[1] = FUN_ends_with_342_0; r1206[2] = lenv;
  r1188[19] = r1206;
  O r1207 = allocate (TC_CLOSURE, 2);
  r1207[1] = FUN_list__3estring_347_0; r1207[2] = lenv;
  r1188[20] = r1207;
  O r1208 = allocate (TC_CLOSURE, 2);
  r1208[1] = FUN_string__3elist_348_0; r1208[2] = lenv;
  r1188[21] = r1208;
  // env 1:
  //   string
  O r2189 = allocate (TC_ENV, 2);
  O r2191 = (O) &constructed_81;
  r2189[2] = r2191;
  O r2190 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_1; t[4] = r1188; k = t;
  r2189[1] = r2190[2]; lenv = r2189; FUN_string__3elist_348_0();
}
static void FUN_toplevel_1 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r1209 = rr;
  r1188[22] = r1209;
  // env 2:
  //   string
  O r2186 = allocate (TC_ENV, 2);
  O r2188 = (O) &constructed_80;
  r2186[2] = r2188;
  O r2187 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_2; t[4] = r1188; k = t;
  r2186[1] = r2187[2]; lenv = r2186; FUN_string__3elist_348_0();
}
static void FUN_toplevel_2 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r1210 = rr;
  r1188[23] = r1210;
  // env 3:
  //   (list char) (list char)
  O r2178 = alloc_cleared (TC_ENV, 3);
  // env 4:
  //   (list char) (list string)
  O r2182 = allocate (TC_ENV, 3);
  O r2184 = top[22];
  r2182[2] = r2184;
  O r2185 = (object *) 10;
  r2182[3] = r2185;
  O r2183 = top[7];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_3; t[4] = r1188; t[5] = r2178; k = t;
  r2182[1] = r2183[2]; lenv = r2182; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_3 (O rr) {
  O r1188 = k[4]; O r2178 = k[5]; lenv = k[2]; k = k[1];
  O r2180 = rr;
  r2178[2] = r2180;
  O r2181 = top[23];
  r2178[3] = r2181;
  O r2179 = top[7];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_4; t[4] = r1188; k = t;
  r2178[1] = r2179[2]; lenv = r2178; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_4 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r1211 = rr;
  r1188[24] = r1211;
  // env 5:
  //   string
  O r2175 = allocate (TC_ENV, 2);
  O r2177 = (O) &constructed_79;
  r2175[2] = r2177;
  O r2176 = top[21];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_5; t[4] = r1188; k = t;
  r2175[1] = r2176[2]; lenv = r2175; FUN_string__3elist_348_0();
}
static void FUN_toplevel_5 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r1212 = rr;
  r1188[25] = r1212;
  // env 6:
  //   (list char) (list char)
  O r2161 = alloc_cleared (TC_ENV, 3);
  r2161[1] = lenv; lenv = r2161;
  // env 7:
  //   string
  O r2172 = allocate (TC_ENV, 2);
  O r2174 = (O) &constructed_78;
  r2172[2] = r2174;
  O r2173 = top[21];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_6; t[4] = r1188; t[5] = r2161; k = t;
  r2172[1] = r2173[2]; lenv = r2172; FUN_string__3elist_348_0();
}
static void FUN_toplevel_6 (O rr) {
  O r1188 = k[4]; O r2161 = k[5]; lenv = k[2]; k = k[1];
  O r2162 = rr;
  r2161[2] = r2162;
  // env 8:
  //   (list char) (list char)
  O r2164 = alloc_cleared (TC_ENV, 3);
  // env 9:
  //   (list char) (list string)
  O r2168 = allocate (TC_ENV, 3);
  O r2170 = top[24];
  r2168[2] = r2170;
  O r2171 = (object *) 10;
  r2168[3] = r2171;
  O r2169 = top[7];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_7; t[4] = r1188; t[5] = r2161; t[6] = r2164; k = t;
  r2168[1] = r2169[2]; lenv = r2168; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_7 (O rr) {
  O r1188 = k[4]; O r2161 = k[5]; O r2164 = k[6]; lenv = k[2]; k = k[1];
  O r2166 = rr;
  r2164[2] = r2166;
  O r2167 = ((object*) lenv) [2];
  r2164[3] = r2167;
  O r2165 = top[7];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_8; t[4] = r1188; t[5] = r2161; k = t;
  r2164[1] = r2165[2]; lenv = r2164; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_8 (O rr) {
  O r1188 = k[4]; O r2161 = k[5]; lenv = k[2]; k = k[1];
  O r2163 = rr;
  r2161[3] = r2163;
  // env 10:
  //   (list char) (list char)
  O r2153 = alloc_cleared (TC_ENV, 3);
  // env 11:
  //   (list char) (list string)
  O r2157 = allocate (TC_ENV, 3);
  O r2159 = top[25];
  r2157[2] = r2159;
  O r2160 = (object *) 10;
  r2157[3] = r2160;
  O r2158 = top[7];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_9; t[4] = r1188; t[5] = r2153; k = t;
  r2157[1] = r2158[2]; lenv = r2157; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_9 (O rr) {
  O r1188 = k[4]; O r2153 = k[5]; lenv = k[2]; k = k[1];
  O r2155 = rr;
  r2153[2] = r2155;
  O r2156 = ((object*) lenv) [3];
  r2153[3] = r2156;
  O r2154 = top[7];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_10; t[4] = r1188; k = t;
  r2153[1] = r2154[2]; lenv = r2153; FUN_reverse_onto_264_0();
}
static void FUN_toplevel_10 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r2152 = rr;
  lenv = lenv[1];
  O r1213 = r2152;
  r1188[26] = r1213;
  // env 12:
  //   int (vector bool)
  O r2146 = alloc_cleared (TC_ENV, 3);
  r2146[1] = lenv; lenv = r2146;
  O r2147 = (object *) 513;
  r2146[2] = r2147;
  O r2151 = ((object*) lenv) [2];
  if (freep + unbox(r2151) >= limit) {
    heap_request = unbox(r2151);
    heap1[3] = r2146;
    heap1[4] = r1188;
    gc_flip (2);
    r2146 = heap0[3];
    r1188 = heap0[4];
  }
  O r2149 = ((object*) lenv) [2];
  O r2150 = (object *) 6;
  O r2148;
  if (unbox(r2149) == 0) { r2148 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r2149));
    for (int i=0; i<unbox(r2149); i++) { t[i+1] = r2150; }
    r2148 = t;
  }
  r2146[3] = r2148;
  // env 13:
  //   (char -> bool)
  O r2139 = alloc_cleared (TC_ENV, 2);
  r2139[1] = lenv; lenv = r2139;
  O r2140 = allocate (TC_CLOSURE, 2);
  r2140[1] = FUN_in_class_question_1369_i6_0; r2140[2] = lenv;
  r2139[2] = r2140;
  // env 14:
  //   ((list char) -> undefined)
  O r2118 = alloc_cleared (TC_ENV, 2);
  r2118[1] = lenv; lenv = r2118;
  O r2119 = allocate (TC_CLOSURE, 2);
  r2119[1] = FUN__24loop31_1371_i6_0; r2119[2] = lenv;
  r2118[2] = r2119;
  // env 15:
  //   (list char)
  O r2115 = allocate (TC_ENV, 2);
  O r2117 = top[26];
  r2115[2] = r2117;
  O r2116 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_11; t[4] = r1188; k = t;
  r2115[1] = r2116[2]; lenv = r2115; FUN__24loop31_1371_i6_0();
}
static void FUN_toplevel_11 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r2114 = rr;
  lenv = lenv[1];
  O r2113 = ((object*) lenv) [2];
  lenv = lenv[1];
  O r2112 = r2113;
  lenv = lenv[1];
  O r1214 = r2112;
  r1188[27] = r1214;
  O r1215 = irk_copy_tuple ((O) constructed_77[0]);
  r1188[28] = r1215;
  O r1216 = allocate (TC_CLOSURE, 2);
  r1216[1] = FUN_safe_char_354_0; r1216[2] = lenv;
  r1188[29] = r1216;
  O r1217 = allocate (TC_CLOSURE, 2);
  r1217[1] = FUN_repr_string_355_0; r1217[2] = lenv;
  r1188[30] = r1217;
  O r1218 = allocate (TC_CLOSURE, 2);
  r1218[1] = FUN_raise_system_error_519_0; r1218[2] = lenv;
  r1188[31] = r1218;
  O r1219 = (object *) 33;
  r1188[32] = r1219;
  O r1220 = (object *) 131071;
  r1188[33] = r1220;
  O r1221 = (object *) 18;
  r1188[34] = r1221;
  O r1222 = irk_copy_tuple ((O) constructed_68[0]);
  r1188[35] = r1222;
  O r1223 = irk_copy_tuple ((O) constructed_57[0]);
  r1188[36] = r1223;
  O r1224 = allocate (TC_CLOSURE, 2);
  r1224[1] = FUN_print_exception_exit_587_0; r1224[2] = lenv;
  r1188[37] = r1224;
  O r1225 = allocate (TC_CLOSURE, 2);
  r1225[1] = FUN_file_2fread_char_634_0; r1225[2] = lenv;
  r1188[38] = r1225;
  O r1226 = allocate (TC_CLOSURE, 2);
  r1226[1] = FUN_string__3ehex_657_0; r1226[2] = lenv;
  r1188[39] = r1226;
  O r1227 = allocate (TC_CLOSURE, 2);
  r1227[1] = FUN_decoder_660_0; r1227[2] = lenv;
  r1188[40] = r1227;
  O r1228 = top[5];
  r1188[41] = r1228;
  O r1229 = top[4];
  r1188[42] = r1229;
  O r1272 = irk_make_argv();
  O r1277 = r1272; // reg varref
  O r1273 = TAG_INTEGER((irk_int)(r1277 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*r1277));
  O r1274 = allocate (UOTAG(2), 2);
  O r1275 = r1273; // reg varref
  r1274[1] = r1275;
  O r1276 = r1272; // reg varref
  r1274[2] = r1276;
  O r1230 = r1274; // reg varref
  r1188[43] = r1230;
  O r1231 = (object *) 1;
  r1188[44] = r1231;
  O r1232 = (O) &constructed_25;
  r1188[45] = r1232;
  // env 16:
  //   int (vector int)
  O r1266 = alloc_cleared (TC_ENV, 3);
  r1266[1] = lenv; lenv = r1266;
  O r1267 = (object *) 513;
  r1266[2] = r1267;
  O r1271 = ((object*) lenv) [2];
  if (freep + unbox(r1271) >= limit) {
    heap_request = unbox(r1271);
    heap1[3] = r1266;
    heap1[4] = r1188;
    gc_flip (2);
    r1266 = heap0[3];
    r1188 = heap0[4];
  }
  O r1269 = ((object*) lenv) [2];
  O r1270 = (object *) -1;
  O r1268;
  if (unbox(r1269) == 0) { r1268 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r1269));
    for (int i=0; i<unbox(r1269); i++) { t[i+1] = r1270; }
    r1268 = t;
  }
  r1266[3] = r1268;
  // env 17:
  //   (int int -> undefined)
  O r1240 = alloc_cleared (TC_ENV, 2);
  r1240[1] = lenv; lenv = r1240;
  O r1241 = allocate (TC_CLOSURE, 2);
  r1241[1] = FUN__24loop180_3483_0; r1241[2] = lenv;
  r1240[2] = r1241;
  // env 18:
  //   int int
  O r1236 = allocate (TC_ENV, 3);
  O r1238 = (object *) 1;
  r1236[2] = r1238;
  O r1239 = (object *) 129;
  r1236[3] = r1239;
  O r1237 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_12; t[4] = r1188; k = t;
  r1236[1] = r1237[2]; lenv = r1236; FUN__24loop180_3483_0();
}
static void FUN_toplevel_12 (O rr) {
  O r1188 = k[4]; lenv = k[2]; k = k[1];
  O r1235 = rr;
  lenv = lenv[1];
  O r1234 = ((object*) lenv) [3];
  lenv = lenv[1];
  O r1233 = r1234;
  r1188[46] = r1233;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  O r1187 = top[37];
  top[3] = r1187;
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
  // env 19:
  //   string {fd=int rbuf=string pos=int end=int } ( -> (maybe char))
  O r1062 = alloc_cleared (TC_ENV, 4);
  r1062[1] = lenv; lenv = r1062;
  O r1186 = top[43];
  O r1184 = ((irk_vector*)r1186)->val[1];
  O r1185 = (object *) 3;
  range_check (GET_TUPLE_LENGTH(*(object*)r1184), unbox(r1185));
  O r1063 = ((irk_vector*)r1184)->val[unbox(r1185)];
  r1062[2] = r1063;
  O r1064 = alloc_cleared (UOTAG(1), 4);
  O r1118 = (object *) 1;
  r1064[1] = r1118;
  // env 20:
  //   int
  O r1133 = alloc_cleared (TC_ENV, 2);
  r1133[1] = lenv; lenv = r1133;
  // env 21:
  //   string
  O r1182 = allocate (TC_ENV, 2);
  r1182[1] = lenv; lenv = r1182;
  O r1183 = ((object***) lenv) [1][1][2];
  r1182[2] = r1183;
  // env 22:
  //   char
  O r1171 = alloc_cleared (TC_ENV, 2);
  r1171[1] = lenv; lenv = r1171;
  // env 23:
  //   string int
  O r1173 = alloc_cleared (TC_ENV, 3);
  O r1175 = ((object**) lenv) [1][2];
  r1173[2] = r1175;
  O r1181 = ((object**) lenv) [1][2];
  O r1177 = TAG_INTEGER((irk_int)((irk_string*)(r1181))->len);
  O r1179 = r1177; // reg varref
  O r1180 = (object *) 3;
  O r1178 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1179)-UNTAG_INTEGER(r1180));
  O r1176 = r1178; // reg varref
  r1173[3] = r1176;
  O r1174 = top[16];
  O t = allocate (TC_SAVE, 7);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_13; t[4] = r1062; t[5] = r1064; t[6] = r1133; t[7] = r1171; k = t;
  r1173[1] = r1174[2]; lenv = r1173; FUN_string_ref_331_0();
}
static void FUN_toplevel_13 (O rr) {
  O r1062 = k[4]; O r1064 = k[5]; O r1133 = k[6]; O r1171 = k[7]; lenv = k[2]; k = k[1];
  O r1172 = rr;
  r1171[2] = r1172;
  O r1169 = ((object*) lenv) [2];
  O r1170 = (object *) 2;
  O r1168 = IRK_TEST(r1169==r1170);
  lenv = lenv[1];
  O r1142 = r1168;
  if IRK_IS_TRUE(r1142) {
    O r1143 = ((object*) lenv) [2];
    O r1141 = r1143;
    JUMP_65(r1141, r1133, r1064, r1062);
  } else {
    // env 24:
    //   int int string
    O r1158 = alloc_cleared (TC_ENV, 4);
    r1158[1] = lenv; lenv = r1158;
    O r1167 = ((object**) lenv) [1][2];
    O r1159 = TAG_INTEGER((irk_int)((irk_string*)(r1167))->len);
    r1158[2] = r1159;
    O r1165 = ((object*) lenv) [2];
    O r1166 = (object *) 3;
    O r1160 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1165)+UNTAG_INTEGER(r1166));
    r1158[3] = r1160;
    O r1164 = ((object*) lenv) [3];
    O r1163 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r1164)));
    if (freep + unbox(r1163) >= limit) {
      heap_request = unbox(r1163);
      heap1[3] = r1158;
      heap1[4] = r1133;
      heap1[5] = r1064;
      heap1[6] = r1062;
      gc_flip (4);
      r1158 = heap0[3];
      r1133 = heap0[4];
      r1064 = heap0[5];
      r1062 = heap0[6];
    }
    O r1162 = ((object*) lenv) [3];
    O r1161 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r1162))), ((irk_string*)(t))->len = UNTAG_INTEGER(r1162), t);
    r1158[4] = r1161;
    // env 25:
    //   string int int string int
    O r1151 = allocate (TC_ENV, 6);
    O r1153 = ((object**) lenv) [1][2];
    r1151[2] = r1153;
    O r1154 = (object *) 1;
    r1151[3] = r1154;
    O r1155 = ((object*) lenv) [2];
    r1151[4] = r1155;
    O r1156 = ((object*) lenv) [4];
    r1151[5] = r1156;
    O r1157 = (object *) 1;
    r1151[6] = r1157;
    O r1152 = top[12];
    O t = allocate (TC_SAVE, 6);
    t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_14; t[4] = r1062; t[5] = r1064; t[6] = r1133; k = t;
    r1151[1] = r1152[2]; lenv = r1151; FUN_buffer_copy_325_0();
  }
}
static void FUN_toplevel_14 (O rr) {
  O r1062 = k[4]; O r1064 = k[5]; O r1133 = k[6]; lenv = k[2]; k = k[1];
  // env 26:
  //   string int char
  O r1146 = allocate (TC_ENV, 4);
  O r1148 = ((object*) lenv) [4];
  r1146[2] = r1148;
  O r1149 = ((object*) lenv) [2];
  r1146[3] = r1149;
  O r1150 = (object *) 2;
  r1146[4] = r1150;
  O r1147 = top[14];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_15; t[4] = r1062; t[5] = r1064; t[6] = r1133; k = t;
  r1146[1] = r1147[2]; lenv = r1146; FUN_string_set_bang_332_0();
}
static void FUN_toplevel_15 (O rr) {
  O r1062 = k[4]; O r1064 = k[5]; O r1133 = k[6]; lenv = k[2]; k = k[1];
  O r1145 = ((object*) lenv) [4];
  lenv = lenv[1];
  O r1144 = r1145;
  O r1141 = r1144;
  JUMP_65(r1141, r1133, r1064, r1062);
}
static void JUMP_65(O r1141, O r1133, O r1064, O r1062) {
  lenv = lenv[1];
  O r1140 = r1141;
  O r1135 = make_string_foreign (r1140);
  O r1136 = top[44];
  O r1138 = (object *) 1;
  O r1139 = (object *) 1;
  O r1137 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1138)|UNTAG_INTEGER(r1139));
  O r1134 = TAG_INTEGER((irk_int)open((char*) get_foreign(r1135), UNTAG_INTEGER(r1136), UNTAG_INTEGER(r1137)));
  r1133[2] = r1134;
  O r1128 = ((object*) lenv) [2];
  O r1129 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1128)<UNTAG_INTEGER(r1129))) {
    O r1131 = top[31];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_16; t[4] = r1062; t[5] = r1064; k = t;
    lenv = r1131[2]; FUN_raise_system_error_519_0();
  } else {
    O r1132 = ((object*) lenv) [2];
    O r1127 = r1132;
    JUMP_64(r1127, r1064, r1062);
  }
}
static void FUN_toplevel_16 (O rr) {
  O r1062 = k[4]; O r1064 = k[5]; lenv = k[2]; k = k[1];
  O r1130 = rr;
  O r1127 = r1130;
  JUMP_64(r1127, r1064, r1062);
}
static void JUMP_64(O r1127, O r1064, O r1062) {
  lenv = lenv[1];
  O r1119 = r1127;
  r1064[2] = r1119;
  O r1120 = (object *) 1;
  r1064[3] = r1120;
  O r1122 = (object *) 32769;
  O r1126 = r1122; // reg varref
  O r1125 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r1126)));
  if (freep + unbox(r1125) >= limit) {
    heap_request = unbox(r1125);
    heap1[3] = r1122;
    heap1[4] = r1064;
    heap1[5] = r1062;
    gc_flip (3);
    r1122 = heap0[3];
    r1064 = heap0[4];
    r1062 = heap0[5];
  }
  O r1124 = r1122; // reg varref
  O r1123 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r1124))), ((irk_string*)(t))->len = UNTAG_INTEGER(r1124), t);
  O r1121 = r1123; // reg varref
  r1064[4] = r1121;
  r1062[3] = r1064;
  // env 27:
  //   (((maybe char) -> undefined) -> undefined)
  O r1066 = alloc_cleared (TC_ENV, 2);
  O r1068 = allocate (TC_CLOSURE, 2);
  r1068[1] = FUN_lambda_20521_i275_i289_0; r1068[2] = lenv;
  r1066[2] = r1068;
  O r1067 = top[6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_17; t[4] = r1062; k = t;
  r1066[1] = r1067[2]; lenv = r1066; FUN_make_generator_246_0();
}
static void FUN_toplevel_17 (O rr) {
  O r1062 = k[4]; lenv = k[2]; k = k[1];
  O r1065 = rr;
  r1062[4] = r1065;
  // env 28:
  //   string
  O r1050 = alloc_cleared (TC_ENV, 2);
  r1050[1] = lenv; lenv = r1050;
  // env 29:
  //   (list string)
  O r1052 = alloc_cleared (TC_ENV, 2);
  O r1055 = (O) &constructed_24;
  O r1061 = top[43];
  O r1059 = ((irk_vector*)r1061)->val[1];
  O r1060 = (object *) 3;
  range_check (GET_TUPLE_LENGTH(*(object*)r1059), unbox(r1060));
  O r1057 = ((irk_vector*)r1059)->val[unbox(r1060)];
  O r1058 = (object*) constructed_2[0];
  O r1056 = alloc_no_clear (TC_PAIR,2);
  r1056[1] = r1057;
  r1056[2] = r1058;
  O r1054 = alloc_no_clear (TC_PAIR,2);
  r1054[1] = r1055;
  r1054[2] = r1056;
  r1052[2] = r1054;
  O r1053 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_18; t[4] = r1050; k = t;
  r1052[1] = r1053[2]; lenv = r1052; FUN_string_concat_333_0();
}
static void FUN_toplevel_18 (O rr) {
  O r1050 = k[4]; lenv = k[2]; k = k[1];
  O r1051 = rr;
  r1050[2] = r1051;
  O r1047 = ((object*) lenv) [2];
  O r1049 = ((object*) lenv) [2];
  O r1048 = TAG_INTEGER((irk_int)((irk_string*)(r1049))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1047))->data, 1, UNTAG_INTEGER(r1048), stdout));
  O r1046 = (object *) 14;
  lenv = lenv[1];
  // env 30:
  //   ( -> (maybe |tuple=(product string string) |))
  O r453 = alloc_cleared (TC_ENV, 2);
  r453[1] = lenv; lenv = r453;
  // env 31:
  //   (((maybe |tuple=(product string string) |) -> undefined) -> undefined)
  O r455 = alloc_cleared (TC_ENV, 2);
  O r457 = allocate (TC_CLOSURE, 2);
  r457[1] = FUN_lambda_21756_i277_i289_0; r457[2] = lenv;
  r455[2] = r457;
  O r456 = top[6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_19; t[4] = r453; k = t;
  r455[1] = r456[2]; lenv = r455; FUN_make_generator_246_0();
}
static void FUN_toplevel_19 (O rr) {
  O r453 = k[4]; lenv = k[2]; k = k[1];
  O r454 = rr;
  r453[2] = r454;
  // env 32:
  //   ((maybe |tuple=(product string string) |) -> undefined)
  O r15 = alloc_cleared (TC_ENV, 2);
  r15[1] = lenv; lenv = r15;
  O r16 = allocate (TC_CLOSURE, 2);
  r16[1] = FUN__24genloop174_3458_i289_0; r16[2] = lenv;
  r15[2] = r16;
  // env 33:
  //   (maybe |tuple=(product string string) |)
  O r11 = alloc_cleared (TC_ENV, 2);
  O r14 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_toplevel_20; t[4] = r11; k = t;
  lenv = r14[2]; ((pfun)(r14[1]))();
}
static void FUN_toplevel_20 (O rr) {
  O r11 = k[4]; lenv = k[2]; k = k[1];
  O r13 = rr;
  r11[2] = r13;
  O r12 = ((object*) lenv) [2];
  r11[1] = r12[2]; lenv = r11; FUN__24genloop174_3458_i289_0();
}
static void FUN__24genloop174_3458_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r19 = ((object*) lenv) [2];
  switch (get_case (r19)) {
    case (UITAG(1)): { // no
      O r21 = (object *) 14;
      IRK_RETURN(21);
    } break;
    default: {
      // env 34:
      //   |tuple=(product string string) |
      O r450 = alloc_cleared (TC_ENV, 2);
      r450[1] = lenv; lenv = r450;
      O r452 = ((object**) lenv) [1][2];
      O r451 = UOBJ_GET(r452,0);
      r450[2] = r451;
      // fatbar jn=3 label=2
      O r30 = ((object*) lenv) [2];
      switch (get_case_noint (r30)) {
        case (UOTAG(4)): {
          // env 35:
          //   string string
          O r440 = alloc_cleared (TC_ENV, 3);
          r440[1] = lenv; lenv = r440;
          O r444 = ((object**) lenv) [1][2];
          O r441 = UOBJ_GET(r444,0);
          r440[2] = r441;
          O r443 = ((object**) lenv) [1][2];
          O r442 = UOBJ_GET(r443,1);
          r440[3] = r442;
          O r438 = ((object*) lenv) [2];
          O r439 = (O) &constructed_19;
          O r434 = irk_string_cmp (((irk_string*)(r438)), ((irk_string*)(r439)));
          O r436 = r434; // reg varref
          O r437 = (object *) 65558;
          O r435 = IRK_TEST(r436==r437);
          O r33 = r435; // reg varref
          if IRK_IS_TRUE(r33) {
            // env 36:
            //   {get=(int -> string) u32=( -> int) string=( -> string) left=( -> int) } string
            O r423 = alloc_cleared (TC_ENV, 3);
            r423[1] = lenv; lenv = r423;
            // env 37:
            //   string
            O r430 = allocate (TC_ENV, 2);
            O r432 = ((object**) lenv) [1][3];
            r430[2] = r432;
            O r431 = top[40];
            O t = allocate (TC_SAVE, 4);
            t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_21; t[4] = r423; k = t;
            r430[1] = r431[2]; lenv = r430; FUN_decoder_660_0();
          } else {
            lenv = ((object *)lenv)[1];
            FAIL_2();
          }
        } break;
        default: {
          FAIL_2();
        };
      }
    } break;
  }
}
static void FUN__24genloop174_3458_i289_21 (O rr) {
  O r423 = k[4]; lenv = k[2]; k = k[1];
  O r424 = rr;
  r423[2] = r424;
  // env 38:
  //   int
  O r426 = allocate (TC_ENV, 2);
  O r429 = (object *) 31;
  r426[2] = r429;
  O r428 = ((object*) lenv) [2];
  O r427 = ((irk_vector*)r428)->val[0];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_22; t[4] = r423; k = t;
  r426[1] = r427[2]; lenv = r426; ((pfun)(r427[1]))();
}
static void FUN__24genloop174_3458_i289_22 (O rr) {
  O r423 = k[4]; lenv = k[2]; k = k[1];
  O r425 = rr;
  r423[3] = r425;
  O r421 = ((object*) lenv) [3];
  O r422 = (O) &constructed_18;
  O r417 = irk_string_cmp (((irk_string*)(r421)), ((irk_string*)(r422)));
  O r419 = r417; // reg varref
  O r420 = (object *) 65558;
  O r418 = IRK_TEST(r419==r420);
  O r411 = r418; // reg varref
  if IRK_IS_TRUE(r411) {
    O r412 = (object *) 14;
    JUMP_29();
  } else {
    // env 39:
    //   |AssertionError=product ...t985|
    O r414 = alloc_cleared (TC_ENV, 2);
    O r416 = (object*)UITAG(6);
    r414[2] = r416;
    O r415 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_23; ; k = t;
    r414[1] = r415[2]; lenv = r414; ((pfun)(r415[1]))();
  }
}
static void FUN__24genloop174_3458_i289_23 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r413 = rr;
  JUMP_29();
}
static void JUMP_29() {
  // env 40:
  //   string string string int (list string) string
  O r396 = alloc_cleared (TC_ENV, 7);
  r396[1] = lenv; lenv = r396;
  O r410 = ((object**) lenv) [1][2];
  O r409 = ((irk_vector*)r410)->val[2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_24; t[4] = r396; k = t;
  lenv = r409[2]; ((pfun)(r409[1]))();
}
static void FUN__24genloop174_3458_i289_24 (O rr) {
  O r396 = k[4]; lenv = k[2]; k = k[1];
  O r397 = rr;
  r396[2] = r397;
  O r408 = ((object**) lenv) [1][2];
  O r407 = ((irk_vector*)r408)->val[2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_25; t[4] = r396; k = t;
  lenv = r407[2]; ((pfun)(r407[1]))();
}
static void FUN__24genloop174_3458_i289_25 (O rr) {
  O r396 = k[4]; lenv = k[2]; k = k[1];
  O r398 = rr;
  r396[3] = r398;
  O r406 = ((object**) lenv) [1][2];
  O r405 = ((irk_vector*)r406)->val[2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_26; t[4] = r396; k = t;
  lenv = r405[2]; ((pfun)(r405[1]))();
}
static void FUN__24genloop174_3458_i289_26 (O rr) {
  O r396 = k[4]; lenv = k[2]; k = k[1];
  O r399 = rr;
  r396[4] = r399;
  O r404 = ((object**) lenv) [1][2];
  O r403 = ((irk_vector*)r404)->val[3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_27; t[4] = r396; k = t;
  lenv = r403[2]; ((pfun)(r403[1]))();
}
static void FUN__24genloop174_3458_i289_27 (O rr) {
  O r396 = k[4]; lenv = k[2]; k = k[1];
  O r400 = rr;
  r396[5] = r400;
  O r401 = (object *) 10;
  r396[6] = r401;
  O r402 = (O) &constructed_17;
  r396[7] = r402;
  // env 41:
  //   (int int -> undefined)
  O r376 = alloc_cleared (TC_ENV, 2);
  r376[1] = lenv; lenv = r376;
  O r377 = allocate (TC_CLOSURE, 2);
  r377[1] = FUN__24loop171_3445_i279_i289_0; r377[2] = lenv;
  r376[2] = r377;
  // env 42:
  //   int int
  O r372 = allocate (TC_ENV, 3);
  O r374 = (object *) 1;
  r372[2] = r374;
  O r375 = ((object**) lenv) [1][5];
  r372[3] = r375;
  O r373 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_28; ; k = t;
  r372[1] = r373[2]; lenv = r372; FUN__24loop171_3445_i279_i289_0();
}
static void FUN__24genloop174_3458_i289_28 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r371 = rr;
  lenv = lenv[1];
  O r370 = ((object**) lenv) [1][2];
  O r369 = ((irk_vector*)r370)->val[2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_29; ; k = t;
  lenv = r369[2]; ((pfun)(r369[1]))();
}
static void FUN__24genloop174_3458_i289_29 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r368 = rr;
  ((object*) lenv) [7] = r368;
  // env 43:
  //   string
  O r338 = alloc_cleared (TC_ENV, 2);
  r338[1] = lenv; lenv = r338;
  // env 44:
  //   (list string)
  O r340 = alloc_cleared (TC_ENV, 2);
  O r343 = (O) &constructed_16;
  // env 45:
  //   string
  O r365 = allocate (TC_ENV, 2);
  O r367 = ((object**) lenv) [1][2];
  r365[2] = r367;
  O r366 = top[30];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_30; t[4] = r338; t[5] = r340; t[6] = r343; k = t;
  r365[1] = r366[2]; lenv = r365; FUN_repr_string_355_0();
}
static void FUN__24genloop174_3458_i289_30 (O rr) {
  O r338 = k[4]; O r340 = k[5]; O r343 = k[6]; lenv = k[2]; k = k[1];
  O r345 = rr;
  O r347 = (O) &constructed_1;
  O r349 = (O) &constructed_15;
  // env 46:
  //   string
  O r362 = allocate (TC_ENV, 2);
  O r364 = ((object**) lenv) [1][3];
  r362[2] = r364;
  O r363 = top[30];
  O t = allocate (TC_SAVE, 9);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_31; t[4] = r338; t[5] = r340; t[6] = r343; t[7] = r345; t[8] = r347; t[9] = r349; k = t;
  r362[1] = r363[2]; lenv = r362; FUN_repr_string_355_0();
}
static void FUN__24genloop174_3458_i289_31 (O rr) {
  O r338 = k[4]; O r340 = k[5]; O r343 = k[6]; O r345 = k[7]; O r347 = k[8]; O r349 = k[9]; lenv = k[2]; k = k[1];
  O r351 = rr;
  O r353 = (O) &constructed_1;
  O r355 = (O) &constructed_14;
  // env 47:
  //   string
  O r359 = allocate (TC_ENV, 2);
  O r361 = ((object**) lenv) [1][4];
  r359[2] = r361;
  O r360 = top[30];
  O t = allocate (TC_SAVE, 12);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_32; t[4] = r338; t[5] = r340; t[6] = r343; t[7] = r345; t[8] = r347; t[9] = r349; t[10] = r351; t[11] = r353; t[12] = r355; k = t;
  r359[1] = r360[2]; lenv = r359; FUN_repr_string_355_0();
}
static void FUN__24genloop174_3458_i289_32 (O rr) {
  O r338 = k[4]; O r340 = k[5]; O r343 = k[6]; O r345 = k[7]; O r347 = k[8]; O r349 = k[9]; O r351 = k[10]; O r353 = k[11]; O r355 = k[12]; lenv = k[2]; k = k[1];
  O r357 = rr;
  O r358 = (object*) constructed_2[0];
  O r356 = alloc_no_clear (TC_PAIR,2);
  r356[1] = r357;
  r356[2] = r358;
  O r354 = alloc_no_clear (TC_PAIR,2);
  r354[1] = r355;
  r354[2] = r356;
  O r352 = alloc_no_clear (TC_PAIR,2);
  r352[1] = r353;
  r352[2] = r354;
  O r350 = alloc_no_clear (TC_PAIR,2);
  r350[1] = r351;
  r350[2] = r352;
  O r348 = alloc_no_clear (TC_PAIR,2);
  r348[1] = r349;
  r348[2] = r350;
  O r346 = alloc_no_clear (TC_PAIR,2);
  r346[1] = r347;
  r346[2] = r348;
  O r344 = alloc_no_clear (TC_PAIR,2);
  r344[1] = r345;
  r344[2] = r346;
  O r342 = alloc_no_clear (TC_PAIR,2);
  r342[1] = r343;
  r342[2] = r344;
  r340[2] = r342;
  O r341 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_33; t[4] = r338; k = t;
  r340[1] = r341[2]; lenv = r340; FUN_string_concat_333_0();
}
static void FUN__24genloop174_3458_i289_33 (O rr) {
  O r338 = k[4]; lenv = k[2]; k = k[1];
  O r339 = rr;
  r338[2] = r339;
  O r335 = ((object*) lenv) [2];
  O r337 = ((object*) lenv) [2];
  O r336 = TAG_INTEGER((irk_int)((irk_string*)(r337))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r335))->data, 1, UNTAG_INTEGER(r336), stdout));
  O r334 = (object *) 14;
  lenv = lenv[1];
  // env 48:
  //   string
  O r264 = alloc_cleared (TC_ENV, 2);
  r264[1] = lenv; lenv = r264;
  // env 49:
  //   (list string)
  O r266 = alloc_cleared (TC_ENV, 2);
  O r269 = (O) &constructed_13;
  // env 50:
  //   int
  O r332 = allocate (TC_ENV, 2);
  r332[1] = lenv; lenv = r332;
  O r333 = ((object***) lenv) [1][1][5];
  r332[2] = r333;
  O r274 = (object *) 1;
  O r275 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r274)==UNTAG_INTEGER(r275))) {
    // env 51:
    //   char
    O r277 = allocate (TC_ENV, 2);
    O r279 = (object *) 3145730;
    r277[2] = r279;
    O r278 = top[15];
    O t = allocate (TC_SAVE, 6);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_34; t[4] = r264; t[5] = r266; t[6] = r269; k = t;
    r277[1] = r278[2]; lenv = r277; FUN_char__3estring_328_0();
  } else {
    // env 52:
    //   (int (list char) -> string)
    O r294 = alloc_cleared (TC_ENV, 2);
    r294[1] = lenv; lenv = r294;
    O r295 = allocate (TC_CLOSURE, 2);
    r295[1] = FUN_loop_1407_i4_0; r295[2] = lenv;
    r294[2] = r295;
    // env 53:
    //   int (list char)
    O r282 = alloc_cleared (TC_ENV, 3);
    O r286 = ((object**) lenv) [1][2];
    O r288 = r286; // reg varref
    O r289 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r288)<UNTAG_INTEGER(r289))) {
      O r291 = (object *) 1;
      O r292 = r286; // reg varref
      O r290 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r291)-UNTAG_INTEGER(r292));
      O r287 = r290;
      JUMP_22(r287, r282, r269, r266, r264);
    } else {
      O r293 = r286; // reg varref
      O r287 = r293;
      JUMP_22(r287, r282, r269, r266, r264);
    }
  }
}
static void JUMP_22(O r287, O r282, O r269, O r266, O r264) {
  O r284 = r287; // reg varref
  r282[2] = r284;
  O r285 = (object *) 10;
  r282[3] = r285;
  O r283 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop174_3458_i289_35; t[4] = r264; t[5] = r266; t[6] = r269; k = t;
  r282[1] = r283[2]; lenv = r282; FUN_loop_1407_i4_0();
}
static void FUN__24genloop174_3458_i289_35 (O rr) {
  O r264 = k[4]; O r266 = k[5]; O r269 = k[6]; lenv = k[2]; k = k[1];
  O r281 = rr;
  lenv = lenv[1];
  O r280 = r281;
  O r273 = r280;
  JUMP_20(r273, r269, r266, r264);
}
static void FUN_loop_1407_i4_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r298 = (object *) 1;
  O r299 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r298)==UNTAG_INTEGER(r299))) {
    // env 54:
    //   (list char)
    O r302 = alloc_cleared (TC_ENV, 2);
    O r305 = ((object***) lenv) [1][1][2];
    O r306 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r305)<UNTAG_INTEGER(r306))) {
      O r308 = (object *) 2949122;
      O r309 = ((object*) lenv) [3];
      O r307 = alloc_no_clear (TC_PAIR,2);
      r307[1] = r308;
      r307[2] = r309;
      O r304 = r307;
      JUMP_26(r304, r302);
    } else {
      O r310 = ((object*) lenv) [3];
      O r304 = r310;
      JUMP_26(r304, r302);
    }
  } else {
    // env 55:
    //   int int
    O r328 = allocate (TC_ENV, 3);
    O r330 = ((object*) lenv) [2];
    r328[2] = r330;
    O r331 = (object *) 21;
    r328[3] = r331;
    O r329 = top[41];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_36; ; k = t;
    r328[1] = r329[2]; lenv = r328; ((pfun)(r329[1]))();
  }
}
static void FUN_loop_1407_i4_36 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r313 = rr;
  // env 56:
  //   int int
  O r319 = alloc_cleared (TC_ENV, 3);
  r319[1] = lenv; lenv = r319;
  // env 57:
  //   int int
  O r324 = allocate (TC_ENV, 3);
  O r326 = ((object**) lenv) [1][2];
  r324[2] = r326;
  O r327 = (object *) 21;
  r324[3] = r327;
  O r325 = top[42];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_37; t[4] = r313; t[5] = r319; k = t;
  r324[1] = r325[2]; lenv = r324; ((pfun)(r325[1]))();
}
static void FUN_loop_1407_i4_37 (O rr) {
  O r313 = k[4]; O r319 = k[5]; lenv = k[2]; k = k[1];
  O r320 = rr;
  r319[2] = r320;
  O r322 = (object *) 97;
  O r323 = ((object*) lenv) [2];
  O r321 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r322)+UNTAG_INTEGER(r323));
  r319[3] = r321;
  O r318 = ((object*) lenv) [3];
  O r317 = TO_CHAR(UNTAG_INTEGER(r318));
  lenv = lenv[1];
  O r315 = r317;
  O r316 = ((object*) lenv) [3];
  O r314 = alloc_no_clear (TC_PAIR,2);
  r314[1] = r315;
  r314[2] = r316;
  lenv[2] = r313;
  lenv[3] = r314;
  FUN_loop_1407_i4_0();
}
static void JUMP_26(O r304, O r302) {
  r302[2] = r304;
  O r303 = top[20];
  r302[1] = r303[2]; lenv = r302; FUN_list__3estring_347_0();
}
static void FUN__24genloop174_3458_i289_34 (O rr) {
  O r264 = k[4]; O r266 = k[5]; O r269 = k[6]; lenv = k[2]; k = k[1];
  O r276 = rr;
  O r273 = r276;
  JUMP_20(r273, r269, r266, r264);
}
static void JUMP_20(O r273, O r269, O r266, O r264) {
  lenv = lenv[1];
  O r271 = r273;
  O r272 = (object*) constructed_12[0];
  O r270 = alloc_no_clear (TC_PAIR,2);
  r270[1] = r271;
  r270[2] = r272;
  O r268 = alloc_no_clear (TC_PAIR,2);
  r268[1] = r269;
  r268[2] = r270;
  r266[2] = r268;
  O r267 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_38; t[4] = r264; k = t;
  r266[1] = r267[2]; lenv = r266; FUN_string_concat_333_0();
}
static void FUN_loop_1407_i4_38 (O rr) {
  O r264 = k[4]; lenv = k[2]; k = k[1];
  O r265 = rr;
  r264[2] = r265;
  O r261 = ((object*) lenv) [2];
  O r263 = ((object*) lenv) [2];
  O r262 = TAG_INTEGER((irk_int)((irk_string*)(r263))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r261))->data, 1, UNTAG_INTEGER(r262), stdout));
  O r260 = (object *) 14;
  lenv = lenv[1];
  // env 58:
  //   ((list string) -> undefined)
  O r190 = alloc_cleared (TC_ENV, 2);
  r190[1] = lenv; lenv = r190;
  O r191 = allocate (TC_CLOSURE, 2);
  r191[1] = FUN__24loop168_3448_i279_i289_0; r191[2] = lenv;
  r190[2] = r191;
  // env 59:
  //   (list string)
  O r180 = alloc_cleared (TC_ENV, 2);
  // env 60:
  //   (list string)
  O r188 = allocate (TC_ENV, 2);
  r188[1] = lenv; lenv = r188;
  O r189 = ((object***) lenv) [1][1][6];
  r188[2] = r189;
  // env 61:
  //   (list string) (list string)
  O r184 = allocate (TC_ENV, 3);
  O r186 = ((object*) lenv) [2];
  r184[2] = r186;
  O r187 = (object *) 10;
  r184[3] = r187;
  O r185 = top[7];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_39; t[4] = r180; k = t;
  r184[1] = r185[2]; lenv = r184; FUN_reverse_onto_264_0();
}
static void FUN_loop_1407_i4_39 (O rr) {
  O r180 = k[4]; lenv = k[2]; k = k[1];
  O r183 = rr;
  lenv = lenv[1];
  O r182 = r183;
  r180[2] = r182;
  O r181 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_40; ; k = t;
  r180[1] = r181[2]; lenv = r180; FUN__24loop168_3448_i279_i289_0();
}
static void FUN_loop_1407_i4_40 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r179 = rr;
  lenv = lenv[1];
  O r174 = (O) &constructed_0;
  O r176 = r174; // reg varref
  O r178 = r174; // reg varref
  O r177 = TAG_INTEGER((irk_int)((irk_string*)(r178))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r176))->data, 1, UNTAG_INTEGER(r177), stdout));
  O r175 = (object *) 14;
  O r169 = (O) &constructed_8;
  O r171 = r169; // reg varref
  O r173 = r169; // reg varref
  O r172 = TAG_INTEGER((irk_int)((irk_string*)(r173))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r171))->data, 1, UNTAG_INTEGER(r172), stdout));
  O r170 = (object *) 14;
  O r167 = ((object*) lenv) [2];
  O r168 = (O) &constructed_7;
  O r163 = irk_string_cmp (((irk_string*)(r167)), ((irk_string*)(r168)));
  O r165 = r163; // reg varref
  O r166 = (object *) 65558;
  O r164 = IRK_TEST(r165==r166);
  O r42 = r164; // reg varref
  if IRK_IS_TRUE(r42) {
    // env 62:
    //   string {get=(int -> string) u32=( -> int) string=( -> string) left=( -> int) } int int (list |tuple=(product string string) |)
    O r144 = alloc_cleared (TC_ENV, 6);
    r144[1] = lenv; lenv = r144;
    O r145 = ((object**) lenv) [1][7];
    r144[2] = r145;
    // env 63:
    //   string
    O r154 = allocate (TC_ENV, 2);
    O r156 = ((object*) lenv) [2];
    r154[2] = r156;
    O r155 = top[40];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_41; t[4] = r144; k = t;
    r154[1] = r155[2]; lenv = r154; FUN_decoder_660_0();
  } else {
    O r158 = (O) &constructed_6;
    O r160 = r158; // reg varref
    O r162 = r158; // reg varref
    O r161 = TAG_INTEGER((irk_int)((irk_string*)(r162))->len);
    TAG_INTEGER((irk_int)fwrite (((irk_string*)(r160))->data, 1, UNTAG_INTEGER(r161), stdout));
    O r159 = (object *) 14;
    O r157 = r159; // reg varref
    JUMP_6();
  }
}
static void FUN_loop_1407_i4_41 (O rr) {
  O r144 = k[4]; lenv = k[2]; k = k[1];
  O r146 = rr;
  r144[3] = r146;
  O r153 = ((object*) lenv) [3];
  O r152 = ((irk_vector*)r153)->val[3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_42; t[4] = r144; k = t;
  lenv = r152[2]; ((pfun)(r152[1]))();
}
static void FUN_loop_1407_i4_42 (O rr) {
  O r144 = k[4]; lenv = k[2]; k = k[1];
  O r147 = rr;
  r144[4] = r147;
  O r151 = ((object*) lenv) [3];
  O r150 = ((irk_vector*)r151)->val[3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_43; t[4] = r144; k = t;
  lenv = r150[2]; ((pfun)(r150[1]))();
}
static void FUN_loop_1407_i4_43 (O rr) {
  O r144 = k[4]; lenv = k[2]; k = k[1];
  O r148 = rr;
  r144[5] = r148;
  O r149 = (object *) 10;
  r144[6] = r149;
  // env 64:
  //   (int int -> undefined)
  O r101 = alloc_cleared (TC_ENV, 2);
  r101[1] = lenv; lenv = r101;
  O r102 = allocate (TC_CLOSURE, 2);
  r102[1] = FUN__24loop166_3426_i271_i279_i289_0; r102[2] = lenv;
  r101[2] = r102;
  // env 65:
  //   int int
  O r97 = allocate (TC_ENV, 3);
  O r99 = (object *) 1;
  r97[2] = r99;
  O r100 = ((object***) lenv) [1][1][5];
  r97[3] = r100;
  O r98 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_44; ; k = t;
  r97[1] = r98[2]; lenv = r97; FUN__24loop166_3426_i271_i279_i289_0();
}
static void FUN_loop_1407_i4_44 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r96 = rr;
  lenv = lenv[1];
  // env 66:
  //   ((list |tuple=(product string string) |) -> undefined)
  O r49 = alloc_cleared (TC_ENV, 2);
  r49[1] = lenv; lenv = r49;
  O r50 = allocate (TC_CLOSURE, 2);
  r50[1] = FUN__24loop163_3430_i271_i279_i289_0; r50[2] = lenv;
  r49[2] = r50;
  // env 67:
  //   (list |tuple=(product string string) |)
  O r46 = allocate (TC_ENV, 2);
  O r48 = ((object**) lenv) [1][6];
  r46[2] = r48;
  O r47 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1407_i4_45; ; k = t;
  r46[1] = r47[2]; lenv = r46; FUN__24loop163_3430_i271_i279_i289_0();
}
static void FUN_loop_1407_i4_45 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r45 = rr;
  lenv = lenv[1];
  O r44 = r45;
  lenv = lenv[1];
  O r43 = r44;
  JUMP_6();
}
static void FUN__24loop163_3430_i271_i279_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r53 = ((object*) lenv) [2];
  switch (get_case (r53)) {
    case (TC_NIL): { // nil
      O r55 = (object *) 14;
      IRK_RETURN(55);
    } break;
    default: {
      // env 68:
      //   |tuple=(product string string) | (list |tuple=(product string string) |)
      O r91 = alloc_cleared (TC_ENV, 3);
      r91[1] = lenv; lenv = r91;
      O r95 = ((object**) lenv) [1][2];
      O r92 = UOBJ_GET(r95,0);
      r91[2] = r92;
      O r94 = ((object**) lenv) [1][2];
      O r93 = UOBJ_GET(r94,1);
      r91[3] = r93;
      // env 69:
      //   string string string
      O r66 = alloc_cleared (TC_ENV, 4);
      r66[1] = lenv; lenv = r66;
      O r90 = ((object**) lenv) [1][2];
      O r67 = UOBJ_GET(r90,0);
      r66[2] = r67;
      O r89 = ((object**) lenv) [1][2];
      O r68 = UOBJ_GET(r89,1);
      r66[3] = r68;
      // env 70:
      //   (list string)
      O r70 = alloc_cleared (TC_ENV, 2);
      O r73 = (O) &constructed_4;
      // env 71:
      //   string
      O r86 = allocate (TC_ENV, 2);
      O r88 = ((object*) lenv) [2];
      r86[2] = r88;
      O r87 = top[39];
      O t = allocate (TC_SAVE, 6);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop163_3430_i271_i279_i289_46; t[4] = r66; t[5] = r70; t[6] = r73; k = t;
      r86[1] = r87[2]; lenv = r86; FUN_string__3ehex_657_0();
    } break;
  }
}
static void FUN__24loop163_3430_i271_i279_i289_46 (O rr) {
  O r66 = k[4]; O r70 = k[5]; O r73 = k[6]; lenv = k[2]; k = k[1];
  O r75 = rr;
  O r77 = (O) &constructed_1;
  O r79 = (O) &constructed_3;
  // env 72:
  //   string
  O r83 = allocate (TC_ENV, 2);
  O r85 = ((object*) lenv) [3];
  r83[2] = r85;
  O r84 = top[39];
  O t = allocate (TC_SAVE, 9);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop163_3430_i271_i279_i289_47; t[4] = r66; t[5] = r70; t[6] = r73; t[7] = r75; t[8] = r77; t[9] = r79; k = t;
  r83[1] = r84[2]; lenv = r83; FUN_string__3ehex_657_0();
}
static void FUN__24loop163_3430_i271_i279_i289_47 (O rr) {
  O r66 = k[4]; O r70 = k[5]; O r73 = k[6]; O r75 = k[7]; O r77 = k[8]; O r79 = k[9]; lenv = k[2]; k = k[1];
  O r81 = rr;
  O r82 = (object*) constructed_2[0];
  O r80 = alloc_no_clear (TC_PAIR,2);
  r80[1] = r81;
  r80[2] = r82;
  O r78 = alloc_no_clear (TC_PAIR,2);
  r78[1] = r79;
  r78[2] = r80;
  O r76 = alloc_no_clear (TC_PAIR,2);
  r76[1] = r77;
  r76[2] = r78;
  O r74 = alloc_no_clear (TC_PAIR,2);
  r74[1] = r75;
  r74[2] = r76;
  O r72 = alloc_no_clear (TC_PAIR,2);
  r72[1] = r73;
  r72[2] = r74;
  r70[2] = r72;
  O r71 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop163_3430_i271_i279_i289_48; t[4] = r66; k = t;
  r70[1] = r71[2]; lenv = r70; FUN_string_concat_333_0();
}
static void FUN__24loop163_3430_i271_i279_i289_48 (O rr) {
  O r66 = k[4]; lenv = k[2]; k = k[1];
  O r69 = rr;
  r66[4] = r69;
  O r63 = ((object*) lenv) [4];
  O r65 = ((object*) lenv) [4];
  O r64 = TAG_INTEGER((irk_int)((irk_string*)(r65))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r63))->data, 1, UNTAG_INTEGER(r64), stdout));
  O r62 = (object *) 14;
  lenv = lenv[1];
  O r61 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r61;
  FUN__24loop163_3430_i271_i279_i289_0();
}
static void FUN__24loop166_3426_i271_i279_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r105 = ((object*) lenv) [2];
  O r106 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r105)==UNTAG_INTEGER(r106))) {
    O r108 = (object *) 14;
    IRK_RETURN(108);
  } else {
    // env 73:
    //   string
    O r140 = alloc_cleared (TC_ENV, 2);
    r140[1] = lenv; lenv = r140;
    O r143 = ((object****) lenv) [1][1][1][3];
    O r142 = ((irk_vector*)r143)->val[2];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop166_3426_i271_i279_i289_49; t[4] = r140; k = t;
    lenv = r142[2]; ((pfun)(r142[1]))();
  }
}
static void FUN__24loop166_3426_i271_i279_i289_49 (O rr) {
  O r140 = k[4]; lenv = k[2]; k = k[1];
  O r141 = rr;
  r140[2] = r141;
  // fatbar jn=12 label=11
  O r134 = ((object*) lenv) [2];
  O r135 = (O) &constructed_5;
  O r130 = irk_string_cmp (((irk_string*)(r134)), ((irk_string*)(r135)));
  O r132 = r130; // reg varref
  O r133 = (object *) 65558;
  O r131 = IRK_TEST(r132==r133);
  O r118 = r131; // reg varref
  if IRK_IS_TRUE(r118) {
    O r121 = alloc_cleared (UOTAG(4), 2);
    O r128 = ((object****) lenv) [1][1][1][3];
    O r127 = ((irk_vector*)r128)->val[2];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop166_3426_i271_i279_i289_50; t[4] = r121; k = t;
    lenv = r127[2]; ((pfun)(r127[1]))();
  } else {
    FAIL_11();
  }
}
static void FUN__24loop166_3426_i271_i279_i289_50 (O rr) {
  O r121 = k[4]; lenv = k[2]; k = k[1];
  O r123 = rr;
  r121[1] = r123;
  O r126 = ((object****) lenv) [1][1][1][3];
  O r125 = ((irk_vector*)r126)->val[2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop166_3426_i271_i279_i289_51; t[4] = r121; k = t;
  lenv = r125[2]; ((pfun)(r125[1]))();
}
static void FUN__24loop166_3426_i271_i279_i289_51 (O rr) {
  O r121 = k[4]; lenv = k[2]; k = k[1];
  O r124 = rr;
  r121[2] = r124;
  O r122 = ((object****) lenv) [1][1][1][6];
  O r120 = alloc_no_clear (TC_PAIR,2);
  r120[1] = r121;
  r120[2] = r122;
  ((object****) lenv) [1][1][1][6] = r120;
  O r119 = (object *) TC_UNDEFINED;
  O r117 = r119;
  JUMP_13(r117);
}
static void JUMP_13(O r117) {
  O r116 = r117;
  JUMP_12(r116);
}
static void JUMP_12(O r116) {
  lenv = lenv[1];
  O r114 = ((object*) lenv) [2];
  O r115 = (object *) 3;
  O r112 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r114)+UNTAG_INTEGER(r115));
  O r113 = ((object*) lenv) [3];
  lenv[2] = r112;
  lenv[3] = r113;
  FUN__24loop166_3426_i271_i279_i289_0();
}
static void FAIL_11() {
  // env 74:
  //   |Readkey/UnknownKeyType=string ...t1160|
  O r136 = alloc_cleared (TC_ENV, 2);
  O r138 = allocate (UOTAG(7), 1);
  O r139 = ((object*) lenv) [2];
  r138[1] = r139;
  r136[2] = r138;
  O r137 = top[3];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop166_3426_i271_i279_i289_52; ; k = t;
  r136[1] = r137[2]; lenv = r136; ((pfun)(r137[1]))();
}
static void FUN__24loop166_3426_i271_i279_i289_52 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r117 = rr;
  O r116 = r117;
  JUMP_12(r116);
}
static void JUMP_6() {
  O r37 = (O) &constructed_0;
  O r39 = r37; // reg varref
  O r41 = r37; // reg varref
  O r40 = TAG_INTEGER((irk_int)((irk_string*)(r41))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r39))->data, 1, UNTAG_INTEGER(r40), stdout));
  O r38 = (object *) 14;
  O r36 = r38; // reg varref
  lenv = lenv[1];
  O r35 = r36;
  lenv = lenv[1];
  O r34 = r35;
  O r32 = r34;
  JUMP_5(r32);
}
static void FUN__24loop168_3448_i279_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r194 = ((object*) lenv) [2];
  switch (get_case (r194)) {
    case (TC_NIL): { // nil
      O r196 = (object *) 14;
      IRK_RETURN(196);
    } break;
    default: {
      // env 75:
      //   string (list string)
      O r255 = alloc_cleared (TC_ENV, 3);
      r255[1] = lenv; lenv = r255;
      O r259 = ((object**) lenv) [1][2];
      O r256 = UOBJ_GET(r259,0);
      r255[2] = r256;
      O r258 = ((object**) lenv) [1][2];
      O r257 = UOBJ_GET(r258,1);
      r255[3] = r257;
      // env 76:
      //   {get=(int -> string) u32=( -> int) string=( -> string) left=( -> int) } string |tuple=(product string string) |
      O r228 = alloc_cleared (TC_ENV, 4);
      r228[1] = lenv; lenv = r228;
      // env 77:
      //   string
      O r252 = allocate (TC_ENV, 2);
      O r254 = ((object**) lenv) [1][2];
      r252[2] = r254;
      O r253 = top[40];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_53; t[4] = r228; k = t;
      r252[1] = r253[2]; lenv = r252; FUN_decoder_660_0();
    } break;
  }
}
static void FUN__24loop168_3448_i279_i289_53 (O rr) {
  O r228 = k[4]; lenv = k[2]; k = k[1];
  O r229 = rr;
  r228[2] = r229;
  O r251 = ((object*) lenv) [2];
  O r250 = ((irk_vector*)r251)->val[2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_54; t[4] = r228; k = t;
  lenv = r250[2]; ((pfun)(r250[1]))();
}
static void FUN__24loop168_3448_i279_i289_54 (O rr) {
  O r228 = k[4]; lenv = k[2]; k = k[1];
  O r230 = rr;
  r228[3] = r230;
  // fatbar jn=17 label=16
  O r244 = ((object*) lenv) [3];
  O r245 = (O) &constructed_5;
  O r240 = irk_string_cmp (((irk_string*)(r244)), ((irk_string*)(r245)));
  O r242 = r240; // reg varref
  O r243 = (object *) 65558;
  O r241 = IRK_TEST(r242==r243);
  O r233 = r241; // reg varref
  if IRK_IS_TRUE(r233) {
    O r234 = alloc_cleared (UOTAG(4), 2);
    O r235 = ((object*) lenv) [3];
    r234[1] = r235;
    O r238 = ((object*) lenv) [2];
    O r237 = ((irk_vector*)r238)->val[2];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_55; t[4] = r228; t[5] = r234; k = t;
    lenv = r237[2]; ((pfun)(r237[1]))();
  } else {
    FAIL_16(r228);
  }
}
static void FUN__24loop168_3448_i279_i289_55 (O rr) {
  O r228 = k[4]; O r234 = k[5]; lenv = k[2]; k = k[1];
  O r236 = rr;
  r234[2] = r236;
  O r232 = r234;
  JUMP_18(r232, r228);
}
static void JUMP_18(O r232, O r228) {
  O r231 = r232;
  JUMP_17(r231, r228);
}
static void JUMP_17(O r231, O r228) {
  r228[4] = r231;
  // env 78:
  //   string string string
  O r208 = alloc_cleared (TC_ENV, 4);
  r208[1] = lenv; lenv = r208;
  O r227 = ((object**) lenv) [1][4];
  O r209 = UOBJ_GET(r227,0);
  r208[2] = r209;
  O r226 = ((object**) lenv) [1][4];
  O r210 = UOBJ_GET(r226,1);
  r208[3] = r210;
  // env 79:
  //   (list string)
  O r212 = alloc_cleared (TC_ENV, 2);
  O r215 = (O) &constructed_10;
  O r217 = ((object*) lenv) [2];
  O r219 = (O) &constructed_9;
  // env 80:
  //   string
  O r223 = allocate (TC_ENV, 2);
  O r225 = ((object*) lenv) [3];
  r223[2] = r225;
  O r224 = top[39];
  O t = allocate (TC_SAVE, 8);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_56; t[4] = r208; t[5] = r212; t[6] = r215; t[7] = r217; t[8] = r219; k = t;
  r223[1] = r224[2]; lenv = r223; FUN_string__3ehex_657_0();
}
static void FUN__24loop168_3448_i279_i289_56 (O rr) {
  O r208 = k[4]; O r212 = k[5]; O r215 = k[6]; O r217 = k[7]; O r219 = k[8]; lenv = k[2]; k = k[1];
  O r221 = rr;
  O r222 = (object*) constructed_2[0];
  O r220 = alloc_no_clear (TC_PAIR,2);
  r220[1] = r221;
  r220[2] = r222;
  O r218 = alloc_no_clear (TC_PAIR,2);
  r218[1] = r219;
  r218[2] = r220;
  O r216 = alloc_no_clear (TC_PAIR,2);
  r216[1] = r217;
  r216[2] = r218;
  O r214 = alloc_no_clear (TC_PAIR,2);
  r214[1] = r215;
  r214[2] = r216;
  r212[2] = r214;
  O r213 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_57; t[4] = r208; k = t;
  r212[1] = r213[2]; lenv = r212; FUN_string_concat_333_0();
}
static void FUN__24loop168_3448_i279_i289_57 (O rr) {
  O r208 = k[4]; lenv = k[2]; k = k[1];
  O r211 = rr;
  r208[4] = r211;
  O r205 = ((object*) lenv) [4];
  O r207 = ((object*) lenv) [4];
  O r206 = TAG_INTEGER((irk_int)((irk_string*)(r207))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r205))->data, 1, UNTAG_INTEGER(r206), stdout));
  O r204 = (object *) 14;
  lenv = lenv[1];
  O r203 = r204;
  lenv = lenv[1];
  O r202 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r202;
  FUN__24loop168_3448_i279_i289_0();
}
static void FAIL_16(O r228) {
  // env 81:
  //   |Readkey/UnknownKeyType=string ...t1085|
  O r246 = alloc_cleared (TC_ENV, 2);
  O r248 = allocate (UOTAG(7), 1);
  O r249 = ((object*) lenv) [3];
  r248[1] = r249;
  r246[2] = r248;
  O r247 = top[3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop168_3448_i279_i289_58; t[4] = r228; k = t;
  r246[1] = r247[2]; lenv = r246; ((pfun)(r247[1]))();
}
static void FUN__24loop168_3448_i279_i289_58 (O rr) {
  O r228 = k[4]; lenv = k[2]; k = k[1];
  O r232 = rr;
  O r231 = r232;
  JUMP_17(r231, r228);
}
static void FUN__24loop171_3445_i279_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r380 = ((object*) lenv) [2];
  O r381 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r380)==UNTAG_INTEGER(r381))) {
    O r383 = (object *) 14;
    IRK_RETURN(383);
  } else {
    O r395 = ((object****) lenv) [1][1][1][2];
    O r394 = ((irk_vector*)r395)->val[2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop171_3445_i279_i289_59; ; k = t;
    lenv = r394[2]; ((pfun)(r394[1]))();
  }
}
static void FUN__24loop171_3445_i279_i289_59 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r392 = rr;
  O r393 = ((object***) lenv) [1][1][6];
  O r391 = alloc_no_clear (TC_PAIR,2);
  r391[1] = r392;
  r391[2] = r393;
  ((object***) lenv) [1][1][6] = r391;
  O r389 = ((object*) lenv) [2];
  O r390 = (object *) 3;
  O r387 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r389)+UNTAG_INTEGER(r390));
  O r388 = ((object*) lenv) [3];
  lenv[2] = r387;
  lenv[3] = r388;
  FUN__24loop171_3445_i279_i289_0();
}
static void JUMP_5(O r32) {
  lenv = lenv[1];
  O r31 = r32;
  O r29 = r31;
  JUMP_4(r29);
}
static void JUMP_4(O r29) {
  JUMP_3();
}
static void JUMP_3() {
  O r28 = ((object****) lenv) [1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop171_3445_i279_i289_60; ; k = t;
  lenv = r28[2]; ((pfun)(r28[1]))();
}
static void FUN__24loop171_3445_i279_i289_60 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r27 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r27;
  FUN__24genloop174_3458_i289_0();
}
static void FAIL_2() {
  // env 82:
  //   |SSH/NotAKeyFile=string ...t1202|
  O r446 = alloc_cleared (TC_ENV, 2);
  O r448 = allocate (UOTAG(8), 1);
  O r449 = ((object*****) lenv) [1][1][1][1][2];
  r448[1] = r449;
  r446[2] = r448;
  O r447 = top[3];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop171_3445_i279_i289_61; ; k = t;
  r446[1] = r447[2]; lenv = r446; ((pfun)(r447[1]))();
}
static void FUN__24loop171_3445_i279_i289_61 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r29 = rr;
  JUMP_3();
}
static void FUN_lambda_21756_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 83:
  //   ( -> (maybe string)) ( -> (maybe |tuple=(product string string) |))
  O r857 = alloc_cleared (TC_ENV, 3);
  r857[1] = lenv; lenv = r857;
  // env 84:
  //   (((maybe string) -> undefined) -> undefined)
  O r978 = alloc_cleared (TC_ENV, 2);
  O r980 = allocate (TC_CLOSURE, 2);
  r980[1] = FUN_lambda_21562_i225_i277_i289_0; r980[2] = lenv;
  r978[2] = r980;
  O r979 = top[6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21756_i277_i289_62; t[4] = r857; k = t;
  r978[1] = r979[2]; lenv = r978; FUN_make_generator_246_0();
}
static void FUN_lambda_21756_i277_i289_62 (O rr) {
  O r857 = k[4]; lenv = k[2]; k = k[1];
  O r858 = rr;
  r857[2] = r858;
  // env 85:
  //   (((maybe |tuple=(product string string) |) -> undefined) -> undefined)
  O r860 = alloc_cleared (TC_ENV, 2);
  O r862 = allocate (TC_CLOSURE, 2);
  r862[1] = FUN_lambda_21680_i226_i277_i289_0; r862[2] = lenv;
  r860[2] = r862;
  O r861 = top[6];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21756_i277_i289_63; t[4] = r857; k = t;
  r860[1] = r861[2]; lenv = r860; FUN_make_generator_246_0();
}
static void FUN_lambda_21756_i277_i289_63 (O rr) {
  O r857 = k[4]; lenv = k[2]; k = k[1];
  O r859 = rr;
  r857[3] = r859;
  // env 86:
  //   ((maybe |tuple=(product string string) |) -> undefined)
  O r478 = alloc_cleared (TC_ENV, 2);
  r478[1] = lenv; lenv = r478;
  O r479 = allocate (TC_CLOSURE, 2);
  r479[1] = FUN__24genloop151_3365_i277_i289_0; r479[2] = lenv;
  r478[2] = r479;
  // env 87:
  //   (maybe |tuple=(product string string) |)
  O r474 = alloc_cleared (TC_ENV, 2);
  O r477 = ((object**) lenv) [1][3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21756_i277_i289_64; t[4] = r474; k = t;
  lenv = r477[2]; ((pfun)(r477[1]))();
}
static void FUN_lambda_21756_i277_i289_64 (O rr) {
  O r474 = k[4]; lenv = k[2]; k = k[1];
  O r476 = rr;
  r474[2] = r476;
  O r475 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21756_i277_i289_65; ; k = t;
  r474[1] = r475[2]; lenv = r474; FUN__24genloop151_3365_i277_i289_0();
}
static void FUN_lambda_21756_i277_i289_65 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r473 = rr;
  lenv = lenv[1];
  O r472 = r473;
  lenv = lenv[1];
  // env 88:
  //   ( -> t952)
  O r464 = alloc_cleared (TC_ENV, 2);
  r464[1] = lenv; lenv = r464;
  O r465 = allocate (TC_CLOSURE, 2);
  r465[1] = FUN__24loop149_3370_i277_i289_0; r465[2] = lenv;
  r464[2] = r465;
  O r463 = ((object*) lenv) [2];
  lenv = r463[2]; FUN__24loop149_3370_i277_i289_0();
}
static void FUN__24loop149_3370_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 89:
  //   (maybe |tuple=(product string string) |)
  O r469 = allocate (TC_ENV, 2);
  O r471 = (object *) 65558;
  r469[2] = r471;
  O r470 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop149_3370_i277_i289_66; ; k = t;
  r469[1] = r470[2]; lenv = r469; ((pfun)(r470[1]))();
}
static void FUN__24loop149_3370_i277_i289_66 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop149_3370_i277_i289_0();
}
static void FUN__24genloop151_3365_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r482 = ((object*) lenv) [2];
  switch (get_case (r482)) {
    case (UITAG(1)): { // no
      O r484 = (object *) 14;
      IRK_RETURN(484);
    } break;
    default: {
      // env 90:
      //   |tuple=(product string string) |
      O r854 = alloc_cleared (TC_ENV, 2);
      r854[1] = lenv; lenv = r854;
      O r856 = ((object**) lenv) [1][2];
      O r855 = UOBJ_GET(r856,0);
      r854[2] = r855;
      // env 91:
      //   string string |tuple=(product string string) |
      O r497 = alloc_cleared (TC_ENV, 4);
      r497[1] = lenv; lenv = r497;
      O r853 = ((object**) lenv) [1][2];
      O r498 = UOBJ_GET(r853,0);
      r497[2] = r498;
      O r852 = ((object**) lenv) [1][2];
      O r499 = UOBJ_GET(r852,1);
      r497[3] = r499;
      O r500 = alloc_cleared (UOTAG(4), 2);
      O r501 = ((object*) lenv) [2];
      r500[1] = r501;
      // env 92:
      //   string int int int int string int
      O r828 = alloc_cleared (TC_ENV, 8);
      r828[1] = lenv; lenv = r828;
      O r829 = ((object**) lenv) [1][3];
      r828[2] = r829;
      O r851 = ((object*) lenv) [2];
      O r830 = TAG_INTEGER((irk_int)((irk_string*)(r851))->len);
      r828[3] = r830;
      O r831 = (object *) 9;
      r828[4] = r831;
      // env 93:
      //   int int
      O r841 = alloc_cleared (TC_ENV, 3);
      O r849 = ((object*) lenv) [3];
      O r850 = ((object*) lenv) [4];
      O r845 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r849)+UNTAG_INTEGER(r850));
      O r847 = r845; // reg varref
      O r848 = (object *) 3;
      O r846 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r847)-UNTAG_INTEGER(r848));
      O r843 = r846; // reg varref
      r841[2] = r843;
      O r844 = ((object*) lenv) [4];
      r841[3] = r844;
      O r842 = top[41];
      O t = allocate (TC_SAVE, 6);
      t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_67; t[4] = r497; t[5] = r500; t[6] = r828; k = t;
      r841[1] = r842[2]; lenv = r841; ((pfun)(r842[1]))();
    } break;
  }
}
static void FUN__24genloop151_3365_i277_i289_67 (O rr) {
  O r497 = k[4]; O r500 = k[5]; O r828 = k[6]; lenv = k[2]; k = k[1];
  O r832 = rr;
  r828[5] = r832;
  O r839 = (object *) 7;
  O r840 = ((object*) lenv) [5];
  O r833 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r839)*UNTAG_INTEGER(r840));
  r828[6] = r833;
  O r838 = ((object*) lenv) [6];
  O r837 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r838)));
  if (freep + unbox(r837) >= limit) {
    heap_request = unbox(r837);
    heap1[3] = r828;
    heap1[4] = r500;
    heap1[5] = r497;
    gc_flip (3);
    r828 = heap0[3];
    r500 = heap0[4];
    r497 = heap0[5];
  }
  O r836 = ((object*) lenv) [6];
  O r834 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r836))), ((irk_string*)(t))->len = UNTAG_INTEGER(r836), t);
  r828[7] = r834;
  O r835 = (object *) 1;
  r828[8] = r835;
  // env 94:
  //   ( -> (maybe char)) int int ( -> (maybe int)) ( -> (maybe char))
  O r551 = alloc_cleared (TC_ENV, 6);
  r551[1] = lenv; lenv = r551;
  // env 95:
  //   (((maybe char) -> undefined) -> undefined)
  O r776 = alloc_cleared (TC_ENV, 2);
  O r778 = allocate (TC_CLOSURE, 2);
  r778[1] = FUN_lambda_5310_i5_i3_0; r778[2] = lenv;
  r776[2] = r778;
  O r777 = top[6];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_68; t[4] = r497; t[5] = r500; t[6] = r551; k = t;
  r776[1] = r777[2]; lenv = r776; FUN_make_generator_246_0();
}
static void FUN__24genloop151_3365_i277_i289_68 (O rr) {
  O r497 = k[4]; O r500 = k[5]; O r551 = k[6]; lenv = k[2]; k = k[1];
  O r552 = rr;
  r551[2] = r552;
  O r553 = (object *) 1;
  r551[3] = r553;
  O r554 = (object *) 1;
  r551[4] = r554;
  // env 96:
  //   (((maybe int) -> undefined) -> undefined)
  O r647 = alloc_cleared (TC_ENV, 2);
  O r649 = allocate (TC_CLOSURE, 2);
  r649[1] = FUN_lambda_21157_i207_i213_i3_0; r649[2] = lenv;
  r647[2] = r649;
  O r648 = top[6];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_69; t[4] = r497; t[5] = r500; t[6] = r551; k = t;
  r647[1] = r648[2]; lenv = r647; FUN_make_generator_246_0();
}
static void FUN__24genloop151_3365_i277_i289_69 (O rr) {
  O r497 = k[4]; O r500 = k[5]; O r551 = k[6]; lenv = k[2]; k = k[1];
  O r555 = rr;
  r551[5] = r555;
  // env 97:
  //   (((maybe char) -> undefined) -> undefined)
  O r557 = alloc_cleared (TC_ENV, 2);
  O r559 = allocate (TC_CLOSURE, 2);
  r559[1] = FUN_lambda_20878_i208_i213_i3_0; r559[2] = lenv;
  r557[2] = r559;
  O r558 = top[6];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_70; t[4] = r497; t[5] = r500; t[6] = r551; k = t;
  r557[1] = r558[2]; lenv = r557; FUN_make_generator_246_0();
}
static void FUN__24genloop151_3365_i277_i289_70 (O rr) {
  O r497 = k[4]; O r500 = k[5]; O r551 = k[6]; lenv = k[2]; k = k[1];
  O r556 = rr;
  r551[6] = r556;
  // env 98:
  //   ((maybe char) -> undefined)
  O r524 = alloc_cleared (TC_ENV, 2);
  r524[1] = lenv; lenv = r524;
  O r525 = allocate (TC_CLOSURE, 2);
  r525[1] = FUN__24genloop120_3312_i3_0; r525[2] = lenv;
  r524[2] = r525;
  // env 99:
  //   (maybe char)
  O r520 = alloc_cleared (TC_ENV, 2);
  O r523 = ((object**) lenv) [1][6];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_71; t[4] = r497; t[5] = r500; t[6] = r520; k = t;
  lenv = r523[2]; ((pfun)(r523[1]))();
}
static void FUN__24genloop151_3365_i277_i289_71 (O rr) {
  O r497 = k[4]; O r500 = k[5]; O r520 = k[6]; lenv = k[2]; k = k[1];
  O r522 = rr;
  r520[2] = r522;
  O r521 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_72; t[4] = r497; t[5] = r500; k = t;
  r520[1] = r521[2]; lenv = r520; FUN__24genloop120_3312_i3_0();
}
static void FUN__24genloop151_3365_i277_i289_72 (O rr) {
  O r497 = k[4]; O r500 = k[5]; lenv = k[2]; k = k[1];
  O r519 = rr;
  lenv = lenv[1];
  O r518 = r519;
  lenv = lenv[1];
  O r512 = ((object*) lenv) [8];
  O r517 = ((object*) lenv) [7];
  O r513 = TAG_INTEGER((irk_int)((irk_string*)(r517))->len);
  O r515 = r512; // reg varref
  O r516 = r513; // reg varref
  O r514 = IRK_TEST(UNTAG_INTEGER(r515)==UNTAG_INTEGER(r516));
  O r504 = r514; // reg varref
  if IRK_IS_TRUE(r504) {
    O r505 = ((object*) lenv) [7];
    O r503 = r505;
    JUMP_32(r503, r500, r497);
  } else {
    // env 100:
    //   string int int
    O r507 = allocate (TC_ENV, 4);
    O r509 = ((object*) lenv) [7];
    r507[2] = r509;
    O r510 = (object *) 1;
    r507[3] = r510;
    O r511 = ((object*) lenv) [8];
    r507[4] = r511;
    O r508 = top[13];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_73; t[4] = r497; t[5] = r500; k = t;
    r507[1] = r508[2]; lenv = r507; FUN_substring_327_0();
  }
}
static void FUN__24genloop151_3365_i277_i289_73 (O rr) {
  O r497 = k[4]; O r500 = k[5]; lenv = k[2]; k = k[1];
  O r506 = rr;
  O r503 = r506;
  JUMP_32(r503, r500, r497);
}
static void JUMP_32(O r503, O r500, O r497) {
  lenv = lenv[1];
  O r502 = r503;
  r500[2] = r502;
  r497[4] = r500;
  // env 101:
  //   (maybe |tuple=(product string string) |)
  O r493 = allocate (TC_ENV, 2);
  O r496 = ((object*) lenv) [4];
  O r495 = alloc_no_clear (UOTAG(0),1);
  r495[1] = r496;
  r493[2] = r495;
  O r494 = ((object******) lenv) [1][1][1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_74; ; k = t;
  r493[1] = r494[2]; lenv = r493; ((pfun)(r494[1]))();
}
static void FUN__24genloop151_3365_i277_i289_74 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r492 = rr;
  lenv = lenv[1];
  O r491 = ((object****) lenv) [1][1][1][3];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop151_3365_i277_i289_75; ; k = t;
  lenv = r491[2]; ((pfun)(r491[1]))();
}
static void FUN__24genloop151_3365_i277_i289_75 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r490 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r490;
  FUN__24genloop151_3365_i277_i289_0();
}
static void FUN__24genloop120_3312_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r528 = ((object*) lenv) [2];
  switch (get_case (r528)) {
    case (UITAG(1)): { // no
      O r530 = (object *) 14;
      IRK_RETURN(530);
    } break;
    default: {
      // env 102:
      //   char
      O r548 = alloc_cleared (TC_ENV, 2);
      r548[1] = lenv; lenv = r548;
      O r550 = ((object**) lenv) [1][2];
      O r549 = UOBJ_GET(r550,0);
      r548[2] = r549;
      // env 103:
      //   string int char
      O r543 = allocate (TC_ENV, 4);
      O r545 = ((object*****) lenv) [1][1][1][1][7];
      r543[2] = r545;
      O r546 = ((object*****) lenv) [1][1][1][1][8];
      r543[3] = r546;
      O r547 = ((object*) lenv) [2];
      r543[4] = r547;
      O r544 = top[14];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN__24genloop120_3312_i3_76; ; k = t;
      r543[1] = r544[2]; lenv = r543; FUN_string_set_bang_332_0();
    } break;
  }
}
static void FUN__24genloop120_3312_i3_76 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r539 = ((object*****) lenv) [1][1][1][1][8];
  O r541 = r539; // reg varref
  O r542 = (object *) 3;
  O r540 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r541)+UNTAG_INTEGER(r542));
  O r538 = r540; // reg varref
  ((object*****) lenv) [1][1][1][1][8] = r538;
  O r537 = ((object****) lenv) [1][1][1][6];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop120_3312_i3_77; ; k = t;
  lenv = r537[2]; ((pfun)(r537[1]))();
}
static void FUN__24genloop120_3312_i3_77 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r536 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r536;
  FUN__24genloop120_3312_i3_0();
}
static void FUN_lambda_20878_i208_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 104:
  //   ( -> (maybe int))
  O r645 = allocate (TC_ENV, 2);
  r645[1] = lenv; lenv = r645;
  O r646 = ((object***) lenv) [1][1][5];
  r645[2] = r646;
  // env 105:
  //   ((maybe int) -> undefined)
  O r580 = alloc_cleared (TC_ENV, 2);
  r580[1] = lenv; lenv = r580;
  O r581 = allocate (TC_CLOSURE, 2);
  r581[1] = FUN__24genloop101_3270_i208_i213_i3_0; r581[2] = lenv;
  r580[2] = r581;
  // env 106:
  //   (maybe int)
  O r576 = alloc_cleared (TC_ENV, 2);
  O r579 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_20878_i208_i213_i3_78; t[4] = r576; k = t;
  lenv = r579[2]; ((pfun)(r579[1]))();
}
static void FUN_lambda_20878_i208_i213_i3_78 (O rr) {
  O r576 = k[4]; lenv = k[2]; k = k[1];
  O r578 = rr;
  r576[2] = r578;
  O r577 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_20878_i208_i213_i3_79; ; k = t;
  r576[1] = r577[2]; lenv = r576; FUN__24genloop101_3270_i208_i213_i3_0();
}
static void FUN_lambda_20878_i208_i213_i3_79 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r575 = rr;
  lenv = lenv[1];
  O r574 = r575;
  lenv = lenv[1];
  // env 107:
  //   ( -> t926)
  O r566 = alloc_cleared (TC_ENV, 2);
  r566[1] = lenv; lenv = r566;
  O r567 = allocate (TC_CLOSURE, 2);
  r567[1] = FUN__24loop99_3278_i208_i213_i3_0; r567[2] = lenv;
  r566[2] = r567;
  O r565 = ((object*) lenv) [2];
  lenv = r565[2]; FUN__24loop99_3278_i208_i213_i3_0();
}
static void FUN__24loop99_3278_i208_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 108:
  //   (maybe char)
  O r571 = allocate (TC_ENV, 2);
  O r573 = (object *) 65558;
  r571[2] = r573;
  O r572 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop99_3278_i208_i213_i3_80; ; k = t;
  r571[1] = r572[2]; lenv = r571; ((pfun)(r572[1]))();
}
static void FUN__24loop99_3278_i208_i213_i3_80 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop99_3278_i208_i213_i3_0();
}
static void FUN__24genloop101_3270_i208_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r584 = ((object*) lenv) [2];
  switch (get_case (r584)) {
    case (UITAG(1)): { // no
      O r586 = (object *) 14;
      IRK_RETURN(586);
    } break;
    default: {
      // env 109:
      //   int
      O r642 = alloc_cleared (TC_ENV, 2);
      r642[1] = lenv; lenv = r642;
      O r644 = ((object**) lenv) [1][2];
      O r643 = UOBJ_GET(r644,0);
      r642[2] = r643;
      // env 110:
      //   int int
      O r635 = allocate (TC_ENV, 3);
      r635[1] = lenv; lenv = r635;
      O r640 = (object *) 7;
      O r641 = ((object**) lenv) [1][2];
      O r636 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r640)&UNTAG_INTEGER(r641));
      r635[2] = r636;
      O r638 = ((object**) lenv) [1][2];
      O r639 = (object *) 5;
      O r637 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r638)>>UNTAG_INTEGER(r639));
      r635[3] = r637;
      // env 111:
      //   (int int -> undefined)
      O r600 = alloc_cleared (TC_ENV, 2);
      r600[1] = lenv; lenv = r600;
      O r601 = allocate (TC_CLOSURE, 2);
      r601[1] = FUN__24loop103_3275_i208_i213_i3_0; r601[2] = lenv;
      r600[2] = r601;
      // env 112:
      //   int int
      O r596 = allocate (TC_ENV, 3);
      O r598 = (object *) 1;
      r596[2] = r598;
      O r599 = ((object**) lenv) [1][2];
      r596[3] = r599;
      O r597 = ((object*) lenv) [2];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN__24genloop101_3270_i208_i213_i3_81; ; k = t;
      r596[1] = r597[2]; lenv = r596; FUN__24loop103_3275_i208_i213_i3_0();
    } break;
  }
}
static void FUN__24genloop101_3270_i208_i213_i3_81 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r595 = rr;
  lenv = lenv[1];
  O r594 = r595;
  lenv = lenv[1];
  O r593 = ((object****) lenv) [1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop101_3270_i208_i213_i3_82; ; k = t;
  lenv = r593[2]; ((pfun)(r593[1]))();
}
static void FUN__24genloop101_3270_i208_i213_i3_82 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r592 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r592;
  FUN__24genloop101_3270_i208_i213_i3_0();
}
static void FUN__24loop103_3275_i208_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r604 = ((object*) lenv) [2];
  O r605 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r604)==UNTAG_INTEGER(r605))) {
    O r607 = (object *) 14;
    IRK_RETURN(607);
  } else {
    // env 113:
    //   int int int int char
    O r620 = allocate (TC_ENV, 6);
    r620[1] = lenv; lenv = r620;
    O r633 = (object *) 17;
    O r634 = ((object**) lenv) [1][2];
    O r621 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r633)*UNTAG_INTEGER(r634));
    r620[2] = r621;
    O r631 = (object *) 33;
    O r632 = ((object*) lenv) [2];
    O r622 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r631)-UNTAG_INTEGER(r632));
    r620[3] = r622;
    O r629 = ((object****) lenv) [1][1][1][3];
    O r630 = ((object*) lenv) [3];
    O r623 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r629)>>UNTAG_INTEGER(r630));
    r620[4] = r623;
    O r627 = (object *) 511;
    O r628 = ((object*) lenv) [4];
    O r624 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r627)&UNTAG_INTEGER(r628));
    r620[5] = r624;
    O r626 = ((object*) lenv) [5];
    O r625 = TO_CHAR(UNTAG_INTEGER(r626));
    r620[6] = r625;
    // env 114:
    //   (maybe char)
    O r616 = allocate (TC_ENV, 2);
    O r619 = ((object*) lenv) [6];
    O r618 = alloc_no_clear (UOTAG(0),1);
    r618[1] = r619;
    r616[2] = r618;
    O r617 = ((object*********) lenv) [1][1][1][1][1][1][1][1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop103_3275_i208_i213_i3_83; ; k = t;
    r616[1] = r617[2]; lenv = r616; ((pfun)(r617[1]))();
  }
}
static void FUN__24loop103_3275_i208_i213_i3_83 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r615 = rr;
  lenv = lenv[1];
  O r613 = ((object*) lenv) [2];
  O r614 = (object *) 3;
  O r611 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r613)+UNTAG_INTEGER(r614));
  O r612 = ((object*) lenv) [3];
  lenv[2] = r611;
  lenv[3] = r612;
  FUN__24loop103_3275_i208_i213_i3_0();
}
static void FUN_lambda_21157_i207_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 115:
  //   ( -> (maybe char))
  O r774 = allocate (TC_ENV, 2);
  r774[1] = lenv; lenv = r774;
  O r775 = ((object***) lenv) [1][1][2];
  r774[2] = r775;
  // env 116:
  //   ((maybe char) -> undefined)
  O r712 = alloc_cleared (TC_ENV, 2);
  r712[1] = lenv; lenv = r712;
  O r713 = allocate (TC_CLOSURE, 2);
  r713[1] = FUN__24genloop117_3301_i207_i213_i3_0; r713[2] = lenv;
  r712[2] = r713;
  // env 117:
  //   (maybe char)
  O r708 = alloc_cleared (TC_ENV, 2);
  O r711 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21157_i207_i213_i3_84; t[4] = r708; k = t;
  lenv = r711[2]; ((pfun)(r711[1]))();
}
static void FUN_lambda_21157_i207_i213_i3_84 (O rr) {
  O r708 = k[4]; lenv = k[2]; k = k[1];
  O r710 = rr;
  r708[2] = r710;
  O r709 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21157_i207_i213_i3_85; ; k = t;
  r708[1] = r709[2]; lenv = r708; FUN__24genloop117_3301_i207_i213_i3_0();
}
static void FUN_lambda_21157_i207_i213_i3_85 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r707 = rr;
  lenv = lenv[1];
  O r706 = r707;
  lenv = lenv[1];
  // fatbar jn=38 label=37
  O r681 = ((object**) lenv) [1][4];
  O r683 = r681; // reg varref
  O r684 = (object *) 5;
  O r682 = IRK_TEST(r683==r684);
  O r665 = r682; // reg varref
  if IRK_IS_TRUE(r665) {
    // env 118:
    //   int int int
    O r672 = allocate (TC_ENV, 4);
    r672[1] = lenv; lenv = r672;
    O r673 = ((object***) lenv) [1][1][3];
    r672[2] = r673;
    O r678 = ((object*) lenv) [2];
    O r679 = (object *) 29;
    O r674 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r678)<<UNTAG_INTEGER(r679));
    r672[3] = r674;
    O r676 = ((object*) lenv) [3];
    O r677 = (object *) 3;
    O r675 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r676)|UNTAG_INTEGER(r677));
    r672[4] = r675;
    // env 119:
    //   (maybe int)
    O r668 = allocate (TC_ENV, 2);
    O r671 = ((object*) lenv) [4];
    O r670 = alloc_no_clear (UOTAG(0),1);
    r670[1] = r671;
    r668[2] = r670;
    O r669 = ((object**) lenv) [1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21157_i207_i213_i3_86; ; k = t;
    r668[1] = r669[2]; lenv = r668; ((pfun)(r669[1]))();
  } else {
    FAIL_37();
  }
}
static void FUN_lambda_21157_i207_i213_i3_86 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r667 = rr;
  lenv = lenv[1];
  O r666 = r667;
  O r664 = r666;
  JUMP_39(r664);
}
static void JUMP_39(O r664) {
  JUMP_38();
}
static void JUMP_38() {
  // env 120:
  //   ( -> t900)
  O r656 = alloc_cleared (TC_ENV, 2);
  r656[1] = lenv; lenv = r656;
  O r657 = allocate (TC_CLOSURE, 2);
  r657[1] = FUN__24loop115_3305_i207_i213_i3_0; r657[2] = lenv;
  r656[2] = r657;
  O r655 = ((object*) lenv) [2];
  lenv = r655[2]; FUN__24loop115_3305_i207_i213_i3_0();
}
static void FUN__24loop115_3305_i207_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 121:
  //   (maybe int)
  O r661 = allocate (TC_ENV, 2);
  O r663 = (object *) 65558;
  r661[2] = r663;
  O r662 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop115_3305_i207_i213_i3_87; ; k = t;
  r661[1] = r662[2]; lenv = r661; ((pfun)(r662[1]))();
}
static void FUN__24loop115_3305_i207_i213_i3_87 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop115_3305_i207_i213_i3_0();
}
static void FAIL_37() {
  // fatbar jn=41 label=40
  O r702 = ((object**) lenv) [1][4];
  O r704 = r702; // reg varref
  O r705 = (object *) 7;
  O r703 = IRK_TEST(r704==r705);
  O r686 = r703; // reg varref
  if IRK_IS_TRUE(r686) {
    // env 122:
    //   int int int
    O r693 = allocate (TC_ENV, 4);
    r693[1] = lenv; lenv = r693;
    O r694 = ((object***) lenv) [1][1][3];
    r693[2] = r694;
    O r699 = ((object*) lenv) [2];
    O r700 = (object *) 17;
    O r695 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r699)<<UNTAG_INTEGER(r700));
    r693[3] = r695;
    O r697 = ((object*) lenv) [3];
    O r698 = (object *) 5;
    O r696 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r697)|UNTAG_INTEGER(r698));
    r693[4] = r696;
    // env 123:
    //   (maybe int)
    O r689 = allocate (TC_ENV, 2);
    O r692 = ((object*) lenv) [4];
    O r691 = alloc_no_clear (UOTAG(0),1);
    r691[1] = r692;
    r689[2] = r691;
    O r690 = ((object**) lenv) [1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop115_3305_i207_i213_i3_88; ; k = t;
    r689[1] = r690[2]; lenv = r689; ((pfun)(r690[1]))();
  } else {
    FAIL_40();
  }
}
static void FUN__24loop115_3305_i207_i213_i3_88 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r688 = rr;
  lenv = lenv[1];
  O r687 = r688;
  O r685 = r687;
  JUMP_42(r685);
}
static void JUMP_42(O r685) {
  O r664 = r685;
  JUMP_41(r664);
}
static void JUMP_41(O r664) {
  JUMP_38();
}
static void FAIL_40() {
  O r685 = (object *) 14;
  O r664 = r685;
  JUMP_41(r664);
}
static void FUN__24genloop117_3301_i207_i213_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r716 = ((object*) lenv) [2];
  switch (get_case (r716)) {
    case (UITAG(1)): { // no
      O r718 = (object *) 14;
      IRK_RETURN(718);
    } break;
    default: {
      // env 124:
      //   char
      O r771 = alloc_cleared (TC_ENV, 2);
      r771[1] = lenv; lenv = r771;
      O r773 = ((object**) lenv) [1][2];
      O r772 = UOBJ_GET(r773,0);
      r771[2] = r772;
      // env 125:
      //   int
      O r766 = alloc_cleared (TC_ENV, 2);
      r766[1] = lenv; lenv = r766;
      O r768 = top[46];
      O r770 = ((object**) lenv) [1][2];
      O r769 = TAG_INTEGER((irk_int)GET_CHAR(r770));
      range_check (GET_TUPLE_LENGTH(*(object*)r768), unbox(r769));
      O r767 = ((irk_vector*)r768)->val[unbox(r769)];
      r766[2] = r767;
      O r727 = ((object*) lenv) [2];
      O r728 = (object *) 1;
      if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r727)>=UNTAG_INTEGER(r728))) {
        O r758 = ((object*******) lenv) [1][1][1][1][1][1][3];
        O r763 = r758; // reg varref
        O r764 = (object *) 13;
        O r759 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r763)<<UNTAG_INTEGER(r764));
        O r761 = r759; // reg varref
        O r762 = ((object*) lenv) [2];
        O r760 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r761)|UNTAG_INTEGER(r762));
        O r757 = r760; // reg varref
        ((object*******) lenv) [1][1][1][1][1][1][3] = r757;
        O r753 = ((object*******) lenv) [1][1][1][1][1][1][4];
        O r755 = r753; // reg varref
        O r756 = (object *) 3;
        O r754 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r755)+UNTAG_INTEGER(r756));
        O r752 = r754; // reg varref
        ((object*******) lenv) [1][1][1][1][1][1][4] = r752;
        O r748 = ((object*******) lenv) [1][1][1][1][1][1][4];
        O r750 = r748; // reg varref
        O r751 = (object *) 9;
        O r749 = IRK_TEST(UNTAG_INTEGER(r750)==UNTAG_INTEGER(r751));
        O r730 = r749; // reg varref
        if IRK_IS_TRUE(r730) {
          // env 126:
          //   int int int
          O r739 = allocate (TC_ENV, 4);
          r739[1] = lenv; lenv = r739;
          O r740 = ((object********) lenv) [1][1][1][1][1][1][1][3];
          r739[2] = r740;
          O r745 = ((object*) lenv) [2];
          O r746 = (object *) 5;
          O r741 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r745)<<UNTAG_INTEGER(r746));
          r739[3] = r741;
          O r743 = ((object*) lenv) [3];
          O r744 = (object *) 7;
          O r742 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r743)|UNTAG_INTEGER(r744));
          r739[4] = r742;
          // env 127:
          //   (maybe int)
          O r735 = allocate (TC_ENV, 2);
          O r738 = ((object*) lenv) [4];
          O r737 = alloc_no_clear (UOTAG(0),1);
          r737[1] = r738;
          r735[2] = r737;
          O r736 = ((object*******) lenv) [1][1][1][1][1][1][2];
          O t = allocate (TC_SAVE, 3);
          t[1] = k; t[2] = lenv; t[3] = FUN__24genloop117_3301_i207_i213_i3_89; ; k = t;
          r735[1] = r736[2]; lenv = r735; ((pfun)(r736[1]))();
        } else {
          O r747 = (object *) 14;
          O r729 = r747;
          JUMP_46(r729);
        }
      } else {
        O r765 = (object *) 14;
        O r726 = r765;
        JUMP_45(r726);
      }
    } break;
  }
}
static void FUN__24genloop117_3301_i207_i213_i3_89 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r734 = rr;
  lenv = lenv[1];
  O r733 = (object *) 1;
  ((object*******) lenv) [1][1][1][1][1][1][3] = r733;
  O r732 = (object *) 1;
  ((object*******) lenv) [1][1][1][1][1][1][4] = r732;
  O r731 = (object *) TC_UNDEFINED;
  O r729 = r731;
  JUMP_46(r729);
}
static void JUMP_46(O r729) {
  O r726 = r729;
  JUMP_45(r726);
}
static void JUMP_45(O r726) {
  lenv = lenv[1];
  O r725 = ((object****) lenv) [1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop117_3301_i207_i213_i3_90; ; k = t;
  lenv = r725[2]; ((pfun)(r725[1]))();
}
static void FUN__24genloop117_3301_i207_i213_i3_90 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r724 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r724;
  FUN__24genloop117_3301_i207_i213_i3_0();
}
static void FUN_lambda_5310_i5_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 128:
  //   string
  O r826 = allocate (TC_ENV, 2);
  r826[1] = lenv; lenv = r826;
  O r827 = ((object****) lenv) [1][1][1][2];
  r826[2] = r827;
  // env 129:
  //   (int int -> undefined)
  O r800 = alloc_cleared (TC_ENV, 2);
  r800[1] = lenv; lenv = r800;
  O r801 = allocate (TC_CLOSURE, 2);
  r801[1] = FUN__24loop41_1437_i5_i3_0; r801[2] = lenv;
  r800[2] = r801;
  // env 130:
  //   int int
  O r795 = allocate (TC_ENV, 3);
  O r797 = (object *) 1;
  r795[2] = r797;
  O r799 = ((object**) lenv) [1][2];
  O r798 = TAG_INTEGER((irk_int)((irk_string*)(r799))->len);
  r795[3] = r798;
  O r796 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_5310_i5_i3_91; ; k = t;
  r795[1] = r796[2]; lenv = r795; FUN__24loop41_1437_i5_i3_0();
}
static void FUN_lambda_5310_i5_i3_91 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r794 = rr;
  lenv = lenv[1];
  O r793 = r794;
  lenv = lenv[1];
  // env 131:
  //   ( -> t868)
  O r785 = alloc_cleared (TC_ENV, 2);
  r785[1] = lenv; lenv = r785;
  O r786 = allocate (TC_CLOSURE, 2);
  r786[1] = FUN__24loop38_1441_i5_i3_0; r786[2] = lenv;
  r785[2] = r786;
  O r784 = ((object*) lenv) [2];
  lenv = r784[2]; FUN__24loop38_1441_i5_i3_0();
}
static void FUN__24loop38_1441_i5_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 132:
  //   (maybe char)
  O r790 = allocate (TC_ENV, 2);
  O r792 = (object *) 65558;
  r790[2] = r792;
  O r791 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop38_1441_i5_i3_92; ; k = t;
  r790[1] = r791[2]; lenv = r790; ((pfun)(r791[1]))();
}
static void FUN__24loop38_1441_i5_i3_92 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop38_1441_i5_i3_0();
}
static void FUN__24loop41_1437_i5_i3_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r804 = ((object*) lenv) [2];
  O r805 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r804)==UNTAG_INTEGER(r805))) {
    O r807 = (object *) 14;
    IRK_RETURN(807);
  } else {
    // env 133:
    //   char
    O r820 = alloc_cleared (TC_ENV, 2);
    r820[1] = lenv; lenv = r820;
    // env 134:
    //   string int
    O r822 = allocate (TC_ENV, 3);
    O r824 = ((object****) lenv) [1][1][1][2];
    r822[2] = r824;
    O r825 = ((object**) lenv) [1][2];
    r822[3] = r825;
    O r823 = top[16];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop41_1437_i5_i3_93; t[4] = r820; k = t;
    r822[1] = r823[2]; lenv = r822; FUN_string_ref_331_0();
  }
}
static void FUN__24loop41_1437_i5_i3_93 (O rr) {
  O r820 = k[4]; lenv = k[2]; k = k[1];
  O r821 = rr;
  r820[2] = r821;
  // env 135:
  //   (maybe char)
  O r816 = allocate (TC_ENV, 2);
  O r819 = ((object*) lenv) [2];
  O r818 = alloc_no_clear (UOTAG(0),1);
  r818[1] = r819;
  r816[2] = r818;
  O r817 = ((object*****) lenv) [1][1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop41_1437_i5_i3_94; ; k = t;
  r816[1] = r817[2]; lenv = r816; ((pfun)(r817[1]))();
}
static void FUN__24loop41_1437_i5_i3_94 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r815 = rr;
  lenv = lenv[1];
  O r813 = ((object*) lenv) [2];
  O r814 = (object *) 3;
  O r811 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r813)+UNTAG_INTEGER(r814));
  O r812 = ((object*) lenv) [3];
  lenv[2] = r811;
  lenv[3] = r812;
  FUN__24loop41_1437_i5_i3_0();
}
static void FUN_lambda_21680_i226_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 136:
  //   string (list string) ( -> (maybe string))
  O r974 = allocate (TC_ENV, 4);
  r974[1] = lenv; lenv = r974;
  O r975 = (O) &constructed_22;
  r974[2] = r975;
  O r976 = (object *) 10;
  r974[3] = r976;
  O r977 = ((object***) lenv) [1][1][2];
  r974[4] = r977;
  // env 137:
  //   ((maybe string) -> undefined)
  O r883 = alloc_cleared (TC_ENV, 2);
  r883[1] = lenv; lenv = r883;
  O r884 = allocate (TC_CLOSURE, 2);
  r884[1] = FUN__24genloop145_3356_i226_i277_i289_0; r884[2] = lenv;
  r883[2] = r884;
  // env 138:
  //   (maybe string)
  O r879 = alloc_cleared (TC_ENV, 2);
  O r882 = ((object**) lenv) [1][4];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21680_i226_i277_i289_95; t[4] = r879; k = t;
  lenv = r882[2]; ((pfun)(r882[1]))();
}
static void FUN_lambda_21680_i226_i277_i289_95 (O rr) {
  O r879 = k[4]; lenv = k[2]; k = k[1];
  O r881 = rr;
  r879[2] = r881;
  O r880 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21680_i226_i277_i289_96; ; k = t;
  r879[1] = r880[2]; lenv = r879; FUN__24genloop145_3356_i226_i277_i289_0();
}
static void FUN_lambda_21680_i226_i277_i289_96 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r878 = rr;
  lenv = lenv[1];
  O r877 = r878;
  lenv = lenv[1];
  // env 139:
  //   ( -> t823)
  O r869 = alloc_cleared (TC_ENV, 2);
  r869[1] = lenv; lenv = r869;
  O r870 = allocate (TC_CLOSURE, 2);
  r870[1] = FUN__24loop143_3359_i226_i277_i289_0; r870[2] = lenv;
  r869[2] = r870;
  O r868 = ((object*) lenv) [2];
  lenv = r868[2]; FUN__24loop143_3359_i226_i277_i289_0();
}
static void FUN__24loop143_3359_i226_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 140:
  //   (maybe |tuple=(product string string) |)
  O r874 = allocate (TC_ENV, 2);
  O r876 = (object *) 65558;
  r874[2] = r876;
  O r875 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop143_3359_i226_i277_i289_97; ; k = t;
  r874[1] = r875[2]; lenv = r874; ((pfun)(r875[1]))();
}
static void FUN__24loop143_3359_i226_i277_i289_97 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop143_3359_i226_i277_i289_0();
}
static void FUN__24genloop145_3356_i226_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r887 = ((object*) lenv) [2];
  switch (get_case (r887)) {
    case (UITAG(1)): { // no
      O r889 = (object *) 14;
      IRK_RETURN(889);
    } break;
    default: {
      // env 141:
      //   string
      O r971 = alloc_cleared (TC_ENV, 2);
      r971[1] = lenv; lenv = r971;
      O r973 = ((object**) lenv) [1][2];
      O r972 = UOBJ_GET(r973,0);
      r971[2] = r972;
      // fatbar jn=51 label=50
      O r926 = ((object****) lenv) [1][1][1][2];
      O r931 = r926; // reg varref
      O r932 = (O) &constructed_22;
      O r927 = irk_string_cmp (((irk_string*)(r931)), ((irk_string*)(r932)));
      O r929 = r927; // reg varref
      O r930 = (object *) 65558;
      O r928 = IRK_TEST(r929==r930);
      O r898 = r928; // reg varref
      if IRK_IS_TRUE(r898) {
        // env 142:
        //   string string
        O r921 = allocate (TC_ENV, 3);
        O r923 = ((object*) lenv) [2];
        r921[2] = r923;
        O r924 = (O) &constructed_21;
        r921[3] = r924;
        O r922 = top[18];
        O t = allocate (TC_SAVE, 3);
        t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_98; ; k = t;
        r921[1] = r922[2]; lenv = r921; FUN_starts_with_341_0();
      } else {
        FAIL_50();
      }
    } break;
  }
}
static void FUN__24genloop145_3356_i226_i277_i289_98 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r914 = rr;
  if IRK_IS_TRUE(r914) {
    // env 143:
    //   string string
    O r916 = allocate (TC_ENV, 3);
    O r918 = ((object*) lenv) [2];
    r916[2] = r918;
    O r919 = (O) &constructed_20;
    r916[3] = r919;
    O r917 = top[19];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_99; ; k = t;
    r916[1] = r917[2]; lenv = r916; FUN_ends_with_342_0();
  } else {
    O r920 = (object *) 6;
    O r900 = r920;
    JUMP_54(r900);
  }
}
static void FUN__24genloop145_3356_i226_i277_i289_99 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r915 = rr;
  O r900 = r915;
  JUMP_54(r900);
}
static void JUMP_54(O r900) {
  if IRK_IS_TRUE(r900) {
    // env 144:
    //   string int int
    O r903 = alloc_cleared (TC_ENV, 4);
    O r905 = ((object*) lenv) [2];
    r903[2] = r905;
    O r906 = (object *) 23;
    r903[3] = r906;
    O r912 = ((object*) lenv) [2];
    O r908 = TAG_INTEGER((irk_int)((irk_string*)(r912))->len);
    O r910 = r908; // reg varref
    O r911 = (object *) 11;
    O r909 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r910)-UNTAG_INTEGER(r911));
    O r907 = r909; // reg varref
    r903[4] = r907;
    O r904 = top[13];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_100; ; k = t;
    r903[1] = r904[2]; lenv = r903; FUN_substring_327_0();
  } else {
    O r913 = (object *) 14;
    O r899 = r913;
    JUMP_53(r899);
  }
}
static void FUN__24genloop145_3356_i226_i277_i289_100 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r902 = rr;
  ((object****) lenv) [1][1][1][2] = r902;
  O r901 = (object *) TC_UNDEFINED;
  O r899 = r901;
  JUMP_53(r899);
}
static void JUMP_53(O r899) {
  O r897 = r899;
  JUMP_52(r897);
}
static void JUMP_52(O r897) {
  JUMP_51();
}
static void JUMP_51() {
  O r896 = ((object****) lenv) [1][1][1][4];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_101; ; k = t;
  lenv = r896[2]; ((pfun)(r896[1]))();
}
static void FUN__24genloop145_3356_i226_i277_i289_101 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r895 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r895;
  FUN__24genloop145_3356_i226_i277_i289_0();
}
static void FAIL_50() {
  // env 145:
  //   string string
  O r967 = allocate (TC_ENV, 3);
  O r969 = ((object*) lenv) [2];
  r967[2] = r969;
  O r970 = (O) &constructed_23;
  r967[3] = r970;
  O r968 = top[18];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_102; ; k = t;
  r967[1] = r968[2]; lenv = r967; FUN_starts_with_341_0();
}
static void FUN__24genloop145_3356_i226_i277_i289_102 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r960 = rr;
  if IRK_IS_TRUE(r960) {
    // env 146:
    //   string string
    O r962 = allocate (TC_ENV, 3);
    O r964 = ((object*) lenv) [2];
    r962[2] = r964;
    O r965 = (O) &constructed_20;
    r962[3] = r965;
    O r963 = top[19];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_103; ; k = t;
    r962[1] = r963[2]; lenv = r962; FUN_ends_with_342_0();
  } else {
    O r966 = (object *) 6;
    O r933 = r966;
    JUMP_56(r933);
  }
}
static void FUN__24genloop145_3356_i226_i277_i289_103 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r961 = rr;
  O r933 = r961;
  JUMP_56(r933);
}
static void JUMP_56(O r933) {
  if IRK_IS_TRUE(r933) {
    // env 147:
    //   |tuple=(product string string) |
    O r942 = alloc_cleared (TC_ENV, 2);
    r942[1] = lenv; lenv = r942;
    O r943 = alloc_cleared (UOTAG(4), 2);
    O r944 = ((object*****) lenv) [1][1][1][1][2];
    r943[1] = r944;
    // env 148:
    //   (list string)
    O r946 = alloc_cleared (TC_ENV, 2);
    // env 149:
    //   (list string)
    O r954 = allocate (TC_ENV, 2);
    r954[1] = lenv; lenv = r954;
    O r955 = ((object******) lenv) [1][1][1][1][1][3];
    r954[2] = r955;
    // env 150:
    //   (list string) (list string)
    O r950 = allocate (TC_ENV, 3);
    O r952 = ((object*) lenv) [2];
    r950[2] = r952;
    O r953 = (object *) 10;
    r950[3] = r953;
    O r951 = top[7];
    O t = allocate (TC_SAVE, 6);
    t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_104; t[4] = r942; t[5] = r943; t[6] = r946; k = t;
    r950[1] = r951[2]; lenv = r950; FUN_reverse_onto_264_0();
  } else {
    O r958 = ((object*) lenv) [2];
    O r959 = ((object****) lenv) [1][1][1][3];
    O r957 = alloc_no_clear (TC_PAIR,2);
    r957[1] = r958;
    r957[2] = r959;
    ((object****) lenv) [1][1][1][3] = r957;
    O r956 = (object *) TC_UNDEFINED;
    O r897 = r956;
    JUMP_55(r897);
  }
}
static void FUN__24genloop145_3356_i226_i277_i289_104 (O rr) {
  O r942 = k[4]; O r943 = k[5]; O r946 = k[6]; lenv = k[2]; k = k[1];
  O r949 = rr;
  lenv = lenv[1];
  O r948 = r949;
  r946[2] = r948;
  O r947 = top[17];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_105; t[4] = r942; t[5] = r943; k = t;
  r946[1] = r947[2]; lenv = r946; FUN_string_concat_333_0();
}
static void FUN__24genloop145_3356_i226_i277_i289_105 (O rr) {
  O r942 = k[4]; O r943 = k[5]; lenv = k[2]; k = k[1];
  O r945 = rr;
  r943[2] = r945;
  r942[2] = r943;
  // env 151:
  //   (maybe |tuple=(product string string) |)
  O r938 = allocate (TC_ENV, 2);
  O r941 = ((object*) lenv) [2];
  O r940 = alloc_no_clear (UOTAG(0),1);
  r940[1] = r941;
  r938[2] = r940;
  O r939 = ((object******) lenv) [1][1][1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop145_3356_i226_i277_i289_106; ; k = t;
  r938[1] = r939[2]; lenv = r938; ((pfun)(r939[1]))();
}
static void FUN__24genloop145_3356_i226_i277_i289_106 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r937 = rr;
  lenv = lenv[1];
  O r936 = (O) &constructed_22;
  ((object****) lenv) [1][1][1][2] = r936;
  O r935 = (object *) 10;
  ((object****) lenv) [1][1][1][3] = r935;
  O r934 = (object *) TC_UNDEFINED;
  O r897 = r934;
  JUMP_55(r897);
}
static void JUMP_55(O r897) {
  JUMP_51();
}
static void FUN_lambda_21562_i225_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 152:
  //   (list char) ( -> (maybe char))
  O r1043 = allocate (TC_ENV, 3);
  r1043[1] = lenv; lenv = r1043;
  O r1044 = (object *) 10;
  r1043[2] = r1044;
  O r1045 = ((object******) lenv) [1][1][1][1][1][4];
  r1043[3] = r1045;
  // env 153:
  //   ((maybe char) -> undefined)
  O r1001 = alloc_cleared (TC_ENV, 2);
  r1001[1] = lenv; lenv = r1001;
  O r1002 = allocate (TC_CLOSURE, 2);
  r1002[1] = FUN__24genloop139_3345_i225_i277_i289_0; r1002[2] = lenv;
  r1001[2] = r1002;
  // env 154:
  //   (maybe char)
  O r997 = alloc_cleared (TC_ENV, 2);
  O r1000 = ((object**) lenv) [1][3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21562_i225_i277_i289_107; t[4] = r997; k = t;
  lenv = r1000[2]; ((pfun)(r1000[1]))();
}
static void FUN_lambda_21562_i225_i277_i289_107 (O rr) {
  O r997 = k[4]; lenv = k[2]; k = k[1];
  O r999 = rr;
  r997[2] = r999;
  O r998 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_21562_i225_i277_i289_108; ; k = t;
  r997[1] = r998[2]; lenv = r997; FUN__24genloop139_3345_i225_i277_i289_0();
}
static void FUN_lambda_21562_i225_i277_i289_108 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r996 = rr;
  lenv = lenv[1];
  O r995 = r996;
  lenv = lenv[1];
  // env 155:
  //   ( -> t780)
  O r987 = alloc_cleared (TC_ENV, 2);
  r987[1] = lenv; lenv = r987;
  O r988 = allocate (TC_CLOSURE, 2);
  r988[1] = FUN__24loop137_3348_i225_i277_i289_0; r988[2] = lenv;
  r987[2] = r988;
  O r986 = ((object*) lenv) [2];
  lenv = r986[2]; FUN__24loop137_3348_i225_i277_i289_0();
}
static void FUN__24loop137_3348_i225_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 156:
  //   (maybe string)
  O r992 = allocate (TC_ENV, 2);
  O r994 = (object *) 65558;
  r992[2] = r994;
  O r993 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop137_3348_i225_i277_i289_109; ; k = t;
  r992[1] = r993[2]; lenv = r992; ((pfun)(r993[1]))();
}
static void FUN__24loop137_3348_i225_i277_i289_109 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop137_3348_i225_i277_i289_0();
}
static void FUN__24genloop139_3345_i225_i277_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1005 = ((object*) lenv) [2];
  switch (get_case (r1005)) {
    case (UITAG(1)): { // no
      O r1007 = (object *) 14;
      IRK_RETURN(1007);
    } break;
    default: {
      // env 157:
      //   char
      O r1040 = alloc_cleared (TC_ENV, 2);
      r1040[1] = lenv; lenv = r1040;
      O r1042 = ((object**) lenv) [1][2];
      O r1041 = UOBJ_GET(r1042,0);
      r1040[2] = r1041;
      O r1015 = ((object*) lenv) [2];
      O r1016 = (object *) 655362;
      if IRK_IS_TRUE(IRK_TEST(r1015==r1016)) {
        // env 158:
        //   string
        O r1024 = alloc_cleared (TC_ENV, 2);
        r1024[1] = lenv; lenv = r1024;
        // env 159:
        //   (list char)
        O r1026 = alloc_cleared (TC_ENV, 2);
        // env 160:
        //   (list char)
        O r1034 = allocate (TC_ENV, 2);
        r1034[1] = lenv; lenv = r1034;
        O r1035 = ((object******) lenv) [1][1][1][1][1][2];
        r1034[2] = r1035;
        // env 161:
        //   (list char) (list string)
        O r1030 = allocate (TC_ENV, 3);
        O r1032 = ((object*) lenv) [2];
        r1030[2] = r1032;
        O r1033 = (object *) 10;
        r1030[3] = r1033;
        O r1031 = top[7];
        O t = allocate (TC_SAVE, 5);
        t[1] = k; t[2] = lenv; t[3] = FUN__24genloop139_3345_i225_i277_i289_110; t[4] = r1024; t[5] = r1026; k = t;
        r1030[1] = r1031[2]; lenv = r1030; FUN_reverse_onto_264_0();
      } else {
        O r1038 = ((object*) lenv) [2];
        O r1039 = ((object****) lenv) [1][1][1][2];
        O r1037 = alloc_no_clear (TC_PAIR,2);
        r1037[1] = r1038;
        r1037[2] = r1039;
        ((object****) lenv) [1][1][1][2] = r1037;
        O r1036 = (object *) TC_UNDEFINED;
        JUMP_59();
      }
    } break;
  }
}
static void FUN__24genloop139_3345_i225_i277_i289_110 (O rr) {
  O r1024 = k[4]; O r1026 = k[5]; lenv = k[2]; k = k[1];
  O r1029 = rr;
  lenv = lenv[1];
  O r1028 = r1029;
  r1026[2] = r1028;
  O r1027 = top[20];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop139_3345_i225_i277_i289_111; t[4] = r1024; k = t;
  r1026[1] = r1027[2]; lenv = r1026; FUN_list__3estring_347_0();
}
static void FUN__24genloop139_3345_i225_i277_i289_111 (O rr) {
  O r1024 = k[4]; lenv = k[2]; k = k[1];
  O r1025 = rr;
  r1024[2] = r1025;
  // env 162:
  //   (maybe string)
  O r1020 = allocate (TC_ENV, 2);
  O r1023 = ((object*) lenv) [2];
  O r1022 = alloc_no_clear (UOTAG(0),1);
  r1022[1] = r1023;
  r1020[2] = r1022;
  O r1021 = ((object******) lenv) [1][1][1][1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop139_3345_i225_i277_i289_112; ; k = t;
  r1020[1] = r1021[2]; lenv = r1020; ((pfun)(r1021[1]))();
}
static void FUN__24genloop139_3345_i225_i277_i289_112 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1019 = rr;
  lenv = lenv[1];
  O r1018 = (object *) 10;
  ((object****) lenv) [1][1][1][2] = r1018;
  O r1017 = (object *) TC_UNDEFINED;
  JUMP_59();
}
static void JUMP_59() {
  O r1014 = ((object****) lenv) [1][1][1][3];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24genloop139_3345_i225_i277_i289_113; ; k = t;
  lenv = r1014[2]; ((pfun)(r1014[1]))();
}
static void FUN__24genloop139_3345_i225_i277_i289_113 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1013 = rr;
  lenv = ((object *)lenv)[1];
  lenv[2] = r1013;
  FUN__24genloop139_3345_i225_i277_i289_0();
}
static void FUN_lambda_20521_i275_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 163:
  //   bool
  O r1116 = allocate (TC_ENV, 2);
  r1116[1] = lenv; lenv = r1116;
  O r1117 = (object *) 6;
  r1116[2] = r1117;
  // env 164:
  //   ( -> undefined)
  O r1087 = alloc_cleared (TC_ENV, 2);
  r1087[1] = lenv; lenv = r1087;
  O r1088 = allocate (TC_CLOSURE, 2);
  r1088[1] = FUN__24loop90_3236_i275_i289_0; r1088[2] = lenv;
  r1087[2] = r1088;
  O r1086 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_lambda_20521_i275_i289_114; ; k = t;
  lenv = r1086[2]; FUN__24loop90_3236_i275_i289_0();
}
static void FUN_lambda_20521_i275_i289_114 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1085 = rr;
  lenv = lenv[1];
  // env 165:
  //   ( -> t739)
  O r1077 = alloc_cleared (TC_ENV, 2);
  r1077[1] = lenv; lenv = r1077;
  O r1078 = allocate (TC_CLOSURE, 2);
  r1078[1] = FUN__24loop89_3238_i275_i289_0; r1078[2] = lenv;
  r1077[2] = r1078;
  O r1076 = ((object*) lenv) [2];
  lenv = r1076[2]; FUN__24loop89_3238_i275_i289_0();
}
static void FUN__24loop89_3238_i275_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 166:
  //   (maybe char)
  O r1082 = allocate (TC_ENV, 2);
  O r1084 = (object *) 65558;
  r1082[2] = r1084;
  O r1083 = ((object***) lenv) [1][1][2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop89_3238_i275_i289_115; ; k = t;
  r1082[1] = r1083[2]; lenv = r1082; ((pfun)(r1083[1]))();
}
static void FUN__24loop89_3238_i275_i289_115 (O rr) {
  ; lenv = k[2]; k = k[1];
  FUN__24loop89_3238_i275_i289_0();
}
static void FUN__24loop90_3236_i275_i289_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1112 = ((object**) lenv) [1][2];
  O r1114 = r1112; // reg varref
  O r1115 = (object *) 6;
  O r1113 = IRK_TEST(r1114==r1115);
  O r1091 = r1113; // reg varref
  if IRK_IS_TRUE(r1091) {
    // env 167:
    //   char
    O r1105 = alloc_cleared (TC_ENV, 2);
    r1105[1] = lenv; lenv = r1105;
    // env 168:
    //   {pos=int end=int rbuf=string fd=int }
    O r1107 = allocate (TC_ENV, 2);
    O r1109 = ((object*****) lenv) [1][1][1][1][3];
    r1107[2] = r1109;
    O r1108 = top[38];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop90_3236_i275_i289_116; t[4] = r1105; k = t;
    r1107[1] = r1108[2]; lenv = r1107; FUN_file_2fread_char_634_0();
  } else {
    O r1111 = (object *) 14;
    IRK_RETURN(1111);
  }
}
static void FUN__24loop90_3236_i275_i289_116 (O rr) {
  O r1105 = k[4]; lenv = k[2]; k = k[1];
  O r1106 = rr;
  r1105[2] = r1106;
  O r1096 = ((object*) lenv) [2];
  O r1097 = (object *) 16842754;
  if IRK_IS_TRUE(IRK_TEST(r1096==r1097)) {
    O r1099 = (object *) 65542;
    ((object***) lenv) [1][1][2] = r1099;
    O r1098 = (object *) TC_UNDEFINED;
    O r1095 = r1098;
    JUMP_62(r1095);
  } else {
    // env 169:
    //   (maybe char)
    O r1101 = allocate (TC_ENV, 2);
    O r1104 = ((object*) lenv) [2];
    O r1103 = alloc_no_clear (UOTAG(0),1);
    r1103[1] = r1104;
    r1101[2] = r1103;
    O r1102 = ((object****) lenv) [1][1][1][2];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop90_3236_i275_i289_117; ; k = t;
    r1101[1] = r1102[2]; lenv = r1101; ((pfun)(r1102[1]))();
  }
}
static void FUN__24loop90_3236_i275_i289_117 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1100 = rr;
  O r1095 = r1100;
  JUMP_62(r1095);
}
static void JUMP_62(O r1095) {
  lenv = lenv[1];
  FUN__24loop90_3236_i275_i289_0();
}
static void FUN__24loop180_3483_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1244 = ((object*) lenv) [2];
  O r1245 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1244)==UNTAG_INTEGER(r1245))) {
    O r1247 = (object *) 14;
    IRK_RETURN(1247);
  } else {
    O r1255 = ((object***) lenv) [1][1][3];
    // env 170:
    //   char
    O r1260 = alloc_cleared (TC_ENV, 2);
    r1260[1] = lenv; lenv = r1260;
    // env 171:
    //   string int
    O r1262 = allocate (TC_ENV, 3);
    O r1264 = top[45];
    r1262[2] = r1264;
    O r1265 = ((object**) lenv) [1][2];
    r1262[3] = r1265;
    O r1263 = top[16];
    O t = allocate (TC_SAVE, 5);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop180_3483_118; t[4] = r1255; t[5] = r1260; k = t;
    r1262[1] = r1263[2]; lenv = r1262; FUN_string_ref_331_0();
  }
}
static void FUN__24loop180_3483_118 (O rr) {
  O r1255 = k[4]; O r1260 = k[5]; lenv = k[2]; k = k[1];
  O r1261 = rr;
  r1260[2] = r1261;
  O r1259 = ((object*) lenv) [2];
  O r1258 = TAG_INTEGER((irk_int)GET_CHAR(r1259));
  lenv = lenv[1];
  O r1256 = r1258;
  O r1257 = ((object*) lenv) [2];
  range_check (GET_TUPLE_LENGTH(*(object*)r1255), unbox(r1256));
  ((irk_vector*)r1255)->val[unbox(r1256)] = r1257;
  O r1253 = ((object*) lenv) [2];
  O r1254 = (object *) 3;
  O r1251 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1253)+UNTAG_INTEGER(r1254));
  O r1252 = ((object*) lenv) [3];
  lenv[2] = r1251;
  lenv[3] = r1252;
  FUN__24loop180_3483_0();
}
static void FUN_decoder_660_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 172:
  //   int int
  O r1385 = allocate (TC_ENV, 3);
  r1385[1] = lenv; lenv = r1385;
  O r1386 = (object *) 1;
  r1385[2] = r1386;
  O r1388 = ((object**) lenv) [1][2];
  O r1387 = TAG_INTEGER((irk_int)((irk_string*)(r1388))->len);
  r1385[3] = r1387;
  // env 173:
  //   (int -> string) ( -> int) ( -> string) ( -> int)
  O r1288 = alloc_cleared (TC_ENV, 5);
  r1288[1] = lenv; lenv = r1288;
  O r1289 = allocate (TC_CLOSURE, 2);
  r1289[1] = FUN_get_3406_0; r1289[2] = lenv;
  r1288[2] = r1289;
  O r1290 = allocate (TC_CLOSURE, 2);
  r1290[1] = FUN_get_u32_3408_0; r1290[2] = lenv;
  r1288[3] = r1290;
  O r1291 = allocate (TC_CLOSURE, 2);
  r1291[1] = FUN_get_string_3409_0; r1291[2] = lenv;
  r1288[4] = r1291;
  O r1292 = allocate (TC_CLOSURE, 2);
  r1292[1] = FUN_left_3410_0; r1292[2] = lenv;
  r1288[5] = r1292;
  O r1283 = allocate (UOTAG(3), 4);
  O r1284 = ((object*) lenv) [2];
  r1283[1] = r1284;
  O r1285 = ((object*) lenv) [5];
  r1283[2] = r1285;
  O r1286 = ((object*) lenv) [4];
  r1283[3] = r1286;
  O r1287 = ((object*) lenv) [3];
  r1283[4] = r1287;
  IRK_RETURN(1283);
}
static void FUN_left_3410_0 (void) {
  O r1295 = ((object**) lenv) [1][2];
  O r1298 = ((object**) lenv) [1][3];
  O r1299 = r1295; // reg varref
  O r1297 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1298)-UNTAG_INTEGER(r1299));
  IRK_RETURN(1297);
}
static void FUN_get_string_3409_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 174:
  //   int
  O r1307 = alloc_cleared (TC_ENV, 2);
  r1307[1] = lenv; lenv = r1307;
  O r1309 = ((object**) lenv) [1][3];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_string_3409_119; t[4] = r1307; k = t;
  lenv = r1309[2]; FUN_get_u32_3408_0();
}
static void FUN_get_string_3409_119 (O rr) {
  O r1307 = k[4]; lenv = k[2]; k = k[1];
  O r1308 = rr;
  r1307[2] = r1308;
  // env 175:
  //   int
  O r1304 = allocate (TC_ENV, 2);
  O r1306 = ((object*) lenv) [2];
  r1304[2] = r1306;
  O r1305 = ((object**) lenv) [1][2];
  r1304[1] = r1305[2]; lenv = r1304; FUN_get_3406_0();
}
static void FUN_get_u32_3408_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 176:
  //   string char int int char int int char int int char int int int
  O r1316 = alloc_cleared (TC_ENV, 15);
  r1316[1] = lenv; lenv = r1316;
  // env 177:
  //   int
  O r1361 = allocate (TC_ENV, 2);
  O r1363 = (object *) 9;
  r1361[2] = r1363;
  O r1362 = ((object**) lenv) [1][2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_u32_3408_120; t[4] = r1316; k = t;
  r1361[1] = r1362[2]; lenv = r1361; FUN_get_3406_0();
}
static void FUN_get_u32_3408_120 (O rr) {
  O r1316 = k[4]; lenv = k[2]; k = k[1];
  O r1317 = rr;
  r1316[2] = r1317;
  // env 178:
  //   string int
  O r1357 = allocate (TC_ENV, 3);
  O r1359 = ((object*) lenv) [2];
  r1357[2] = r1359;
  O r1360 = (object *) 1;
  r1357[3] = r1360;
  O r1358 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_u32_3408_121; t[4] = r1316; k = t;
  r1357[1] = r1358[2]; lenv = r1357; FUN_string_ref_331_0();
}
static void FUN_get_u32_3408_121 (O rr) {
  O r1316 = k[4]; lenv = k[2]; k = k[1];
  O r1318 = rr;
  r1316[3] = r1318;
  O r1356 = ((object*) lenv) [3];
  O r1319 = TAG_INTEGER((irk_int)GET_CHAR(r1356));
  r1316[4] = r1319;
  O r1354 = ((object*) lenv) [4];
  O r1355 = (object *) 49;
  O r1320 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1354)<<UNTAG_INTEGER(r1355));
  r1316[5] = r1320;
  // env 179:
  //   string int
  O r1350 = allocate (TC_ENV, 3);
  O r1352 = ((object*) lenv) [2];
  r1350[2] = r1352;
  O r1353 = (object *) 3;
  r1350[3] = r1353;
  O r1351 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_u32_3408_122; t[4] = r1316; k = t;
  r1350[1] = r1351[2]; lenv = r1350; FUN_string_ref_331_0();
}
static void FUN_get_u32_3408_122 (O rr) {
  O r1316 = k[4]; lenv = k[2]; k = k[1];
  O r1321 = rr;
  r1316[6] = r1321;
  O r1349 = ((object*) lenv) [6];
  O r1322 = TAG_INTEGER((irk_int)GET_CHAR(r1349));
  r1316[7] = r1322;
  O r1347 = ((object*) lenv) [7];
  O r1348 = (object *) 33;
  O r1323 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1347)<<UNTAG_INTEGER(r1348));
  r1316[8] = r1323;
  // env 180:
  //   string int
  O r1343 = allocate (TC_ENV, 3);
  O r1345 = ((object*) lenv) [2];
  r1343[2] = r1345;
  O r1346 = (object *) 5;
  r1343[3] = r1346;
  O r1344 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_u32_3408_123; t[4] = r1316; k = t;
  r1343[1] = r1344[2]; lenv = r1343; FUN_string_ref_331_0();
}
static void FUN_get_u32_3408_123 (O rr) {
  O r1316 = k[4]; lenv = k[2]; k = k[1];
  O r1324 = rr;
  r1316[9] = r1324;
  O r1342 = ((object*) lenv) [9];
  O r1325 = TAG_INTEGER((irk_int)GET_CHAR(r1342));
  r1316[10] = r1325;
  O r1340 = ((object*) lenv) [10];
  O r1341 = (object *) 17;
  O r1326 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1340)<<UNTAG_INTEGER(r1341));
  r1316[11] = r1326;
  // env 181:
  //   string int
  O r1336 = allocate (TC_ENV, 3);
  O r1338 = ((object*) lenv) [2];
  r1336[2] = r1338;
  O r1339 = (object *) 7;
  r1336[3] = r1339;
  O r1337 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_u32_3408_124; t[4] = r1316; k = t;
  r1336[1] = r1337[2]; lenv = r1336; FUN_string_ref_331_0();
}
static void FUN_get_u32_3408_124 (O rr) {
  O r1316 = k[4]; lenv = k[2]; k = k[1];
  O r1327 = rr;
  r1316[12] = r1327;
  O r1335 = ((object*) lenv) [12];
  O r1328 = TAG_INTEGER((irk_int)GET_CHAR(r1335));
  r1316[13] = r1328;
  O r1333 = ((object*) lenv) [11];
  O r1334 = ((object*) lenv) [13];
  O r1329 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1333)+UNTAG_INTEGER(r1334));
  r1316[14] = r1329;
  O r1331 = ((object*) lenv) [8];
  O r1332 = ((object*) lenv) [14];
  O r1330 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1331)+UNTAG_INTEGER(r1332));
  r1316[15] = r1330;
  O r1314 = ((object*) lenv) [5];
  O r1315 = ((object*) lenv) [15];
  O r1313 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1314)+UNTAG_INTEGER(r1315));
  IRK_RETURN(1313);
}
static void FUN_get_3406_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 182:
  //   string
  O r1374 = alloc_cleared (TC_ENV, 2);
  r1374[1] = lenv; lenv = r1374;
  // env 183:
  //   string int int
  O r1376 = alloc_cleared (TC_ENV, 4);
  O r1378 = ((object*****) lenv) [1][1][1][1][2];
  r1376[2] = r1378;
  O r1379 = ((object****) lenv) [1][1][1][2];
  r1376[3] = r1379;
  O r1381 = ((object****) lenv) [1][1][1][2];
  O r1383 = r1381; // reg varref
  O r1384 = ((object**) lenv) [1][2];
  O r1382 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1383)+UNTAG_INTEGER(r1384));
  O r1380 = r1382; // reg varref
  r1376[4] = r1380;
  O r1377 = top[13];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_get_3406_125; t[4] = r1374; k = t;
  r1376[1] = r1377[2]; lenv = r1376; FUN_substring_327_0();
}
static void FUN_get_3406_125 (O rr) {
  O r1374 = k[4]; lenv = k[2]; k = k[1];
  O r1375 = rr;
  r1374[2] = r1375;
  O r1370 = ((object****) lenv) [1][1][1][2];
  O r1372 = r1370; // reg varref
  O r1373 = ((object**) lenv) [1][2];
  O r1371 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1372)+UNTAG_INTEGER(r1373));
  O r1369 = r1371; // reg varref
  ((object****) lenv) [1][1][1][2] = r1369;
  O r1368 = ((object*) lenv) [2];
  IRK_RETURN(1368);
}
static void FUN_string__3ehex_657_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 184:
  //   int int string
  O r1455 = alloc_cleared (TC_ENV, 4);
  r1455[1] = lenv; lenv = r1455;
  O r1464 = ((object**) lenv) [1][2];
  O r1456 = TAG_INTEGER((irk_int)((irk_string*)(r1464))->len);
  r1455[2] = r1456;
  O r1462 = (object *) 5;
  O r1463 = ((object*) lenv) [2];
  O r1457 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1462)*UNTAG_INTEGER(r1463));
  r1455[3] = r1457;
  O r1461 = ((object*) lenv) [3];
  O r1460 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r1461)));
  if (freep + unbox(r1460) >= limit) {
    heap_request = unbox(r1460);
    heap1[3] = r1455;
    gc_flip (1);
    r1455 = heap0[3];
  }
  O r1459 = ((object*) lenv) [3];
  O r1458 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r1459))), ((irk_string*)(t))->len = UNTAG_INTEGER(r1459), t);
  r1455[4] = r1458;
  // env 185:
  //   (int int -> undefined)
  O r1399 = alloc_cleared (TC_ENV, 2);
  r1399[1] = lenv; lenv = r1399;
  O r1400 = allocate (TC_CLOSURE, 2);
  r1400[1] = FUN__24loop159_3386_0; r1400[2] = lenv;
  r1399[2] = r1400;
  // env 186:
  //   int int
  O r1395 = allocate (TC_ENV, 3);
  O r1397 = (object *) 1;
  r1395[2] = r1397;
  O r1398 = ((object**) lenv) [1][2];
  r1395[3] = r1398;
  O r1396 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_string__3ehex_657_126; ; k = t;
  r1395[1] = r1396[2]; lenv = r1395; FUN__24loop159_3386_0();
}
static void FUN_string__3ehex_657_126 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1394 = rr;
  lenv = lenv[1];
  O r1393 = ((object*) lenv) [4];
  IRK_RETURN(1393);
}
static void FUN__24loop159_3386_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1403 = ((object*) lenv) [2];
  O r1404 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1403)==UNTAG_INTEGER(r1404))) {
    O r1406 = (object *) 14;
    IRK_RETURN(1406);
  } else {
    // env 187:
    //   char int char char
    O r1433 = alloc_cleared (TC_ENV, 5);
    r1433[1] = lenv; lenv = r1433;
    // env 188:
    //   string int
    O r1451 = allocate (TC_ENV, 3);
    O r1453 = ((object*****) lenv) [1][1][1][1][2];
    r1451[2] = r1453;
    O r1454 = ((object**) lenv) [1][2];
    r1451[3] = r1454;
    O r1452 = top[16];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN__24loop159_3386_127; t[4] = r1433; k = t;
    r1451[1] = r1452[2]; lenv = r1451; FUN_string_ref_331_0();
  }
}
static void FUN__24loop159_3386_127 (O rr) {
  O r1433 = k[4]; lenv = k[2]; k = k[1];
  O r1434 = rr;
  r1433[2] = r1434;
  O r1450 = ((object*) lenv) [2];
  O r1435 = TAG_INTEGER((irk_int)GET_CHAR(r1450));
  r1433[3] = r1435;
  O r1442 = top[28];
  O r1448 = ((object*) lenv) [3];
  O r1449 = (object *) 9;
  O r1444 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1448)>>UNTAG_INTEGER(r1449));
  O r1446 = (object *) 31;
  O r1447 = r1444; // reg varref
  O r1445 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1446)&UNTAG_INTEGER(r1447));
  O r1443 = r1445; // reg varref
  range_check (GET_TUPLE_LENGTH(*(object*)r1442), unbox(r1443));
  O r1436 = ((irk_vector*)r1442)->val[unbox(r1443)];
  r1433[4] = r1436;
  O r1438 = top[28];
  O r1440 = (object *) 31;
  O r1441 = ((object*) lenv) [3];
  O r1439 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1440)&UNTAG_INTEGER(r1441));
  range_check (GET_TUPLE_LENGTH(*(object*)r1438), unbox(r1439));
  O r1437 = ((irk_vector*)r1438)->val[unbox(r1439)];
  r1433[5] = r1437;
  // env 189:
  //   string int char
  O r1426 = allocate (TC_ENV, 4);
  O r1428 = ((object****) lenv) [1][1][1][4];
  r1426[2] = r1428;
  O r1431 = ((object**) lenv) [1][2];
  O r1432 = (object *) 5;
  O r1429 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1431)*UNTAG_INTEGER(r1432));
  r1426[3] = r1429;
  O r1430 = ((object*) lenv) [4];
  r1426[4] = r1430;
  O r1427 = top[14];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop159_3386_128; ; k = t;
  r1426[1] = r1427[2]; lenv = r1426; FUN_string_set_bang_332_0();
}
static void FUN__24loop159_3386_128 (O rr) {
  ; lenv = k[2]; k = k[1];
  // env 190:
  //   string int char
  O r1415 = alloc_cleared (TC_ENV, 4);
  O r1417 = ((object****) lenv) [1][1][1][4];
  r1415[2] = r1417;
  O r1424 = ((object**) lenv) [1][2];
  O r1425 = (object *) 5;
  O r1420 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1424)*UNTAG_INTEGER(r1425));
  O r1422 = (object *) 3;
  O r1423 = r1420; // reg varref
  O r1421 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1422)+UNTAG_INTEGER(r1423));
  O r1418 = r1421; // reg varref
  r1415[3] = r1418;
  O r1419 = ((object*) lenv) [5];
  r1415[4] = r1419;
  O r1416 = top[14];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN__24loop159_3386_129; ; k = t;
  r1415[1] = r1416[2]; lenv = r1415; FUN_string_set_bang_332_0();
}
static void FUN__24loop159_3386_129 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r1414 = rr;
  lenv = lenv[1];
  O r1412 = ((object*) lenv) [2];
  O r1413 = (object *) 3;
  O r1410 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1412)+UNTAG_INTEGER(r1413));
  O r1411 = ((object*) lenv) [3];
  lenv[2] = r1410;
  lenv[3] = r1411;
  FUN__24loop159_3386_0();
}
static void FUN_file_2fread_char_634_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1530 = ((object*) lenv) [2];
  O r1524 = ((irk_vector*)r1530)->val[2];
  O r1529 = ((object*) lenv) [2];
  O r1525 = ((irk_vector*)r1529)->val[0];
  O r1527 = r1524; // reg varref
  O r1528 = r1525; // reg varref
  O r1526 = IRK_TEST(UNTAG_INTEGER(r1527)<UNTAG_INTEGER(r1528));
  O r1467 = r1526; // reg varref
  if IRK_IS_TRUE(r1467) {
    O r1481 = ((object*) lenv) [2];
    O r1487 = ((object*) lenv) [2];
    O r1483 = ((irk_vector*)r1487)->val[2];
    O r1485 = r1483; // reg varref
    O r1486 = (object *) 3;
    O r1484 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1485)+UNTAG_INTEGER(r1486));
    O r1482 = r1484; // reg varref
    ((irk_vector*)r1481)->val[2] = r1482;
    // env 191:
    //   string int
    O r1471 = alloc_cleared (TC_ENV, 3);
    O r1480 = ((object*) lenv) [2];
    O r1473 = ((irk_vector*)r1480)->val[3];
    r1471[2] = r1473;
    O r1479 = ((object*) lenv) [2];
    O r1475 = ((irk_vector*)r1479)->val[2];
    O r1477 = r1475; // reg varref
    O r1478 = (object *) 3;
    O r1476 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1477)-UNTAG_INTEGER(r1478));
    O r1474 = r1476; // reg varref
    r1471[3] = r1474;
    O r1472 = top[16];
    r1471[1] = r1472[2]; lenv = r1471; FUN_string_ref_331_0();
  } else {
    // env 192:
    //   {pos=int end=int rbuf=string fd=int ...t394} int string int (cref void) int int int
    O r1499 = alloc_cleared (TC_ENV, 9);
    r1499[1] = lenv; lenv = r1499;
    O r1500 = ((object**) lenv) [1][2];
    r1499[2] = r1500;
    O r1523 = ((object*) lenv) [2];
    O r1501 = ((irk_vector*)r1523)->val[1];
    r1499[3] = r1501;
    O r1522 = ((object*) lenv) [2];
    O r1502 = ((irk_vector*)r1522)->val[3];
    r1499[4] = r1502;
    O r1521 = ((object*) lenv) [4];
    O r1503 = TAG_INTEGER((irk_int)((irk_string*)(r1521))->len);
    r1499[5] = r1503;
    O r1520 = ((object*) lenv) [4];
    O r1504 = make_string_foreign (r1520);
    r1499[6] = r1504;
    O r1517 = ((object*) lenv) [3];
    O r1518 = ((object*) lenv) [6];
    O r1519 = ((object*) lenv) [5];
    O r1505 = TAG_INTEGER((irk_int)read(UNTAG_INTEGER(r1517), (void*) get_foreign(r1518), UNTAG_INTEGER(r1519)));
    r1499[7] = r1505;
    O r1512 = ((object*) lenv) [7];
    O r1513 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1512)<UNTAG_INTEGER(r1513))) {
      O r1515 = top[31];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_file_2fread_char_634_130; t[4] = r1499; k = t;
      lenv = r1515[2]; FUN_raise_system_error_519_0();
    } else {
      O r1516 = ((object*) lenv) [7];
      O r1506 = r1516;
      JUMP_73(r1506, r1499);
    }
  }
}
static void FUN_file_2fread_char_634_130 (O rr) {
  O r1499 = k[4]; lenv = k[2]; k = k[1];
  O r1514 = rr;
  O r1506 = r1514;
  JUMP_73(r1506, r1499);
}
static void JUMP_73(O r1506, O r1499) {
  r1499[8] = r1506;
  O r1510 = ((object*) lenv) [2];
  O r1511 = ((object*) lenv) [8];
  ((irk_vector*)r1510)->val[0] = r1511;
  O r1508 = ((object*) lenv) [2];
  O r1509 = (object *) 1;
  ((irk_vector*)r1508)->val[2] = r1509;
  O r1507 = ((object*) lenv) [8];
  r1499[9] = r1507;
  O r1497 = ((object*) lenv) [9];
  O r1498 = (object *) 1;
  O r1496 = IRK_TEST(UNTAG_INTEGER(r1497)==UNTAG_INTEGER(r1498));
  lenv = lenv[1];
  O r1490 = r1496;
  if IRK_IS_TRUE(r1490) {
    O r1492 = (object *) 16842754;
    IRK_RETURN(1492);
  } else {
    O r1495 = ((object*) lenv) [2];
    lenv[2] = r1495;
    FUN_file_2fread_char_634_0();
  }
}
static void FUN_print_exception_exit_587_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 193:
  //   |...t29| symbol
  O r1559 = alloc_cleared (TC_ENV, 3);
  r1559[1] = lenv; lenv = r1559;
  O r1560 = ((object**) lenv) [1][2];
  r1559[2] = r1560;
  O r1910 = top[34];
  O r1915 = r1910; // reg varref
  O r1911 = TAG_INTEGER((irk_int)(r1915 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*r1915));
  O r1913 = (object *) 1;
  O r1914 = r1911; // reg varref
  O r1912 = IRK_TEST(UNTAG_INTEGER(r1913)==UNTAG_INTEGER(r1914));
  O r1705 = r1912; // reg varref
  if IRK_IS_TRUE(r1705) {
    // env 194:
    //   (list sexp)
    O r1805 = alloc_cleared (TC_ENV, 2);
    r1805[1] = lenv; lenv = r1805;
    // env 195:
    //   (symbol (list sexp) -> (list sexp))
    O r1847 = alloc_cleared (TC_ENV, 2);
    r1847[1] = lenv; lenv = r1847;
    O r1848 = allocate (TC_CLOSURE, 2);
    r1848[1] = FUN_search_sexp_list_3006_i130_i159_i160_i165_0; r1848[2] = lenv;
    r1847[2] = r1848;
    // env 196:
    //   sexp
    O r1845 = allocate (TC_ENV, 2);
    r1845[1] = lenv; lenv = r1845;
    O r1846 = irk_get_metadata();
    r1845[2] = r1846;
    // fatbar jn=99 label=98
    O r1810 = ((object*) lenv) [2];
    switch (get_case (r1810)) {
      case (UOTAG(0)): { // list
        // env 197:
        //   (list sexp)
        O r1837 = alloc_cleared (TC_ENV, 2);
        r1837[1] = lenv; lenv = r1837;
        O r1839 = ((object**) lenv) [1][2];
        O r1838 = UOBJ_GET(r1839,0);
        r1837[2] = r1838;
        O r1813 = ((object*) lenv) [2];
        switch (get_case (r1813)) {
          case (TC_PAIR): { // cons
            // env 198:
            //   sexp (list sexp)
            O r1831 = alloc_cleared (TC_ENV, 3);
            r1831[1] = lenv; lenv = r1831;
            O r1835 = ((object**) lenv) [1][2];
            O r1832 = UOBJ_GET(r1835,0);
            r1831[2] = r1832;
            O r1834 = ((object**) lenv) [1][2];
            O r1833 = UOBJ_GET(r1834,1);
            r1831[3] = r1833;
            O r1816 = ((object*) lenv) [2];
            switch (get_case (r1816)) {
              case (UOTAG(1)): { // symbol
                // env 199:
                //   symbol
                O r1827 = alloc_cleared (TC_ENV, 2);
                r1827[1] = lenv; lenv = r1827;
                O r1829 = ((object**) lenv) [1][2];
                O r1828 = UOBJ_GET(r1829,0);
                r1827[2] = r1828;
                O r1819 = ((object*) lenv) [2];
                O r1820 = (object*) constructed_37[0];
                if IRK_IS_TRUE(IRK_TEST(r1819==r1820)) {
                  // env 200:
                  //   symbol (list sexp)
                  O r1822 = allocate (TC_ENV, 3);
                  O r1824 = (object*) constructed_35[0];
                  r1822[2] = r1824;
                  O r1825 = ((object**) lenv) [1][3];
                  r1822[3] = r1825;
                  O r1823 = ((object*****) lenv) [1][1][1][1][2];
                  O t = allocate (TC_SAVE, 5);
                  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_587_131; t[4] = r1559; t[5] = r1805; k = t;
                  r1822[1] = r1823[2]; lenv = r1822; FUN_search_sexp_list_3006_i130_i159_i160_i165_0();
                } else {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_98(r1805, r1559);
                }
              } break;
              default: {
                lenv = ((object **)lenv)[1][1];
                FAIL_98(r1805, r1559);
              }
            }
          } break;
          default: {
            lenv = ((object *)lenv)[1];
            FAIL_98(r1805, r1559);
          }
        }
      } break;
      default: {
        FAIL_98(r1805, r1559);
      }
    }
  } else {
    O r1909 = (object *) 14;
    JUMP_88(r1559);
  }
}
static void FUN_print_exception_exit_587_131 (O rr) {
  O r1559 = k[4]; O r1805 = k[5]; lenv = k[2]; k = k[1];
  O r1821 = rr;
  O r1818 = r1821;
  JUMP_104(r1818, r1805, r1559);
}
static void JUMP_104(O r1818, O r1805, O r1559) {
  lenv = lenv[1];
  O r1817 = r1818;
  O r1815 = r1817;
  JUMP_102(r1815, r1805, r1559);
}
static void JUMP_102(O r1815, O r1805, O r1559) {
  lenv = lenv[1];
  O r1814 = r1815;
  O r1812 = r1814;
  JUMP_101(r1812, r1805, r1559);
}
static void JUMP_101(O r1812, O r1805, O r1559) {
  lenv = lenv[1];
  O r1811 = r1812;
  O r1809 = r1811;
  JUMP_100(r1809, r1805, r1559);
}
static void JUMP_100(O r1809, O r1805, O r1559) {
  O r1808 = r1809;
  JUMP_99(r1808, r1805, r1559);
}
static void JUMP_99(O r1808, O r1805, O r1559) {
  lenv = lenv[1];
  O r1807 = r1808;
  lenv = lenv[1];
  O r1806 = r1807;
  r1805[2] = r1806;
  // env 201:
  //   int
  O r1799 = alloc_cleared (TC_ENV, 2);
  r1799[1] = lenv; lenv = r1799;
  // env 202:
  //   (list sexp) int
  O r1801 = allocate (TC_ENV, 3);
  O r1803 = ((object**) lenv) [1][2];
  r1801[2] = r1803;
  O r1804 = (object *) 1;
  r1801[3] = r1804;
  O r1802 = top[8];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_587_132; t[4] = r1559; t[5] = r1799; k = t;
  r1801[1] = r1802[2]; lenv = r1801; FUN_length_splat_267_0();
}
static void FUN_print_exception_exit_587_132 (O rr) {
  O r1559 = k[4]; O r1799 = k[5]; lenv = k[2]; k = k[1];
  O r1800 = rr;
  r1799[2] = r1800;
  O r1798 = ((object*) lenv) [2];
  if (freep + unbox(r1798) >= limit) {
    heap_request = unbox(r1798);
    heap1[3] = r1559;
    gc_flip (1);
    r1559 = heap0[3];
  }
  O r1796 = ((object*) lenv) [2];
  O r1797 = (object*) constructed_33[0];
  O r1795;
  if (unbox(r1796) == 0) { r1795 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r1796));
    for (int i=0; i<unbox(r1796); i++) { t[i+1] = r1797; }
    r1795 = t;
  }
  lenv = lenv[1];
  O r1794 = r1795;
  top[34] = r1794;
  // env 203:
  //   ((list sexp) -> undefined)
  O r1712 = alloc_cleared (TC_ENV, 2);
  r1712[1] = lenv; lenv = r1712;
  O r1713 = allocate (TC_CLOSURE, 2);
  r1713[1] = FUN_loop_3021_i159_i160_i165_0; r1713[2] = lenv;
  r1712[2] = r1713;
  // env 204:
  //   (list sexp)
  O r1709 = allocate (TC_ENV, 2);
  O r1711 = ((object**) lenv) [1][2];
  r1709[2] = r1711;
  O r1710 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_587_133; t[4] = r1559; k = t;
  r1709[1] = r1710[2]; lenv = r1709; FUN_loop_3021_i159_i160_i165_0();
}
static void FUN_print_exception_exit_587_133 (O rr) {
  O r1559 = k[4]; lenv = k[2]; k = k[1];
  O r1708 = rr;
  lenv = lenv[1];
  O r1707 = r1708;
  lenv = lenv[1];
  O r1706 = r1707;
  JUMP_88(r1559);
}
static void FUN_loop_3021_i159_i160_i165_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=90 label=89
  O r1718 = ((object*) lenv) [2];
  switch (get_case (r1718)) {
    case (TC_PAIR): { // cons
      // env 205:
      //   sexp (list sexp)
      O r1786 = alloc_cleared (TC_ENV, 3);
      r1786[1] = lenv; lenv = r1786;
      O r1790 = ((object**) lenv) [1][2];
      O r1787 = UOBJ_GET(r1790,0);
      r1786[2] = r1787;
      O r1789 = ((object**) lenv) [1][2];
      O r1788 = UOBJ_GET(r1789,1);
      r1786[3] = r1788;
      O r1723 = ((object*) lenv) [2];
      switch (get_case (r1723)) {
        case (UOTAG(0)): { // list
          // env 206:
          //   (list sexp)
          O r1781 = alloc_cleared (TC_ENV, 2);
          r1781[1] = lenv; lenv = r1781;
          O r1783 = ((object**) lenv) [1][2];
          O r1782 = UOBJ_GET(r1783,0);
          r1781[2] = r1782;
          O r1728 = ((object*) lenv) [2];
          switch (get_case (r1728)) {
            case (TC_PAIR): { // cons
              // env 207:
              //   sexp (list sexp)
              O r1774 = alloc_cleared (TC_ENV, 3);
              r1774[1] = lenv; lenv = r1774;
              O r1778 = ((object**) lenv) [1][2];
              O r1775 = UOBJ_GET(r1778,0);
              r1774[2] = r1775;
              O r1777 = ((object**) lenv) [1][2];
              O r1776 = UOBJ_GET(r1777,1);
              r1774[3] = r1776;
              O r1733 = ((object*) lenv) [2];
              switch (get_case (r1733)) {
                case (UOTAG(1)): { // symbol
                  // env 208:
                  //   symbol
                  O r1769 = alloc_cleared (TC_ENV, 2);
                  r1769[1] = lenv; lenv = r1769;
                  O r1771 = ((object**) lenv) [1][2];
                  O r1770 = UOBJ_GET(r1771,0);
                  r1769[2] = r1770;
                  O r1738 = ((object**) lenv) [1][3];
                  switch (get_case (r1738)) {
                    case (TC_PAIR): { // cons
                      // env 209:
                      //   sexp (list sexp)
                      O r1762 = alloc_cleared (TC_ENV, 3);
                      r1762[1] = lenv; lenv = r1762;
                      O r1766 = ((object***) lenv) [1][1][3];
                      O r1763 = UOBJ_GET(r1766,0);
                      r1762[2] = r1763;
                      O r1765 = ((object***) lenv) [1][1][3];
                      O r1764 = UOBJ_GET(r1765,1);
                      r1762[3] = r1764;
                      O r1743 = ((object*) lenv) [2];
                      switch (get_case (r1743)) {
                        case (UOTAG(5)): { // int
                          O r1759 = ((object*) lenv) [2];
                          O r1746 = UOBJ_GET(r1759,0);
                          O r1749 = ((object*) lenv) [3];
                          switch (get_case (r1749)) {
                            case (TC_NIL): { // nil
                              O r1754 = top[34];
                              O r1755 = r1746; // reg varref
                              O r1756 = ((object**) lenv) [1][2];
                              range_check (GET_TUPLE_LENGTH(*(object*)r1754), unbox(r1755));
                              ((irk_vector*)r1754)->val[unbox(r1755)] = r1756;
                              O r1753 = ((object*****) lenv) [1][1][1][1][3];
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              lenv[2] = r1753;
                              FUN_loop_3021_i159_i160_i165_0();
                            } break;
                            default: {
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              FAIL_89();
                            }
                          }
                        } break;
                        default: {
                          lenv = ((object *****)lenv)[1][1][1][1][1];
                          FAIL_89();
                        }
                      }
                    } break;
                    default: {
                      lenv = ((object ****)lenv)[1][1][1][1];
                      FAIL_89();
                    }
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_89();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_89();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_89();
        }
      }
    } break;
    default: {
      FAIL_89();
    }
  }
}
static void FAIL_89() {
  O r1793 = (object *) 14;
  IRK_RETURN(1793);
}
static void FAIL_98(O r1805, O r1559) {
  // env 210:
  //   |Metadata/Malformed=sexp ...t259|
  O r1841 = alloc_cleared (TC_ENV, 2);
  O r1843 = allocate (UOTAG(3), 1);
  O r1844 = ((object*) lenv) [2];
  r1843[1] = r1844;
  r1841[2] = r1843;
  O r1842 = top[3];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_3021_i159_i160_i165_134; t[4] = r1559; t[5] = r1805; k = t;
  r1841[1] = r1842[2]; lenv = r1841; ((pfun)(r1842[1]))();
}
static void FUN_loop_3021_i159_i160_i165_134 (O rr) {
  O r1559 = k[4]; O r1805 = k[5]; lenv = k[2]; k = k[1];
  O r1809 = rr;
  O r1808 = r1809;
  JUMP_99(r1808, r1805, r1559);
}
static void FUN_search_sexp_list_3006_i130_i159_i160_i165_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=106 label=105
  O r1853 = ((object*) lenv) [3];
  switch (get_case (r1853)) {
    case (TC_PAIR): { // cons
      // env 211:
      //   sexp (list sexp)
      O r1897 = alloc_cleared (TC_ENV, 3);
      r1897[1] = lenv; lenv = r1897;
      O r1901 = ((object**) lenv) [1][3];
      O r1898 = UOBJ_GET(r1901,0);
      r1897[2] = r1898;
      O r1900 = ((object**) lenv) [1][3];
      O r1899 = UOBJ_GET(r1900,1);
      r1897[3] = r1899;
      O r1858 = ((object*) lenv) [2];
      switch (get_case (r1858)) {
        case (UOTAG(0)): { // list
          // env 212:
          //   (list sexp)
          O r1892 = alloc_cleared (TC_ENV, 2);
          r1892[1] = lenv; lenv = r1892;
          O r1894 = ((object**) lenv) [1][2];
          O r1893 = UOBJ_GET(r1894,0);
          r1892[2] = r1893;
          O r1863 = ((object*) lenv) [2];
          switch (get_case (r1863)) {
            case (TC_PAIR): { // cons
              // env 213:
              //   sexp (list sexp)
              O r1885 = alloc_cleared (TC_ENV, 3);
              r1885[1] = lenv; lenv = r1885;
              O r1889 = ((object**) lenv) [1][2];
              O r1886 = UOBJ_GET(r1889,0);
              r1885[2] = r1886;
              O r1888 = ((object**) lenv) [1][2];
              O r1887 = UOBJ_GET(r1888,1);
              r1885[3] = r1887;
              O r1868 = ((object*) lenv) [2];
              switch (get_case (r1868)) {
                case (UOTAG(1)): { // symbol
                  O r1882 = ((object*) lenv) [2];
                  O r1871 = UOBJ_GET(r1882,0);
                  O r1874 = r1871; // reg varref
                  O r1875 = ((object****) lenv) [1][1][1][2];
                  if IRK_IS_TRUE(IRK_TEST(r1874==r1875)) {
                    O r1877 = ((object*) lenv) [3];
                    IRK_RETURN(1877);
                  } else {
                    O r1880 = ((object****) lenv) [1][1][1][2];
                    O r1881 = ((object***) lenv) [1][1][3];
                    lenv = ((object ***)lenv)[1][1][1];
                    lenv[2] = r1880;
                    lenv[3] = r1881;
                    FUN_search_sexp_list_3006_i130_i159_i160_i165_0();
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_105();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_105();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_105();
        }
      }
    } break;
    default: {
      FAIL_105();
    }
  }
}
static void FAIL_105() {
  // env 214:
  //   |Metadata/KeyError=symbol ...t240|
  O r1905 = alloc_cleared (TC_ENV, 2);
  O r1907 = allocate (UOTAG(2), 1);
  O r1908 = ((object*) lenv) [2];
  r1907[1] = r1908;
  r1905[2] = r1907;
  O r1906 = top[3];
  r1905[1] = r1906[2]; lenv = r1905; ((pfun)(r1906[1]))();
}
static void JUMP_88(O r1559) {
  // env 215:
  //   |...t29| int |tuple=(product int int int) | ident
  O r1596 = alloc_cleared (TC_ENV, 5);
  r1596[1] = lenv; lenv = r1596;
  O r1597 = ((object**) lenv) [1][2];
  r1596[2] = r1597;
  O r1704 = ((object*) lenv) [2];
  O r1598 = TAG_INTEGER((irk_int)unbox(irk_object2int(r1704)));
  r1596[3] = r1598;
  O r1702 = (object *) 3;
  O r1703 = ((object*) lenv) [3];
  O r1698 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1702)&UNTAG_INTEGER(r1703));
  O r1700 = (object *) 3;
  O r1701 = r1698; // reg varref
  O r1699 = IRK_TEST(UNTAG_INTEGER(r1700)==UNTAG_INTEGER(r1701));
  O r1664 = r1699; // reg varref
  if IRK_IS_TRUE(r1664) {
    O r1665 = allocate (UOTAG(4), 3);
    O r1669 = ((object*) lenv) [3];
    O r1670 = (object *) 3;
    O r1666 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1669)>>UNTAG_INTEGER(r1670));
    r1665[1] = r1666;
    O r1667 = (object *) 1;
    r1665[2] = r1667;
    O r1668 = (object *) 1;
    r1665[3] = r1668;
    O r1599 = r1665;
    JUMP_86(r1599, r1596, r1559);
  } else {
    O r1696 = (object *) 7;
    O r1697 = ((object*) lenv) [3];
    O r1692 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1696)&UNTAG_INTEGER(r1697));
    O r1694 = r1692; // reg varref
    O r1695 = (object *) 1;
    O r1693 = IRK_TEST(UNTAG_INTEGER(r1694)>UNTAG_INTEGER(r1695));
    O r1672 = r1693; // reg varref
    if IRK_IS_TRUE(r1672) {
      O r1673 = allocate (UOTAG(4), 3);
      O r1679 = ((object*) lenv) [3];
      O r1680 = top[32];
      O r1674 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1679)>>UNTAG_INTEGER(r1680));
      r1673[1] = r1674;
      O r1675 = (object *) 1;
      r1673[2] = r1675;
      O r1677 = top[33];
      O r1678 = ((object*) lenv) [3];
      O r1676 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1677)&UNTAG_INTEGER(r1678));
      r1673[3] = r1676;
      O r1671 = r1673;
      JUMP_87(r1671, r1596, r1559);
    } else {
      O r1691 = ((object*) lenv) [2];
      O r1682 = TAG_INTEGER((irk_int)unbox(irk_objectptr2int(r1691)));
      O r1683 = allocate (UOTAG(4), 3);
      O r1684 = (object *) 1;
      r1683[1] = r1684;
      O r1689 = r1682; // reg varref
      O r1690 = top[32];
      O r1685 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1689)>>UNTAG_INTEGER(r1690));
      r1683[2] = r1685;
      O r1687 = top[33];
      O r1688 = r1682; // reg varref
      O r1686 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1687)&UNTAG_INTEGER(r1688));
      r1683[3] = r1686;
      O r1681 = r1683; // reg varref
      O r1671 = r1681;
      JUMP_87(r1671, r1596, r1559);
    }
  }
}
static void JUMP_87(O r1671, O r1596, O r1559) {
  O r1599 = r1671;
  JUMP_86(r1599, r1596, r1559);
}
static void JUMP_86(O r1599, O r1596, O r1559) {
  r1596[4] = r1599;
  // env 216:
  //   int int int
  O r1657 = alloc_cleared (TC_ENV, 4);
  r1657[1] = lenv; lenv = r1657;
  O r1663 = ((object**) lenv) [1][4];
  O r1658 = UOBJ_GET(r1663,0);
  r1657[2] = r1658;
  O r1662 = ((object**) lenv) [1][4];
  O r1659 = UOBJ_GET(r1662,1);
  r1657[3] = r1659;
  O r1661 = ((object**) lenv) [1][4];
  O r1660 = UOBJ_GET(r1661,2);
  r1657[4] = r1660;
  O r1651 = ((object*) lenv) [3];
  O r1652 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1651)==UNTAG_INTEGER(r1652))) {
    O r1654 = ((object*) lenv) [4];
    O r1655 = (object *) 1;
    O r1653 = IRK_TEST(UNTAG_INTEGER(r1654)==UNTAG_INTEGER(r1655));
    O r1602 = r1653;
    JUMP_85(r1602, r1596, r1559);
  } else {
    O r1656 = (object *) 6;
    O r1602 = r1656;
    JUMP_85(r1602, r1596, r1559);
  }
}
static void JUMP_85(O r1602, O r1596, O r1559) {
  if IRK_IS_TRUE(r1602) {
    O r1604 = ((object*) lenv) [2];
    O r1603 = alloc_no_clear (UOTAG(0),1);
    r1603[1] = r1604;
    O r1601 = r1603;
    JUMP_76(r1601, r1596, r1559);
  } else {
    O r1645 = ((object*) lenv) [3];
    O r1646 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1645)>UNTAG_INTEGER(r1646))) {
      O r1648 = ((object*) lenv) [2];
      O r1649 = (object *) 1;
      O r1647 = IRK_TEST(UNTAG_INTEGER(r1648)==UNTAG_INTEGER(r1649));
      O r1606 = r1647;
      JUMP_83(r1606, r1596, r1559);
    } else {
      O r1650 = (object *) 6;
      O r1606 = r1650;
      JUMP_83(r1606, r1596, r1559);
    }
  }
}
static void JUMP_83(O r1606, O r1596, O r1559) {
  if IRK_IS_TRUE(r1606) {
    O r1608 = ((object*) lenv) [4];
    O r1609 = (object *) 81;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1608)>=UNTAG_INTEGER(r1609))) {
      O r1617 = ((object*) lenv) [4];
      O r1618 = (object *) 5;
      O r1613 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1617)>>UNTAG_INTEGER(r1618));
      O r1615 = r1613; // reg varref
      O r1616 = (object *) 21;
      O r1614 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1615)-UNTAG_INTEGER(r1616));
      O r1611 = r1614; // reg varref
      O r1612 = ((object*) lenv) [3];
      O r1610 = alloc_no_clear (UOTAG(2),2);
      r1610[1] = r1611;
      r1610[2] = r1612;
      O r1607 = r1610;
      JUMP_79(r1607, r1596, r1559);
    } else {
      O r1622 = top[36];
      O r1624 = ((object*) lenv) [4];
      O r1625 = (object *) 5;
      O r1623 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1624)>>UNTAG_INTEGER(r1625));
      range_check (GET_TUPLE_LENGTH(*(object*)r1622), unbox(r1623));
      O r1620 = ((irk_vector*)r1622)->val[unbox(r1623)];
      O r1621 = ((object*) lenv) [3];
      O r1619 = alloc_no_clear (UOTAG(4),2);
      r1619[1] = r1620;
      r1619[2] = r1621;
      O r1607 = r1619;
      JUMP_79(r1607, r1596, r1559);
    }
  } else {
    O r1627 = ((object*) lenv) [4];
    O r1628 = (object *) 45;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r1627)>=UNTAG_INTEGER(r1628))) {
      O r1636 = ((object*) lenv) [4];
      O r1637 = (object *) 5;
      O r1632 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1636)>>UNTAG_INTEGER(r1637));
      O r1634 = r1632; // reg varref
      O r1635 = (object *) 11;
      O r1633 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1634)-UNTAG_INTEGER(r1635));
      O r1630 = r1633; // reg varref
      O r1631 = ((object*) lenv) [2];
      O r1629 = alloc_no_clear (UOTAG(1),2);
      r1629[1] = r1630;
      r1629[2] = r1631;
      O r1626 = r1629;
      JUMP_81(r1626, r1596, r1559);
    } else {
      O r1641 = top[35];
      O r1643 = ((object*) lenv) [4];
      O r1644 = (object *) 5;
      O r1642 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r1643)>>UNTAG_INTEGER(r1644));
      range_check (GET_TUPLE_LENGTH(*(object*)r1641), unbox(r1642));
      O r1639 = ((irk_vector*)r1641)->val[unbox(r1642)];
      O r1640 = ((object*) lenv) [2];
      O r1638 = alloc_no_clear (UOTAG(3),2);
      r1638[1] = r1639;
      r1638[2] = r1640;
      O r1626 = r1638;
      JUMP_81(r1626, r1596, r1559);
    }
  }
}
static void JUMP_81(O r1626, O r1596, O r1559) {
  O r1605 = r1626;
  JUMP_77(r1605, r1596, r1559);
}
static void JUMP_79(O r1607, O r1596, O r1559) {
  O r1605 = r1607;
  JUMP_77(r1605, r1596, r1559);
}
static void JUMP_77(O r1605, O r1596, O r1559) {
  O r1601 = r1605;
  JUMP_76(r1601, r1596, r1559);
}
static void JUMP_76(O r1601, O r1596, O r1559) {
  lenv = lenv[1];
  O r1600 = r1601;
  r1596[5] = r1600;
  O r1563 = ((object*) lenv) [5];
  switch (get_case (r1563)) {
    case (UOTAG(1)): { // userimm
      O r1571 = ((object*) lenv) [5];
      O r1565 = UOBJ_GET(r1571,0);
      O r1570 = ((object*) lenv) [5];
      O r1566 = UOBJ_GET(r1570,1);
      O r1568 = top[34];
      O r1569 = r1566; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r1568), unbox(r1569));
      O r1567 = ((irk_vector*)r1568)->val[unbox(r1569)];
      O r1564 = r1567; // reg varref
      O r1562 = r1564;
      JUMP_74(r1562, r1559);
    } break;
    case (UOTAG(2)): { // usertup
      O r1579 = ((object*) lenv) [5];
      O r1573 = UOBJ_GET(r1579,0);
      O r1578 = ((object*) lenv) [5];
      O r1574 = UOBJ_GET(r1578,1);
      O r1576 = top[34];
      O r1577 = r1573; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r1576), unbox(r1577));
      O r1575 = ((irk_vector*)r1576)->val[unbox(r1577)];
      O r1572 = r1575; // reg varref
      O r1562 = r1572;
      JUMP_74(r1562, r1559);
    } break;
    case (UOTAG(3)): { // sysimm
      O r1585 = ((object*) lenv) [5];
      O r1581 = UOBJ_GET(r1585,0);
      O r1584 = ((object*) lenv) [5];
      O r1582 = UOBJ_GET(r1584,1);
      O r1583 = r1581; // reg varref
      O r1580 = r1583; // reg varref
      O r1562 = r1580;
      JUMP_74(r1562, r1559);
    } break;
    case (UOTAG(4)): { // systup
      O r1591 = ((object*) lenv) [5];
      O r1587 = UOBJ_GET(r1591,0);
      O r1590 = ((object*) lenv) [5];
      O r1588 = UOBJ_GET(r1590,1);
      O r1589 = r1587; // reg varref
      O r1586 = r1589; // reg varref
      O r1562 = r1586;
      JUMP_74(r1562, r1559);
    } break;
    default: {
      O r1595 = ((object*) lenv) [5];
      O r1593 = UOBJ_GET(r1595,0);
      O r1594 = (object*) constructed_31[0];
      O r1592 = r1594; // reg varref
      O r1562 = r1592;
      JUMP_74(r1562, r1559);
    } break;
  }
}
static void JUMP_74(O r1562, O r1559) {
  lenv = lenv[1];
  O r1561 = r1562;
  r1559[3] = r1561;
  // env 217:
  //   string
  O r1547 = alloc_cleared (TC_ENV, 2);
  r1547[1] = lenv; lenv = r1547;
  // env 218:
  //   (list string)
  O r1549 = alloc_cleared (TC_ENV, 2);
  O r1552 = (O) &constructed_29;
  O r1558 = ((object**) lenv) [1][3];
  O r1556 = UOBJ_GET(r1558,0);
  O r1557 = r1556; // reg varref
  O r1554 = r1557; // reg varref
  O r1555 = (object*) constructed_28[0];
  O r1553 = alloc_no_clear (TC_PAIR,2);
  r1553[1] = r1554;
  r1553[2] = r1555;
  O r1551 = alloc_no_clear (TC_PAIR,2);
  r1551[1] = r1552;
  r1551[2] = r1553;
  r1549[2] = r1551;
  O r1550 = top[17];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_search_sexp_list_3006_i130_i159_i160_i165_135; t[4] = r1547; k = t;
  r1549[1] = r1550[2]; lenv = r1549; FUN_string_concat_333_0();
}
static void FUN_search_sexp_list_3006_i130_i159_i160_i165_135 (O rr) {
  O r1547 = k[4]; lenv = k[2]; k = k[1];
  O r1548 = rr;
  r1547[2] = r1548;
  O r1544 = ((object*) lenv) [2];
  O r1546 = ((object*) lenv) [2];
  O r1545 = TAG_INTEGER((irk_int)((irk_string*)(r1546))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1544))->data, 1, UNTAG_INTEGER(r1545), stdout));
  O r1543 = (object *) 14;
  lenv = lenv[1];
  O r1538 = (O) &constructed_26;
  O r1540 = r1538; // reg varref
  O r1542 = r1538; // reg varref
  O r1541 = TAG_INTEGER((irk_int)((irk_string*)(r1542))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r1540))->data, 1, UNTAG_INTEGER(r1541), stdout));
  O r1539 = (object *) 14;
  O r1537 = ((object*) lenv) [2];
  dump_object (r1537, 0);
  O r1536 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r1536), stdout));
  // dead  = (object *) 14;
  O r1535 = (object *) 14;
  lenv = lenv[1];
  O r1534 = (object *) -1;
  exit_continuation(r1534);
  O r1533 = (object *) TC_UNDEFINED;
  IRK_RETURN(1533);
}
static void FUN_raise_system_error_519_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r1918 = TAG_INTEGER((irk_int)errno);
  O r1926 = r1918; // reg varref
  O r1919 = (make_foreign((void*)strerror(UNTAG_INTEGER(r1926))));
  // env 219:
  //   |OSError=int ...t201|
  O r1922 = alloc_cleared (TC_ENV, 2);
  O r1924 = allocate (UOTAG(1), 1);
  O r1925 = r1918; // reg varref
  r1924[1] = r1925;
  r1922[2] = r1924;
  O r1923 = top[3];
  r1922[1] = r1923[2]; lenv = r1922; ((pfun)(r1923[1]))();
}
static void FUN_repr_string_355_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 220:
  //   (list string)
  O r1929 = alloc_cleared (TC_ENV, 2);
  O r1932 = (O) &constructed_69;
  // env 221:
  //   (list string)
  O r1936 = alloc_cleared (TC_ENV, 2);
  // env 222:
  //   (char -> string) (list char)
  O r1939 = alloc_cleared (TC_ENV, 3);
  O r1941 = top[29];
  r1939[2] = r1941;
  // env 223:
  //   string
  O r1943 = allocate (TC_ENV, 2);
  O r1945 = ((object*) lenv) [2];
  r1943[2] = r1945;
  O r1944 = top[21];
  O t = allocate (TC_SAVE, 7);
  t[1] = k; t[2] = lenv; t[3] = FUN_repr_string_355_136; t[4] = r1929; t[5] = r1932; t[6] = r1936; t[7] = r1939; k = t;
  r1943[1] = r1944[2]; lenv = r1943; FUN_string__3elist_348_0();
}
static void FUN_repr_string_355_136 (O rr) {
  O r1929 = k[4]; O r1932 = k[5]; O r1936 = k[6]; O r1939 = k[7]; lenv = k[2]; k = k[1];
  O r1942 = rr;
  r1939[3] = r1942;
  O r1940 = top[10];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_repr_string_355_137; t[4] = r1929; t[5] = r1932; t[6] = r1936; k = t;
  r1939[1] = r1940[2]; lenv = r1939; FUN_map_287_0();
}
static void FUN_repr_string_355_137 (O rr) {
  O r1929 = k[4]; O r1932 = k[5]; O r1936 = k[6]; lenv = k[2]; k = k[1];
  O r1938 = rr;
  r1936[2] = r1938;
  O r1937 = top[17];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_repr_string_355_138; t[4] = r1929; t[5] = r1932; k = t;
  r1936[1] = r1937[2]; lenv = r1936; FUN_string_concat_333_0();
}
static void FUN_repr_string_355_138 (O rr) {
  O r1929 = k[4]; O r1932 = k[5]; lenv = k[2]; k = k[1];
  O r1934 = rr;
  O r1935 = (object*) constructed_70[0];
  O r1933 = alloc_no_clear (TC_PAIR,2);
  r1933[1] = r1934;
  r1933[2] = r1935;
  O r1931 = alloc_no_clear (TC_PAIR,2);
  r1931[1] = r1932;
  r1931[2] = r1933;
  r1929[2] = r1931;
  O r1930 = top[17];
  r1929[1] = r1930[2]; lenv = r1929; FUN_string_concat_333_0();
}
static void FUN_safe_char_354_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=114 label=113
  O r1950 = ((object*) lenv) [2];
  O r1951 = (object *) 6029314;
  if IRK_IS_TRUE(IRK_TEST(r1950==r1951)) {
    O r1953 = (O) &constructed_71;
    IRK_RETURN(1953);
  } else {
    FAIL_113();
  }
}
static void FAIL_113() {
  // fatbar jn=118 label=117
  O r1959 = ((object*) lenv) [2];
  O r1960 = (object *) 2228226;
  if IRK_IS_TRUE(IRK_TEST(r1959==r1960)) {
    O r1962 = (O) &constructed_72;
    IRK_RETURN(1962);
  } else {
    FAIL_117();
  }
}
static void FAIL_117() {
  // fatbar jn=122 label=121
  O r1968 = ((object*) lenv) [2];
  O r1969 = (object *) 851970;
  if IRK_IS_TRUE(IRK_TEST(r1968==r1969)) {
    O r1971 = (O) &constructed_73;
    IRK_RETURN(1971);
  } else {
    FAIL_121();
  }
}
static void FAIL_121() {
  // fatbar jn=126 label=125
  O r1977 = ((object*) lenv) [2];
  O r1978 = (object *) 589826;
  if IRK_IS_TRUE(IRK_TEST(r1977==r1978)) {
    O r1980 = (O) &constructed_74;
    IRK_RETURN(1980);
  } else {
    FAIL_125();
  }
}
static void FAIL_125() {
  // fatbar jn=130 label=129
  O r1986 = ((object*) lenv) [2];
  O r1987 = (object *) 655362;
  if IRK_IS_TRUE(IRK_TEST(r1986==r1987)) {
    O r1989 = (O) &constructed_75;
    IRK_RETURN(1989);
  } else {
    FAIL_129();
  }
}
static void FAIL_129() {
  // fatbar jn=134 label=133
  O r1995 = ((object*) lenv) [2];
  O r1996 = (object *) 2097154;
  if IRK_IS_TRUE(IRK_TEST(r1995==r1996)) {
    O r1998 = (O) &constructed_9;
    IRK_RETURN(1998);
  } else {
    FAIL_133();
  }
}
static void FAIL_133() {
  // env 224:
  //   char
  O r2109 = allocate (TC_ENV, 2);
  O r2111 = ((object*) lenv) [2];
  r2109[2] = r2111;
  O r2110 = top[27];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_354_139; ; k = t;
  r2109[1] = r2110[2]; lenv = r2109; ((pfun)(r2110[1]))();
}
static void FUN_safe_char_354_139 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2002 = rr;
  if IRK_IS_TRUE(r2002) {
    // env 225:
    //   char
    O r2005 = allocate (TC_ENV, 2);
    O r2007 = ((object*) lenv) [2];
    r2005[2] = r2007;
    O r2006 = top[15];
    r2005[1] = r2006[2]; lenv = r2005; FUN_char__3estring_328_0();
  } else {
    // env 226:
    //   (list string)
    O r2010 = alloc_cleared (TC_ENV, 2);
    O r2013 = (O) &constructed_76;
    // env 227:
    //   int string int string char int
    O r2058 = alloc_cleared (TC_ENV, 7);
    r2058[1] = lenv; lenv = r2058;
    O r2108 = ((object**) lenv) [1][2];
    O r2059 = TAG_INTEGER((irk_int)GET_CHAR(r2108));
    r2058[2] = r2059;
    // env 228:
    //   (int (list char) -> string)
    O r2079 = alloc_cleared (TC_ENV, 2);
    r2079[1] = lenv; lenv = r2079;
    O r2080 = allocate (TC_CLOSURE, 2);
    r2080[1] = FUN_loop_1443_i2_0; r2080[2] = lenv;
    r2079[2] = r2080;
    // env 229:
    //   int (list char)
    O r2067 = alloc_cleared (TC_ENV, 3);
    O r2071 = ((object**) lenv) [1][2];
    O r2073 = r2071; // reg varref
    O r2074 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2073)<UNTAG_INTEGER(r2074))) {
      O r2076 = (object *) 1;
      O r2077 = r2071; // reg varref
      O r2075 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2076)-UNTAG_INTEGER(r2077));
      O r2072 = r2075;
      JUMP_143(r2072, r2067, r2058, r2013, r2010);
    } else {
      O r2078 = r2071; // reg varref
      O r2072 = r2078;
      JUMP_143(r2072, r2067, r2058, r2013, r2010);
    }
  }
}
static void JUMP_143(O r2072, O r2067, O r2058, O r2013, O r2010) {
  O r2069 = r2072; // reg varref
  r2067[2] = r2069;
  O r2070 = (object *) 10;
  r2067[3] = r2070;
  O r2068 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_354_140; t[4] = r2010; t[5] = r2013; t[6] = r2058; k = t;
  r2067[1] = r2068[2]; lenv = r2067; FUN_loop_1443_i2_0();
}
static void FUN_safe_char_354_140 (O rr) {
  O r2010 = k[4]; O r2013 = k[5]; O r2058 = k[6]; lenv = k[2]; k = k[1];
  O r2066 = rr;
  lenv = lenv[1];
  O r2060 = r2066;
  r2058[3] = r2060;
  O r2061 = (object *) 5;
  r2058[4] = r2061;
  O r2062 = ((object*) lenv) [3];
  r2058[5] = r2062;
  O r2063 = (object *) 3145730;
  r2058[6] = r2063;
  O r2065 = ((object*) lenv) [5];
  O r2064 = TAG_INTEGER((irk_int)((irk_string*)(r2065))->len);
  r2058[7] = r2064;
  O r2018 = ((object*) lenv) [7];
  O r2019 = ((object*) lenv) [4];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2018)>UNTAG_INTEGER(r2019))) {
    O r2020 = ((object*) lenv) [5];
    O r2017 = r2020;
    JUMP_139(r2017, r2013, r2010);
  } else {
    // env 230:
    //   int
    O r2054 = allocate (TC_ENV, 2);
    r2054[1] = lenv; lenv = r2054;
    O r2056 = ((object**) lenv) [1][4];
    O r2057 = ((object**) lenv) [1][7];
    O r2055 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2056)-UNTAG_INTEGER(r2057));
    r2054[2] = r2055;
    // env 231:
    //   (list string)
    O r2023 = alloc_cleared (TC_ENV, 2);
    // env 232:
    //   (list char)
    O r2030 = alloc_cleared (TC_ENV, 2);
    // env 233:
    //   (int (list char) -> (list char))
    O r2038 = alloc_cleared (TC_ENV, 2);
    r2038[1] = lenv; lenv = r2038;
    O r2039 = allocate (TC_CLOSURE, 2);
    r2039[1] = FUN_loop_1041_i0_0; r2039[2] = lenv;
    r2038[2] = r2039;
    // env 234:
    //   int (list t683)
    O r2034 = allocate (TC_ENV, 3);
    O r2036 = ((object**) lenv) [1][2];
    r2034[2] = r2036;
    O r2037 = (object *) 10;
    r2034[3] = r2037;
    O r2035 = ((object*) lenv) [2];
    O t = allocate (TC_SAVE, 7);
    t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_354_141; t[4] = r2010; t[5] = r2013; t[6] = r2023; t[7] = r2030; k = t;
    r2034[1] = r2035[2]; lenv = r2034; ((pfun)(r2035[1]))();
  }
}
static void FUN_safe_char_354_141 (O rr) {
  O r2010 = k[4]; O r2013 = k[5]; O r2023 = k[6]; O r2030 = k[7]; lenv = k[2]; k = k[1];
  O r2033 = rr;
  lenv = lenv[1];
  O r2032 = r2033;
  r2030[2] = r2032;
  O r2031 = top[20];
  O t = allocate (TC_SAVE, 6);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_354_142; t[4] = r2010; t[5] = r2013; t[6] = r2023; k = t;
  r2030[1] = r2031[2]; lenv = r2030; FUN_list__3estring_347_0();
}
static void FUN_safe_char_354_142 (O rr) {
  O r2010 = k[4]; O r2013 = k[5]; O r2023 = k[6]; lenv = k[2]; k = k[1];
  O r2026 = rr;
  O r2028 = ((object**) lenv) [1][5];
  O r2029 = (object *) 10;
  O r2027 = alloc_no_clear (TC_PAIR,2);
  r2027[1] = r2028;
  r2027[2] = r2029;
  O r2025 = alloc_no_clear (TC_PAIR,2);
  r2025[1] = r2026;
  r2025[2] = r2027;
  r2023[2] = r2025;
  O r2024 = top[17];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_safe_char_354_143; t[4] = r2010; t[5] = r2013; k = t;
  r2023[1] = r2024[2]; lenv = r2023; FUN_string_concat_333_0();
}
static void FUN_safe_char_354_143 (O rr) {
  O r2010 = k[4]; O r2013 = k[5]; lenv = k[2]; k = k[1];
  O r2022 = rr;
  lenv = lenv[1];
  O r2021 = r2022;
  O r2017 = r2021;
  JUMP_139(r2017, r2013, r2010);
}
static void FUN_loop_1041_i0_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2042 = ((object*) lenv) [2];
  O r2043 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2042)<=UNTAG_INTEGER(r2043))) {
    O r2045 = ((object*) lenv) [3];
    IRK_RETURN(2045);
  } else {
    O r2052 = ((object*) lenv) [2];
    O r2053 = (object *) 3;
    O r2048 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2052)-UNTAG_INTEGER(r2053));
    O r2050 = ((object****) lenv) [1][1][1][6];
    O r2051 = ((object*) lenv) [3];
    O r2049 = alloc_no_clear (TC_PAIR,2);
    r2049[1] = r2050;
    r2049[2] = r2051;
    lenv[2] = r2048;
    lenv[3] = r2049;
    FUN_loop_1041_i0_0();
  }
}
static void JUMP_139(O r2017, O r2013, O r2010) {
  lenv = lenv[1];
  O r2015 = r2017;
  O r2016 = (object *) 10;
  O r2014 = alloc_no_clear (TC_PAIR,2);
  r2014[1] = r2015;
  r2014[2] = r2016;
  O r2012 = alloc_no_clear (TC_PAIR,2);
  r2012[1] = r2013;
  r2012[2] = r2014;
  r2010[2] = r2012;
  O r2011 = top[17];
  r2010[1] = r2011[2]; lenv = r2010; FUN_string_concat_333_0();
}
static void FUN_loop_1443_i2_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2083 = (object *) 1;
  O r2084 = ((object*) lenv) [2];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2083)==UNTAG_INTEGER(r2084))) {
    // env 235:
    //   (list char)
    O r2087 = alloc_cleared (TC_ENV, 2);
    O r2090 = ((object***) lenv) [1][1][2];
    O r2091 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2090)<UNTAG_INTEGER(r2091))) {
      O r2093 = (object *) 2949122;
      O r2094 = ((object*) lenv) [3];
      O r2092 = alloc_no_clear (TC_PAIR,2);
      r2092[1] = r2093;
      r2092[2] = r2094;
      O r2089 = r2092;
      JUMP_147(r2089, r2087);
    } else {
      O r2095 = ((object*) lenv) [3];
      O r2089 = r2095;
      JUMP_147(r2089, r2087);
    }
  } else {
    O r2106 = ((object*) lenv) [2];
    O r2107 = (object *) 9;
    O r2098 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2106)>>UNTAG_INTEGER(r2107));
    O r2102 = top[28];
    O r2104 = ((object*) lenv) [2];
    O r2105 = (object *) 31;
    O r2103 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2104)&UNTAG_INTEGER(r2105));
    range_check (GET_TUPLE_LENGTH(*(object*)r2102), unbox(r2103));
    O r2100 = ((irk_vector*)r2102)->val[unbox(r2103)];
    O r2101 = ((object*) lenv) [3];
    O r2099 = alloc_no_clear (TC_PAIR,2);
    r2099[1] = r2100;
    r2099[2] = r2101;
    lenv[2] = r2098;
    lenv[3] = r2099;
    FUN_loop_1443_i2_0();
  }
}
static void JUMP_147(O r2089, O r2087) {
  r2087[2] = r2089;
  O r2088 = top[20];
  r2087[1] = r2088[2]; lenv = r2087; FUN_list__3estring_347_0();
}
static void FUN__24loop31_1371_i6_0 (void) {
  O r2122 = ((object*) lenv) [2];
  switch (get_case (r2122)) {
    case (TC_NIL): { // nil
      O r2124 = (object *) 14;
      IRK_RETURN(2124);
    } break;
    default: {
      O r2138 = ((object*) lenv) [2];
      O r2127 = UOBJ_GET(r2138,0);
      O r2137 = ((object*) lenv) [2];
      O r2128 = UOBJ_GET(r2137,1);
      O r2133 = ((object****) lenv) [1][1][1][3];
      O r2136 = r2127; // reg varref
      O r2134 = TAG_INTEGER((irk_int)GET_CHAR(r2136));
      O r2135 = (object *) 65542;
      range_check (GET_TUPLE_LENGTH(*(object*)r2133), unbox(r2134));
      ((irk_vector*)r2133)->val[unbox(r2134)] = r2135;
      O r2132 = r2128; // reg varref
      lenv[2] = r2132;
      FUN__24loop31_1371_i6_0();
    } break;
  }
}
static void FUN_in_class_question_1369_i6_0 (void) {
  O r2143 = ((object***) lenv) [1][1][3];
  O r2145 = ((object*) lenv) [2];
  O r2144 = TAG_INTEGER((irk_int)GET_CHAR(r2145));
  range_check (GET_TUPLE_LENGTH(*(object*)r2143), unbox(r2144));
  O r2142 = ((irk_vector*)r2143)->val[unbox(r2144)];
  IRK_RETURN(2142);
}
static void FUN_string__3elist_348_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 236:
  //   ((list char) int -> (list char))
  O r2201 = alloc_cleared (TC_ENV, 2);
  r2201[1] = lenv; lenv = r2201;
  O r2202 = allocate (TC_CLOSURE, 2);
  r2202[1] = FUN_loop_1364_0; r2202[2] = lenv;
  r2201[2] = r2202;
  // env 237:
  //   (list char) int
  O r2196 = allocate (TC_ENV, 3);
  O r2198 = (object *) 10;
  r2196[2] = r2198;
  O r2200 = ((object**) lenv) [1][2];
  O r2199 = TAG_INTEGER((irk_int)((irk_string*)(r2200))->len);
  r2196[3] = r2199;
  O r2197 = ((object*) lenv) [2];
  r2196[1] = r2197[2]; lenv = r2196; FUN_loop_1364_0();
}
static void FUN_loop_1364_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2205 = ((object*) lenv) [3];
  O r2206 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2205)==UNTAG_INTEGER(r2206))) {
    O r2208 = ((object*) lenv) [2];
    IRK_RETURN(2208);
  } else {
    // env 238:
    //   string int
    O r2217 = allocate (TC_ENV, 3);
    O r2219 = ((object***) lenv) [1][1][2];
    r2217[2] = r2219;
    O r2221 = ((object*) lenv) [3];
    O r2222 = (object *) 3;
    O r2220 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2221)-UNTAG_INTEGER(r2222));
    r2217[3] = r2220;
    O r2218 = top[16];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1364_144; ; k = t;
    r2217[1] = r2218[2]; lenv = r2217; FUN_string_ref_331_0();
  }
}
static void FUN_loop_1364_144 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2215 = rr;
  O r2216 = ((object*) lenv) [2];
  O r2211 = alloc_no_clear (TC_PAIR,2);
  r2211[1] = r2215;
  r2211[2] = r2216;
  O r2213 = ((object*) lenv) [3];
  O r2214 = (object *) 3;
  O r2212 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2213)-UNTAG_INTEGER(r2214));
  lenv[2] = r2211;
  lenv[3] = r2212;
  FUN_loop_1364_0();
}
static void FUN_list__3estring_347_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 239:
  //   int int string int
  O r2260 = alloc_cleared (TC_ENV, 5);
  r2260[1] = lenv; lenv = r2260;
  // env 240:
  //   (list char) int
  O r2268 = allocate (TC_ENV, 3);
  O r2270 = ((object**) lenv) [1][2];
  r2268[2] = r2270;
  O r2271 = (object *) 1;
  r2268[3] = r2271;
  O r2269 = top[8];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_list__3estring_347_145; t[4] = r2260; k = t;
  r2268[1] = r2269[2]; lenv = r2268; FUN_length_splat_267_0();
}
static void FUN_list__3estring_347_145 (O rr) {
  O r2260 = k[4]; lenv = k[2]; k = k[1];
  O r2261 = rr;
  r2260[2] = r2261;
  O r2262 = ((object*) lenv) [2];
  r2260[3] = r2262;
  O r2267 = ((object*) lenv) [3];
  O r2266 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r2267)));
  if (freep + unbox(r2266) >= limit) {
    heap_request = unbox(r2266);
    heap1[3] = r2260;
    gc_flip (1);
    r2260 = heap0[3];
  }
  O r2265 = ((object*) lenv) [3];
  O r2263 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r2265))), ((irk_string*)(t))->len = UNTAG_INTEGER(r2265), t);
  r2260[4] = r2263;
  O r2264 = (object *) 1;
  r2260[5] = r2264;
  // env 241:
  //   ((list char) -> undefined)
  O r2232 = alloc_cleared (TC_ENV, 2);
  r2232[1] = lenv; lenv = r2232;
  O r2233 = allocate (TC_CLOSURE, 2);
  r2233[1] = FUN__24loop28_1359_0; r2233[2] = lenv;
  r2232[2] = r2233;
  // env 242:
  //   (list char)
  O r2229 = allocate (TC_ENV, 2);
  O r2231 = ((object***) lenv) [1][1][2];
  r2229[2] = r2231;
  O r2230 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_list__3estring_347_146; ; k = t;
  r2229[1] = r2230[2]; lenv = r2229; FUN__24loop28_1359_0();
}
static void FUN_list__3estring_347_146 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2228 = rr;
  lenv = lenv[1];
  O r2227 = ((object*) lenv) [4];
  IRK_RETURN(2227);
}
static void FUN__24loop28_1359_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2236 = ((object*) lenv) [2];
  switch (get_case (r2236)) {
    case (TC_NIL): { // nil
      O r2238 = (object *) 14;
      IRK_RETURN(2238);
    } break;
    default: {
      // env 243:
      //   char (list char)
      O r2255 = alloc_cleared (TC_ENV, 3);
      r2255[1] = lenv; lenv = r2255;
      O r2259 = ((object**) lenv) [1][2];
      O r2256 = UOBJ_GET(r2259,0);
      r2255[2] = r2256;
      O r2258 = ((object**) lenv) [1][2];
      O r2257 = UOBJ_GET(r2258,1);
      r2255[3] = r2257;
      // env 244:
      //   string int char
      O r2250 = allocate (TC_ENV, 4);
      O r2252 = ((object****) lenv) [1][1][1][4];
      r2250[2] = r2252;
      O r2253 = ((object****) lenv) [1][1][1][5];
      r2250[3] = r2253;
      O r2254 = ((object*) lenv) [2];
      r2250[4] = r2254;
      O r2251 = top[14];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN__24loop28_1359_147; ; k = t;
      r2250[1] = r2251[2]; lenv = r2250; FUN_string_set_bang_332_0();
    } break;
  }
}
static void FUN__24loop28_1359_147 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2246 = ((object****) lenv) [1][1][1][5];
  O r2248 = r2246; // reg varref
  O r2249 = (object *) 3;
  O r2247 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2248)+UNTAG_INTEGER(r2249));
  O r2245 = r2247; // reg varref
  ((object****) lenv) [1][1][1][5] = r2245;
  O r2244 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r2244;
  FUN__24loop28_1359_0();
}
static void FUN_ends_with_342_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 245:
  //   int int
  O r2331 = allocate (TC_ENV, 3);
  r2331[1] = lenv; lenv = r2331;
  O r2335 = ((object**) lenv) [1][2];
  O r2332 = TAG_INTEGER((irk_int)((irk_string*)(r2335))->len);
  r2331[2] = r2332;
  O r2334 = ((object**) lenv) [1][3];
  O r2333 = TAG_INTEGER((irk_int)((irk_string*)(r2334))->len);
  r2331[3] = r2333;
  O r2276 = ((object*) lenv) [2];
  O r2277 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2276)<UNTAG_INTEGER(r2277))) {
    O r2279 = (object *) 6;
    IRK_RETURN(2279);
  } else {
    // env 246:
    //   (int -> bool)
    O r2287 = alloc_cleared (TC_ENV, 2);
    r2287[1] = lenv; lenv = r2287;
    O r2288 = allocate (TC_CLOSURE, 2);
    r2288[1] = FUN_loop_1344_0; r2288[2] = lenv;
    r2287[2] = r2288;
    // env 247:
    //   int
    O r2284 = allocate (TC_ENV, 2);
    O r2286 = (object *) 1;
    r2284[2] = r2286;
    O r2285 = ((object*) lenv) [2];
    r2284[1] = r2285[2]; lenv = r2284; FUN_loop_1344_0();
  }
}
static void FUN_loop_1344_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2291 = ((object*) lenv) [2];
  O r2292 = ((object***) lenv) [1][1][3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2291)==UNTAG_INTEGER(r2292))) {
    O r2294 = (object *) 65542;
    IRK_RETURN(2294);
  } else {
    // env 248:
    //   char char
    O r2308 = alloc_cleared (TC_ENV, 3);
    r2308[1] = lenv; lenv = r2308;
    // env 249:
    //   string int
    O r2321 = alloc_cleared (TC_ENV, 3);
    O r2323 = ((object*****) lenv) [1][1][1][1][2];
    r2321[2] = r2323;
    O r2329 = (object *) 3;
    O r2330 = ((object**) lenv) [1][2];
    O r2325 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2329)+UNTAG_INTEGER(r2330));
    O r2327 = ((object****) lenv) [1][1][1][2];
    O r2328 = r2325; // reg varref
    O r2326 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2327)-UNTAG_INTEGER(r2328));
    O r2324 = r2326; // reg varref
    r2321[3] = r2324;
    O r2322 = top[16];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1344_148; t[4] = r2308; k = t;
    r2321[1] = r2322[2]; lenv = r2321; FUN_string_ref_331_0();
  }
}
static void FUN_loop_1344_148 (O rr) {
  O r2308 = k[4]; lenv = k[2]; k = k[1];
  O r2309 = rr;
  r2308[2] = r2309;
  // env 250:
  //   string int
  O r2311 = alloc_cleared (TC_ENV, 3);
  O r2313 = ((object*****) lenv) [1][1][1][1][3];
  r2311[2] = r2313;
  O r2319 = (object *) 3;
  O r2320 = ((object**) lenv) [1][2];
  O r2315 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2319)+UNTAG_INTEGER(r2320));
  O r2317 = ((object****) lenv) [1][1][1][3];
  O r2318 = r2315; // reg varref
  O r2316 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2317)-UNTAG_INTEGER(r2318));
  O r2314 = r2316; // reg varref
  r2311[3] = r2314;
  O r2312 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1344_149; t[4] = r2308; k = t;
  r2311[1] = r2312[2]; lenv = r2311; FUN_string_ref_331_0();
}
static void FUN_loop_1344_149 (O rr) {
  O r2308 = k[4]; lenv = k[2]; k = k[1];
  O r2310 = rr;
  r2308[3] = r2310;
  O r2306 = ((object*) lenv) [2];
  O r2307 = ((object*) lenv) [3];
  O r2305 = IRK_TEST(r2306==r2307);
  lenv = lenv[1];
  O r2297 = r2305;
  if IRK_IS_TRUE(r2297) {
    O r2301 = ((object*) lenv) [2];
    O r2302 = (object *) 3;
    O r2300 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2301)+UNTAG_INTEGER(r2302));
    lenv[2] = r2300;
    FUN_loop_1344_0();
  } else {
    O r2304 = (object *) 6;
    IRK_RETURN(2304);
  }
}
static void FUN_starts_with_341_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 251:
  //   int int
  O r2383 = allocate (TC_ENV, 3);
  r2383[1] = lenv; lenv = r2383;
  O r2387 = ((object**) lenv) [1][2];
  O r2384 = TAG_INTEGER((irk_int)((irk_string*)(r2387))->len);
  r2383[2] = r2384;
  O r2386 = ((object**) lenv) [1][3];
  O r2385 = TAG_INTEGER((irk_int)((irk_string*)(r2386))->len);
  r2383[3] = r2385;
  O r2340 = ((object*) lenv) [2];
  O r2341 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2340)<UNTAG_INTEGER(r2341))) {
    O r2343 = (object *) 6;
    IRK_RETURN(2343);
  } else {
    // env 252:
    //   (int -> bool)
    O r2351 = alloc_cleared (TC_ENV, 2);
    r2351[1] = lenv; lenv = r2351;
    O r2352 = allocate (TC_CLOSURE, 2);
    r2352[1] = FUN_loop_1338_0; r2352[2] = lenv;
    r2351[2] = r2352;
    // env 253:
    //   int
    O r2348 = allocate (TC_ENV, 2);
    O r2350 = (object *) 1;
    r2348[2] = r2350;
    O r2349 = ((object*) lenv) [2];
    r2348[1] = r2349[2]; lenv = r2348; FUN_loop_1338_0();
  }
}
static void FUN_loop_1338_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2355 = ((object*) lenv) [2];
  O r2356 = ((object***) lenv) [1][1][3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2355)==UNTAG_INTEGER(r2356))) {
    O r2358 = (object *) 65542;
    IRK_RETURN(2358);
  } else {
    // env 254:
    //   char char
    O r2372 = alloc_cleared (TC_ENV, 3);
    r2372[1] = lenv; lenv = r2372;
    // env 255:
    //   string int
    O r2379 = allocate (TC_ENV, 3);
    O r2381 = ((object*****) lenv) [1][1][1][1][2];
    r2379[2] = r2381;
    O r2382 = ((object**) lenv) [1][2];
    r2379[3] = r2382;
    O r2380 = top[16];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1338_150; t[4] = r2372; k = t;
    r2379[1] = r2380[2]; lenv = r2379; FUN_string_ref_331_0();
  }
}
static void FUN_loop_1338_150 (O rr) {
  O r2372 = k[4]; lenv = k[2]; k = k[1];
  O r2373 = rr;
  r2372[2] = r2373;
  // env 256:
  //   string int
  O r2375 = allocate (TC_ENV, 3);
  O r2377 = ((object*****) lenv) [1][1][1][1][3];
  r2375[2] = r2377;
  O r2378 = ((object**) lenv) [1][2];
  r2375[3] = r2378;
  O r2376 = top[16];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_1338_151; t[4] = r2372; k = t;
  r2375[1] = r2376[2]; lenv = r2375; FUN_string_ref_331_0();
}
static void FUN_loop_1338_151 (O rr) {
  O r2372 = k[4]; lenv = k[2]; k = k[1];
  O r2374 = rr;
  r2372[3] = r2374;
  O r2370 = ((object*) lenv) [2];
  O r2371 = ((object*) lenv) [3];
  O r2369 = IRK_TEST(r2370==r2371);
  lenv = lenv[1];
  O r2361 = r2369;
  if IRK_IS_TRUE(r2361) {
    O r2365 = ((object*) lenv) [2];
    O r2366 = (object *) 3;
    O r2364 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2365)+UNTAG_INTEGER(r2366));
    lenv[2] = r2364;
    FUN_loop_1338_0();
  } else {
    O r2368 = (object *) 6;
    IRK_RETURN(2368);
  }
}
static void FUN_string_concat_333_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=163 label=162
  O r2392 = ((object*) lenv) [2];
  switch (get_case (r2392)) {
    case (TC_NIL): { // nil
      O r2394 = (O) &constructed_17;
      IRK_RETURN(2394);
    } break;
    default: {
      O r2407 = ((object*) lenv) [2];
      O r2397 = UOBJ_GET(r2407,0);
      O r2406 = ((object*) lenv) [2];
      O r2398 = UOBJ_GET(r2406,1);
      O r2401 = r2398; // reg varref
      switch (get_case (r2401)) {
        case (TC_NIL): { // nil
          O r2403 = r2397; // reg varref
          IRK_RETURN(2403);
        } break;
        default: {
          FAIL_162();
        }
      }
    } break;
  }
}
static void FAIL_162() {
  // env 257:
  //   int int string
  O r2449 = alloc_cleared (TC_ENV, 4);
  r2449[1] = lenv; lenv = r2449;
  // env 258:
  //   (string int -> int) int (list string)
  O r2456 = alloc_cleared (TC_ENV, 4);
  O r2458 = allocate (TC_CLOSURE, 2);
  r2458[1] = FUN_lambda_3898_0; r2458[2] = lenv;
  r2456[2] = r2458;
  O r2459 = (object *) 1;
  r2456[3] = r2459;
  O r2460 = ((object**) lenv) [1][2];
  r2456[4] = r2460;
  O r2457 = top[11];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_string_concat_333_152; t[4] = r2449; k = t;
  r2456[1] = r2457[2]; lenv = r2456; FUN_fold_294_0();
}
static void FUN_string_concat_333_152 (O rr) {
  O r2449 = k[4]; lenv = k[2]; k = k[1];
  O r2450 = rr;
  r2449[2] = r2450;
  O r2451 = ((object*) lenv) [2];
  r2449[3] = r2451;
  O r2455 = ((object*) lenv) [3];
  O r2454 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r2455)));
  if (freep + unbox(r2454) >= limit) {
    heap_request = unbox(r2454);
    heap1[3] = r2449;
    gc_flip (1);
    r2449 = heap0[3];
  }
  O r2453 = ((object*) lenv) [3];
  O r2452 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r2453))), ((irk_string*)(t))->len = UNTAG_INTEGER(r2453), t);
  r2449[4] = r2452;
  // env 259:
  //   ((list string) int -> string)
  O r2417 = alloc_cleared (TC_ENV, 2);
  r2417[1] = lenv; lenv = r2417;
  O r2418 = allocate (TC_CLOSURE, 2);
  r2418[1] = FUN_loop_1292_0; r2418[2] = lenv;
  r2417[2] = r2418;
  // env 260:
  //   (list string) int
  O r2413 = allocate (TC_ENV, 3);
  O r2415 = ((object***) lenv) [1][1][2];
  r2413[2] = r2415;
  O r2416 = (object *) 1;
  r2413[3] = r2416;
  O r2414 = ((object*) lenv) [2];
  r2413[1] = r2414[2]; lenv = r2413; FUN_loop_1292_0();
}
static void FUN_loop_1292_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2421 = ((object*) lenv) [2];
  switch (get_case (r2421)) {
    case (TC_NIL): { // nil
      O r2423 = ((object***) lenv) [1][1][4];
      IRK_RETURN(2423);
    } break;
    default: {
      // env 261:
      //   string (list string)
      O r2444 = alloc_cleared (TC_ENV, 3);
      r2444[1] = lenv; lenv = r2444;
      O r2448 = ((object**) lenv) [1][2];
      O r2445 = UOBJ_GET(r2448,0);
      r2444[2] = r2445;
      O r2447 = ((object**) lenv) [1][2];
      O r2446 = UOBJ_GET(r2447,1);
      r2444[3] = r2446;
      // env 262:
      //   string int int string int
      O r2436 = allocate (TC_ENV, 6);
      O r2438 = ((object*) lenv) [2];
      r2436[2] = r2438;
      O r2439 = (object *) 1;
      r2436[3] = r2439;
      O r2443 = ((object*) lenv) [2];
      O r2440 = TAG_INTEGER((irk_int)((irk_string*)(r2443))->len);
      r2436[4] = r2440;
      O r2441 = ((object****) lenv) [1][1][1][4];
      r2436[5] = r2441;
      O r2442 = ((object**) lenv) [1][3];
      r2436[6] = r2442;
      O r2437 = top[12];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_loop_1292_153; ; k = t;
      r2436[1] = r2437[2]; lenv = r2436; FUN_buffer_copy_325_0();
    } break;
  }
}
static void FUN_loop_1292_153 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2429 = ((object*) lenv) [3];
  O r2435 = ((object*) lenv) [2];
  O r2431 = TAG_INTEGER((irk_int)((irk_string*)(r2435))->len);
  O r2433 = ((object**) lenv) [1][3];
  O r2434 = r2431; // reg varref
  O r2432 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2433)+UNTAG_INTEGER(r2434));
  O r2430 = r2432; // reg varref
  lenv = ((object *)lenv)[1];
  lenv[2] = r2429;
  lenv[3] = r2430;
  FUN_loop_1292_0();
}
static void FUN_lambda_3898_0 (void) {
  O r2468 = ((object*) lenv) [2];
  O r2463 = TAG_INTEGER((irk_int)((irk_string*)(r2468))->len);
  O r2466 = r2463; // reg varref
  O r2467 = ((object*) lenv) [3];
  O r2465 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2466)+UNTAG_INTEGER(r2467));
  IRK_RETURN(2465);
}
static void FUN_string_ref_331_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2487 = (object *) 1;
  O r2488 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2487)<=UNTAG_INTEGER(r2488))) {
    O r2494 = ((object*) lenv) [2];
    O r2490 = TAG_INTEGER((irk_int)((irk_string*)(r2494))->len);
    O r2492 = ((object*) lenv) [3];
    O r2493 = r2490; // reg varref
    O r2491 = IRK_TEST(UNTAG_INTEGER(r2492)<UNTAG_INTEGER(r2493));
    O r2489 = r2491; // reg varref
    O r2483 = r2489;
    JUMP_169(r2483);
  } else {
    O r2495 = (object *) 6;
    O r2483 = r2495;
    JUMP_169(r2483);
  }
}
static void JUMP_169(O r2483) {
  O r2485 = r2483; // reg varref
  O r2486 = (object *) 6;
  O r2484 = IRK_TEST(r2485==r2486);
  O r2474 = r2484; // reg varref
  if IRK_IS_TRUE(r2474) {
    // env 263:
    //   |String/Range=(product string int int) ...t145|
    O r2476 = alloc_cleared (TC_ENV, 2);
    O r2478 = allocate (UOTAG(0), 3);
    O r2479 = ((object*) lenv) [2];
    r2478[1] = r2479;
    O r2480 = ((object*) lenv) [3];
    r2478[2] = r2480;
    O r2481 = ((object*) lenv) [3];
    r2478[3] = r2481;
    r2476[2] = r2478;
    O r2477 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_string_ref_331_154; ; k = t;
    r2476[1] = r2477[2]; lenv = r2476; ((pfun)(r2477[1]))();
  } else {
    O r2482 = (object *) 14;
    JUMP_167();
  }
}
static void FUN_string_ref_331_154 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2475 = rr;
  JUMP_167();
}
static void JUMP_167() {
  O r2472 = ((object*) lenv) [2];
  O r2473 = ((object*) lenv) [3];
  O r2471 = TO_CHAR(((unsigned char *)((irk_string*)(r2472))->data)[UNTAG_INTEGER(r2473)]);
  IRK_RETURN(2471);
}
static void FUN_char__3estring_328_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 264:
  //   int string
  O r2506 = alloc_cleared (TC_ENV, 3);
  r2506[1] = lenv; lenv = r2506;
  O r2507 = (object *) 3;
  r2506[2] = r2507;
  O r2511 = ((object*) lenv) [2];
  O r2510 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r2511)));
  if (freep + unbox(r2510) >= limit) {
    heap_request = unbox(r2510);
    heap1[3] = r2506;
    gc_flip (1);
    r2506 = heap0[3];
  }
  O r2509 = ((object*) lenv) [2];
  O r2508 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r2509))), ((irk_string*)(t))->len = UNTAG_INTEGER(r2509), t);
  r2506[3] = r2508;
  // env 265:
  //   string int char
  O r2501 = allocate (TC_ENV, 4);
  O r2503 = ((object*) lenv) [3];
  r2501[2] = r2503;
  O r2504 = (object *) 1;
  r2501[3] = r2504;
  O r2505 = ((object**) lenv) [1][2];
  r2501[4] = r2505;
  O r2502 = top[14];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_char__3estring_328_155; ; k = t;
  r2501[1] = r2502[2]; lenv = r2501; FUN_string_set_bang_332_0();
}
static void FUN_char__3estring_328_155 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2500 = ((object*) lenv) [3];
  IRK_RETURN(2500);
}
static void FUN_string_set_bang_332_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2531 = (object *) 1;
  O r2532 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2531)<=UNTAG_INTEGER(r2532))) {
    O r2538 = ((object*) lenv) [2];
    O r2534 = TAG_INTEGER((irk_int)((irk_string*)(r2538))->len);
    O r2536 = ((object*) lenv) [3];
    O r2537 = r2534; // reg varref
    O r2535 = IRK_TEST(UNTAG_INTEGER(r2536)<UNTAG_INTEGER(r2537));
    O r2533 = r2535; // reg varref
    O r2527 = r2533;
    JUMP_172(r2527);
  } else {
    O r2539 = (object *) 6;
    O r2527 = r2539;
    JUMP_172(r2527);
  }
}
static void JUMP_172(O r2527) {
  O r2529 = r2527; // reg varref
  O r2530 = (object *) 6;
  O r2528 = IRK_TEST(r2529==r2530);
  O r2518 = r2528; // reg varref
  if IRK_IS_TRUE(r2518) {
    // env 266:
    //   |String/Range=(product string int int) ...t160|
    O r2520 = alloc_cleared (TC_ENV, 2);
    O r2522 = allocate (UOTAG(0), 3);
    O r2523 = ((object*) lenv) [2];
    r2522[1] = r2523;
    O r2524 = ((object*) lenv) [3];
    r2522[2] = r2524;
    O r2525 = ((object*) lenv) [3];
    r2522[3] = r2525;
    r2520[2] = r2522;
    O r2521 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_string_set_bang_332_156; ; k = t;
    r2520[1] = r2521[2]; lenv = r2520; ((pfun)(r2521[1]))();
  } else {
    O r2526 = (object *) 14;
    JUMP_170();
  }
}
static void FUN_string_set_bang_332_156 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2519 = rr;
  JUMP_170();
}
static void JUMP_170() {
  O r2515 = ((object*) lenv) [2];
  O r2516 = ((object*) lenv) [3];
  O r2517 = ((object*) lenv) [4];
  ((irk_string*)(r2515))->data[UNTAG_INTEGER(r2516)] = GET_CHAR (r2517);
  O r2514 = (object *) 14;
  IRK_RETURN(2514);
}
static void FUN_substring_327_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 267:
  //   int int string
  O r2552 = alloc_cleared (TC_ENV, 4);
  r2552[1] = lenv; lenv = r2552;
  O r2559 = ((object**) lenv) [1][4];
  O r2560 = ((object**) lenv) [1][3];
  O r2553 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2559)-UNTAG_INTEGER(r2560));
  r2552[2] = r2553;
  O r2554 = ((object*) lenv) [2];
  r2552[3] = r2554;
  O r2558 = ((object*) lenv) [3];
  O r2557 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r2558)));
  if (freep + unbox(r2557) >= limit) {
    heap_request = unbox(r2557);
    heap1[3] = r2552;
    gc_flip (1);
    r2552 = heap0[3];
  }
  O r2556 = ((object*) lenv) [3];
  O r2555 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r2556))), ((irk_string*)(t))->len = UNTAG_INTEGER(r2556), t);
  r2552[4] = r2555;
  // env 268:
  //   string int int string int
  O r2545 = allocate (TC_ENV, 6);
  O r2547 = ((object**) lenv) [1][2];
  r2545[2] = r2547;
  O r2548 = ((object**) lenv) [1][3];
  r2545[3] = r2548;
  O r2549 = ((object*) lenv) [2];
  r2545[4] = r2549;
  O r2550 = ((object*) lenv) [4];
  r2545[5] = r2550;
  O r2551 = (object *) 1;
  r2545[6] = r2551;
  O r2546 = top[12];
  O t = allocate (TC_SAVE, 3);
  t[1] = k; t[2] = lenv; t[3] = FUN_substring_327_157; ; k = t;
  r2545[1] = r2546[2]; lenv = r2545; FUN_buffer_copy_325_0();
}
static void FUN_substring_327_157 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2544 = ((object*) lenv) [4];
  IRK_RETURN(2544);
}
static void FUN_buffer_copy_325_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 269:
  //   bool
  O r2620 = alloc_cleared (TC_ENV, 2);
  r2620[1] = lenv; lenv = r2620;
  O r2622 = (object *) 1;
  O r2623 = ((object**) lenv) [1][3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2622)<=UNTAG_INTEGER(r2623))) {
    O r2640 = ((object**) lenv) [1][3];
    O r2641 = ((object**) lenv) [1][4];
    O r2636 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2640)+UNTAG_INTEGER(r2641));
    O r2638 = ((object**) lenv) [1][3];
    O r2639 = r2636; // reg varref
    O r2637 = IRK_TEST(UNTAG_INTEGER(r2638)<=UNTAG_INTEGER(r2639));
    O r2625 = r2637; // reg varref
    if IRK_IS_TRUE(r2625) {
      O r2633 = ((object**) lenv) [1][3];
      O r2634 = ((object**) lenv) [1][4];
      O r2627 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2633)+UNTAG_INTEGER(r2634));
      O r2632 = ((object**) lenv) [1][2];
      O r2628 = TAG_INTEGER((irk_int)((irk_string*)(r2632))->len);
      O r2630 = r2627; // reg varref
      O r2631 = r2628; // reg varref
      O r2629 = IRK_TEST(UNTAG_INTEGER(r2630)<=UNTAG_INTEGER(r2631));
      O r2626 = r2629; // reg varref
      O r2624 = r2626;
      JUMP_180(r2624, r2620);
    } else {
      O r2635 = (object *) 6;
      O r2624 = r2635;
      JUMP_180(r2624, r2620);
    }
  } else {
    O r2642 = (object *) 6;
    O r2621 = r2642;
    JUMP_179(r2621, r2620);
  }
}
static void JUMP_180(O r2624, O r2620) {
  O r2621 = r2624;
  JUMP_179(r2621, r2620);
}
static void JUMP_179(O r2621, O r2620) {
  r2620[2] = r2621;
  O r2618 = ((object*) lenv) [2];
  O r2619 = (object *) 6;
  O r2617 = IRK_TEST(r2618==r2619);
  lenv = lenv[1];
  O r2606 = r2617;
  if IRK_IS_TRUE(r2606) {
    // env 270:
    //   |String/Range=(product string int int) ...t118|
    O r2608 = alloc_cleared (TC_ENV, 2);
    O r2610 = allocate (UOTAG(0), 3);
    O r2611 = ((object*) lenv) [2];
    r2610[1] = r2611;
    O r2612 = ((object*) lenv) [3];
    r2610[2] = r2612;
    O r2614 = ((object*) lenv) [3];
    O r2615 = ((object*) lenv) [4];
    O r2613 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2614)+UNTAG_INTEGER(r2615));
    r2610[3] = r2613;
    r2608[2] = r2610;
    O r2609 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_buffer_copy_325_158; ; k = t;
    r2608[1] = r2609[2]; lenv = r2608; ((pfun)(r2609[1]))();
  } else {
    O r2616 = (object *) 14;
    JUMP_177();
  }
}
static void FUN_buffer_copy_325_158 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2607 = rr;
  JUMP_177();
}
static void JUMP_177() {
  // env 271:
  //   bool
  O r2583 = alloc_cleared (TC_ENV, 2);
  r2583[1] = lenv; lenv = r2583;
  O r2585 = (object *) 1;
  O r2586 = ((object**) lenv) [1][6];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2585)<=UNTAG_INTEGER(r2586))) {
    O r2603 = ((object**) lenv) [1][6];
    O r2604 = ((object**) lenv) [1][4];
    O r2599 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2603)+UNTAG_INTEGER(r2604));
    O r2601 = ((object**) lenv) [1][6];
    O r2602 = r2599; // reg varref
    O r2600 = IRK_TEST(UNTAG_INTEGER(r2601)<=UNTAG_INTEGER(r2602));
    O r2588 = r2600; // reg varref
    if IRK_IS_TRUE(r2588) {
      O r2596 = ((object**) lenv) [1][6];
      O r2597 = ((object**) lenv) [1][4];
      O r2590 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2596)+UNTAG_INTEGER(r2597));
      O r2595 = ((object**) lenv) [1][5];
      O r2591 = TAG_INTEGER((irk_int)((irk_string*)(r2595))->len);
      O r2593 = r2590; // reg varref
      O r2594 = r2591; // reg varref
      O r2592 = IRK_TEST(UNTAG_INTEGER(r2593)<=UNTAG_INTEGER(r2594));
      O r2589 = r2592; // reg varref
      O r2587 = r2589;
      JUMP_176(r2587, r2583);
    } else {
      O r2598 = (object *) 6;
      O r2587 = r2598;
      JUMP_176(r2587, r2583);
    }
  } else {
    O r2605 = (object *) 6;
    O r2584 = r2605;
    JUMP_175(r2584, r2583);
  }
}
static void JUMP_176(O r2587, O r2583) {
  O r2584 = r2587;
  JUMP_175(r2584, r2583);
}
static void JUMP_175(O r2584, O r2583) {
  r2583[2] = r2584;
  O r2581 = ((object*) lenv) [2];
  O r2582 = (object *) 6;
  O r2580 = IRK_TEST(r2581==r2582);
  lenv = lenv[1];
  O r2569 = r2580;
  if IRK_IS_TRUE(r2569) {
    // env 272:
    //   |String/Range=(product string int int) ...t131|
    O r2571 = alloc_cleared (TC_ENV, 2);
    O r2573 = allocate (UOTAG(0), 3);
    O r2574 = ((object*) lenv) [5];
    r2573[1] = r2574;
    O r2575 = ((object*) lenv) [6];
    r2573[2] = r2575;
    O r2577 = ((object*) lenv) [6];
    O r2578 = ((object*) lenv) [4];
    O r2576 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2577)+UNTAG_INTEGER(r2578));
    r2573[3] = r2576;
    r2571[2] = r2573;
    O r2572 = top[3];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_buffer_copy_325_159; ; k = t;
    r2571[1] = r2572[2]; lenv = r2571; ((pfun)(r2572[1]))();
  } else {
    O r2579 = (object *) 14;
    JUMP_173();
  }
}
static void FUN_buffer_copy_325_159 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2570 = rr;
  JUMP_173();
}
static void JUMP_173() {
  O r2564 = ((object*) lenv) [5];
  O r2565 = ((object*) lenv) [6];
  O r2566 = ((object*) lenv) [2];
  O r2567 = ((object*) lenv) [3];
  O r2568 = ((object*) lenv) [4];
  O r2563 = memcpy (((irk_string*)(r2564))->data+UNTAG_INTEGER(r2565), ((irk_string*)(r2566))->data+UNTAG_INTEGER(r2567), UNTAG_INTEGER(r2568));
  IRK_RETURN(2563);
}
static void FUN_fold_294_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2645 = ((object*) lenv) [4];
  switch (get_case (r2645)) {
    case (TC_NIL): { // nil
      O r2647 = ((object*) lenv) [3];
      IRK_RETURN(2647);
    } break;
    default: {
      // env 273:
      //   t166 (list t166)
      O r2659 = alloc_cleared (TC_ENV, 3);
      r2659[1] = lenv; lenv = r2659;
      O r2663 = ((object**) lenv) [1][4];
      O r2660 = UOBJ_GET(r2663,0);
      r2659[2] = r2660;
      O r2662 = ((object**) lenv) [1][4];
      O r2661 = UOBJ_GET(r2662,1);
      r2659[3] = r2661;
      O r2652 = ((object**) lenv) [1][2];
      // env 274:
      //   t166 t163
      O r2655 = allocate (TC_ENV, 3);
      O r2657 = ((object*) lenv) [2];
      r2655[2] = r2657;
      O r2658 = ((object**) lenv) [1][3];
      r2655[3] = r2658;
      O r2656 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_fold_294_160; t[4] = r2652; k = t;
      r2655[1] = r2656[2]; lenv = r2655; ((pfun)(r2656[1]))();
    } break;
  }
}
static void FUN_fold_294_160 (O rr) {
  O r2652 = k[4]; lenv = k[2]; k = k[1];
  O r2653 = rr;
  O r2654 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r2652;
  lenv[3] = r2653;
  lenv[4] = r2654;
  FUN_fold_294_0();
}
static void FUN_map_287_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2666 = ((object*) lenv) [3];
  switch (get_case (r2666)) {
    case (TC_NIL): { // nil
      O r2668 = (object *) 10;
      IRK_RETURN(2668);
    } break;
    default: {
      // env 275:
      //   t523 (list t523)
      O r2682 = alloc_cleared (TC_ENV, 3);
      r2682[1] = lenv; lenv = r2682;
      O r2686 = ((object**) lenv) [1][3];
      O r2683 = UOBJ_GET(r2686,0);
      r2682[2] = r2683;
      O r2685 = ((object**) lenv) [1][3];
      O r2684 = UOBJ_GET(r2685,1);
      r2682[3] = r2684;
      // env 276:
      //   t523
      O r2679 = allocate (TC_ENV, 2);
      O r2681 = ((object*) lenv) [2];
      r2679[2] = r2681;
      O r2680 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 3);
      t[1] = k; t[2] = lenv; t[3] = FUN_map_287_161; ; k = t;
      r2679[1] = r2680[2]; lenv = r2679; ((pfun)(r2680[1]))();
    } break;
  }
}
static void FUN_map_287_161 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r2673 = rr;
  // env 277:
  //   (t523 -> t524) (list t523)
  O r2675 = allocate (TC_ENV, 3);
  O r2677 = ((object**) lenv) [1][2];
  r2675[2] = r2677;
  O r2678 = ((object*) lenv) [3];
  r2675[3] = r2678;
  O r2676 = top[10];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_map_287_162; t[4] = r2673; k = t;
  r2675[1] = r2676[2]; lenv = r2675; FUN_map_287_0();
}
static void FUN_map_287_162 (O rr) {
  O r2673 = k[4]; lenv = k[2]; k = k[1];
  O r2674 = rr;
  O r2672 = alloc_no_clear (TC_PAIR,2);
  r2672[1] = r2673;
  r2672[2] = r2674;
  IRK_RETURN(2672);
}
static void FUN_n_of_286_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 278:
  //   (int (list t683) -> (list t683))
  O r2695 = alloc_cleared (TC_ENV, 2);
  r2695[1] = lenv; lenv = r2695;
  O r2696 = allocate (TC_CLOSURE, 2);
  r2696[1] = FUN_loop_1041_0; r2696[2] = lenv;
  r2695[2] = r2696;
  // env 279:
  //   int (list t683)
  O r2691 = allocate (TC_ENV, 3);
  O r2693 = ((object**) lenv) [1][2];
  r2691[2] = r2693;
  O r2694 = (object *) 10;
  r2691[3] = r2694;
  O r2692 = ((object*) lenv) [2];
  r2691[1] = r2692[2]; lenv = r2691; FUN_loop_1041_0();
}
static void FUN_loop_1041_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2699 = ((object*) lenv) [2];
  O r2700 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2699)<=UNTAG_INTEGER(r2700))) {
    O r2702 = ((object*) lenv) [3];
    IRK_RETURN(2702);
  } else {
    O r2709 = ((object*) lenv) [2];
    O r2710 = (object *) 3;
    O r2705 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2709)-UNTAG_INTEGER(r2710));
    O r2707 = ((object***) lenv) [1][1][3];
    O r2708 = ((object*) lenv) [3];
    O r2706 = alloc_no_clear (TC_PAIR,2);
    r2706[1] = r2707;
    r2706[2] = r2708;
    lenv[2] = r2705;
    lenv[3] = r2706;
    FUN_loop_1041_0();
  }
}
static void FUN_length_splat_267_0 (void) {
  O r2713 = ((object*) lenv) [2];
  switch (get_case (r2713)) {
    case (TC_NIL): { // nil
      O r2715 = ((object*) lenv) [3];
      IRK_RETURN(2715);
    } break;
    default: {
      O r2725 = ((object*) lenv) [2];
      O r2718 = UOBJ_GET(r2725,1);
      O r2721 = r2718; // reg varref
      O r2723 = (object *) 3;
      O r2724 = ((object*) lenv) [3];
      O r2722 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2723)+UNTAG_INTEGER(r2724));
      lenv[2] = r2721;
      lenv[3] = r2722;
      FUN_length_splat_267_0();
    } break;
  }
}
static void FUN_reverse_onto_264_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2728 = ((object*) lenv) [2];
  switch (get_case (r2728)) {
    case (TC_NIL): { // nil
      O r2730 = ((object*) lenv) [3];
      IRK_RETURN(2730);
    } break;
    default: {
      O r2742 = ((object*) lenv) [2];
      O r2733 = UOBJ_GET(r2742,0);
      O r2741 = ((object*) lenv) [2];
      O r2734 = UOBJ_GET(r2741,1);
      O r2737 = r2734; // reg varref
      O r2739 = r2733; // reg varref
      O r2740 = ((object*) lenv) [3];
      O r2738 = alloc_no_clear (TC_PAIR,2);
      r2738[1] = r2739;
      r2738[2] = r2740;
      lenv[2] = r2737;
      lenv[3] = r2738;
      FUN_reverse_onto_264_0();
    } break;
  }
}
static void FUN_make_generator_246_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 280:
  //   bool (continuation (maybe t23)) (continuation undefined)
  O r2773 = alloc_cleared (TC_ENV, 4);
  r2773[1] = lenv; lenv = r2773;
  O r2774 = (object *) 6;
  r2773[2] = r2774;
  O r2775 = k; // %getcc
  r2773[3] = r2775;
  O r2776 = k; // %getcc
  r2773[4] = r2776;
  // env 281:
  //   ((maybe t23) -> t85) ( -> undefined)
  O r2749 = alloc_cleared (TC_ENV, 3);
  r2749[1] = lenv; lenv = r2749;
  O r2750 = allocate (TC_CLOSURE, 2);
  r2750[1] = FUN_yield_917_0; r2750[2] = lenv;
  r2749[2] = r2750;
  O r2751 = allocate (TC_CLOSURE, 2);
  r2751[1] = FUN_entry_point_916_0; r2751[2] = lenv;
  r2749[3] = r2751;
  O r2748 = ((object*) lenv) [3];
  IRK_RETURN(2748);
}
static void FUN_entry_point_916_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2765 = k; // %getcc
  ((object**) lenv) [1][3] = r2765;
  O r2755 = ((object**) lenv) [1][2];
  if IRK_IS_TRUE(r2755) {
    O r2758 = ((object**) lenv) [1][4];
    O r2759 = (object *) 14;
    k = r2758; // %putcc
    O r2757 = r2759;
    IRK_RETURN(2757);
  } else {
    // env 282:
    //   ((maybe t23) -> undefined)
    O r2762 = allocate (TC_ENV, 2);
    O r2764 = ((object*) lenv) [2];
    r2762[2] = r2764;
    O r2763 = ((object***) lenv) [1][1][2];
    r2762[1] = r2763[2]; lenv = r2762; ((pfun)(r2763[1]))();
  }
}
static void FUN_yield_917_0 (void) {
  O r2772 = k; // %getcc
  ((object***) lenv) [1][1][4] = r2772;
  O r2771 = (object *) 65542;
  ((object***) lenv) [1][1][2] = r2771;
  O r2769 = ((object***) lenv) [1][1][3];
  O r2770 = ((object*) lenv) [2];
  k = r2769; // %putcc
  O r2768 = r2770;
  IRK_RETURN(2768);
}
static void FUN_euclidean_quotient_196_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 283:
  //   int int |tuple=(product int int) |
  O r2788 = alloc_cleared (TC_ENV, 4);
  r2788[1] = lenv; lenv = r2788;
  O r2789 = ((object**) lenv) [1][2];
  r2788[2] = r2789;
  O r2790 = ((object**) lenv) [1][3];
  r2788[3] = r2790;
  O r2792 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2792)==0)) {
    // env 284:
    //   |DivideByZero=int ...t460|
    O r2794 = alloc_cleared (TC_ENV, 2);
    O r2796 = allocate (UOTAG(5), 1);
    O r2797 = ((object*) lenv) [2];
    r2796[1] = r2797;
    r2794[2] = r2796;
    O r2795 = top[3];
    O t = allocate (TC_SAVE, 4);
    t[1] = k; t[2] = lenv; t[3] = FUN_euclidean_quotient_196_163; t[4] = r2788; k = t;
    r2794[1] = r2795[2]; lenv = r2794; ((pfun)(r2795[1]))();
  } else {
    // env 285:
    //   int int cmp cmp
    O r2822 = allocate (TC_ENV, 5);
    r2822[1] = lenv; lenv = r2822;
    O r2833 = ((object**) lenv) [1][2];
    O r2834 = ((object**) lenv) [1][3];
    O r2823 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2833) / UNTAG_INTEGER(r2834));
    r2822[2] = r2823;
    O r2831 = ((object**) lenv) [1][2];
    O r2832 = ((object**) lenv) [1][3];
    O r2824 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2831) % UNTAG_INTEGER(r2832));
    r2822[3] = r2824;
    O r2829 = ((object**) lenv) [1][3];
    O r2830 = (object *) 1;
    O r2825 = (object*)(irk_int)UITAG((UNTAG_INTEGER(r2829) < UNTAG_INTEGER(r2830)) ? 0 : ((UNTAG_INTEGER(r2830) < UNTAG_INTEGER(r2829)) ? 2 : 1));
    r2822[4] = r2825;
    O r2827 = ((object*) lenv) [3];
    O r2828 = (object *) 1;
    O r2826 = (object*)(irk_int)UITAG((UNTAG_INTEGER(r2827) < UNTAG_INTEGER(r2828)) ? 0 : ((UNTAG_INTEGER(r2828) < UNTAG_INTEGER(r2827)) ? 2 : 1));
    r2822[5] = r2826;
    // fatbar jn=192 label=191
    O r2801 = ((object*) lenv) [5];
    switch (get_case (r2801)) {
      case (UITAG(0)): { // <
        // fatbar jn=195 label=194
        O r2804 = ((object*) lenv) [4];
        switch (get_case (r2804)) {
          case (UITAG(2)): { // >
            O r2805 = allocate (UOTAG(4), 2);
            O r2810 = ((object*) lenv) [2];
            O r2811 = (object *) 3;
            O r2806 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2810)-UNTAG_INTEGER(r2811));
            r2805[1] = r2806;
            O r2808 = ((object*) lenv) [3];
            O r2809 = ((object**) lenv) [1][3];
            O r2807 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2808)+UNTAG_INTEGER(r2809));
            r2805[2] = r2807;
            O r2803 = r2805;
            JUMP_196(r2803, r2788);
          } break;
          default: {
            FAIL_194(r2788);
          }
        }
      } break;
      default: {
        FAIL_191(r2788);
      }
    }
  }
}
static void JUMP_196(O r2803, O r2788) {
  O r2802 = r2803;
  JUMP_195(r2802, r2788);
}
static void JUMP_195(O r2802, O r2788) {
  O r2800 = r2802;
  JUMP_193(r2800, r2788);
}
static void FAIL_194(O r2788) {
  O r2803 = allocate (UOTAG(4), 2);
  O r2817 = ((object*) lenv) [2];
  O r2818 = (object *) 3;
  O r2813 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2817)+UNTAG_INTEGER(r2818));
  r2803[1] = r2813;
  O r2815 = ((object*) lenv) [3];
  O r2816 = ((object**) lenv) [1][3];
  O r2814 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2815)-UNTAG_INTEGER(r2816));
  r2803[2] = r2814;
  O r2802 = r2803;
  JUMP_195(r2802, r2788);
}
static void JUMP_193(O r2800, O r2788) {
  O r2799 = r2800;
  JUMP_192(r2799, r2788);
}
static void JUMP_192(O r2799, O r2788) {
  lenv = lenv[1];
  O r2798 = r2799;
  O r2791 = r2798;
  JUMP_190(r2791, r2788);
}
static void FAIL_191(O r2788) {
  O r2800 = allocate (UOTAG(4), 2);
  O r2820 = ((object*) lenv) [2];
  r2800[1] = r2820;
  O r2821 = ((object*) lenv) [3];
  r2800[2] = r2821;
  O r2799 = r2800;
  JUMP_192(r2799, r2788);
}
static void FUN_euclidean_quotient_196_163 (O rr) {
  O r2788 = k[4]; lenv = k[2]; k = k[1];
  O r2793 = rr;
  O r2791 = r2793;
  JUMP_190(r2791, r2788);
}
static void JUMP_190(O r2791, O r2788) {
  r2788[4] = r2791;
  O r2787 = ((object*) lenv) [4];
  O r2782 = UOBJ_GET(r2787,0);
  O r2786 = ((object*) lenv) [4];
  O r2783 = UOBJ_GET(r2786,1);
  O r2785 = r2782; // reg varref
  IRK_RETURN(2785);
}
static void FUN_truncate_remainder_191_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r2837 = ((object*) lenv) [3];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r2837)==0)) {
    // env 286:
    //   |DivideByZero=int ...t679|
    O r2840 = alloc_cleared (TC_ENV, 2);
    O r2842 = allocate (UOTAG(5), 1);
    O r2843 = ((object*) lenv) [2];
    r2842[1] = r2843;
    r2840[2] = r2842;
    O r2841 = top[3];
    r2840[1] = r2841[2]; lenv = r2840; ((pfun)(r2841[1]))();
  } else {
    O r2846 = ((object*) lenv) [2];
    O r2847 = ((object*) lenv) [3];
    O r2845 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r2846) % UNTAG_INTEGER(r2847));
    IRK_RETURN(2845);
  }
}
static void FUN_base_exception_handler_250_0 (void) {
  O r2869 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r2869), stdout));
  // dead  = (object *) 14;
  O r2864 = (O) &constructed_83;
  O r2866 = r2864; // reg varref
  O r2868 = r2864; // reg varref
  O r2867 = TAG_INTEGER((irk_int)((irk_string*)(r2868))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r2866))->data, 1, UNTAG_INTEGER(r2867), stdout));
  O r2865 = (object *) 14;
  O r2859 = (O) &constructed_9;
  O r2861 = r2859; // reg varref
  O r2863 = r2859; // reg varref
  O r2862 = TAG_INTEGER((irk_int)((irk_string*)(r2863))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r2861))->data, 1, UNTAG_INTEGER(r2862), stdout));
  O r2860 = (object *) 14;
  O r2854 = (O) &constructed_82;
  O r2856 = r2854; // reg varref
  O r2858 = r2854; // reg varref
  O r2857 = TAG_INTEGER((irk_int)((irk_string*)(r2858))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r2856))->data, 1, UNTAG_INTEGER(r2857), stdout));
  O r2855 = (object *) 14;
  O r2853 = ((object*) lenv) [2];
  dump_object (r2853, 0);
  O r2852 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r2852), stdout));
  // dead  = (object *) 14;
  O r2851 = (object *) -1;
  exit_continuation(r2851);
  O r2850 = (object *) TC_UNDEFINED;
  IRK_RETURN(2850);
}
object * irk_get_metadata (void) {
  return (object *) constructed_264[0];
}

uint32_t irk_ambig_size = 17;
int32_t G[] = {3, 0, 0, 0, 1, 0, 6, 0, -5, -14, -8, -16, 1, 0, -13, -17, -4, 0};
int32_t V[] = {1014, 1011, 1012, 1018, 1020, 1024, 1021, 1013, 1010, 1017, 1015, 1019, 1026, 1022, 1025, 1023, 1016, 0};
