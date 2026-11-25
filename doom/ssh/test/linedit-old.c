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
irk_string constructed_0 = {STRING_HEADER(11), 11, "   values: " };
irk_string constructed_1 = {STRING_HEADER(0), 0, "" };
irk_string constructed_2 = {STRING_HEADER(10), 10, "\" raised.\n" };
irk_int constructed_3[] = {UPTR(3,1),CONS_HEADER,UPTR0(2),TC_NIL};
irk_string constructed_4 = {STRING_HEADER(11), 11, "Exception \"" };
irk_string constructed_5 = {STRING_HEADER(3), 3, "int" };
// symbol int
irk_int constructed_6[] = {UPTR(6,1), SYMBOL_HEADER, UPTR0(5), INTCON(0)};
irk_string constructed_7 = {STRING_HEADER(7), 7, "unknown" };
// symbol unknown
irk_int constructed_8[] = {UPTR(8,1), SYMBOL_HEADER, UPTR0(7), INTCON(1)};
irk_string constructed_9 = {STRING_HEADER(8), 8, "variants" };
// symbol variants
irk_int constructed_10[] = {UPTR(10,1), SYMBOL_HEADER, UPTR0(9), INTCON(2)};
irk_string constructed_11 = {STRING_HEADER(8), 8, "metadata" };
// symbol metadata
irk_int constructed_12[] = {UPTR(12,1), SYMBOL_HEADER, UPTR0(11), INTCON(3)};
irk_string constructed_13 = {STRING_HEADER(4), 4, "none" };
// symbol none
irk_int constructed_14[] = {UPTR(14,1), SYMBOL_HEADER, UPTR0(13), INTCON(4)};
irk_string constructed_15 = {STRING_HEADER(4), 4, "save" };
// symbol save
irk_int constructed_16[] = {UPTR(16,1), SYMBOL_HEADER, UPTR0(15), INTCON(5)};
irk_string constructed_17 = {STRING_HEADER(7), 7, "closure" };
// symbol closure
irk_int constructed_18[] = {UPTR(18,1), SYMBOL_HEADER, UPTR0(17), INTCON(6)};
irk_string constructed_19 = {STRING_HEADER(3), 3, "env" };
// symbol env
irk_int constructed_20[] = {UPTR(20,1), SYMBOL_HEADER, UPTR0(19), INTCON(7)};
irk_string constructed_21 = {STRING_HEADER(6), 6, "string" };
// symbol string
irk_int constructed_22[] = {UPTR(22,1), SYMBOL_HEADER, UPTR0(21), INTCON(8)};
irk_string constructed_23 = {STRING_HEADER(6), 6, "vector" };
// symbol vector
irk_int constructed_24[] = {UPTR(24,1), SYMBOL_HEADER, UPTR0(23), INTCON(9)};
irk_string constructed_25 = {STRING_HEADER(4), 4, "pair" };
// symbol pair
irk_int constructed_26[] = {UPTR(26,1), SYMBOL_HEADER, UPTR0(25), INTCON(10)};
irk_string constructed_27 = {STRING_HEADER(6), 6, "symbol" };
// symbol symbol
irk_int constructed_28[] = {UPTR(28,1), SYMBOL_HEADER, UPTR0(27), INTCON(11)};
irk_string constructed_29 = {STRING_HEADER(6), 6, "buffer" };
// symbol buffer
irk_int constructed_30[] = {UPTR(30,1), SYMBOL_HEADER, UPTR0(29), INTCON(12)};
irk_string constructed_31 = {STRING_HEADER(7), 7, "foreign" };
// symbol foreign
irk_int constructed_32[] = {UPTR(32,1), SYMBOL_HEADER, UPTR0(31), INTCON(13)};
irk_int constructed_33[] = {UPTR(33,1),(10<<TAGSIZE)|TC_VECTOR,UPTR(14,1),UPTR(16,1),UPTR(18,1),UPTR(20,1),UPTR(22,1),UPTR(24,1),UPTR(26,1),UPTR(28,1),UPTR(30,1),
	UPTR(32,1)};
irk_string constructed_34 = {STRING_HEADER(4), 4, "char" };
// symbol char
irk_int constructed_35[] = {UPTR(35,1), SYMBOL_HEADER, UPTR0(34), INTCON(14)};
irk_string constructed_36 = {STRING_HEADER(4), 4, "bool" };
// symbol bool
irk_int constructed_37[] = {UPTR(37,1), SYMBOL_HEADER, UPTR0(36), INTCON(15)};
irk_string constructed_38 = {STRING_HEADER(3), 3, "nil" };
// symbol nil
irk_int constructed_39[] = {UPTR(39,1), SYMBOL_HEADER, UPTR0(38), INTCON(16)};
irk_string constructed_40 = {STRING_HEADER(9), 9, "undefined" };
// symbol undefined
irk_int constructed_41[] = {UPTR(41,1), SYMBOL_HEADER, UPTR0(40), INTCON(17)};
irk_string constructed_42 = {STRING_HEADER(12), 12, "empty-vector" };
// symbol empty-vector
irk_int constructed_43[] = {UPTR(43,1), SYMBOL_HEADER, UPTR0(42), INTCON(18)};
irk_int constructed_44[] = {UPTR(44,1),(5<<TAGSIZE)|TC_VECTOR,UPTR(35,1),UPTR(37,1),UPTR(39,1),UPTR(41,1),UPTR(43,1)};
irk_string constructed_45 = {STRING_HEADER(29), 29, "\n***\nRuntime Error, halting: " };
irk_string constructed_46 = {STRING_HEADER(1), 1, " " };
irk_string constructed_47 = {STRING_HEADER(18), 18, "uncaught exception" };
irk_string constructed_48 = {STRING_HEADER(9), 9, "datatypes" };
// symbol datatypes
irk_int constructed_49[] = {UPTR(49,1), SYMBOL_HEADER, UPTR0(48), INTCON(19)};
irk_string constructed_50 = {STRING_HEADER(1), 1, "t" };
// symbol t
irk_int constructed_51[] = {UPTR(51,1), SYMBOL_HEADER, UPTR0(50), INTCON(20)};
irk_string constructed_52 = {STRING_HEADER(3), 3, "cmp" };
// symbol cmp
irk_int constructed_53[] = {UPTR(53,1), SYMBOL_HEADER, UPTR0(52), INTCON(21)};
irk_string constructed_54 = {STRING_HEADER(1), 1, "<" };
// symbol <
irk_int constructed_55[] = {UPTR(55,1), SYMBOL_HEADER, UPTR0(54), INTCON(22)};
irk_string constructed_56 = {STRING_HEADER(1), 1, "=" };
// symbol =
irk_int constructed_57[] = {UPTR(57,1), SYMBOL_HEADER, UPTR0(56), INTCON(23)};
irk_string constructed_58 = {STRING_HEADER(1), 1, ">" };
// symbol >
irk_int constructed_59[] = {UPTR(59,1), SYMBOL_HEADER, UPTR0(58), INTCON(24)};
irk_string constructed_60 = {STRING_HEADER(5), 5, "maybe" };
// symbol maybe
irk_int constructed_61[] = {UPTR(61,1), SYMBOL_HEADER, UPTR0(60), INTCON(25)};
irk_string constructed_62 = {STRING_HEADER(3), 3, "yes" };
// symbol yes
irk_int constructed_63[] = {UPTR(63,1), SYMBOL_HEADER, UPTR0(62), INTCON(26)};
irk_string constructed_64 = {STRING_HEADER(5), 5, "quote" };
// symbol quote
irk_int constructed_65[] = {UPTR(65,1), SYMBOL_HEADER, UPTR0(64), INTCON(27)};
irk_string constructed_66 = {STRING_HEADER(1), 1, "a" };
// symbol a
irk_int constructed_67[] = {UPTR(67,1), SYMBOL_HEADER, UPTR0(66), INTCON(28)};
irk_string constructed_68 = {STRING_HEADER(2), 2, "no" };
// symbol no
irk_int constructed_69[] = {UPTR(69,1), SYMBOL_HEADER, UPTR0(68), INTCON(29)};
irk_string constructed_70 = {STRING_HEADER(4), 4, "true" };
// symbol true
irk_int constructed_71[] = {UPTR(71,1), SYMBOL_HEADER, UPTR0(70), INTCON(30)};
irk_string constructed_72 = {STRING_HEADER(5), 5, "false" };
// symbol false
irk_int constructed_73[] = {UPTR(73,1), SYMBOL_HEADER, UPTR0(72), INTCON(31)};
irk_string constructed_74 = {STRING_HEADER(6), 6, "result" };
// symbol result
irk_int constructed_75[] = {UPTR(75,1), SYMBOL_HEADER, UPTR0(74), INTCON(32)};
irk_string constructed_76 = {STRING_HEADER(2), 2, "ok" };
// symbol ok
irk_int constructed_77[] = {UPTR(77,1), SYMBOL_HEADER, UPTR0(76), INTCON(33)};
irk_string constructed_78 = {STRING_HEADER(3), 3, "err" };
// symbol err
irk_int constructed_79[] = {UPTR(79,1), SYMBOL_HEADER, UPTR0(78), INTCON(34)};
irk_string constructed_80 = {STRING_HEADER(1), 1, "b" };
// symbol b
irk_int constructed_81[] = {UPTR(81,1), SYMBOL_HEADER, UPTR0(80), INTCON(35)};
irk_string constructed_82 = {STRING_HEADER(4), 4, "list" };
// symbol list
irk_int constructed_83[] = {UPTR(83,1), SYMBOL_HEADER, UPTR0(82), INTCON(36)};
irk_string constructed_84 = {STRING_HEADER(4), 4, "cons" };
// symbol cons
irk_int constructed_85[] = {UPTR(85,1), SYMBOL_HEADER, UPTR0(84), INTCON(37)};
irk_string constructed_86 = {STRING_HEADER(4), 4, "sexp" };
// symbol sexp
irk_int constructed_87[] = {UPTR(87,1), SYMBOL_HEADER, UPTR0(86), INTCON(38)};
irk_string constructed_88 = {STRING_HEADER(3), 3, "src" };
// symbol src
irk_int constructed_89[] = {UPTR(89,1), SYMBOL_HEADER, UPTR0(88), INTCON(39)};
irk_string constructed_90 = {STRING_HEADER(5), 5, "undef" };
// symbol undef
irk_int constructed_91[] = {UPTR(91,1), SYMBOL_HEADER, UPTR0(90), INTCON(40)};
irk_string constructed_92 = {STRING_HEADER(6), 6, "record" };
// symbol record
irk_int constructed_93[] = {UPTR(93,1), SYMBOL_HEADER, UPTR0(92), INTCON(41)};
irk_string constructed_94 = {STRING_HEADER(5), 5, "field" };
// symbol field
irk_int constructed_95[] = {UPTR(95,1), SYMBOL_HEADER, UPTR0(94), INTCON(42)};
irk_string constructed_96 = {STRING_HEADER(4), 4, "attr" };
// symbol attr
irk_int constructed_97[] = {UPTR(97,1), SYMBOL_HEADER, UPTR0(96), INTCON(43)};
irk_string constructed_98 = {STRING_HEADER(4), 4, "base" };
// symbol base
irk_int constructed_99[] = {UPTR(99,1), SYMBOL_HEADER, UPTR0(98), INTCON(44)};
irk_string constructed_100 = {STRING_HEADER(8), 8, "rproduct" };
// symbol rproduct
irk_int constructed_101[] = {UPTR(101,1), SYMBOL_HEADER, UPTR0(100), INTCON(45)};
irk_string constructed_102 = {STRING_HEADER(6), 6, "rlabel" };
// symbol rlabel
irk_int constructed_103[] = {UPTR(103,1), SYMBOL_HEADER, UPTR0(102), INTCON(46)};
irk_string constructed_104 = {STRING_HEADER(4), 4, "path" };
// symbol path
irk_int constructed_105[] = {UPTR(105,1), SYMBOL_HEADER, UPTR0(104), INTCON(47)};
irk_string constructed_106 = {STRING_HEADER(3), 3, "pre" };
// symbol pre
irk_int constructed_107[] = {UPTR(107,1), SYMBOL_HEADER, UPTR0(106), INTCON(48)};
irk_string constructed_108 = {STRING_HEADER(4), 4, "line" };
// symbol line
irk_int constructed_109[] = {UPTR(109,1), SYMBOL_HEADER, UPTR0(108), INTCON(49)};
irk_string constructed_110 = {STRING_HEADER(3), 3, "pos" };
// symbol pos
irk_int constructed_111[] = {UPTR(111,1), SYMBOL_HEADER, UPTR0(110), INTCON(50)};
irk_string constructed_112 = {STRING_HEADER(8), 8, "rdefault" };
// symbol rdefault
irk_int constructed_113[] = {UPTR(113,1), SYMBOL_HEADER, UPTR0(112), INTCON(51)};
irk_string constructed_114 = {STRING_HEADER(3), 3, "abs" };
// symbol abs
irk_int constructed_115[] = {UPTR(115,1), SYMBOL_HEADER, UPTR0(114), INTCON(52)};
irk_string constructed_116 = {STRING_HEADER(6), 6, "inline" };
// symbol inline
irk_int constructed_117[] = {UPTR(117,1), SYMBOL_HEADER, UPTR0(116), INTCON(53)};
irk_string constructed_118 = {STRING_HEADER(5), 5, "macro" };
// symbol macro
irk_int constructed_119[] = {UPTR(119,1), SYMBOL_HEADER, UPTR0(118), INTCON(54)};
irk_string constructed_120 = {STRING_HEADER(8), 8, "src-step" };
// symbol src-step
irk_int constructed_121[] = {UPTR(121,1), SYMBOL_HEADER, UPTR0(120), INTCON(55)};
irk_string constructed_122 = {STRING_HEADER(2), 2, "at" };
// symbol at
irk_int constructed_123[] = {UPTR(123,1), SYMBOL_HEADER, UPTR0(122), INTCON(56)};
irk_string constructed_124 = {STRING_HEADER(7), 7, "inlined" };
// symbol inlined
irk_int constructed_125[] = {UPTR(125,1), SYMBOL_HEADER, UPTR0(124), INTCON(57)};
irk_string constructed_126 = {STRING_HEADER(6), 6, "fqueue" };
// symbol fqueue
irk_int constructed_127[] = {UPTR(127,1), SYMBOL_HEADER, UPTR0(126), INTCON(58)};
irk_string constructed_128 = {STRING_HEADER(3), 3, "set" };
// symbol set
irk_int constructed_129[] = {UPTR(129,1), SYMBOL_HEADER, UPTR0(128), INTCON(59)};
irk_string constructed_130 = {STRING_HEADER(3), 3, "red" };
// symbol red
irk_int constructed_131[] = {UPTR(131,1), SYMBOL_HEADER, UPTR0(130), INTCON(60)};
irk_string constructed_132 = {STRING_HEADER(5), 5, "black" };
// symbol black
irk_int constructed_133[] = {UPTR(133,1), SYMBOL_HEADER, UPTR0(132), INTCON(61)};
irk_string constructed_134 = {STRING_HEADER(5), 5, "empty" };
// symbol empty
irk_int constructed_135[] = {UPTR(135,1), SYMBOL_HEADER, UPTR0(134), INTCON(62)};
irk_string constructed_136 = {STRING_HEADER(5), 5, "alist" };
// symbol alist
irk_int constructed_137[] = {UPTR(137,1), SYMBOL_HEADER, UPTR0(136), INTCON(63)};
irk_string constructed_138 = {STRING_HEADER(5), 5, "entry" };
// symbol entry
irk_int constructed_139[] = {UPTR(139,1), SYMBOL_HEADER, UPTR0(138), INTCON(64)};
irk_string constructed_140 = {STRING_HEADER(8), 8, "alist-ob" };
// symbol alist-ob
irk_int constructed_141[] = {UPTR(141,1), SYMBOL_HEADER, UPTR0(140), INTCON(65)};
irk_string constructed_142 = {STRING_HEADER(4), 4, "cint" };
// symbol cint
irk_int constructed_143[] = {UPTR(143,1), SYMBOL_HEADER, UPTR0(142), INTCON(66)};
irk_string constructed_144 = {STRING_HEADER(5), 5, "short" };
// symbol short
irk_int constructed_145[] = {UPTR(145,1), SYMBOL_HEADER, UPTR0(144), INTCON(67)};
irk_string constructed_146 = {STRING_HEADER(4), 4, "long" };
// symbol long
irk_int constructed_147[] = {UPTR(147,1), SYMBOL_HEADER, UPTR0(146), INTCON(68)};
irk_string constructed_148 = {STRING_HEADER(8), 8, "longlong" };
// symbol longlong
irk_int constructed_149[] = {UPTR(149,1), SYMBOL_HEADER, UPTR0(148), INTCON(69)};
irk_string constructed_150 = {STRING_HEADER(5), 5, "width" };
// symbol width
irk_int constructed_151[] = {UPTR(151,1), SYMBOL_HEADER, UPTR0(150), INTCON(70)};
irk_string constructed_152 = {STRING_HEADER(5), 5, "ctype" };
// symbol ctype
irk_int constructed_153[] = {UPTR(153,1), SYMBOL_HEADER, UPTR0(152), INTCON(71)};
irk_string constructed_154 = {STRING_HEADER(4), 4, "name" };
// symbol name
irk_int constructed_155[] = {UPTR(155,1), SYMBOL_HEADER, UPTR0(154), INTCON(72)};
irk_string constructed_156 = {STRING_HEADER(5), 5, "array" };
// symbol array
irk_int constructed_157[] = {UPTR(157,1), SYMBOL_HEADER, UPTR0(156), INTCON(73)};
irk_string constructed_158 = {STRING_HEADER(7), 7, "pointer" };
// symbol pointer
irk_int constructed_159[] = {UPTR(159,1), SYMBOL_HEADER, UPTR0(158), INTCON(74)};
irk_string constructed_160 = {STRING_HEADER(6), 6, "struct" };
// symbol struct
irk_int constructed_161[] = {UPTR(161,1), SYMBOL_HEADER, UPTR0(160), INTCON(75)};
irk_string constructed_162 = {STRING_HEADER(5), 5, "union" };
// symbol union
irk_int constructed_163[] = {UPTR(163,1), SYMBOL_HEADER, UPTR0(162), INTCON(76)};
irk_string constructed_164 = {STRING_HEADER(6), 6, "cfield" };
// symbol cfield
irk_int constructed_165[] = {UPTR(165,1), SYMBOL_HEADER, UPTR0(164), INTCON(77)};
irk_string constructed_166 = {STRING_HEADER(4), 4, "cdef" };
// symbol cdef
irk_int constructed_167[] = {UPTR(167,1), SYMBOL_HEADER, UPTR0(166), INTCON(78)};
irk_string constructed_168 = {STRING_HEADER(4), 4, "csig" };
// symbol csig
irk_int constructed_169[] = {UPTR(169,1), SYMBOL_HEADER, UPTR0(168), INTCON(79)};
irk_string constructed_170 = {STRING_HEADER(3), 3, "fun" };
// symbol fun
irk_int constructed_171[] = {UPTR(171,1), SYMBOL_HEADER, UPTR0(170), INTCON(80)};
irk_string constructed_172 = {STRING_HEADER(3), 3, "obj" };
// symbol obj
irk_int constructed_173[] = {UPTR(173,1), SYMBOL_HEADER, UPTR0(172), INTCON(81)};
irk_string constructed_174 = {STRING_HEADER(4), 4, "tree" };
// symbol tree
irk_int constructed_175[] = {UPTR(175,1), SYMBOL_HEADER, UPTR0(174), INTCON(82)};
irk_string constructed_176 = {STRING_HEADER(5), 5, "ident" };
// symbol ident
irk_int constructed_177[] = {UPTR(177,1), SYMBOL_HEADER, UPTR0(176), INTCON(83)};
irk_string constructed_178 = {STRING_HEADER(7), 7, "userimm" };
// symbol userimm
irk_int constructed_179[] = {UPTR(179,1), SYMBOL_HEADER, UPTR0(178), INTCON(84)};
irk_string constructed_180 = {STRING_HEADER(7), 7, "usertup" };
// symbol usertup
irk_int constructed_181[] = {UPTR(181,1), SYMBOL_HEADER, UPTR0(180), INTCON(85)};
irk_string constructed_182 = {STRING_HEADER(6), 6, "sysimm" };
// symbol sysimm
irk_int constructed_183[] = {UPTR(183,1), SYMBOL_HEADER, UPTR0(182), INTCON(86)};
irk_string constructed_184 = {STRING_HEADER(6), 6, "systup" };
// symbol systup
irk_int constructed_185[] = {UPTR(185,1), SYMBOL_HEADER, UPTR0(184), INTCON(87)};
irk_string constructed_186 = {STRING_HEADER(6), 6, "map-ob" };
// symbol map-ob
irk_int constructed_187[] = {UPTR(187,1), SYMBOL_HEADER, UPTR0(186), INTCON(88)};
irk_string constructed_188 = {STRING_HEADER(5), 5, "arrow" };
// symbol arrow
irk_int constructed_189[] = {UPTR(189,1), SYMBOL_HEADER, UPTR0(188), INTCON(89)};
irk_string constructed_190 = {STRING_HEADER(7), 7, "set2-ob" };
// symbol set2-ob
irk_int constructed_191[] = {UPTR(191,1), SYMBOL_HEADER, UPTR0(190), INTCON(90)};
irk_string constructed_192 = {STRING_HEADER(3), 3, "csi" };
// symbol csi
irk_int constructed_193[] = {UPTR(193,1), SYMBOL_HEADER, UPTR0(192), INTCON(91)};
irk_string constructed_194 = {STRING_HEADER(3), 3, "cuu" };
// symbol cuu
irk_int constructed_195[] = {UPTR(195,1), SYMBOL_HEADER, UPTR0(194), INTCON(92)};
irk_string constructed_196 = {STRING_HEADER(3), 3, "cud" };
// symbol cud
irk_int constructed_197[] = {UPTR(197,1), SYMBOL_HEADER, UPTR0(196), INTCON(93)};
irk_string constructed_198 = {STRING_HEADER(3), 3, "cuf" };
// symbol cuf
irk_int constructed_199[] = {UPTR(199,1), SYMBOL_HEADER, UPTR0(198), INTCON(94)};
irk_string constructed_200 = {STRING_HEADER(3), 3, "cub" };
// symbol cub
irk_int constructed_201[] = {UPTR(201,1), SYMBOL_HEADER, UPTR0(200), INTCON(95)};
irk_string constructed_202 = {STRING_HEADER(3), 3, "cnl" };
// symbol cnl
irk_int constructed_203[] = {UPTR(203,1), SYMBOL_HEADER, UPTR0(202), INTCON(96)};
irk_string constructed_204 = {STRING_HEADER(3), 3, "cpl" };
// symbol cpl
irk_int constructed_205[] = {UPTR(205,1), SYMBOL_HEADER, UPTR0(204), INTCON(97)};
irk_string constructed_206 = {STRING_HEADER(3), 3, "cha" };
// symbol cha
irk_int constructed_207[] = {UPTR(207,1), SYMBOL_HEADER, UPTR0(206), INTCON(98)};
irk_string constructed_208 = {STRING_HEADER(3), 3, "cup" };
// symbol cup
irk_int constructed_209[] = {UPTR(209,1), SYMBOL_HEADER, UPTR0(208), INTCON(99)};
irk_string constructed_210 = {STRING_HEADER(2), 2, "ed" };
// symbol ed
irk_int constructed_211[] = {UPTR(211,1), SYMBOL_HEADER, UPTR0(210), INTCON(100)};
irk_string constructed_212 = {STRING_HEADER(2), 2, "el" };
// symbol el
irk_int constructed_213[] = {UPTR(213,1), SYMBOL_HEADER, UPTR0(212), INTCON(101)};
irk_string constructed_214 = {STRING_HEADER(2), 2, "ds" };
// symbol ds
irk_int constructed_215[] = {UPTR(215,1), SYMBOL_HEADER, UPTR0(214), INTCON(102)};
irk_string constructed_216 = {STRING_HEADER(2), 2, "su" };
// symbol su
irk_int constructed_217[] = {UPTR(217,1), SYMBOL_HEADER, UPTR0(216), INTCON(103)};
irk_string constructed_218 = {STRING_HEADER(2), 2, "sd" };
// symbol sd
irk_int constructed_219[] = {UPTR(219,1), SYMBOL_HEADER, UPTR0(218), INTCON(104)};
irk_string constructed_220 = {STRING_HEADER(3), 3, "hvp" };
// symbol hvp
irk_int constructed_221[] = {UPTR(221,1), SYMBOL_HEADER, UPTR0(220), INTCON(105)};
irk_string constructed_222 = {STRING_HEADER(3), 3, "sgr" };
// symbol sgr
irk_int constructed_223[] = {UPTR(223,1), SYMBOL_HEADER, UPTR0(222), INTCON(106)};
irk_string constructed_224 = {STRING_HEADER(3), 3, "dsr" };
// symbol dsr
irk_int constructed_225[] = {UPTR(225,1), SYMBOL_HEADER, UPTR0(224), INTCON(107)};
irk_string constructed_226 = {STRING_HEADER(5), 5, "other" };
// symbol other
irk_int constructed_227[] = {UPTR(227,1), SYMBOL_HEADER, UPTR0(226), INTCON(108)};
irk_string constructed_228 = {STRING_HEADER(16), 16, "dirent-file-type" };
// symbol dirent-file-type
irk_int constructed_229[] = {UPTR(229,1), SYMBOL_HEADER, UPTR0(228), INTCON(109)};
irk_string constructed_230 = {STRING_HEADER(7), 7, "regular" };
// symbol regular
irk_int constructed_231[] = {UPTR(231,1), SYMBOL_HEADER, UPTR0(230), INTCON(110)};
irk_string constructed_232 = {STRING_HEADER(9), 9, "directory" };
// symbol directory
irk_int constructed_233[] = {UPTR(233,1), SYMBOL_HEADER, UPTR0(232), INTCON(111)};
irk_string constructed_234 = {STRING_HEADER(4), 4, "fifo" };
// symbol fifo
irk_int constructed_235[] = {UPTR(235,1), SYMBOL_HEADER, UPTR0(234), INTCON(112)};
irk_string constructed_236 = {STRING_HEADER(6), 6, "socket" };
// symbol socket
irk_int constructed_237[] = {UPTR(237,1), SYMBOL_HEADER, UPTR0(236), INTCON(113)};
irk_string constructed_238 = {STRING_HEADER(5), 5, "block" };
// symbol block
irk_int constructed_239[] = {UPTR(239,1), SYMBOL_HEADER, UPTR0(238), INTCON(114)};
irk_string constructed_240 = {STRING_HEADER(7), 7, "symlink" };
// symbol symlink
irk_int constructed_241[] = {UPTR(241,1), SYMBOL_HEADER, UPTR0(240), INTCON(115)};
irk_string constructed_242 = {STRING_HEADER(10), 10, "exceptions" };
// symbol exceptions
irk_int constructed_243[] = {UPTR(243,1), SYMBOL_HEADER, UPTR0(242), INTCON(116)};
irk_string constructed_244 = {STRING_HEADER(17), 17, "Metadata/KeyError" };
// symbol Metadata/KeyError
irk_int constructed_245[] = {UPTR(245,1), SYMBOL_HEADER, UPTR0(244), INTCON(117)};
irk_string constructed_246 = {STRING_HEADER(18), 18, "Metadata/Malformed" };
// symbol Metadata/Malformed
irk_int constructed_247[] = {UPTR(247,1), SYMBOL_HEADER, UPTR0(246), INTCON(118)};
irk_string constructed_248 = {STRING_HEADER(12), 12, "String/Range" };
// symbol String/Range
irk_int constructed_249[] = {UPTR(249,1), SYMBOL_HEADER, UPTR0(248), INTCON(119)};
irk_string constructed_250 = {STRING_HEADER(7), 7, "product" };
// symbol product
irk_int constructed_251[] = {UPTR(251,1), SYMBOL_HEADER, UPTR0(250), INTCON(120)};
irk_string constructed_252 = {STRING_HEADER(5), 5, "tuple" };
// symbol tuple
irk_int constructed_253[] = {UPTR(253,1), SYMBOL_HEADER, UPTR0(252), INTCON(121)};
irk_int constructed_254[] = {UPTR(254,5587),UOHEAD(2,1),UPTR(12,1),UITAG(0),UOHEAD(2,1),UPTR(49,1),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),UOHEAD(2,1),
	UPTR(51,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(22,1),UITAG(0),CONS_HEADER,UPTR(254,16),
	TC_NIL,UOHEAD(2,0),UPTR(254,19),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,25),TC_NIL,
	UOHEAD(2,0),UPTR(254,28),UITAG(0),CONS_HEADER,UPTR(254,31),TC_NIL,CONS_HEADER,UPTR(254,22),UPTR(254,34),UOHEAD(2,0),
	UPTR(254,37),UITAG(0),CONS_HEADER,UPTR(254,40),TC_NIL,CONS_HEADER,UPTR(254,13),UPTR(254,43),CONS_HEADER,UPTR(254,10),
	UPTR(254,46),UOHEAD(2,0),UPTR(254,49),UITAG(0),CONS_HEADER,UPTR(254,52),TC_NIL,UOHEAD(2,0),UPTR(254,55),UITAG(0),
	CONS_HEADER,UPTR(254,58),TC_NIL,CONS_HEADER,UPTR(254,7),UPTR(254,61),UOHEAD(2,0),UPTR(254,64),UITAG(0),UOHEAD(2,1),
	UPTR(53,1),UITAG(0),UOHEAD(2,1),UPTR(55,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(254,79),TC_NIL,CONS_HEADER,UPTR(254,76),UPTR(254,82),CONS_HEADER,UPTR(254,73),UPTR(254,85),
	UOHEAD(2,0),UPTR(254,88),UITAG(0),UOHEAD(2,1),UPTR(57,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,100),TC_NIL,CONS_HEADER,UPTR(254,97),UPTR(254,103),CONS_HEADER,UPTR(254,94),
	UPTR(254,106),UOHEAD(2,0),UPTR(254,109),UITAG(0),UOHEAD(2,1),UPTR(59,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,121),TC_NIL,CONS_HEADER,UPTR(254,118),UPTR(254,124),CONS_HEADER,
	UPTR(254,115),UPTR(254,127),UOHEAD(2,0),UPTR(254,130),UITAG(0),CONS_HEADER,UPTR(254,133),TC_NIL,CONS_HEADER,UPTR(254,112),
	UPTR(254,136),CONS_HEADER,UPTR(254,91),UPTR(254,139),UOHEAD(2,0),UPTR(254,142),UITAG(0),CONS_HEADER,UPTR(254,145),TC_NIL,
	CONS_HEADER,UPTR(254,70),UPTR(254,148),UOHEAD(2,0),UPTR(254,151),UITAG(0),UOHEAD(2,1),UPTR(61,1),UITAG(0),UOHEAD(2,1),
	UPTR(63,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),
	UITAG(0),CONS_HEADER,UPTR(254,169),TC_NIL,CONS_HEADER,UPTR(254,166),UPTR(254,172),UOHEAD(2,0),UPTR(254,175),UITAG(0),
	CONS_HEADER,UPTR(254,178),TC_NIL,UOHEAD(2,0),UPTR(254,181),UITAG(0),CONS_HEADER,UPTR(254,184),TC_NIL,CONS_HEADER,
	UPTR(254,163),UPTR(254,187),CONS_HEADER,UPTR(254,160),UPTR(254,190),UOHEAD(2,0),UPTR(254,193),UITAG(0),UOHEAD(2,1),UPTR(69,1),
	UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,205),TC_NIL,
	CONS_HEADER,UPTR(254,202),UPTR(254,208),CONS_HEADER,UPTR(254,199),UPTR(254,211),UOHEAD(2,0),UPTR(254,214),UITAG(0),CONS_HEADER,
	UPTR(254,217),TC_NIL,CONS_HEADER,UPTR(254,196),UPTR(254,220),UOHEAD(2,0),UPTR(254,223),UITAG(0),CONS_HEADER,UPTR(254,226),
	TC_NIL,CONS_HEADER,UPTR(254,157),UPTR(254,229),UOHEAD(2,0),UPTR(254,232),UITAG(0),UOHEAD(2,1),UPTR(37,1),UITAG(0),
	UOHEAD(2,1),UPTR(71,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(254,247),TC_NIL,CONS_HEADER,UPTR(254,244),UPTR(254,250),CONS_HEADER,UPTR(254,241),UPTR(254,253),UOHEAD(2,0),UPTR(254,256),
	UITAG(0),UOHEAD(2,1),UPTR(73,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(254,268),TC_NIL,CONS_HEADER,UPTR(254,265),UPTR(254,271),CONS_HEADER,UPTR(254,262),UPTR(254,274),UOHEAD(2,0),
	UPTR(254,277),UITAG(0),CONS_HEADER,UPTR(254,280),TC_NIL,CONS_HEADER,UPTR(254,259),UPTR(254,283),UOHEAD(2,0),UPTR(254,286),
	UITAG(0),CONS_HEADER,UPTR(254,289),TC_NIL,CONS_HEADER,UPTR(254,238),UPTR(254,292),UOHEAD(2,0),UPTR(254,295),UITAG(0),
	UOHEAD(2,1),UPTR(75,1),UITAG(0),UOHEAD(2,1),UPTR(77,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,313),TC_NIL,CONS_HEADER,UPTR(254,310),
	UPTR(254,316),UOHEAD(2,0),UPTR(254,319),UITAG(0),CONS_HEADER,UPTR(254,322),TC_NIL,UOHEAD(2,0),UPTR(254,325),UITAG(0),
	CONS_HEADER,UPTR(254,328),TC_NIL,CONS_HEADER,UPTR(254,307),UPTR(254,331),CONS_HEADER,UPTR(254,304),UPTR(254,334),UOHEAD(2,0),
	UPTR(254,337),UITAG(0),UOHEAD(2,1),UPTR(79,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(65,1),
	UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,352),TC_NIL,CONS_HEADER,UPTR(254,349),UPTR(254,355),
	UOHEAD(2,0),UPTR(254,358),UITAG(0),CONS_HEADER,UPTR(254,361),TC_NIL,UOHEAD(2,0),UPTR(254,364),UITAG(0),CONS_HEADER,
	UPTR(254,367),TC_NIL,CONS_HEADER,UPTR(254,346),UPTR(254,370),CONS_HEADER,UPTR(254,343),UPTR(254,373),UOHEAD(2,0),UPTR(254,376),
	UITAG(0),CONS_HEADER,UPTR(254,379),TC_NIL,CONS_HEADER,UPTR(254,340),UPTR(254,382),UOHEAD(2,0),UPTR(254,385),UITAG(0),
	CONS_HEADER,UPTR(254,388),TC_NIL,CONS_HEADER,UPTR(254,301),UPTR(254,391),UOHEAD(2,0),UPTR(254,394),UITAG(0),UOHEAD(2,1),
	UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(39,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(254,409),TC_NIL,CONS_HEADER,UPTR(254,406),UPTR(254,412),CONS_HEADER,UPTR(254,403),UPTR(254,415),
	UOHEAD(2,0),UPTR(254,418),UITAG(0),UOHEAD(2,1),UPTR(85,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,433),TC_NIL,CONS_HEADER,UPTR(254,430),
	UPTR(254,436),UOHEAD(2,0),UPTR(254,439),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,451),TC_NIL,CONS_HEADER,UPTR(254,448),UPTR(254,454),UOHEAD(2,0),
	UPTR(254,457),UITAG(0),CONS_HEADER,UPTR(254,460),TC_NIL,CONS_HEADER,UPTR(254,445),UPTR(254,463),UOHEAD(2,0),UPTR(254,466),
	UITAG(0),CONS_HEADER,UPTR(254,469),TC_NIL,CONS_HEADER,UPTR(254,442),UPTR(254,472),UOHEAD(2,0),UPTR(254,475),UITAG(0),
	CONS_HEADER,UPTR(254,478),TC_NIL,CONS_HEADER,UPTR(254,427),UPTR(254,481),CONS_HEADER,UPTR(254,424),UPTR(254,484),UOHEAD(2,0),
	UPTR(254,487),UITAG(0),CONS_HEADER,UPTR(254,490),TC_NIL,CONS_HEADER,UPTR(254,421),UPTR(254,493),UOHEAD(2,0),UPTR(254,496),
	UITAG(0),CONS_HEADER,UPTR(254,499),TC_NIL,CONS_HEADER,UPTR(254,400),UPTR(254,502),UOHEAD(2,0),UPTR(254,505),UITAG(0),
	UOHEAD(2,1),UPTR(87,1),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(254,523),TC_NIL,UOHEAD(2,0),UPTR(254,526),
	UITAG(0),CONS_HEADER,UPTR(254,529),TC_NIL,CONS_HEADER,UPTR(254,520),UPTR(254,532),UOHEAD(2,0),UPTR(254,535),UITAG(0),
	UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,541),TC_NIL,UOHEAD(2,0),UPTR(254,544),UITAG(0),CONS_HEADER,
	UPTR(254,547),TC_NIL,CONS_HEADER,UPTR(254,538),UPTR(254,550),UOHEAD(2,0),UPTR(254,553),UITAG(0),CONS_HEADER,UPTR(254,556),
	TC_NIL,CONS_HEADER,UPTR(254,517),UPTR(254,559),CONS_HEADER,UPTR(254,514),UPTR(254,562),UOHEAD(2,0),UPTR(254,565),UITAG(0),
	UOHEAD(2,1),UPTR(28,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,
	UPTR(254,577),TC_NIL,UOHEAD(2,0),UPTR(254,580),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,586),
	TC_NIL,UOHEAD(2,0),UPTR(254,589),UITAG(0),CONS_HEADER,UPTR(254,592),TC_NIL,CONS_HEADER,UPTR(254,583),UPTR(254,595),
	UOHEAD(2,0),UPTR(254,598),UITAG(0),CONS_HEADER,UPTR(254,601),TC_NIL,CONS_HEADER,UPTR(254,574),UPTR(254,604),CONS_HEADER,
	UPTR(254,571),UPTR(254,607),UOHEAD(2,0),UPTR(254,610),UITAG(0),UOHEAD(2,1),UPTR(22,1),UITAG(0),UOHEAD(2,5),5,
	UITAG(0),UOHEAD(2,1),UPTR(22,1),UITAG(0),CONS_HEADER,UPTR(254,622),TC_NIL,UOHEAD(2,0),UPTR(254,625),UITAG(0),
	UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,631),TC_NIL,UOHEAD(2,0),UPTR(254,634),UITAG(0),CONS_HEADER,
	UPTR(254,637),TC_NIL,CONS_HEADER,UPTR(254,628),UPTR(254,640),UOHEAD(2,0),UPTR(254,643),UITAG(0),CONS_HEADER,UPTR(254,646),
	TC_NIL,CONS_HEADER,UPTR(254,619),UPTR(254,649),CONS_HEADER,UPTR(254,616),UPTR(254,652),UOHEAD(2,0),UPTR(254,655),UITAG(0),
	UOHEAD(2,1),UPTR(35,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(35,1),UITAG(0),CONS_HEADER,
	UPTR(254,667),TC_NIL,UOHEAD(2,0),UPTR(254,670),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,676),
	TC_NIL,UOHEAD(2,0),UPTR(254,679),UITAG(0),CONS_HEADER,UPTR(254,682),TC_NIL,CONS_HEADER,UPTR(254,673),UPTR(254,685),
	UOHEAD(2,0),UPTR(254,688),UITAG(0),CONS_HEADER,UPTR(254,691),TC_NIL,CONS_HEADER,UPTR(254,664),UPTR(254,694),CONS_HEADER,
	UPTR(254,661),UPTR(254,697),UOHEAD(2,0),UPTR(254,700),UITAG(0),UOHEAD(2,1),UPTR(37,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,1),UPTR(37,1),UITAG(0),CONS_HEADER,UPTR(254,712),TC_NIL,UOHEAD(2,0),UPTR(254,715),UITAG(0),
	UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,721),TC_NIL,UOHEAD(2,0),UPTR(254,724),UITAG(0),CONS_HEADER,
	UPTR(254,727),TC_NIL,CONS_HEADER,UPTR(254,718),UPTR(254,730),UOHEAD(2,0),UPTR(254,733),UITAG(0),CONS_HEADER,UPTR(254,736),
	TC_NIL,CONS_HEADER,UPTR(254,709),UPTR(254,739),CONS_HEADER,UPTR(254,706),UPTR(254,742),UOHEAD(2,0),UPTR(254,745),UITAG(0),
	UOHEAD(2,1),UPTR(6,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,
	UPTR(254,757),TC_NIL,UOHEAD(2,0),UPTR(254,760),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,766),
	TC_NIL,UOHEAD(2,0),UPTR(254,769),UITAG(0),CONS_HEADER,UPTR(254,772),TC_NIL,CONS_HEADER,UPTR(254,763),UPTR(254,775),
	UOHEAD(2,0),UPTR(254,778),UITAG(0),CONS_HEADER,UPTR(254,781),TC_NIL,CONS_HEADER,UPTR(254,754),UPTR(254,784),CONS_HEADER,
	UPTR(254,751),UPTR(254,787),UOHEAD(2,0),UPTR(254,790),UITAG(0),UOHEAD(2,1),UPTR(91,1),UITAG(0),UOHEAD(2,5),13,
	UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,802),TC_NIL,UOHEAD(2,0),UPTR(254,805),UITAG(0),
	CONS_HEADER,UPTR(254,808),TC_NIL,UOHEAD(2,0),UPTR(254,811),UITAG(0),CONS_HEADER,UPTR(254,814),TC_NIL,CONS_HEADER,
	UPTR(254,799),UPTR(254,817),CONS_HEADER,UPTR(254,796),UPTR(254,820),UOHEAD(2,0),UPTR(254,823),UITAG(0),UOHEAD(2,1),UPTR(24,1),
	UITAG(0),UOHEAD(2,5),15,UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),
	CONS_HEADER,UPTR(254,838),TC_NIL,UOHEAD(2,0),UPTR(254,841),UITAG(0),CONS_HEADER,UPTR(254,844),TC_NIL,CONS_HEADER,
	UPTR(254,835),UPTR(254,847),UOHEAD(2,0),UPTR(254,850),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,856),
	TC_NIL,UOHEAD(2,0),UPTR(254,859),UITAG(0),CONS_HEADER,UPTR(254,862),TC_NIL,CONS_HEADER,UPTR(254,853),UPTR(254,865),
	UOHEAD(2,0),UPTR(254,868),UITAG(0),CONS_HEADER,UPTR(254,871),TC_NIL,CONS_HEADER,UPTR(254,832),UPTR(254,874),CONS_HEADER,
	UPTR(254,829),UPTR(254,877),UOHEAD(2,0),UPTR(254,880),UITAG(0),UOHEAD(2,1),UPTR(93,1),UITAG(0),UOHEAD(2,5),17,
	UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(95,1),UITAG(0),CONS_HEADER,UPTR(254,895),TC_NIL,
	UOHEAD(2,0),UPTR(254,898),UITAG(0),CONS_HEADER,UPTR(254,901),TC_NIL,CONS_HEADER,UPTR(254,892),UPTR(254,904),UOHEAD(2,0),
	UPTR(254,907),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,913),TC_NIL,UOHEAD(2,0),UPTR(254,916),
	UITAG(0),CONS_HEADER,UPTR(254,919),TC_NIL,CONS_HEADER,UPTR(254,910),UPTR(254,922),UOHEAD(2,0),UPTR(254,925),UITAG(0),
	CONS_HEADER,UPTR(254,928),TC_NIL,CONS_HEADER,UPTR(254,889),UPTR(254,931),CONS_HEADER,UPTR(254,886),UPTR(254,934),UOHEAD(2,0),
	UPTR(254,937),UITAG(0),UOHEAD(2,1),UPTR(85,1),UITAG(0),UOHEAD(2,5),19,UITAG(0),UOHEAD(2,1),UPTR(28,1),
	UITAG(0),CONS_HEADER,UPTR(254,949),TC_NIL,UOHEAD(2,0),UPTR(254,952),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),
	CONS_HEADER,UPTR(254,958),TC_NIL,UOHEAD(2,0),UPTR(254,961),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,
	UPTR(254,967),TC_NIL,UOHEAD(2,0),UPTR(254,970),UITAG(0),CONS_HEADER,UPTR(254,973),TC_NIL,CONS_HEADER,UPTR(254,964),
	UPTR(254,976),CONS_HEADER,UPTR(254,955),UPTR(254,979),UOHEAD(2,0),UPTR(254,982),UITAG(0),CONS_HEADER,UPTR(254,985),TC_NIL,
	CONS_HEADER,UPTR(254,946),UPTR(254,988),CONS_HEADER,UPTR(254,943),UPTR(254,991),UOHEAD(2,0),UPTR(254,994),UITAG(0),UOHEAD(2,1),
	UPTR(97,1),UITAG(0),UOHEAD(2,5),21,UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(254,1006),
	TC_NIL,UOHEAD(2,0),UPTR(254,1009),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,1015),TC_NIL,
	UOHEAD(2,0),UPTR(254,1018),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,1024),TC_NIL,UOHEAD(2,0),
	UPTR(254,1027),UITAG(0),CONS_HEADER,UPTR(254,1030),TC_NIL,CONS_HEADER,UPTR(254,1021),UPTR(254,1033),CONS_HEADER,UPTR(254,1012),
	UPTR(254,1036),UOHEAD(2,0),UPTR(254,1039),UITAG(0),CONS_HEADER,UPTR(254,1042),TC_NIL,CONS_HEADER,UPTR(254,1003),UPTR(254,1045),
	CONS_HEADER,UPTR(254,1000),UPTR(254,1048),UOHEAD(2,0),UPTR(254,1051),UITAG(0),CONS_HEADER,UPTR(254,1054),TC_NIL,CONS_HEADER,
	UPTR(254,997),UPTR(254,1057),CONS_HEADER,UPTR(254,940),UPTR(254,1060),CONS_HEADER,UPTR(254,883),UPTR(254,1063),CONS_HEADER,UPTR(254,826),
	UPTR(254,1066),CONS_HEADER,UPTR(254,793),UPTR(254,1069),CONS_HEADER,UPTR(254,748),UPTR(254,1072),CONS_HEADER,UPTR(254,703),UPTR(254,1075),
	CONS_HEADER,UPTR(254,658),UPTR(254,1078),CONS_HEADER,UPTR(254,613),UPTR(254,1081),CONS_HEADER,UPTR(254,568),UPTR(254,1084),UOHEAD(2,0),
	UPTR(254,1087),UITAG(0),CONS_HEADER,UPTR(254,1090),TC_NIL,CONS_HEADER,UPTR(254,511),UPTR(254,1093),UOHEAD(2,0),UPTR(254,1096),
	UITAG(0),UOHEAD(2,1),UPTR(95,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),
	UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,1111),TC_NIL,UOHEAD(2,0),UPTR(254,1114),UITAG(0),UOHEAD(2,1),
	UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(254,1120),TC_NIL,UOHEAD(2,0),UPTR(254,1123),UITAG(0),UOHEAD(2,1),UPTR(89,1),
	UITAG(0),CONS_HEADER,UPTR(254,1129),TC_NIL,UOHEAD(2,0),UPTR(254,1132),UITAG(0),CONS_HEADER,UPTR(254,1135),TC_NIL,
	CONS_HEADER,UPTR(254,1126),UPTR(254,1138),CONS_HEADER,UPTR(254,1117),UPTR(254,1141),UOHEAD(2,0),UPTR(254,1144),UITAG(0),CONS_HEADER,
	UPTR(254,1147),TC_NIL,CONS_HEADER,UPTR(254,1108),UPTR(254,1150),CONS_HEADER,UPTR(254,1105),UPTR(254,1153),UOHEAD(2,0),UPTR(254,1156),
	UITAG(0),CONS_HEADER,UPTR(254,1159),TC_NIL,UOHEAD(2,0),UPTR(254,1162),UITAG(0),CONS_HEADER,UPTR(254,1165),TC_NIL,
	CONS_HEADER,UPTR(254,1102),UPTR(254,1168),UOHEAD(2,0),UPTR(254,1171),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),UOHEAD(2,1),
	UPTR(39,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,1186),
	TC_NIL,CONS_HEADER,UPTR(254,1183),UPTR(254,1189),CONS_HEADER,UPTR(254,1180),UPTR(254,1192),UOHEAD(2,0),UPTR(254,1195),UITAG(0),
	UOHEAD(2,1),UPTR(99,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),
	UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(105,1),UITAG(0),CONS_HEADER,UPTR(254,1213),TC_NIL,UOHEAD(2,0),UPTR(254,1216),
	UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(22,1),UITAG(0),CONS_HEADER,UPTR(254,1225),TC_NIL,
	UOHEAD(2,0),UPTR(254,1228),UITAG(0),CONS_HEADER,UPTR(254,1231),TC_NIL,CONS_HEADER,UPTR(254,1222),UPTR(254,1234),UOHEAD(2,0),
	UPTR(254,1237),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(109,1),UITAG(0),CONS_HEADER,UPTR(254,1246),
	TC_NIL,UOHEAD(2,0),UPTR(254,1249),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,1258),TC_NIL,UOHEAD(2,0),UPTR(254,1261),UITAG(0),CONS_HEADER,UPTR(254,1264),TC_NIL,CONS_HEADER,
	UPTR(254,1255),UPTR(254,1267),UOHEAD(2,0),UPTR(254,1270),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(111,1),
	UITAG(0),CONS_HEADER,UPTR(254,1279),TC_NIL,UOHEAD(2,0),UPTR(254,1282),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),
	UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,1291),TC_NIL,UOHEAD(2,0),UPTR(254,1294),UITAG(0),CONS_HEADER,
	UPTR(254,1297),TC_NIL,CONS_HEADER,UPTR(254,1288),UPTR(254,1300),UOHEAD(2,0),UPTR(254,1303),UITAG(0),UOHEAD(2,1),UPTR(113,1),
	UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(254,1312),TC_NIL,UOHEAD(2,0),UPTR(254,1315),UITAG(0),
	CONS_HEADER,UPTR(254,1318),TC_NIL,CONS_HEADER,UPTR(254,1309),UPTR(254,1321),UOHEAD(2,0),UPTR(254,1324),UITAG(0),CONS_HEADER,
	UPTR(254,1327),TC_NIL,CONS_HEADER,UPTR(254,1306),UPTR(254,1330),CONS_HEADER,UPTR(254,1285),UPTR(254,1333),CONS_HEADER,UPTR(254,1276),
	UPTR(254,1336),UOHEAD(2,0),UPTR(254,1339),UITAG(0),CONS_HEADER,UPTR(254,1342),TC_NIL,CONS_HEADER,UPTR(254,1273),UPTR(254,1345),
	CONS_HEADER,UPTR(254,1252),UPTR(254,1348),CONS_HEADER,UPTR(254,1243),UPTR(254,1351),UOHEAD(2,0),UPTR(254,1354),UITAG(0),CONS_HEADER,
	UPTR(254,1357),TC_NIL,CONS_HEADER,UPTR(254,1240),UPTR(254,1360),CONS_HEADER,UPTR(254,1219),UPTR(254,1363),CONS_HEADER,UPTR(254,1210),
	UPTR(254,1366),UOHEAD(2,0),UPTR(254,1369),UITAG(0),CONS_HEADER,UPTR(254,1372),TC_NIL,CONS_HEADER,UPTR(254,1207),UPTR(254,1375),
	UOHEAD(2,0),UPTR(254,1378),UITAG(0),CONS_HEADER,UPTR(254,1381),TC_NIL,UOHEAD(2,0),UPTR(254,1384),UITAG(0),CONS_HEADER,
	UPTR(254,1387),TC_NIL,CONS_HEADER,UPTR(254,1204),UPTR(254,1390),CONS_HEADER,UPTR(254,1201),UPTR(254,1393),UOHEAD(2,0),UPTR(254,1396),
	UITAG(0),UOHEAD(2,1),UPTR(117,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),
	CONS_HEADER,UPTR(254,1408),TC_NIL,UOHEAD(2,0),UPTR(254,1411),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,
	UPTR(254,1417),TC_NIL,UOHEAD(2,0),UPTR(254,1420),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,1426),
	TC_NIL,UOHEAD(2,0),UPTR(254,1429),UITAG(0),CONS_HEADER,UPTR(254,1432),TC_NIL,CONS_HEADER,UPTR(254,1423),UPTR(254,1435),
	CONS_HEADER,UPTR(254,1414),UPTR(254,1438),UOHEAD(2,0),UPTR(254,1441),UITAG(0),CONS_HEADER,UPTR(254,1444),TC_NIL,CONS_HEADER,
	UPTR(254,1405),UPTR(254,1447),CONS_HEADER,UPTR(254,1402),UPTR(254,1450),UOHEAD(2,0),UPTR(254,1453),UITAG(0),UOHEAD(2,1),UPTR(119,1),
	UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,1465),TC_NIL,
	UOHEAD(2,0),UPTR(254,1468),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,1474),TC_NIL,UOHEAD(2,0),
	UPTR(254,1477),UITAG(0),UOHEAD(2,1),UPTR(89,1),UITAG(0),CONS_HEADER,UPTR(254,1483),TC_NIL,UOHEAD(2,0),UPTR(254,1486),
	UITAG(0),CONS_HEADER,UPTR(254,1489),TC_NIL,CONS_HEADER,UPTR(254,1480),UPTR(254,1492),CONS_HEADER,UPTR(254,1471),UPTR(254,1495),
	UOHEAD(2,0),UPTR(254,1498),UITAG(0),CONS_HEADER,UPTR(254,1501),TC_NIL,CONS_HEADER,UPTR(254,1462),UPTR(254,1504),CONS_HEADER,
	UPTR(254,1459),UPTR(254,1507),UOHEAD(2,0),UPTR(254,1510),UITAG(0),CONS_HEADER,UPTR(254,1513),TC_NIL,CONS_HEADER,UPTR(254,1456),
	UPTR(254,1516),CONS_HEADER,UPTR(254,1399),UPTR(254,1519),CONS_HEADER,UPTR(254,1198),UPTR(254,1522),UOHEAD(2,0),UPTR(254,1525),UITAG(0),
	CONS_HEADER,UPTR(254,1528),TC_NIL,CONS_HEADER,UPTR(254,1177),UPTR(254,1531),UOHEAD(2,0),UPTR(254,1534),UITAG(0),UOHEAD(2,1),
	UPTR(121,1),UITAG(0),UOHEAD(2,1),UPTR(123,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(101,1),
	UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(105,1),UITAG(0),CONS_HEADER,UPTR(254,1555),TC_NIL,
	UOHEAD(2,0),UPTR(254,1558),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(22,1),UITAG(0),CONS_HEADER,
	UPTR(254,1567),TC_NIL,UOHEAD(2,0),UPTR(254,1570),UITAG(0),CONS_HEADER,UPTR(254,1573),TC_NIL,CONS_HEADER,UPTR(254,1564),
	UPTR(254,1576),UOHEAD(2,0),UPTR(254,1579),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(109,1),UITAG(0),
	CONS_HEADER,UPTR(254,1588),TC_NIL,UOHEAD(2,0),UPTR(254,1591),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),
	UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,1600),TC_NIL,UOHEAD(2,0),UPTR(254,1603),UITAG(0),CONS_HEADER,UPTR(254,1606),
	TC_NIL,CONS_HEADER,UPTR(254,1597),UPTR(254,1609),UOHEAD(2,0),UPTR(254,1612),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),
	UOHEAD(2,1),UPTR(111,1),UITAG(0),CONS_HEADER,UPTR(254,1621),TC_NIL,UOHEAD(2,0),UPTR(254,1624),UITAG(0),UOHEAD(2,1),
	UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,1633),TC_NIL,UOHEAD(2,0),UPTR(254,1636),
	UITAG(0),CONS_HEADER,UPTR(254,1639),TC_NIL,CONS_HEADER,UPTR(254,1630),UPTR(254,1642),UOHEAD(2,0),UPTR(254,1645),UITAG(0),
	UOHEAD(2,1),UPTR(113,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(254,1654),TC_NIL,UOHEAD(2,0),
	UPTR(254,1657),UITAG(0),CONS_HEADER,UPTR(254,1660),TC_NIL,CONS_HEADER,UPTR(254,1651),UPTR(254,1663),UOHEAD(2,0),UPTR(254,1666),
	UITAG(0),CONS_HEADER,UPTR(254,1669),TC_NIL,CONS_HEADER,UPTR(254,1648),UPTR(254,1672),CONS_HEADER,UPTR(254,1627),UPTR(254,1675),
	CONS_HEADER,UPTR(254,1618),UPTR(254,1678),UOHEAD(2,0),UPTR(254,1681),UITAG(0),CONS_HEADER,UPTR(254,1684),TC_NIL,CONS_HEADER,
	UPTR(254,1615),UPTR(254,1687),CONS_HEADER,UPTR(254,1594),UPTR(254,1690),CONS_HEADER,UPTR(254,1585),UPTR(254,1693),UOHEAD(2,0),UPTR(254,1696),
	UITAG(0),CONS_HEADER,UPTR(254,1699),TC_NIL,CONS_HEADER,UPTR(254,1582),UPTR(254,1702),CONS_HEADER,UPTR(254,1561),UPTR(254,1705),
	CONS_HEADER,UPTR(254,1552),UPTR(254,1708),UOHEAD(2,0),UPTR(254,1711),UITAG(0),CONS_HEADER,UPTR(254,1714),TC_NIL,CONS_HEADER,
	UPTR(254,1549),UPTR(254,1717),UOHEAD(2,0),UPTR(254,1720),UITAG(0),CONS_HEADER,UPTR(254,1723),TC_NIL,UOHEAD(2,0),UPTR(254,1726),
	UITAG(0),CONS_HEADER,UPTR(254,1729),TC_NIL,CONS_HEADER,UPTR(254,1546),UPTR(254,1732),CONS_HEADER,UPTR(254,1543),UPTR(254,1735),
	UOHEAD(2,0),UPTR(254,1738),UITAG(0),UOHEAD(2,1),UPTR(119,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),
	UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,1750),TC_NIL,UOHEAD(2,0),UPTR(254,1753),UITAG(0),CONS_HEADER,UPTR(254,1756),
	TC_NIL,UOHEAD(2,0),UPTR(254,1759),UITAG(0),CONS_HEADER,UPTR(254,1762),TC_NIL,CONS_HEADER,UPTR(254,1747),UPTR(254,1765),
	CONS_HEADER,UPTR(254,1744),UPTR(254,1768),UOHEAD(2,0),UPTR(254,1771),UITAG(0),UOHEAD(2,1),UPTR(125,1),UITAG(0),UOHEAD(2,5),
	5,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,1783),TC_NIL,UOHEAD(2,0),UPTR(254,1786),
	UITAG(0),CONS_HEADER,UPTR(254,1789),TC_NIL,UOHEAD(2,0),UPTR(254,1792),UITAG(0),CONS_HEADER,UPTR(254,1795),TC_NIL,
	CONS_HEADER,UPTR(254,1780),UPTR(254,1798),CONS_HEADER,UPTR(254,1777),UPTR(254,1801),UOHEAD(2,0),UPTR(254,1804),UITAG(0),CONS_HEADER,
	UPTR(254,1807),TC_NIL,CONS_HEADER,UPTR(254,1774),UPTR(254,1810),CONS_HEADER,UPTR(254,1741),UPTR(254,1813),UOHEAD(2,0),UPTR(254,1816),
	UITAG(0),CONS_HEADER,UPTR(254,1819),TC_NIL,CONS_HEADER,UPTR(254,1540),UPTR(254,1822),UOHEAD(2,0),UPTR(254,1825),UITAG(0),
	UOHEAD(2,1),UPTR(127,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),
	UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,1846),
	TC_NIL,CONS_HEADER,UPTR(254,1843),UPTR(254,1849),UOHEAD(2,0),UPTR(254,1852),UITAG(0),CONS_HEADER,UPTR(254,1855),TC_NIL,
	CONS_HEADER,UPTR(254,1840),UPTR(254,1858),UOHEAD(2,0),UPTR(254,1861),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,1873),TC_NIL,CONS_HEADER,UPTR(254,1870),
	UPTR(254,1876),UOHEAD(2,0),UPTR(254,1879),UITAG(0),CONS_HEADER,UPTR(254,1882),TC_NIL,CONS_HEADER,UPTR(254,1867),UPTR(254,1885),
	UOHEAD(2,0),UPTR(254,1888),UITAG(0),CONS_HEADER,UPTR(254,1891),TC_NIL,CONS_HEADER,UPTR(254,1864),UPTR(254,1894),UOHEAD(2,0),
	UPTR(254,1897),UITAG(0),CONS_HEADER,UPTR(254,1900),TC_NIL,CONS_HEADER,UPTR(254,1837),UPTR(254,1903),CONS_HEADER,UPTR(254,1834),
	UPTR(254,1906),UOHEAD(2,0),UPTR(254,1909),UITAG(0),CONS_HEADER,UPTR(254,1912),TC_NIL,UOHEAD(2,0),UPTR(254,1915),UITAG(0),
	CONS_HEADER,UPTR(254,1918),TC_NIL,CONS_HEADER,UPTR(254,1831),UPTR(254,1921),UOHEAD(2,0),UPTR(254,1924),UITAG(0),UOHEAD(2,1),
	UPTR(129,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(129,1),
	UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,1945),TC_NIL,
	CONS_HEADER,UPTR(254,1942),UPTR(254,1948),UOHEAD(2,0),UPTR(254,1951),UITAG(0),CONS_HEADER,UPTR(254,1954),TC_NIL,CONS_HEADER,
	UPTR(254,1939),UPTR(254,1957),UOHEAD(2,0),UPTR(254,1960),UITAG(0),UOHEAD(2,1),UPTR(129,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),
	UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,1972),TC_NIL,CONS_HEADER,UPTR(254,1969),UPTR(254,1975),
	UOHEAD(2,0),UPTR(254,1978),UITAG(0),CONS_HEADER,UPTR(254,1981),TC_NIL,CONS_HEADER,UPTR(254,1966),UPTR(254,1984),UOHEAD(2,0),
	UPTR(254,1987),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,1996),
	TC_NIL,CONS_HEADER,UPTR(254,1993),UPTR(254,1999),UOHEAD(2,0),UPTR(254,2002),UITAG(0),CONS_HEADER,UPTR(254,2005),TC_NIL,
	CONS_HEADER,UPTR(254,1990),UPTR(254,2008),CONS_HEADER,UPTR(254,1963),UPTR(254,2011),UOHEAD(2,0),UPTR(254,2014),UITAG(0),CONS_HEADER,
	UPTR(254,2017),TC_NIL,CONS_HEADER,UPTR(254,1936),UPTR(254,2020),CONS_HEADER,UPTR(254,1933),UPTR(254,2023),UOHEAD(2,0),UPTR(254,2026),
	UITAG(0),UOHEAD(2,1),UPTR(133,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(129,1),UITAG(0),
	UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,2044),TC_NIL,CONS_HEADER,
	UPTR(254,2041),UPTR(254,2047),UOHEAD(2,0),UPTR(254,2050),UITAG(0),CONS_HEADER,UPTR(254,2053),TC_NIL,CONS_HEADER,UPTR(254,2038),
	UPTR(254,2056),UOHEAD(2,0),UPTR(254,2059),UITAG(0),UOHEAD(2,1),UPTR(129,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,2071),TC_NIL,CONS_HEADER,UPTR(254,2068),UPTR(254,2074),UOHEAD(2,0),
	UPTR(254,2077),UITAG(0),CONS_HEADER,UPTR(254,2080),TC_NIL,CONS_HEADER,UPTR(254,2065),UPTR(254,2083),UOHEAD(2,0),UPTR(254,2086),
	UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,2095),TC_NIL,
	CONS_HEADER,UPTR(254,2092),UPTR(254,2098),UOHEAD(2,0),UPTR(254,2101),UITAG(0),CONS_HEADER,UPTR(254,2104),TC_NIL,CONS_HEADER,
	UPTR(254,2089),UPTR(254,2107),CONS_HEADER,UPTR(254,2062),UPTR(254,2110),UOHEAD(2,0),UPTR(254,2113),UITAG(0),CONS_HEADER,UPTR(254,2116),
	TC_NIL,CONS_HEADER,UPTR(254,2035),UPTR(254,2119),CONS_HEADER,UPTR(254,2032),UPTR(254,2122),UOHEAD(2,0),UPTR(254,2125),UITAG(0),
	UOHEAD(2,1),UPTR(135,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(254,2137),TC_NIL,CONS_HEADER,UPTR(254,2134),UPTR(254,2140),CONS_HEADER,UPTR(254,2131),UPTR(254,2143),UOHEAD(2,0),UPTR(254,2146),
	UITAG(0),CONS_HEADER,UPTR(254,2149),TC_NIL,CONS_HEADER,UPTR(254,2128),UPTR(254,2152),CONS_HEADER,UPTR(254,2029),UPTR(254,2155),
	UOHEAD(2,0),UPTR(254,2158),UITAG(0),CONS_HEADER,UPTR(254,2161),TC_NIL,CONS_HEADER,UPTR(254,1930),UPTR(254,2164),UOHEAD(2,0),
	UPTR(254,2167),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),UPTR(39,1),UITAG(0),UOHEAD(2,5),1,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,2182),TC_NIL,CONS_HEADER,UPTR(254,2179),UPTR(254,2185),
	CONS_HEADER,UPTR(254,2176),UPTR(254,2188),UOHEAD(2,0),UPTR(254,2191),UITAG(0),UOHEAD(2,1),UPTR(139,1),UITAG(0),UOHEAD(2,5),
	3,UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,2206),
	TC_NIL,CONS_HEADER,UPTR(254,2203),UPTR(254,2209),UOHEAD(2,0),UPTR(254,2212),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,2221),TC_NIL,CONS_HEADER,UPTR(254,2218),UPTR(254,2224),UOHEAD(2,0),
	UPTR(254,2227),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),
	UITAG(0),CONS_HEADER,UPTR(254,2239),TC_NIL,CONS_HEADER,UPTR(254,2236),UPTR(254,2242),UOHEAD(2,0),UPTR(254,2245),UITAG(0),
	UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,2254),TC_NIL,CONS_HEADER,
	UPTR(254,2251),UPTR(254,2257),UOHEAD(2,0),UPTR(254,2260),UITAG(0),CONS_HEADER,UPTR(254,2263),TC_NIL,CONS_HEADER,UPTR(254,2248),
	UPTR(254,2266),CONS_HEADER,UPTR(254,2233),UPTR(254,2269),UOHEAD(2,0),UPTR(254,2272),UITAG(0),CONS_HEADER,UPTR(254,2275),TC_NIL,
	CONS_HEADER,UPTR(254,2230),UPTR(254,2278),CONS_HEADER,UPTR(254,2215),UPTR(254,2281),UOHEAD(2,0),UPTR(254,2284),UITAG(0),CONS_HEADER,
	UPTR(254,2287),TC_NIL,CONS_HEADER,UPTR(254,2200),UPTR(254,2290),CONS_HEADER,UPTR(254,2197),UPTR(254,2293),UOHEAD(2,0),UPTR(254,2296),
	UITAG(0),CONS_HEADER,UPTR(254,2299),TC_NIL,CONS_HEADER,UPTR(254,2194),UPTR(254,2302),UOHEAD(2,0),UPTR(254,2305),UITAG(0),
	CONS_HEADER,UPTR(254,2308),TC_NIL,CONS_HEADER,UPTR(254,2173),UPTR(254,2311),UOHEAD(2,0),UPTR(254,2314),UITAG(0),UOHEAD(2,1),
	UPTR(141,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(101,1),
	UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),CONS_HEADER,UPTR(254,2335),TC_NIL,
	UOHEAD(2,0),UPTR(254,2338),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(137,1),UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,2353),TC_NIL,CONS_HEADER,UPTR(254,2350),
	UPTR(254,2356),UOHEAD(2,0),UPTR(254,2359),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),
	CONS_HEADER,UPTR(254,2368),TC_NIL,CONS_HEADER,UPTR(254,2365),UPTR(254,2371),UOHEAD(2,0),UPTR(254,2374),UITAG(0),CONS_HEADER,
	UPTR(254,2377),TC_NIL,CONS_HEADER,UPTR(254,2362),UPTR(254,2380),CONS_HEADER,UPTR(254,2347),UPTR(254,2383),UOHEAD(2,0),UPTR(254,2386),
	UITAG(0),CONS_HEADER,UPTR(254,2389),TC_NIL,CONS_HEADER,UPTR(254,2344),UPTR(254,2392),UOHEAD(2,0),UPTR(254,2395),UITAG(0),
	UOHEAD(2,1),UPTR(113,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(254,2404),TC_NIL,UOHEAD(2,0),
	UPTR(254,2407),UITAG(0),CONS_HEADER,UPTR(254,2410),TC_NIL,CONS_HEADER,UPTR(254,2401),UPTR(254,2413),UOHEAD(2,0),UPTR(254,2416),
	UITAG(0),CONS_HEADER,UPTR(254,2419),TC_NIL,CONS_HEADER,UPTR(254,2398),UPTR(254,2422),CONS_HEADER,UPTR(254,2341),UPTR(254,2425),
	CONS_HEADER,UPTR(254,2332),UPTR(254,2428),UOHEAD(2,0),UPTR(254,2431),UITAG(0),CONS_HEADER,UPTR(254,2434),TC_NIL,CONS_HEADER,
	UPTR(254,2329),UPTR(254,2437),UOHEAD(2,0),UPTR(254,2440),UITAG(0),CONS_HEADER,UPTR(254,2443),TC_NIL,UOHEAD(2,0),UPTR(254,2446),
	UITAG(0),CONS_HEADER,UPTR(254,2449),TC_NIL,CONS_HEADER,UPTR(254,2326),UPTR(254,2452),CONS_HEADER,UPTR(254,2323),UPTR(254,2455),
	UOHEAD(2,0),UPTR(254,2458),UITAG(0),CONS_HEADER,UPTR(254,2461),TC_NIL,UOHEAD(2,0),UPTR(254,2464),UITAG(0),CONS_HEADER,
	UPTR(254,2467),TC_NIL,CONS_HEADER,UPTR(254,2320),UPTR(254,2470),UOHEAD(2,0),UPTR(254,2473),UITAG(0),UOHEAD(2,1),UPTR(143,1),
	UITAG(0),UOHEAD(2,1),UPTR(35,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(254,2488),TC_NIL,CONS_HEADER,UPTR(254,2485),UPTR(254,2491),CONS_HEADER,UPTR(254,2482),UPTR(254,2494),UOHEAD(2,0),
	UPTR(254,2497),UITAG(0),UOHEAD(2,1),UPTR(145,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(254,2509),TC_NIL,CONS_HEADER,UPTR(254,2506),UPTR(254,2512),CONS_HEADER,UPTR(254,2503),UPTR(254,2515),
	UOHEAD(2,0),UPTR(254,2518),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,2530),TC_NIL,CONS_HEADER,UPTR(254,2527),UPTR(254,2533),CONS_HEADER,UPTR(254,2524),
	UPTR(254,2536),UOHEAD(2,0),UPTR(254,2539),UITAG(0),UOHEAD(2,1),UPTR(147,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,2551),TC_NIL,CONS_HEADER,UPTR(254,2548),UPTR(254,2554),CONS_HEADER,
	UPTR(254,2545),UPTR(254,2557),UOHEAD(2,0),UPTR(254,2560),UITAG(0),UOHEAD(2,1),UPTR(149,1),UITAG(0),UOHEAD(2,5),9,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,2572),TC_NIL,CONS_HEADER,UPTR(254,2569),UPTR(254,2575),
	CONS_HEADER,UPTR(254,2566),UPTR(254,2578),UOHEAD(2,0),UPTR(254,2581),UITAG(0),UOHEAD(2,1),UPTR(151,1),UITAG(0),UOHEAD(2,5),
	11,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,2593),TC_NIL,UOHEAD(2,0),UPTR(254,2596),
	UITAG(0),CONS_HEADER,UPTR(254,2599),TC_NIL,UOHEAD(2,0),UPTR(254,2602),UITAG(0),CONS_HEADER,UPTR(254,2605),TC_NIL,
	CONS_HEADER,UPTR(254,2590),UPTR(254,2608),CONS_HEADER,UPTR(254,2587),UPTR(254,2611),UOHEAD(2,0),UPTR(254,2614),UITAG(0),CONS_HEADER,
	UPTR(254,2617),TC_NIL,CONS_HEADER,UPTR(254,2584),UPTR(254,2620),CONS_HEADER,UPTR(254,2563),UPTR(254,2623),CONS_HEADER,UPTR(254,2542),
	UPTR(254,2626),CONS_HEADER,UPTR(254,2521),UPTR(254,2629),CONS_HEADER,UPTR(254,2500),UPTR(254,2632),UOHEAD(2,0),UPTR(254,2635),UITAG(0),
	CONS_HEADER,UPTR(254,2638),TC_NIL,CONS_HEADER,UPTR(254,2479),UPTR(254,2641),UOHEAD(2,0),UPTR(254,2644),UITAG(0),UOHEAD(2,1),
	UPTR(153,1),UITAG(0),UOHEAD(2,1),UPTR(155,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(28,1),
	UITAG(0),CONS_HEADER,UPTR(254,2659),TC_NIL,UOHEAD(2,0),UPTR(254,2662),UITAG(0),CONS_HEADER,UPTR(254,2665),TC_NIL,
	UOHEAD(2,0),UPTR(254,2668),UITAG(0),CONS_HEADER,UPTR(254,2671),TC_NIL,CONS_HEADER,UPTR(254,2656),UPTR(254,2674),CONS_HEADER,
	UPTR(254,2653),UPTR(254,2677),UOHEAD(2,0),UPTR(254,2680),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),UOHEAD(2,5),3,
	UITAG(0),UOHEAD(2,1),UPTR(143,1),UITAG(0),CONS_HEADER,UPTR(254,2692),TC_NIL,UOHEAD(2,0),UPTR(254,2695),UITAG(0),
	UOHEAD(2,1),UPTR(37,1),UITAG(0),CONS_HEADER,UPTR(254,2701),TC_NIL,UOHEAD(2,0),UPTR(254,2704),UITAG(0),CONS_HEADER,
	UPTR(254,2707),TC_NIL,CONS_HEADER,UPTR(254,2698),UPTR(254,2710),UOHEAD(2,0),UPTR(254,2713),UITAG(0),CONS_HEADER,UPTR(254,2716),
	TC_NIL,CONS_HEADER,UPTR(254,2689),UPTR(254,2719),CONS_HEADER,UPTR(254,2686),UPTR(254,2722),UOHEAD(2,0),UPTR(254,2725),UITAG(0),
	UOHEAD(2,1),UPTR(157,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,
	UPTR(254,2737),TC_NIL,UOHEAD(2,0),UPTR(254,2740),UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),CONS_HEADER,UPTR(254,2746),
	TC_NIL,UOHEAD(2,0),UPTR(254,2749),UITAG(0),CONS_HEADER,UPTR(254,2752),TC_NIL,CONS_HEADER,UPTR(254,2743),UPTR(254,2755),
	UOHEAD(2,0),UPTR(254,2758),UITAG(0),CONS_HEADER,UPTR(254,2761),TC_NIL,CONS_HEADER,UPTR(254,2734),UPTR(254,2764),CONS_HEADER,
	UPTR(254,2731),UPTR(254,2767),UOHEAD(2,0),UPTR(254,2770),UITAG(0),UOHEAD(2,1),UPTR(159,1),UITAG(0),UOHEAD(2,5),7,
	UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),CONS_HEADER,UPTR(254,2782),TC_NIL,UOHEAD(2,0),UPTR(254,2785),UITAG(0),
	CONS_HEADER,UPTR(254,2788),TC_NIL,UOHEAD(2,0),UPTR(254,2791),UITAG(0),CONS_HEADER,UPTR(254,2794),TC_NIL,CONS_HEADER,
	UPTR(254,2779),UPTR(254,2797),CONS_HEADER,UPTR(254,2776),UPTR(254,2800),UOHEAD(2,0),UPTR(254,2803),UITAG(0),UOHEAD(2,1),UPTR(161,1),
	UITAG(0),UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,2815),TC_NIL,
	UOHEAD(2,0),UPTR(254,2818),UITAG(0),CONS_HEADER,UPTR(254,2821),TC_NIL,UOHEAD(2,0),UPTR(254,2824),UITAG(0),CONS_HEADER,
	UPTR(254,2827),TC_NIL,CONS_HEADER,UPTR(254,2812),UPTR(254,2830),CONS_HEADER,UPTR(254,2809),UPTR(254,2833),UOHEAD(2,0),UPTR(254,2836),
	UITAG(0),UOHEAD(2,1),UPTR(163,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),
	CONS_HEADER,UPTR(254,2848),TC_NIL,UOHEAD(2,0),UPTR(254,2851),UITAG(0),CONS_HEADER,UPTR(254,2854),TC_NIL,UOHEAD(2,0),
	UPTR(254,2857),UITAG(0),CONS_HEADER,UPTR(254,2860),TC_NIL,CONS_HEADER,UPTR(254,2845),UPTR(254,2863),CONS_HEADER,UPTR(254,2842),
	UPTR(254,2866),UOHEAD(2,0),UPTR(254,2869),UITAG(0),CONS_HEADER,UPTR(254,2872),TC_NIL,CONS_HEADER,UPTR(254,2839),UPTR(254,2875),
	CONS_HEADER,UPTR(254,2806),UPTR(254,2878),CONS_HEADER,UPTR(254,2773),UPTR(254,2881),CONS_HEADER,UPTR(254,2728),UPTR(254,2884),CONS_HEADER,
	UPTR(254,2683),UPTR(254,2887),UOHEAD(2,0),UPTR(254,2890),UITAG(0),CONS_HEADER,UPTR(254,2893),TC_NIL,CONS_HEADER,UPTR(254,2650),
	UPTR(254,2896),UOHEAD(2,0),UPTR(254,2899),UITAG(0),UOHEAD(2,1),UPTR(165,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,2914),TC_NIL,UOHEAD(2,0),
	UPTR(254,2917),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,2923),TC_NIL,UOHEAD(2,0),UPTR(254,2926),
	UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),CONS_HEADER,UPTR(254,2932),TC_NIL,UOHEAD(2,0),UPTR(254,2935),UITAG(0),
	CONS_HEADER,UPTR(254,2938),TC_NIL,CONS_HEADER,UPTR(254,2929),UPTR(254,2941),CONS_HEADER,UPTR(254,2920),UPTR(254,2944),UOHEAD(2,0),
	UPTR(254,2947),UITAG(0),CONS_HEADER,UPTR(254,2950),TC_NIL,CONS_HEADER,UPTR(254,2911),UPTR(254,2953),CONS_HEADER,UPTR(254,2908),
	UPTR(254,2956),UOHEAD(2,0),UPTR(254,2959),UITAG(0),CONS_HEADER,UPTR(254,2962),TC_NIL,UOHEAD(2,0),UPTR(254,2965),UITAG(0),
	CONS_HEADER,UPTR(254,2968),TC_NIL,CONS_HEADER,UPTR(254,2905),UPTR(254,2971),UOHEAD(2,0),UPTR(254,2974),UITAG(0),UOHEAD(2,1),
	UPTR(167,1),UITAG(0),UOHEAD(2,1),UPTR(161,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(6,1),
	UITAG(0),CONS_HEADER,UPTR(254,2989),TC_NIL,UOHEAD(2,0),UPTR(254,2992),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),
	CONS_HEADER,UPTR(254,2998),TC_NIL,UOHEAD(2,0),UPTR(254,3001),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),
	UPTR(165,1),UITAG(0),CONS_HEADER,UPTR(254,3010),TC_NIL,UOHEAD(2,0),UPTR(254,3013),UITAG(0),CONS_HEADER,UPTR(254,3016),
	TC_NIL,CONS_HEADER,UPTR(254,3007),UPTR(254,3019),UOHEAD(2,0),UPTR(254,3022),UITAG(0),CONS_HEADER,UPTR(254,3025),TC_NIL,
	CONS_HEADER,UPTR(254,3004),UPTR(254,3028),CONS_HEADER,UPTR(254,2995),UPTR(254,3031),UOHEAD(2,0),UPTR(254,3034),UITAG(0),CONS_HEADER,
	UPTR(254,3037),TC_NIL,CONS_HEADER,UPTR(254,2986),UPTR(254,3040),CONS_HEADER,UPTR(254,2983),UPTR(254,3043),UOHEAD(2,0),UPTR(254,3046),
	UITAG(0),UOHEAD(2,1),UPTR(163,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,3058),TC_NIL,UOHEAD(2,0),UPTR(254,3061),UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,
	UPTR(254,3067),TC_NIL,UOHEAD(2,0),UPTR(254,3070),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(165,1),
	UITAG(0),CONS_HEADER,UPTR(254,3079),TC_NIL,UOHEAD(2,0),UPTR(254,3082),UITAG(0),CONS_HEADER,UPTR(254,3085),TC_NIL,
	CONS_HEADER,UPTR(254,3076),UPTR(254,3088),UOHEAD(2,0),UPTR(254,3091),UITAG(0),CONS_HEADER,UPTR(254,3094),TC_NIL,CONS_HEADER,
	UPTR(254,3073),UPTR(254,3097),CONS_HEADER,UPTR(254,3064),UPTR(254,3100),UOHEAD(2,0),UPTR(254,3103),UITAG(0),CONS_HEADER,UPTR(254,3106),
	TC_NIL,CONS_HEADER,UPTR(254,3055),UPTR(254,3109),CONS_HEADER,UPTR(254,3052),UPTR(254,3112),UOHEAD(2,0),UPTR(254,3115),UITAG(0),
	CONS_HEADER,UPTR(254,3118),TC_NIL,CONS_HEADER,UPTR(254,3049),UPTR(254,3121),UOHEAD(2,0),UPTR(254,3124),UITAG(0),CONS_HEADER,
	UPTR(254,3127),TC_NIL,CONS_HEADER,UPTR(254,2980),UPTR(254,3130),UOHEAD(2,0),UPTR(254,3133),UITAG(0),UOHEAD(2,1),UPTR(169,1),
	UITAG(0),UOHEAD(2,1),UPTR(171,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),
	CONS_HEADER,UPTR(254,3148),TC_NIL,UOHEAD(2,0),UPTR(254,3151),UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),CONS_HEADER,
	UPTR(254,3157),TC_NIL,UOHEAD(2,0),UPTR(254,3160),UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(153,1),
	UITAG(0),CONS_HEADER,UPTR(254,3169),TC_NIL,UOHEAD(2,0),UPTR(254,3172),UITAG(0),CONS_HEADER,UPTR(254,3175),TC_NIL,
	CONS_HEADER,UPTR(254,3166),UPTR(254,3178),UOHEAD(2,0),UPTR(254,3181),UITAG(0),UOHEAD(2,1),UPTR(37,1),UITAG(0),CONS_HEADER,
	UPTR(254,3187),TC_NIL,UOHEAD(2,0),UPTR(254,3190),UITAG(0),CONS_HEADER,UPTR(254,3193),TC_NIL,CONS_HEADER,UPTR(254,3184),
	UPTR(254,3196),CONS_HEADER,UPTR(254,3163),UPTR(254,3199),CONS_HEADER,UPTR(254,3154),UPTR(254,3202),UOHEAD(2,0),UPTR(254,3205),UITAG(0),
	CONS_HEADER,UPTR(254,3208),TC_NIL,CONS_HEADER,UPTR(254,3145),UPTR(254,3211),CONS_HEADER,UPTR(254,3142),UPTR(254,3214),UOHEAD(2,0),
	UPTR(254,3217),UITAG(0),UOHEAD(2,1),UPTR(173,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(28,1),
	UITAG(0),CONS_HEADER,UPTR(254,3229),TC_NIL,UOHEAD(2,0),UPTR(254,3232),UITAG(0),UOHEAD(2,1),UPTR(153,1),UITAG(0),
	CONS_HEADER,UPTR(254,3238),TC_NIL,UOHEAD(2,0),UPTR(254,3241),UITAG(0),CONS_HEADER,UPTR(254,3244),TC_NIL,CONS_HEADER,
	UPTR(254,3235),UPTR(254,3247),UOHEAD(2,0),UPTR(254,3250),UITAG(0),CONS_HEADER,UPTR(254,3253),TC_NIL,CONS_HEADER,UPTR(254,3226),
	UPTR(254,3256),CONS_HEADER,UPTR(254,3223),UPTR(254,3259),UOHEAD(2,0),UPTR(254,3262),UITAG(0),CONS_HEADER,UPTR(254,3265),TC_NIL,
	CONS_HEADER,UPTR(254,3220),UPTR(254,3268),UOHEAD(2,0),UPTR(254,3271),UITAG(0),CONS_HEADER,UPTR(254,3274),TC_NIL,CONS_HEADER,
	UPTR(254,3139),UPTR(254,3277),UOHEAD(2,0),UPTR(254,3280),UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,1),UPTR(131,1),
	UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3301),TC_NIL,CONS_HEADER,UPTR(254,3298),UPTR(254,3304),UOHEAD(2,0),
	UPTR(254,3307),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,3316),
	TC_NIL,CONS_HEADER,UPTR(254,3313),UPTR(254,3319),UOHEAD(2,0),UPTR(254,3322),UITAG(0),CONS_HEADER,UPTR(254,3325),TC_NIL,
	CONS_HEADER,UPTR(254,3310),UPTR(254,3328),CONS_HEADER,UPTR(254,3295),UPTR(254,3331),UOHEAD(2,0),UPTR(254,3334),UITAG(0),UOHEAD(2,1),
	UPTR(175,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3346),
	TC_NIL,CONS_HEADER,UPTR(254,3343),UPTR(254,3349),UOHEAD(2,0),UPTR(254,3352),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,3361),TC_NIL,CONS_HEADER,UPTR(254,3358),UPTR(254,3364),UOHEAD(2,0),
	UPTR(254,3367),UITAG(0),CONS_HEADER,UPTR(254,3370),TC_NIL,CONS_HEADER,UPTR(254,3355),UPTR(254,3373),CONS_HEADER,UPTR(254,3340),
	UPTR(254,3376),UOHEAD(2,0),UPTR(254,3379),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),
	CONS_HEADER,UPTR(254,3388),TC_NIL,CONS_HEADER,UPTR(254,3385),UPTR(254,3391),UOHEAD(2,0),UPTR(254,3394),UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,3403),TC_NIL,CONS_HEADER,UPTR(254,3400),
	UPTR(254,3406),UOHEAD(2,0),UPTR(254,3409),UITAG(0),CONS_HEADER,UPTR(254,3412),TC_NIL,CONS_HEADER,UPTR(254,3397),UPTR(254,3415),
	CONS_HEADER,UPTR(254,3382),UPTR(254,3418),CONS_HEADER,UPTR(254,3337),UPTR(254,3421),UOHEAD(2,0),UPTR(254,3424),UITAG(0),CONS_HEADER,
	UPTR(254,3427),TC_NIL,CONS_HEADER,UPTR(254,3292),UPTR(254,3430),CONS_HEADER,UPTR(254,3289),UPTR(254,3433),UOHEAD(2,0),UPTR(254,3436),
	UITAG(0),UOHEAD(2,1),UPTR(133,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),
	UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3454),TC_NIL,CONS_HEADER,
	UPTR(254,3451),UPTR(254,3457),UOHEAD(2,0),UPTR(254,3460),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),
	UITAG(0),CONS_HEADER,UPTR(254,3469),TC_NIL,CONS_HEADER,UPTR(254,3466),UPTR(254,3472),UOHEAD(2,0),UPTR(254,3475),UITAG(0),
	CONS_HEADER,UPTR(254,3478),TC_NIL,CONS_HEADER,UPTR(254,3463),UPTR(254,3481),CONS_HEADER,UPTR(254,3448),UPTR(254,3484),UOHEAD(2,0),
	UPTR(254,3487),UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),
	UITAG(0),CONS_HEADER,UPTR(254,3499),TC_NIL,CONS_HEADER,UPTR(254,3496),UPTR(254,3502),UOHEAD(2,0),UPTR(254,3505),UITAG(0),
	UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,3514),TC_NIL,CONS_HEADER,
	UPTR(254,3511),UPTR(254,3517),UOHEAD(2,0),UPTR(254,3520),UITAG(0),CONS_HEADER,UPTR(254,3523),TC_NIL,CONS_HEADER,UPTR(254,3508),
	UPTR(254,3526),CONS_HEADER,UPTR(254,3493),UPTR(254,3529),UOHEAD(2,0),UPTR(254,3532),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3541),TC_NIL,CONS_HEADER,UPTR(254,3538),UPTR(254,3544),UOHEAD(2,0),
	UPTR(254,3547),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,3556),
	TC_NIL,CONS_HEADER,UPTR(254,3553),UPTR(254,3559),UOHEAD(2,0),UPTR(254,3562),UITAG(0),CONS_HEADER,UPTR(254,3565),TC_NIL,
	CONS_HEADER,UPTR(254,3550),UPTR(254,3568),CONS_HEADER,UPTR(254,3535),UPTR(254,3571),CONS_HEADER,UPTR(254,3490),UPTR(254,3574),UOHEAD(2,0),
	UPTR(254,3577),UITAG(0),CONS_HEADER,UPTR(254,3580),TC_NIL,CONS_HEADER,UPTR(254,3445),UPTR(254,3583),CONS_HEADER,UPTR(254,3442),
	UPTR(254,3586),UOHEAD(2,0),UPTR(254,3589),UITAG(0),UOHEAD(2,1),UPTR(135,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,3601),TC_NIL,CONS_HEADER,UPTR(254,3598),UPTR(254,3604),CONS_HEADER,
	UPTR(254,3595),UPTR(254,3607),UOHEAD(2,0),UPTR(254,3610),UITAG(0),CONS_HEADER,UPTR(254,3613),TC_NIL,CONS_HEADER,UPTR(254,3592),
	UPTR(254,3616),CONS_HEADER,UPTR(254,3439),UPTR(254,3619),UOHEAD(2,0),UPTR(254,3622),UITAG(0),CONS_HEADER,UPTR(254,3625),TC_NIL,
	CONS_HEADER,UPTR(254,3286),UPTR(254,3628),UOHEAD(2,0),UPTR(254,3631),UITAG(0),UOHEAD(2,1),UPTR(177,1),UITAG(0),UOHEAD(2,1),
	UPTR(6,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,3646),
	TC_NIL,UOHEAD(2,0),UPTR(254,3649),UITAG(0),CONS_HEADER,UPTR(254,3652),TC_NIL,UOHEAD(2,0),UPTR(254,3655),UITAG(0),
	CONS_HEADER,UPTR(254,3658),TC_NIL,CONS_HEADER,UPTR(254,3643),UPTR(254,3661),CONS_HEADER,UPTR(254,3640),UPTR(254,3664),UOHEAD(2,0),
	UPTR(254,3667),UITAG(0),UOHEAD(2,1),UPTR(179,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,1),UPTR(6,1),
	UITAG(0),CONS_HEADER,UPTR(254,3679),TC_NIL,UOHEAD(2,0),UPTR(254,3682),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,3688),TC_NIL,UOHEAD(2,0),UPTR(254,3691),UITAG(0),CONS_HEADER,UPTR(254,3694),TC_NIL,CONS_HEADER,
	UPTR(254,3685),UPTR(254,3697),UOHEAD(2,0),UPTR(254,3700),UITAG(0),CONS_HEADER,UPTR(254,3703),TC_NIL,CONS_HEADER,UPTR(254,3676),
	UPTR(254,3706),CONS_HEADER,UPTR(254,3673),UPTR(254,3709),UOHEAD(2,0),UPTR(254,3712),UITAG(0),UOHEAD(2,1),UPTR(181,1),UITAG(0),
	UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,3724),TC_NIL,UOHEAD(2,0),
	UPTR(254,3727),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,3733),TC_NIL,UOHEAD(2,0),UPTR(254,3736),
	UITAG(0),CONS_HEADER,UPTR(254,3739),TC_NIL,CONS_HEADER,UPTR(254,3730),UPTR(254,3742),UOHEAD(2,0),UPTR(254,3745),UITAG(0),
	CONS_HEADER,UPTR(254,3748),TC_NIL,CONS_HEADER,UPTR(254,3721),UPTR(254,3751),CONS_HEADER,UPTR(254,3718),UPTR(254,3754),UOHEAD(2,0),
	UPTR(254,3757),UITAG(0),UOHEAD(2,1),UPTR(183,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(28,1),
	UITAG(0),CONS_HEADER,UPTR(254,3769),TC_NIL,UOHEAD(2,0),UPTR(254,3772),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,3778),TC_NIL,UOHEAD(2,0),UPTR(254,3781),UITAG(0),CONS_HEADER,UPTR(254,3784),TC_NIL,CONS_HEADER,
	UPTR(254,3775),UPTR(254,3787),UOHEAD(2,0),UPTR(254,3790),UITAG(0),CONS_HEADER,UPTR(254,3793),TC_NIL,CONS_HEADER,UPTR(254,3766),
	UPTR(254,3796),CONS_HEADER,UPTR(254,3763),UPTR(254,3799),UOHEAD(2,0),UPTR(254,3802),UITAG(0),UOHEAD(2,1),UPTR(185,1),UITAG(0),
	UOHEAD(2,5),9,UITAG(0),UOHEAD(2,1),UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,3814),TC_NIL,UOHEAD(2,0),
	UPTR(254,3817),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,3823),TC_NIL,UOHEAD(2,0),UPTR(254,3826),
	UITAG(0),CONS_HEADER,UPTR(254,3829),TC_NIL,CONS_HEADER,UPTR(254,3820),UPTR(254,3832),UOHEAD(2,0),UPTR(254,3835),UITAG(0),
	CONS_HEADER,UPTR(254,3838),TC_NIL,CONS_HEADER,UPTR(254,3811),UPTR(254,3841),CONS_HEADER,UPTR(254,3808),UPTR(254,3844),UOHEAD(2,0),
	UPTR(254,3847),UITAG(0),CONS_HEADER,UPTR(254,3850),TC_NIL,CONS_HEADER,UPTR(254,3805),UPTR(254,3853),CONS_HEADER,UPTR(254,3760),
	UPTR(254,3856),CONS_HEADER,UPTR(254,3715),UPTR(254,3859),CONS_HEADER,UPTR(254,3670),UPTR(254,3862),UOHEAD(2,0),UPTR(254,3865),UITAG(0),
	CONS_HEADER,UPTR(254,3868),TC_NIL,CONS_HEADER,UPTR(254,3637),UPTR(254,3871),UOHEAD(2,0),UPTR(254,3874),UITAG(0),UOHEAD(2,1),
	UPTR(187,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(101,1),
	UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),CONS_HEADER,UPTR(254,3895),TC_NIL,
	UOHEAD(2,0),UPTR(254,3898),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3913),TC_NIL,CONS_HEADER,UPTR(254,3910),
	UPTR(254,3916),UOHEAD(2,0),UPTR(254,3919),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),
	CONS_HEADER,UPTR(254,3928),TC_NIL,CONS_HEADER,UPTR(254,3925),UPTR(254,3931),UOHEAD(2,0),UPTR(254,3934),UITAG(0),CONS_HEADER,
	UPTR(254,3937),TC_NIL,CONS_HEADER,UPTR(254,3922),UPTR(254,3940),CONS_HEADER,UPTR(254,3907),UPTR(254,3943),UOHEAD(2,0),UPTR(254,3946),
	UITAG(0),CONS_HEADER,UPTR(254,3949),TC_NIL,CONS_HEADER,UPTR(254,3904),UPTR(254,3952),UOHEAD(2,0),UPTR(254,3955),UITAG(0),
	UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(53,1),UITAG(0),CONS_HEADER,UPTR(254,3964),TC_NIL,UOHEAD(2,0),
	UPTR(254,3967),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(189,1),UITAG(0),UOHEAD(2,1),UPTR(53,1),
	UITAG(0),CONS_HEADER,UPTR(254,3979),TC_NIL,UOHEAD(2,0),UPTR(254,3982),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,3991),TC_NIL,CONS_HEADER,UPTR(254,3988),UPTR(254,3994),UOHEAD(2,0),
	UPTR(254,3997),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,4006),
	TC_NIL,CONS_HEADER,UPTR(254,4003),UPTR(254,4009),UOHEAD(2,0),UPTR(254,4012),UITAG(0),CONS_HEADER,UPTR(254,4015),TC_NIL,
	CONS_HEADER,UPTR(254,4000),UPTR(254,4018),CONS_HEADER,UPTR(254,3985),UPTR(254,4021),CONS_HEADER,UPTR(254,3976),UPTR(254,4024),UOHEAD(2,0),
	UPTR(254,4027),UITAG(0),CONS_HEADER,UPTR(254,4030),TC_NIL,CONS_HEADER,UPTR(254,3973),UPTR(254,4033),UOHEAD(2,0),UPTR(254,4036),
	UITAG(0),UOHEAD(2,1),UPTR(113,1),UITAG(0),UOHEAD(2,1),UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(254,4045),TC_NIL,
	UOHEAD(2,0),UPTR(254,4048),UITAG(0),CONS_HEADER,UPTR(254,4051),TC_NIL,CONS_HEADER,UPTR(254,4042),UPTR(254,4054),UOHEAD(2,0),
	UPTR(254,4057),UITAG(0),CONS_HEADER,UPTR(254,4060),TC_NIL,CONS_HEADER,UPTR(254,4039),UPTR(254,4063),CONS_HEADER,UPTR(254,3970),
	UPTR(254,4066),CONS_HEADER,UPTR(254,3961),UPTR(254,4069),UOHEAD(2,0),UPTR(254,4072),UITAG(0),CONS_HEADER,UPTR(254,4075),TC_NIL,
	CONS_HEADER,UPTR(254,3958),UPTR(254,4078),CONS_HEADER,UPTR(254,3901),UPTR(254,4081),CONS_HEADER,UPTR(254,3892),UPTR(254,4084),UOHEAD(2,0),
	UPTR(254,4087),UITAG(0),CONS_HEADER,UPTR(254,4090),TC_NIL,CONS_HEADER,UPTR(254,3889),UPTR(254,4093),UOHEAD(2,0),UPTR(254,4096),
	UITAG(0),CONS_HEADER,UPTR(254,4099),TC_NIL,UOHEAD(2,0),UPTR(254,4102),UITAG(0),CONS_HEADER,UPTR(254,4105),TC_NIL,
	CONS_HEADER,UPTR(254,3886),UPTR(254,4108),CONS_HEADER,UPTR(254,3883),UPTR(254,4111),UOHEAD(2,0),UPTR(254,4114),UITAG(0),CONS_HEADER,
	UPTR(254,4117),TC_NIL,UOHEAD(2,0),UPTR(254,4120),UITAG(0),CONS_HEADER,UPTR(254,4123),TC_NIL,CONS_HEADER,UPTR(254,3880),
	UPTR(254,4126),UOHEAD(2,0),UPTR(254,4129),UITAG(0),UOHEAD(2,1),UPTR(191,1),UITAG(0),UOHEAD(2,1),UPTR(51,1),UITAG(0),
	UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(101,1),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),
	UPTR(51,1),UITAG(0),CONS_HEADER,UPTR(254,4150),TC_NIL,UOHEAD(2,0),UPTR(254,4153),UITAG(0),UOHEAD(2,1),UPTR(107,1),
	UITAG(0),UOHEAD(2,1),UPTR(175,1),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),
	CONS_HEADER,UPTR(254,4168),TC_NIL,CONS_HEADER,UPTR(254,4165),UPTR(254,4171),UOHEAD(2,0),UPTR(254,4174),UITAG(0),UOHEAD(2,1),
	UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(81,1),UITAG(0),CONS_HEADER,UPTR(254,4183),TC_NIL,CONS_HEADER,UPTR(254,4180),
	UPTR(254,4186),UOHEAD(2,0),UPTR(254,4189),UITAG(0),CONS_HEADER,UPTR(254,4192),TC_NIL,CONS_HEADER,UPTR(254,4177),UPTR(254,4195),
	CONS_HEADER,UPTR(254,4162),UPTR(254,4198),UOHEAD(2,0),UPTR(254,4201),UITAG(0),CONS_HEADER,UPTR(254,4204),TC_NIL,CONS_HEADER,
	UPTR(254,4159),UPTR(254,4207),UOHEAD(2,0),UPTR(254,4210),UITAG(0),UOHEAD(2,1),UPTR(103,1),UITAG(0),UOHEAD(2,1),UPTR(53,1),
	UITAG(0),CONS_HEADER,UPTR(254,4219),TC_NIL,UOHEAD(2,0),UPTR(254,4222),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),
	UOHEAD(2,1),UPTR(189,1),UITAG(0),UOHEAD(2,1),UPTR(53,1),UITAG(0),CONS_HEADER,UPTR(254,4234),TC_NIL,UOHEAD(2,0),
	UPTR(254,4237),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,4246),
	TC_NIL,CONS_HEADER,UPTR(254,4243),UPTR(254,4249),UOHEAD(2,0),UPTR(254,4252),UITAG(0),UOHEAD(2,1),UPTR(65,1),UITAG(0),
	UOHEAD(2,1),UPTR(67,1),UITAG(0),CONS_HEADER,UPTR(254,4261),TC_NIL,CONS_HEADER,UPTR(254,4258),UPTR(254,4264),UOHEAD(2,0),
	UPTR(254,4267),UITAG(0),CONS_HEADER,UPTR(254,4270),TC_NIL,CONS_HEADER,UPTR(254,4255),UPTR(254,4273),CONS_HEADER,UPTR(254,4240),
	UPTR(254,4276),CONS_HEADER,UPTR(254,4231),UPTR(254,4279),UOHEAD(2,0),UPTR(254,4282),UITAG(0),CONS_HEADER,UPTR(254,4285),TC_NIL,
	CONS_HEADER,UPTR(254,4228),UPTR(254,4288),UOHEAD(2,0),UPTR(254,4291),UITAG(0),UOHEAD(2,1),UPTR(113,1),UITAG(0),UOHEAD(2,1),
	UPTR(115,1),UITAG(0),CONS_HEADER,UPTR(254,4300),TC_NIL,UOHEAD(2,0),UPTR(254,4303),UITAG(0),CONS_HEADER,UPTR(254,4306),
	TC_NIL,CONS_HEADER,UPTR(254,4297),UPTR(254,4309),UOHEAD(2,0),UPTR(254,4312),UITAG(0),CONS_HEADER,UPTR(254,4315),TC_NIL,
	CONS_HEADER,UPTR(254,4294),UPTR(254,4318),CONS_HEADER,UPTR(254,4225),UPTR(254,4321),CONS_HEADER,UPTR(254,4216),UPTR(254,4324),UOHEAD(2,0),
	UPTR(254,4327),UITAG(0),CONS_HEADER,UPTR(254,4330),TC_NIL,CONS_HEADER,UPTR(254,4213),UPTR(254,4333),CONS_HEADER,UPTR(254,4156),
	UPTR(254,4336),CONS_HEADER,UPTR(254,4147),UPTR(254,4339),UOHEAD(2,0),UPTR(254,4342),UITAG(0),CONS_HEADER,UPTR(254,4345),TC_NIL,
	CONS_HEADER,UPTR(254,4144),UPTR(254,4348),UOHEAD(2,0),UPTR(254,4351),UITAG(0),CONS_HEADER,UPTR(254,4354),TC_NIL,UOHEAD(2,0),
	UPTR(254,4357),UITAG(0),CONS_HEADER,UPTR(254,4360),TC_NIL,CONS_HEADER,UPTR(254,4141),UPTR(254,4363),CONS_HEADER,UPTR(254,4138),
	UPTR(254,4366),UOHEAD(2,0),UPTR(254,4369),UITAG(0),CONS_HEADER,UPTR(254,4372),TC_NIL,UOHEAD(2,0),UPTR(254,4375),UITAG(0),
	CONS_HEADER,UPTR(254,4378),TC_NIL,CONS_HEADER,UPTR(254,4135),UPTR(254,4381),UOHEAD(2,0),UPTR(254,4384),UITAG(0),UOHEAD(2,1),
	UPTR(193,1),UITAG(0),UOHEAD(2,1),UPTR(195,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,1),UPTR(6,1),
	UITAG(0),CONS_HEADER,UPTR(254,4399),TC_NIL,UOHEAD(2,0),UPTR(254,4402),UITAG(0),CONS_HEADER,UPTR(254,4405),TC_NIL,
	UOHEAD(2,0),UPTR(254,4408),UITAG(0),CONS_HEADER,UPTR(254,4411),TC_NIL,CONS_HEADER,UPTR(254,4396),UPTR(254,4414),CONS_HEADER,
	UPTR(254,4393),UPTR(254,4417),UOHEAD(2,0),UPTR(254,4420),UITAG(0),UOHEAD(2,1),UPTR(197,1),UITAG(0),UOHEAD(2,5),3,
	UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4432),TC_NIL,UOHEAD(2,0),UPTR(254,4435),UITAG(0),
	CONS_HEADER,UPTR(254,4438),TC_NIL,UOHEAD(2,0),UPTR(254,4441),UITAG(0),CONS_HEADER,UPTR(254,4444),TC_NIL,CONS_HEADER,
	UPTR(254,4429),UPTR(254,4447),CONS_HEADER,UPTR(254,4426),UPTR(254,4450),UOHEAD(2,0),UPTR(254,4453),UITAG(0),UOHEAD(2,1),UPTR(199,1),
	UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4465),TC_NIL,
	UOHEAD(2,0),UPTR(254,4468),UITAG(0),CONS_HEADER,UPTR(254,4471),TC_NIL,UOHEAD(2,0),UPTR(254,4474),UITAG(0),CONS_HEADER,
	UPTR(254,4477),TC_NIL,CONS_HEADER,UPTR(254,4462),UPTR(254,4480),CONS_HEADER,UPTR(254,4459),UPTR(254,4483),UOHEAD(2,0),UPTR(254,4486),
	UITAG(0),UOHEAD(2,1),UPTR(201,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,4498),TC_NIL,UOHEAD(2,0),UPTR(254,4501),UITAG(0),CONS_HEADER,UPTR(254,4504),TC_NIL,UOHEAD(2,0),
	UPTR(254,4507),UITAG(0),CONS_HEADER,UPTR(254,4510),TC_NIL,CONS_HEADER,UPTR(254,4495),UPTR(254,4513),CONS_HEADER,UPTR(254,4492),
	UPTR(254,4516),UOHEAD(2,0),UPTR(254,4519),UITAG(0),UOHEAD(2,1),UPTR(203,1),UITAG(0),UOHEAD(2,5),9,UITAG(0),
	UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4531),TC_NIL,UOHEAD(2,0),UPTR(254,4534),UITAG(0),CONS_HEADER,
	UPTR(254,4537),TC_NIL,UOHEAD(2,0),UPTR(254,4540),UITAG(0),CONS_HEADER,UPTR(254,4543),TC_NIL,CONS_HEADER,UPTR(254,4528),
	UPTR(254,4546),CONS_HEADER,UPTR(254,4525),UPTR(254,4549),UOHEAD(2,0),UPTR(254,4552),UITAG(0),UOHEAD(2,1),UPTR(205,1),UITAG(0),
	UOHEAD(2,5),11,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4564),TC_NIL,UOHEAD(2,0),
	UPTR(254,4567),UITAG(0),CONS_HEADER,UPTR(254,4570),TC_NIL,UOHEAD(2,0),UPTR(254,4573),UITAG(0),CONS_HEADER,UPTR(254,4576),
	TC_NIL,CONS_HEADER,UPTR(254,4561),UPTR(254,4579),CONS_HEADER,UPTR(254,4558),UPTR(254,4582),UOHEAD(2,0),UPTR(254,4585),UITAG(0),
	UOHEAD(2,1),UPTR(207,1),UITAG(0),UOHEAD(2,5),13,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,
	UPTR(254,4597),TC_NIL,UOHEAD(2,0),UPTR(254,4600),UITAG(0),CONS_HEADER,UPTR(254,4603),TC_NIL,UOHEAD(2,0),UPTR(254,4606),
	UITAG(0),CONS_HEADER,UPTR(254,4609),TC_NIL,CONS_HEADER,UPTR(254,4594),UPTR(254,4612),CONS_HEADER,UPTR(254,4591),UPTR(254,4615),
	UOHEAD(2,0),UPTR(254,4618),UITAG(0),UOHEAD(2,1),UPTR(209,1),UITAG(0),UOHEAD(2,5),15,UITAG(0),UOHEAD(2,1),
	UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4630),TC_NIL,UOHEAD(2,0),UPTR(254,4633),UITAG(0),UOHEAD(2,1),UPTR(6,1),
	UITAG(0),CONS_HEADER,UPTR(254,4639),TC_NIL,UOHEAD(2,0),UPTR(254,4642),UITAG(0),CONS_HEADER,UPTR(254,4645),TC_NIL,
	CONS_HEADER,UPTR(254,4636),UPTR(254,4648),UOHEAD(2,0),UPTR(254,4651),UITAG(0),CONS_HEADER,UPTR(254,4654),TC_NIL,CONS_HEADER,
	UPTR(254,4627),UPTR(254,4657),CONS_HEADER,UPTR(254,4624),UPTR(254,4660),UOHEAD(2,0),UPTR(254,4663),UITAG(0),UOHEAD(2,1),UPTR(211,1),
	UITAG(0),UOHEAD(2,5),17,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4675),TC_NIL,
	UOHEAD(2,0),UPTR(254,4678),UITAG(0),CONS_HEADER,UPTR(254,4681),TC_NIL,UOHEAD(2,0),UPTR(254,4684),UITAG(0),CONS_HEADER,
	UPTR(254,4687),TC_NIL,CONS_HEADER,UPTR(254,4672),UPTR(254,4690),CONS_HEADER,UPTR(254,4669),UPTR(254,4693),UOHEAD(2,0),UPTR(254,4696),
	UITAG(0),UOHEAD(2,1),UPTR(213,1),UITAG(0),UOHEAD(2,5),19,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,4708),TC_NIL,UOHEAD(2,0),UPTR(254,4711),UITAG(0),CONS_HEADER,UPTR(254,4714),TC_NIL,UOHEAD(2,0),
	UPTR(254,4717),UITAG(0),CONS_HEADER,UPTR(254,4720),TC_NIL,CONS_HEADER,UPTR(254,4705),UPTR(254,4723),CONS_HEADER,UPTR(254,4702),
	UPTR(254,4726),UOHEAD(2,0),UPTR(254,4729),UITAG(0),UOHEAD(2,1),UPTR(215,1),UITAG(0),UOHEAD(2,5),21,UITAG(0),
	UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4741),TC_NIL,UOHEAD(2,0),UPTR(254,4744),UITAG(0),UOHEAD(2,1),
	UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4750),TC_NIL,UOHEAD(2,0),UPTR(254,4753),UITAG(0),CONS_HEADER,UPTR(254,4756),
	TC_NIL,CONS_HEADER,UPTR(254,4747),UPTR(254,4759),UOHEAD(2,0),UPTR(254,4762),UITAG(0),CONS_HEADER,UPTR(254,4765),TC_NIL,
	CONS_HEADER,UPTR(254,4738),UPTR(254,4768),CONS_HEADER,UPTR(254,4735),UPTR(254,4771),UOHEAD(2,0),UPTR(254,4774),UITAG(0),UOHEAD(2,1),
	UPTR(217,1),UITAG(0),UOHEAD(2,5),23,UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4786),
	TC_NIL,UOHEAD(2,0),UPTR(254,4789),UITAG(0),CONS_HEADER,UPTR(254,4792),TC_NIL,UOHEAD(2,0),UPTR(254,4795),UITAG(0),
	CONS_HEADER,UPTR(254,4798),TC_NIL,CONS_HEADER,UPTR(254,4783),UPTR(254,4801),CONS_HEADER,UPTR(254,4780),UPTR(254,4804),UOHEAD(2,0),
	UPTR(254,4807),UITAG(0),UOHEAD(2,1),UPTR(219,1),UITAG(0),UOHEAD(2,5),25,UITAG(0),UOHEAD(2,1),UPTR(6,1),
	UITAG(0),CONS_HEADER,UPTR(254,4819),TC_NIL,UOHEAD(2,0),UPTR(254,4822),UITAG(0),CONS_HEADER,UPTR(254,4825),TC_NIL,
	UOHEAD(2,0),UPTR(254,4828),UITAG(0),CONS_HEADER,UPTR(254,4831),TC_NIL,CONS_HEADER,UPTR(254,4816),UPTR(254,4834),CONS_HEADER,
	UPTR(254,4813),UPTR(254,4837),UOHEAD(2,0),UPTR(254,4840),UITAG(0),UOHEAD(2,1),UPTR(221,1),UITAG(0),UOHEAD(2,5),27,
	UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4852),TC_NIL,UOHEAD(2,0),UPTR(254,4855),UITAG(0),
	UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4861),TC_NIL,UOHEAD(2,0),UPTR(254,4864),UITAG(0),CONS_HEADER,
	UPTR(254,4867),TC_NIL,CONS_HEADER,UPTR(254,4858),UPTR(254,4870),UOHEAD(2,0),UPTR(254,4873),UITAG(0),CONS_HEADER,UPTR(254,4876),
	TC_NIL,CONS_HEADER,UPTR(254,4849),UPTR(254,4879),CONS_HEADER,UPTR(254,4846),UPTR(254,4882),UOHEAD(2,0),UPTR(254,4885),UITAG(0),
	UOHEAD(2,1),UPTR(223,1),UITAG(0),UOHEAD(2,5),29,UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),
	UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4900),TC_NIL,UOHEAD(2,0),UPTR(254,4903),UITAG(0),CONS_HEADER,UPTR(254,4906),
	TC_NIL,CONS_HEADER,UPTR(254,4897),UPTR(254,4909),UOHEAD(2,0),UPTR(254,4912),UITAG(0),CONS_HEADER,UPTR(254,4915),TC_NIL,
	UOHEAD(2,0),UPTR(254,4918),UITAG(0),CONS_HEADER,UPTR(254,4921),TC_NIL,CONS_HEADER,UPTR(254,4894),UPTR(254,4924),CONS_HEADER,
	UPTR(254,4891),UPTR(254,4927),UOHEAD(2,0),UPTR(254,4930),UITAG(0),UOHEAD(2,1),UPTR(225,1),UITAG(0),UOHEAD(2,5),31,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,4942),TC_NIL,CONS_HEADER,UPTR(254,4939),UPTR(254,4945),
	CONS_HEADER,UPTR(254,4936),UPTR(254,4948),UOHEAD(2,0),UPTR(254,4951),UITAG(0),UOHEAD(2,1),UPTR(227,1),UITAG(0),UOHEAD(2,5),
	33,UITAG(0),UOHEAD(2,1),UPTR(35,1),UITAG(0),CONS_HEADER,UPTR(254,4963),TC_NIL,UOHEAD(2,0),UPTR(254,4966),
	UITAG(0),UOHEAD(2,1),UPTR(83,1),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,UPTR(254,4975),TC_NIL,
	UOHEAD(2,0),UPTR(254,4978),UITAG(0),CONS_HEADER,UPTR(254,4981),TC_NIL,CONS_HEADER,UPTR(254,4972),UPTR(254,4984),UOHEAD(2,0),
	UPTR(254,4987),UITAG(0),CONS_HEADER,UPTR(254,4990),TC_NIL,CONS_HEADER,UPTR(254,4969),UPTR(254,4993),UOHEAD(2,0),UPTR(254,4996),
	UITAG(0),CONS_HEADER,UPTR(254,4999),TC_NIL,CONS_HEADER,UPTR(254,4960),UPTR(254,5002),CONS_HEADER,UPTR(254,4957),UPTR(254,5005),
	UOHEAD(2,0),UPTR(254,5008),UITAG(0),CONS_HEADER,UPTR(254,5011),TC_NIL,CONS_HEADER,UPTR(254,4954),UPTR(254,5014),CONS_HEADER,
	UPTR(254,4933),UPTR(254,5017),CONS_HEADER,UPTR(254,4888),UPTR(254,5020),CONS_HEADER,UPTR(254,4843),UPTR(254,5023),CONS_HEADER,UPTR(254,4810),
	UPTR(254,5026),CONS_HEADER,UPTR(254,4777),UPTR(254,5029),CONS_HEADER,UPTR(254,4732),UPTR(254,5032),CONS_HEADER,UPTR(254,4699),UPTR(254,5035),
	CONS_HEADER,UPTR(254,4666),UPTR(254,5038),CONS_HEADER,UPTR(254,4621),UPTR(254,5041),CONS_HEADER,UPTR(254,4588),UPTR(254,5044),CONS_HEADER,
	UPTR(254,4555),UPTR(254,5047),CONS_HEADER,UPTR(254,4522),UPTR(254,5050),CONS_HEADER,UPTR(254,4489),UPTR(254,5053),CONS_HEADER,UPTR(254,4456),
	UPTR(254,5056),CONS_HEADER,UPTR(254,4423),UPTR(254,5059),UOHEAD(2,0),UPTR(254,5062),UITAG(0),CONS_HEADER,UPTR(254,5065),TC_NIL,
	CONS_HEADER,UPTR(254,4390),UPTR(254,5068),UOHEAD(2,0),UPTR(254,5071),UITAG(0),UOHEAD(2,1),UPTR(229,1),UITAG(0),UOHEAD(2,1),
	UPTR(231,1),UITAG(0),UOHEAD(2,5),1,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,5086),
	TC_NIL,CONS_HEADER,UPTR(254,5083),UPTR(254,5089),CONS_HEADER,UPTR(254,5080),UPTR(254,5092),UOHEAD(2,0),UPTR(254,5095),UITAG(0),
	UOHEAD(2,1),UPTR(233,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,
	UPTR(254,5107),TC_NIL,CONS_HEADER,UPTR(254,5104),UPTR(254,5110),CONS_HEADER,UPTR(254,5101),UPTR(254,5113),UOHEAD(2,0),UPTR(254,5116),
	UITAG(0),UOHEAD(2,1),UPTR(235,1),UITAG(0),UOHEAD(2,5),5,UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),
	CONS_HEADER,UPTR(254,5128),TC_NIL,CONS_HEADER,UPTR(254,5125),UPTR(254,5131),CONS_HEADER,UPTR(254,5122),UPTR(254,5134),UOHEAD(2,0),
	UPTR(254,5137),UITAG(0),UOHEAD(2,1),UPTR(237,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),UOHEAD(2,0),TC_NIL,
	UITAG(0),CONS_HEADER,UPTR(254,5149),TC_NIL,CONS_HEADER,UPTR(254,5146),UPTR(254,5152),CONS_HEADER,UPTR(254,5143),UPTR(254,5155),
	UOHEAD(2,0),UPTR(254,5158),UITAG(0),UOHEAD(2,1),UPTR(35,1),UITAG(0),UOHEAD(2,5),9,UITAG(0),UOHEAD(2,0),
	TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,5170),TC_NIL,CONS_HEADER,UPTR(254,5167),UPTR(254,5173),CONS_HEADER,UPTR(254,5164),
	UPTR(254,5176),UOHEAD(2,0),UPTR(254,5179),UITAG(0),UOHEAD(2,1),UPTR(239,1),UITAG(0),UOHEAD(2,5),11,UITAG(0),
	UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,5191),TC_NIL,CONS_HEADER,UPTR(254,5188),UPTR(254,5194),CONS_HEADER,
	UPTR(254,5185),UPTR(254,5197),UOHEAD(2,0),UPTR(254,5200),UITAG(0),UOHEAD(2,1),UPTR(241,1),UITAG(0),UOHEAD(2,5),13,
	UITAG(0),UOHEAD(2,0),TC_NIL,UITAG(0),CONS_HEADER,UPTR(254,5212),TC_NIL,CONS_HEADER,UPTR(254,5209),UPTR(254,5215),
	CONS_HEADER,UPTR(254,5206),UPTR(254,5218),UOHEAD(2,0),UPTR(254,5221),UITAG(0),CONS_HEADER,UPTR(254,5224),TC_NIL,CONS_HEADER,
	UPTR(254,5203),UPTR(254,5227),CONS_HEADER,UPTR(254,5182),UPTR(254,5230),CONS_HEADER,UPTR(254,5161),UPTR(254,5233),CONS_HEADER,UPTR(254,5140),
	UPTR(254,5236),CONS_HEADER,UPTR(254,5119),UPTR(254,5239),CONS_HEADER,UPTR(254,5098),UPTR(254,5242),UOHEAD(2,0),UPTR(254,5245),UITAG(0),
	CONS_HEADER,UPTR(254,5248),TC_NIL,CONS_HEADER,UPTR(254,5077),UPTR(254,5251),UOHEAD(2,0),UPTR(254,5254),UITAG(0),CONS_HEADER,
	UPTR(254,5257),TC_NIL,CONS_HEADER,UPTR(254,5074),UPTR(254,5260),CONS_HEADER,UPTR(254,4387),UPTR(254,5263),CONS_HEADER,UPTR(254,4132),
	UPTR(254,5266),CONS_HEADER,UPTR(254,3877),UPTR(254,5269),CONS_HEADER,UPTR(254,3634),UPTR(254,5272),CONS_HEADER,UPTR(254,3283),UPTR(254,5275),
	CONS_HEADER,UPTR(254,3136),UPTR(254,5278),CONS_HEADER,UPTR(254,2977),UPTR(254,5281),CONS_HEADER,UPTR(254,2902),UPTR(254,5284),CONS_HEADER,
	UPTR(254,2647),UPTR(254,5287),CONS_HEADER,UPTR(254,2476),UPTR(254,5290),CONS_HEADER,UPTR(254,2317),UPTR(254,5293),CONS_HEADER,UPTR(254,2170),
	UPTR(254,5296),CONS_HEADER,UPTR(254,1927),UPTR(254,5299),CONS_HEADER,UPTR(254,1828),UPTR(254,5302),CONS_HEADER,UPTR(254,1537),UPTR(254,5305),
	CONS_HEADER,UPTR(254,1174),UPTR(254,5308),CONS_HEADER,UPTR(254,1099),UPTR(254,5311),CONS_HEADER,UPTR(254,508),UPTR(254,5314),CONS_HEADER,
	UPTR(254,397),UPTR(254,5317),CONS_HEADER,UPTR(254,298),UPTR(254,5320),CONS_HEADER,UPTR(254,235),UPTR(254,5323),CONS_HEADER,UPTR(254,154),
	UPTR(254,5326),CONS_HEADER,UPTR(254,67),UPTR(254,5329),CONS_HEADER,UPTR(254,4),UPTR(254,5332),UOHEAD(2,0),UPTR(254,5335),UITAG(0),
	UOHEAD(2,1),UPTR(243,1),UITAG(0),UOHEAD(2,1),UPTR(245,1),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),
	UPTR(28,1),UITAG(0),CONS_HEADER,UPTR(254,5350),TC_NIL,UOHEAD(2,0),UPTR(254,5353),UITAG(0),CONS_HEADER,UPTR(254,5356),
	TC_NIL,CONS_HEADER,UPTR(254,5347),UPTR(254,5359),UOHEAD(2,0),UPTR(254,5362),UITAG(0),CONS_HEADER,UPTR(254,5365),TC_NIL,
	CONS_HEADER,UPTR(254,5344),UPTR(254,5368),UOHEAD(2,0),UPTR(254,5371),UITAG(0),UOHEAD(2,1),UPTR(247,1),UITAG(0),UOHEAD(2,1),
	UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(87,1),UITAG(0),CONS_HEADER,UPTR(254,5383),TC_NIL,UOHEAD(2,0),UPTR(254,5386),
	UITAG(0),CONS_HEADER,UPTR(254,5389),TC_NIL,CONS_HEADER,UPTR(254,5380),UPTR(254,5392),UOHEAD(2,0),UPTR(254,5395),UITAG(0),
	CONS_HEADER,UPTR(254,5398),TC_NIL,CONS_HEADER,UPTR(254,5377),UPTR(254,5401),UOHEAD(2,0),UPTR(254,5404),UITAG(0),UOHEAD(2,1),
	UPTR(249,1),UITAG(0),UOHEAD(2,1),UPTR(107,1),UITAG(0),UOHEAD(2,1),UPTR(251,1),UITAG(0),UOHEAD(2,1),UPTR(22,1),
	UITAG(0),CONS_HEADER,UPTR(254,5419),TC_NIL,UOHEAD(2,0),UPTR(254,5422),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),
	CONS_HEADER,UPTR(254,5428),TC_NIL,UOHEAD(2,0),UPTR(254,5431),UITAG(0),UOHEAD(2,1),UPTR(6,1),UITAG(0),CONS_HEADER,
	UPTR(254,5437),TC_NIL,UOHEAD(2,0),UPTR(254,5440),UITAG(0),CONS_HEADER,UPTR(254,5443),TC_NIL,CONS_HEADER,UPTR(254,5434),
	UPTR(254,5446),CONS_HEADER,UPTR(254,5425),UPTR(254,5449),CONS_HEADER,UPTR(254,5416),UPTR(254,5452),UOHEAD(2,0),UPTR(254,5455),UITAG(0),
	CONS_HEADER,UPTR(254,5458),TC_NIL,CONS_HEADER,UPTR(254,5413),UPTR(254,5461),UOHEAD(2,0),UPTR(254,5464),UITAG(0),CONS_HEADER,
	UPTR(254,5467),TC_NIL,CONS_HEADER,UPTR(254,5410),UPTR(254,5470),UOHEAD(2,0),UPTR(254,5473),UITAG(0),CONS_HEADER,UPTR(254,5476),
	TC_NIL,CONS_HEADER,UPTR(254,5407),UPTR(254,5479),CONS_HEADER,UPTR(254,5374),UPTR(254,5482),CONS_HEADER,UPTR(254,5341),UPTR(254,5485),
	UOHEAD(2,0),UPTR(254,5488),UITAG(0),UOHEAD(2,1),UPTR(10,1),UITAG(0),UOHEAD(2,1),UPTR(245,1),UITAG(0),UOHEAD(2,5),
	1,UITAG(0),CONS_HEADER,UPTR(254,5500),TC_NIL,CONS_HEADER,UPTR(254,5497),UPTR(254,5503),UOHEAD(2,0),UPTR(254,5506),
	UITAG(0),UOHEAD(2,1),UPTR(247,1),UITAG(0),UOHEAD(2,5),3,UITAG(0),CONS_HEADER,UPTR(254,5515),TC_NIL,
	CONS_HEADER,UPTR(254,5512),UPTR(254,5518),UOHEAD(2,0),UPTR(254,5521),UITAG(0),UOHEAD(2,1),UPTR(253,1),UITAG(0),UOHEAD(2,5),
	5,UITAG(0),CONS_HEADER,UPTR(254,5530),TC_NIL,CONS_HEADER,UPTR(254,5527),UPTR(254,5533),UOHEAD(2,0),UPTR(254,5536),
	UITAG(0),UOHEAD(2,1),UPTR(249,1),UITAG(0),UOHEAD(2,5),7,UITAG(0),CONS_HEADER,UPTR(254,5545),TC_NIL,
	CONS_HEADER,UPTR(254,5542),UPTR(254,5548),UOHEAD(2,0),UPTR(254,5551),UITAG(0),CONS_HEADER,UPTR(254,5554),TC_NIL,CONS_HEADER,
	UPTR(254,5539),UPTR(254,5557),CONS_HEADER,UPTR(254,5524),UPTR(254,5560),CONS_HEADER,UPTR(254,5509),UPTR(254,5563),CONS_HEADER,UPTR(254,5494),
	UPTR(254,5566),UOHEAD(2,0),UPTR(254,5569),UITAG(0),CONS_HEADER,UPTR(254,5572),TC_NIL,CONS_HEADER,UPTR(254,5491),UPTR(254,5575),
	CONS_HEADER,UPTR(254,5338),UPTR(254,5578),CONS_HEADER,UPTR(254,1),UPTR(254,5581),UOHEAD(2,0),UPTR(254,5584),UITAG(0)};
irk_int irk_internal_symbols[] = {(122<<TAGSIZE)|TC_VECTOR,
	UPTR(223,1),UPTR(221,1),UPTR(217,1),UPTR(207,1),UPTR(205,1),UPTR(203,1),UPTR(199,1),UPTR(241,1),UPTR(239,1),UPTR(237,1),
	UPTR(235,1),UPTR(233,1),UPTR(231,1),UPTR(229,1),UPTR(227,1),UPTR(209,1),UPTR(211,1),UPTR(213,1),UPTR(201,1),UPTR(215,1),
	UPTR(197,1),UPTR(195,1),UPTR(225,1),UPTR(193,1),UPTR(247,1),UPTR(245,1),UPTR(249,1),UPTR(191,1),UPTR(187,1),UPTR(185,1),
	UPTR(183,1),UPTR(181,1),UPTR(179,1),UPTR(177,1),UPTR(175,1),UPTR(173,1),UPTR(169,1),UPTR(167,1),UPTR(165,1),UPTR(159,1),
	UPTR(155,1),UPTR(153,1),UPTR(151,1),UPTR(143,1),UPTR(141,1),UPTR(139,1),UPTR(137,1),UPTR(135,1),UPTR(133,1),UPTR(131,1),
	UPTR(127,1),UPTR(125,1),UPTR(123,1),UPTR(121,1),UPTR(119,1),UPTR(117,1),UPTR(111,1),UPTR(109,1),UPTR(105,1),UPTR(99,1),
	UPTR(95,1),UPTR(79,1),UPTR(77,1),UPTR(75,1),UPTR(69,1),UPTR(63,1),UPTR(61,1),UPTR(59,1),UPTR(57,1),UPTR(55,1),
	UPTR(43,1),UPTR(32,1),UPTR(26,1),UPTR(18,1),UPTR(16,1),UPTR(14,1),UPTR(8,1),UPTR(41,1),UPTR(97,1),UPTR(91,1),
	UPTR(163,1),UPTR(65,1),UPTR(101,1),UPTR(30,1),UPTR(157,1),UPTR(103,1),UPTR(115,1),UPTR(113,1),UPTR(251,1),UPTR(107,1),
	UPTR(10,1),UPTR(243,1),UPTR(49,1),UPTR(12,1),UPTR(219,1),UPTR(67,1),UPTR(53,1),UPTR(253,1),UPTR(81,1),UPTR(35,1),
	UPTR(145,1),UPTR(6,1),UPTR(147,1),UPTR(149,1),UPTR(129,1),UPTR(20,1),UPTR(89,1),UPTR(87,1),UPTR(171,1),UPTR(85,1),
	UPTR(28,1),UPTR(51,1),UPTR(83,1),UPTR(39,1),UPTR(37,1),UPTR(71,1),UPTR(73,1),UPTR(161,1),UPTR(189,1),UPTR(22,1),
	UPTR(93,1),UPTR(24,1)};
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
// name: csi
//  (:other char (list int))
//  (:dsr )
//  (:sgr (list int))
//  (:hvp int int)
//  (:sd int)
//  (:su int)
//  (:ds int int)
//  (:el int)
//  (:ed int)
//  (:cup int int)
//  (:cha int)
//  (:cpl int)
//  (:cnl int)
//  (:cub int)
//  (:cuf int)
//  (:cud int)
//  (:cuu int)
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
static void FUN_base_exception_handler_287_0(void);
static void FUN_length_splat_304_0(void);
static void FUN_fold_331_0(void);
static void FUN_print_exception_exit_624_0(void);
static void JUMP_28(O);
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_0(void);
static void JUMP_39(O, O, O);
static void JUMP_40(O, O, O);
static void JUMP_41(O, O, O);
static void JUMP_42(O, O, O);
static void JUMP_44(O, O, O);
static void FUN_print_exception_exit_624_1(O);
static void FAIL_38(O, O);
static void FAIL_38(O, O);
static void FAIL_38(O, O);
static void FAIL_38(O, O);
static void FUN_print_exception_exit_624_2(O);
static void FUN_loop_3093_i66_i67_i72_0(void);
static void FUN_print_exception_exit_624_3(O);
static void FAIL_29();
static void FAIL_29();
static void FAIL_29();
static void FAIL_29();
static void FAIL_29();
static void FAIL_29();
static void FAIL_29();
static void FUN_loop_3093_i66_i67_i72_4(O);
static void FAIL_45();
static void FAIL_45();
static void FAIL_45();
static void FAIL_45();
static void JUMP_26(O, O, O);
static void JUMP_27(O, O, O);
static void JUMP_25(O, O, O);
static void JUMP_16(O, O, O);
static void JUMP_23(O, O, O);
static void JUMP_17(O, O, O);
static void JUMP_19(O, O, O);
static void JUMP_21(O, O, O);
static void JUMP_14(O, O);
static void JUMP_2(O, O);
static void JUMP_3(O, O);
static void JUMP_4(O, O);
static void FAIL_1(O);
static void FUN_lambda_3969_i1_0(void);
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_5(O);
static void FUN_loop_1364_i1_0(void);
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_6(O);
static void JUMP_12(O, O);
static void JUMP_13(O, O);
static void JUMP_10();
static void FUN_loop_1364_i1_7(O);
static void JUMP_8(O, O);
static void JUMP_9(O, O);
static void JUMP_6();
static void FUN_loop_1364_i1_8(O);
static void FUN_fold_331_9(O);
void toplevel (void) {
  clear_tospace = 0;
  // env 0:
  //   (|...t25| -> t26) ((list t47) int -> int) ((t55 t52 -> t52) t52 (list t55) -> t52) (|...t68| -> t67) int int (vector symbol) (vector symbol) (vector symbol) (|...t30| -> t31)
  O r5 = alloc_cleared (TC_ENV, 11);
  top = r5;
  r5[1] = lenv; lenv = r5;
  O r6 = allocate (TC_CLOSURE, 2);
  r6[1] = FUN_base_exception_handler_287_0; r6[2] = lenv;
  r5[2] = r6;
  O r7 = allocate (TC_CLOSURE, 2);
  r7[1] = FUN_length_splat_304_0; r7[2] = lenv;
  r5[3] = r7;
  O r8 = allocate (TC_CLOSURE, 2);
  r8[1] = FUN_fold_331_0; r8[2] = lenv;
  r5[4] = r8;
  O r9 = top[2];
  r5[5] = r9;
  O r10 = (object *) 33;
  r5[6] = r10;
  O r11 = (object *) 131071;
  r5[7] = r11;
  O r12 = (object *) 18;
  r5[8] = r12;
  O r13 = irk_copy_tuple ((O) constructed_44[0]);
  r5[9] = r13;
  O r14 = irk_copy_tuple ((O) constructed_33[0]);
  r5[10] = r14;
  O r15 = allocate (TC_CLOSURE, 2);
  r15[1] = FUN_print_exception_exit_624_0; r15[2] = lenv;
  r5[11] = r15;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  // dead  = (object *) 14;
  O r4 = top[11];
  top[5] = r4;
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
  O r3 = (object *) 14;
  IRK_RETURN(3);
}
static void FUN_print_exception_exit_624_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // env 1:
  //   |...t30| symbol
  O r191 = alloc_cleared (TC_ENV, 3);
  r191[1] = lenv; lenv = r191;
  O r192 = ((object**) lenv) [1][2];
  r191[2] = r192;
  O r542 = top[8];
  O r547 = r542; // reg varref
  O r543 = TAG_INTEGER((irk_int)(r547 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*r547));
  O r545 = (object *) 1;
  O r546 = r543; // reg varref
  O r544 = IRK_TEST(UNTAG_INTEGER(r545)==UNTAG_INTEGER(r546));
  O r337 = r544; // reg varref
  if IRK_IS_TRUE(r337) {
    // env 2:
    //   (list sexp)
    O r437 = alloc_cleared (TC_ENV, 2);
    r437[1] = lenv; lenv = r437;
    // env 3:
    //   (symbol (list sexp) -> (list sexp))
    O r479 = alloc_cleared (TC_ENV, 2);
    r479[1] = lenv; lenv = r479;
    O r480 = allocate (TC_CLOSURE, 2);
    r480[1] = FUN_search_sexp_list_3078_i37_i66_i67_i72_0; r480[2] = lenv;
    r479[2] = r480;
    // env 4:
    //   sexp
    O r477 = allocate (TC_ENV, 2);
    r477[1] = lenv; lenv = r477;
    O r478 = irk_get_metadata();
    r477[2] = r478;
    // fatbar jn=39 label=38
    O r442 = ((object*) lenv) [2];
    switch (get_case (r442)) {
      case (UOTAG(0)): { // list
        // env 5:
        //   (list sexp)
        O r469 = alloc_cleared (TC_ENV, 2);
        r469[1] = lenv; lenv = r469;
        O r471 = ((object**) lenv) [1][2];
        O r470 = UOBJ_GET(r471,0);
        r469[2] = r470;
        O r445 = ((object*) lenv) [2];
        switch (get_case (r445)) {
          case (TC_PAIR): { // cons
            // env 6:
            //   sexp (list sexp)
            O r463 = alloc_cleared (TC_ENV, 3);
            r463[1] = lenv; lenv = r463;
            O r467 = ((object**) lenv) [1][2];
            O r464 = UOBJ_GET(r467,0);
            r463[2] = r464;
            O r466 = ((object**) lenv) [1][2];
            O r465 = UOBJ_GET(r466,1);
            r463[3] = r465;
            O r448 = ((object*) lenv) [2];
            switch (get_case (r448)) {
              case (UOTAG(1)): { // symbol
                // env 7:
                //   symbol
                O r459 = alloc_cleared (TC_ENV, 2);
                r459[1] = lenv; lenv = r459;
                O r461 = ((object**) lenv) [1][2];
                O r460 = UOBJ_GET(r461,0);
                r459[2] = r460;
                O r451 = ((object*) lenv) [2];
                O r452 = (object*) constructed_12[0];
                if IRK_IS_TRUE(IRK_TEST(r451==r452)) {
                  // env 8:
                  //   symbol (list sexp)
                  O r454 = allocate (TC_ENV, 3);
                  O r456 = (object*) constructed_10[0];
                  r454[2] = r456;
                  O r457 = ((object**) lenv) [1][3];
                  r454[3] = r457;
                  O r455 = ((object*****) lenv) [1][1][1][1][2];
                  O t = allocate (TC_SAVE, 5);
                  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_624_1; t[4] = r191; t[5] = r437; k = t;
                  r454[1] = r455[2]; lenv = r454; FUN_search_sexp_list_3078_i37_i66_i67_i72_0();
                } else {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_38(r437, r191);
                }
              } break;
              default: {
                lenv = ((object **)lenv)[1][1];
                FAIL_38(r437, r191);
              }
            }
          } break;
          default: {
            lenv = ((object *)lenv)[1];
            FAIL_38(r437, r191);
          }
        }
      } break;
      default: {
        FAIL_38(r437, r191);
      }
    }
  } else {
    O r541 = (object *) 14;
    JUMP_28(r191);
  }
}
static void FUN_print_exception_exit_624_1 (O rr) {
  O r191 = k[4]; O r437 = k[5]; lenv = k[2]; k = k[1];
  O r453 = rr;
  O r450 = r453;
  JUMP_44(r450, r437, r191);
}
static void JUMP_44(O r450, O r437, O r191) {
  lenv = lenv[1];
  O r449 = r450;
  O r447 = r449;
  JUMP_42(r447, r437, r191);
}
static void JUMP_42(O r447, O r437, O r191) {
  lenv = lenv[1];
  O r446 = r447;
  O r444 = r446;
  JUMP_41(r444, r437, r191);
}
static void JUMP_41(O r444, O r437, O r191) {
  lenv = lenv[1];
  O r443 = r444;
  O r441 = r443;
  JUMP_40(r441, r437, r191);
}
static void JUMP_40(O r441, O r437, O r191) {
  O r440 = r441;
  JUMP_39(r440, r437, r191);
}
static void JUMP_39(O r440, O r437, O r191) {
  lenv = lenv[1];
  O r439 = r440;
  lenv = lenv[1];
  O r438 = r439;
  r437[2] = r438;
  // env 9:
  //   int
  O r431 = alloc_cleared (TC_ENV, 2);
  r431[1] = lenv; lenv = r431;
  // env 10:
  //   (list sexp) int
  O r433 = allocate (TC_ENV, 3);
  O r435 = ((object**) lenv) [1][2];
  r433[2] = r435;
  O r436 = (object *) 1;
  r433[3] = r436;
  O r434 = top[3];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_624_2; t[4] = r191; t[5] = r431; k = t;
  r433[1] = r434[2]; lenv = r433; FUN_length_splat_304_0();
}
static void FUN_print_exception_exit_624_2 (O rr) {
  O r191 = k[4]; O r431 = k[5]; lenv = k[2]; k = k[1];
  O r432 = rr;
  r431[2] = r432;
  O r430 = ((object*) lenv) [2];
  if (freep + unbox(r430) >= limit) {
    heap_request = unbox(r430);
    heap1[3] = r191;
    gc_flip (1);
    r191 = heap0[3];
  }
  O r428 = ((object*) lenv) [2];
  O r429 = (object*) constructed_8[0];
  O r427;
  if (unbox(r428) == 0) { r427 = (object *) TC_EMPTY_VECTOR; } else {
    O t = alloc_no_clear (TC_VECTOR, unbox(r428));
    for (int i=0; i<unbox(r428); i++) { t[i+1] = r429; }
    r427 = t;
  }
  lenv = lenv[1];
  O r426 = r427;
  top[8] = r426;
  // env 11:
  //   ((list sexp) -> undefined)
  O r344 = alloc_cleared (TC_ENV, 2);
  r344[1] = lenv; lenv = r344;
  O r345 = allocate (TC_CLOSURE, 2);
  r345[1] = FUN_loop_3093_i66_i67_i72_0; r345[2] = lenv;
  r344[2] = r345;
  // env 12:
  //   (list sexp)
  O r341 = allocate (TC_ENV, 2);
  O r343 = ((object**) lenv) [1][2];
  r341[2] = r343;
  O r342 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_print_exception_exit_624_3; t[4] = r191; k = t;
  r341[1] = r342[2]; lenv = r341; FUN_loop_3093_i66_i67_i72_0();
}
static void FUN_print_exception_exit_624_3 (O rr) {
  O r191 = k[4]; lenv = k[2]; k = k[1];
  O r340 = rr;
  lenv = lenv[1];
  O r339 = r340;
  lenv = lenv[1];
  O r338 = r339;
  JUMP_28(r191);
}
static void FUN_loop_3093_i66_i67_i72_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=30 label=29
  O r350 = ((object*) lenv) [2];
  switch (get_case (r350)) {
    case (TC_PAIR): { // cons
      // env 13:
      //   sexp (list sexp)
      O r418 = alloc_cleared (TC_ENV, 3);
      r418[1] = lenv; lenv = r418;
      O r422 = ((object**) lenv) [1][2];
      O r419 = UOBJ_GET(r422,0);
      r418[2] = r419;
      O r421 = ((object**) lenv) [1][2];
      O r420 = UOBJ_GET(r421,1);
      r418[3] = r420;
      O r355 = ((object*) lenv) [2];
      switch (get_case (r355)) {
        case (UOTAG(0)): { // list
          // env 14:
          //   (list sexp)
          O r413 = alloc_cleared (TC_ENV, 2);
          r413[1] = lenv; lenv = r413;
          O r415 = ((object**) lenv) [1][2];
          O r414 = UOBJ_GET(r415,0);
          r413[2] = r414;
          O r360 = ((object*) lenv) [2];
          switch (get_case (r360)) {
            case (TC_PAIR): { // cons
              // env 15:
              //   sexp (list sexp)
              O r406 = alloc_cleared (TC_ENV, 3);
              r406[1] = lenv; lenv = r406;
              O r410 = ((object**) lenv) [1][2];
              O r407 = UOBJ_GET(r410,0);
              r406[2] = r407;
              O r409 = ((object**) lenv) [1][2];
              O r408 = UOBJ_GET(r409,1);
              r406[3] = r408;
              O r365 = ((object*) lenv) [2];
              switch (get_case (r365)) {
                case (UOTAG(1)): { // symbol
                  // env 16:
                  //   symbol
                  O r401 = alloc_cleared (TC_ENV, 2);
                  r401[1] = lenv; lenv = r401;
                  O r403 = ((object**) lenv) [1][2];
                  O r402 = UOBJ_GET(r403,0);
                  r401[2] = r402;
                  O r370 = ((object**) lenv) [1][3];
                  switch (get_case (r370)) {
                    case (TC_PAIR): { // cons
                      // env 17:
                      //   sexp (list sexp)
                      O r394 = alloc_cleared (TC_ENV, 3);
                      r394[1] = lenv; lenv = r394;
                      O r398 = ((object***) lenv) [1][1][3];
                      O r395 = UOBJ_GET(r398,0);
                      r394[2] = r395;
                      O r397 = ((object***) lenv) [1][1][3];
                      O r396 = UOBJ_GET(r397,1);
                      r394[3] = r396;
                      O r375 = ((object*) lenv) [2];
                      switch (get_case (r375)) {
                        case (UOTAG(5)): { // int
                          O r391 = ((object*) lenv) [2];
                          O r378 = UOBJ_GET(r391,0);
                          O r381 = ((object*) lenv) [3];
                          switch (get_case (r381)) {
                            case (TC_NIL): { // nil
                              O r386 = top[8];
                              O r387 = r378; // reg varref
                              O r388 = ((object**) lenv) [1][2];
                              range_check (GET_TUPLE_LENGTH(*(object*)r386), unbox(r387));
                              ((irk_vector*)r386)->val[unbox(r387)] = r388;
                              O r385 = ((object*****) lenv) [1][1][1][1][3];
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              lenv[2] = r385;
                              FUN_loop_3093_i66_i67_i72_0();
                            } break;
                            default: {
                              lenv = ((object *****)lenv)[1][1][1][1][1];
                              FAIL_29();
                            }
                          }
                        } break;
                        default: {
                          lenv = ((object *****)lenv)[1][1][1][1][1];
                          FAIL_29();
                        }
                      }
                    } break;
                    default: {
                      lenv = ((object ****)lenv)[1][1][1][1];
                      FAIL_29();
                    }
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_29();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_29();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_29();
        }
      }
    } break;
    default: {
      FAIL_29();
    }
  }
}
static void FAIL_29() {
  O r425 = (object *) 14;
  IRK_RETURN(425);
}
static void FAIL_38(O r437, O r191) {
  // env 18:
  //   |Metadata/Malformed=sexp ...t116|
  O r473 = alloc_cleared (TC_ENV, 2);
  O r475 = allocate (UOTAG(1), 1);
  O r476 = ((object*) lenv) [2];
  r475[1] = r476;
  r473[2] = r475;
  O r474 = top[5];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_loop_3093_i66_i67_i72_4; t[4] = r191; t[5] = r437; k = t;
  r473[1] = r474[2]; lenv = r473; ((pfun)(r474[1]))();
}
static void FUN_loop_3093_i66_i67_i72_4 (O rr) {
  O r191 = k[4]; O r437 = k[5]; lenv = k[2]; k = k[1];
  O r441 = rr;
  O r440 = r441;
  JUMP_39(r440, r437, r191);
}
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  // fatbar jn=46 label=45
  O r485 = ((object*) lenv) [3];
  switch (get_case (r485)) {
    case (TC_PAIR): { // cons
      // env 19:
      //   sexp (list sexp)
      O r529 = alloc_cleared (TC_ENV, 3);
      r529[1] = lenv; lenv = r529;
      O r533 = ((object**) lenv) [1][3];
      O r530 = UOBJ_GET(r533,0);
      r529[2] = r530;
      O r532 = ((object**) lenv) [1][3];
      O r531 = UOBJ_GET(r532,1);
      r529[3] = r531;
      O r490 = ((object*) lenv) [2];
      switch (get_case (r490)) {
        case (UOTAG(0)): { // list
          // env 20:
          //   (list sexp)
          O r524 = alloc_cleared (TC_ENV, 2);
          r524[1] = lenv; lenv = r524;
          O r526 = ((object**) lenv) [1][2];
          O r525 = UOBJ_GET(r526,0);
          r524[2] = r525;
          O r495 = ((object*) lenv) [2];
          switch (get_case (r495)) {
            case (TC_PAIR): { // cons
              // env 21:
              //   sexp (list sexp)
              O r517 = alloc_cleared (TC_ENV, 3);
              r517[1] = lenv; lenv = r517;
              O r521 = ((object**) lenv) [1][2];
              O r518 = UOBJ_GET(r521,0);
              r517[2] = r518;
              O r520 = ((object**) lenv) [1][2];
              O r519 = UOBJ_GET(r520,1);
              r517[3] = r519;
              O r500 = ((object*) lenv) [2];
              switch (get_case (r500)) {
                case (UOTAG(1)): { // symbol
                  O r514 = ((object*) lenv) [2];
                  O r503 = UOBJ_GET(r514,0);
                  O r506 = r503; // reg varref
                  O r507 = ((object****) lenv) [1][1][1][2];
                  if IRK_IS_TRUE(IRK_TEST(r506==r507)) {
                    O r509 = ((object*) lenv) [3];
                    IRK_RETURN(509);
                  } else {
                    O r512 = ((object****) lenv) [1][1][1][2];
                    O r513 = ((object***) lenv) [1][1][3];
                    lenv = ((object ***)lenv)[1][1][1];
                    lenv[2] = r512;
                    lenv[3] = r513;
                    FUN_search_sexp_list_3078_i37_i66_i67_i72_0();
                  }
                } break;
                default: {
                  lenv = ((object ***)lenv)[1][1][1];
                  FAIL_45();
                }
              }
            } break;
            default: {
              lenv = ((object **)lenv)[1][1];
              FAIL_45();
            }
          }
        } break;
        default: {
          lenv = ((object *)lenv)[1];
          FAIL_45();
        }
      }
    } break;
    default: {
      FAIL_45();
    }
  }
}
static void FAIL_45() {
  // env 22:
  //   |Metadata/KeyError=symbol ...t97|
  O r537 = alloc_cleared (TC_ENV, 2);
  O r539 = allocate (UOTAG(0), 1);
  O r540 = ((object*) lenv) [2];
  r539[1] = r540;
  r537[2] = r539;
  O r538 = top[5];
  r537[1] = r538[2]; lenv = r537; ((pfun)(r538[1]))();
}
static void JUMP_28(O r191) {
  // env 23:
  //   |...t30| int |tuple=(product int int int) | ident
  O r228 = alloc_cleared (TC_ENV, 5);
  r228[1] = lenv; lenv = r228;
  O r229 = ((object**) lenv) [1][2];
  r228[2] = r229;
  O r336 = ((object*) lenv) [2];
  O r230 = TAG_INTEGER((irk_int)unbox(irk_object2int(r336)));
  r228[3] = r230;
  O r334 = (object *) 3;
  O r335 = ((object*) lenv) [3];
  O r330 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r334)&UNTAG_INTEGER(r335));
  O r332 = (object *) 3;
  O r333 = r330; // reg varref
  O r331 = IRK_TEST(UNTAG_INTEGER(r332)==UNTAG_INTEGER(r333));
  O r296 = r331; // reg varref
  if IRK_IS_TRUE(r296) {
    O r297 = allocate (UOTAG(2), 3);
    O r301 = ((object*) lenv) [3];
    O r302 = (object *) 3;
    O r298 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r301)>>UNTAG_INTEGER(r302));
    r297[1] = r298;
    O r299 = (object *) 1;
    r297[2] = r299;
    O r300 = (object *) 1;
    r297[3] = r300;
    O r231 = r297;
    JUMP_26(r231, r228, r191);
  } else {
    O r328 = (object *) 7;
    O r329 = ((object*) lenv) [3];
    O r324 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r328)&UNTAG_INTEGER(r329));
    O r326 = r324; // reg varref
    O r327 = (object *) 1;
    O r325 = IRK_TEST(UNTAG_INTEGER(r326)>UNTAG_INTEGER(r327));
    O r304 = r325; // reg varref
    if IRK_IS_TRUE(r304) {
      O r305 = allocate (UOTAG(2), 3);
      O r311 = ((object*) lenv) [3];
      O r312 = top[6];
      O r306 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r311)>>UNTAG_INTEGER(r312));
      r305[1] = r306;
      O r307 = (object *) 1;
      r305[2] = r307;
      O r309 = top[7];
      O r310 = ((object*) lenv) [3];
      O r308 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r309)&UNTAG_INTEGER(r310));
      r305[3] = r308;
      O r303 = r305;
      JUMP_27(r303, r228, r191);
    } else {
      O r323 = ((object*) lenv) [2];
      O r314 = TAG_INTEGER((irk_int)unbox(irk_objectptr2int(r323)));
      O r315 = allocate (UOTAG(2), 3);
      O r316 = (object *) 1;
      r315[1] = r316;
      O r321 = r314; // reg varref
      O r322 = top[6];
      O r317 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r321)>>UNTAG_INTEGER(r322));
      r315[2] = r317;
      O r319 = top[7];
      O r320 = r314; // reg varref
      O r318 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r319)&UNTAG_INTEGER(r320));
      r315[3] = r318;
      O r313 = r315; // reg varref
      O r303 = r313;
      JUMP_27(r303, r228, r191);
    }
  }
}
static void JUMP_27(O r303, O r228, O r191) {
  O r231 = r303;
  JUMP_26(r231, r228, r191);
}
static void JUMP_26(O r231, O r228, O r191) {
  r228[4] = r231;
  // env 24:
  //   int int int
  O r289 = alloc_cleared (TC_ENV, 4);
  r289[1] = lenv; lenv = r289;
  O r295 = ((object**) lenv) [1][4];
  O r290 = UOBJ_GET(r295,0);
  r289[2] = r290;
  O r294 = ((object**) lenv) [1][4];
  O r291 = UOBJ_GET(r294,1);
  r289[3] = r291;
  O r293 = ((object**) lenv) [1][4];
  O r292 = UOBJ_GET(r293,2);
  r289[4] = r292;
  O r283 = ((object*) lenv) [3];
  O r284 = (object *) 1;
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r283)==UNTAG_INTEGER(r284))) {
    O r286 = ((object*) lenv) [4];
    O r287 = (object *) 1;
    O r285 = IRK_TEST(UNTAG_INTEGER(r286)==UNTAG_INTEGER(r287));
    O r234 = r285;
    JUMP_25(r234, r228, r191);
  } else {
    O r288 = (object *) 6;
    O r234 = r288;
    JUMP_25(r234, r228, r191);
  }
}
static void JUMP_25(O r234, O r228, O r191) {
  if IRK_IS_TRUE(r234) {
    O r236 = ((object*) lenv) [2];
    O r235 = alloc_no_clear (UOTAG(0),1);
    r235[1] = r236;
    O r233 = r235;
    JUMP_16(r233, r228, r191);
  } else {
    O r277 = ((object*) lenv) [3];
    O r278 = (object *) 1;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r277)>UNTAG_INTEGER(r278))) {
      O r280 = ((object*) lenv) [2];
      O r281 = (object *) 1;
      O r279 = IRK_TEST(UNTAG_INTEGER(r280)==UNTAG_INTEGER(r281));
      O r238 = r279;
      JUMP_23(r238, r228, r191);
    } else {
      O r282 = (object *) 6;
      O r238 = r282;
      JUMP_23(r238, r228, r191);
    }
  }
}
static void JUMP_23(O r238, O r228, O r191) {
  if IRK_IS_TRUE(r238) {
    O r240 = ((object*) lenv) [4];
    O r241 = (object *) 81;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r240)>=UNTAG_INTEGER(r241))) {
      O r249 = ((object*) lenv) [4];
      O r250 = (object *) 5;
      O r245 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r249)>>UNTAG_INTEGER(r250));
      O r247 = r245; // reg varref
      O r248 = (object *) 21;
      O r246 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r247)-UNTAG_INTEGER(r248));
      O r243 = r246; // reg varref
      O r244 = ((object*) lenv) [3];
      O r242 = alloc_no_clear (UOTAG(2),2);
      r242[1] = r243;
      r242[2] = r244;
      O r239 = r242;
      JUMP_19(r239, r228, r191);
    } else {
      O r254 = top[10];
      O r256 = ((object*) lenv) [4];
      O r257 = (object *) 5;
      O r255 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r256)>>UNTAG_INTEGER(r257));
      range_check (GET_TUPLE_LENGTH(*(object*)r254), unbox(r255));
      O r252 = ((irk_vector*)r254)->val[unbox(r255)];
      O r253 = ((object*) lenv) [3];
      O r251 = alloc_no_clear (UOTAG(4),2);
      r251[1] = r252;
      r251[2] = r253;
      O r239 = r251;
      JUMP_19(r239, r228, r191);
    }
  } else {
    O r259 = ((object*) lenv) [4];
    O r260 = (object *) 45;
    if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r259)>=UNTAG_INTEGER(r260))) {
      O r268 = ((object*) lenv) [4];
      O r269 = (object *) 5;
      O r264 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r268)>>UNTAG_INTEGER(r269));
      O r266 = r264; // reg varref
      O r267 = (object *) 11;
      O r265 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r266)-UNTAG_INTEGER(r267));
      O r262 = r265; // reg varref
      O r263 = ((object*) lenv) [2];
      O r261 = alloc_no_clear (UOTAG(1),2);
      r261[1] = r262;
      r261[2] = r263;
      O r258 = r261;
      JUMP_21(r258, r228, r191);
    } else {
      O r273 = top[9];
      O r275 = ((object*) lenv) [4];
      O r276 = (object *) 5;
      O r274 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r275)>>UNTAG_INTEGER(r276));
      range_check (GET_TUPLE_LENGTH(*(object*)r273), unbox(r274));
      O r271 = ((irk_vector*)r273)->val[unbox(r274)];
      O r272 = ((object*) lenv) [2];
      O r270 = alloc_no_clear (UOTAG(3),2);
      r270[1] = r271;
      r270[2] = r272;
      O r258 = r270;
      JUMP_21(r258, r228, r191);
    }
  }
}
static void JUMP_21(O r258, O r228, O r191) {
  O r237 = r258;
  JUMP_17(r237, r228, r191);
}
static void JUMP_19(O r239, O r228, O r191) {
  O r237 = r239;
  JUMP_17(r237, r228, r191);
}
static void JUMP_17(O r237, O r228, O r191) {
  O r233 = r237;
  JUMP_16(r233, r228, r191);
}
static void JUMP_16(O r233, O r228, O r191) {
  lenv = lenv[1];
  O r232 = r233;
  r228[5] = r232;
  O r195 = ((object*) lenv) [5];
  switch (get_case (r195)) {
    case (UOTAG(1)): { // userimm
      O r203 = ((object*) lenv) [5];
      O r197 = UOBJ_GET(r203,0);
      O r202 = ((object*) lenv) [5];
      O r198 = UOBJ_GET(r202,1);
      O r200 = top[8];
      O r201 = r198; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r200), unbox(r201));
      O r199 = ((irk_vector*)r200)->val[unbox(r201)];
      O r196 = r199; // reg varref
      O r194 = r196;
      JUMP_14(r194, r191);
    } break;
    case (UOTAG(2)): { // usertup
      O r211 = ((object*) lenv) [5];
      O r205 = UOBJ_GET(r211,0);
      O r210 = ((object*) lenv) [5];
      O r206 = UOBJ_GET(r210,1);
      O r208 = top[8];
      O r209 = r205; // reg varref
      range_check (GET_TUPLE_LENGTH(*(object*)r208), unbox(r209));
      O r207 = ((irk_vector*)r208)->val[unbox(r209)];
      O r204 = r207; // reg varref
      O r194 = r204;
      JUMP_14(r194, r191);
    } break;
    case (UOTAG(3)): { // sysimm
      O r217 = ((object*) lenv) [5];
      O r213 = UOBJ_GET(r217,0);
      O r216 = ((object*) lenv) [5];
      O r214 = UOBJ_GET(r216,1);
      O r215 = r213; // reg varref
      O r212 = r215; // reg varref
      O r194 = r212;
      JUMP_14(r194, r191);
    } break;
    case (UOTAG(4)): { // systup
      O r223 = ((object*) lenv) [5];
      O r219 = UOBJ_GET(r223,0);
      O r222 = ((object*) lenv) [5];
      O r220 = UOBJ_GET(r222,1);
      O r221 = r219; // reg varref
      O r218 = r221; // reg varref
      O r194 = r218;
      JUMP_14(r194, r191);
    } break;
    default: {
      O r227 = ((object*) lenv) [5];
      O r225 = UOBJ_GET(r227,0);
      O r226 = (object*) constructed_6[0];
      O r224 = r226; // reg varref
      O r194 = r224;
      JUMP_14(r194, r191);
    } break;
  }
}
static void JUMP_14(O r194, O r191) {
  lenv = lenv[1];
  O r193 = r194;
  r191[3] = r193;
  // env 25:
  //   (list string) string
  O r32 = alloc_cleared (TC_ENV, 3);
  r32[1] = lenv; lenv = r32;
  O r184 = (O) &constructed_4;
  O r190 = ((object**) lenv) [1][3];
  O r188 = UOBJ_GET(r190,0);
  O r189 = r188; // reg varref
  O r186 = r189; // reg varref
  O r187 = (object*) constructed_3[0];
  O r185 = alloc_no_clear (TC_PAIR,2);
  r185[1] = r186;
  r185[2] = r187;
  O r33 = alloc_no_clear (TC_PAIR,2);
  r33[1] = r184;
  r33[2] = r185;
  r32[2] = r33;
  // fatbar jn=2 label=1
  O r36 = ((object*) lenv) [2];
  switch (get_case (r36)) {
    case (TC_NIL): { // nil
      O r37 = (O) &constructed_1;
      O r35 = r37;
      JUMP_3(r35, r32);
    } break;
    default: {
      O r46 = ((object*) lenv) [2];
      O r39 = UOBJ_GET(r46,0);
      O r45 = ((object*) lenv) [2];
      O r40 = UOBJ_GET(r45,1);
      O r42 = r40; // reg varref
      switch (get_case (r42)) {
        case (TC_NIL): { // nil
          O r43 = r39; // reg varref
          O r41 = r43;
          JUMP_4(r41, r32);
        } break;
        default: {
          FAIL_1(r32);
        }
      }
    } break;
  }
}
static void JUMP_4(O r41, O r32) {
  O r38 = r41; // reg varref
  O r35 = r38;
  JUMP_3(r35, r32);
}
static void JUMP_3(O r35, O r32) {
  O r34 = r35;
  JUMP_2(r34, r32);
}
static void JUMP_2(O r34, O r32) {
  r32[3] = r34;
  O r29 = ((object*) lenv) [3];
  O r31 = ((object*) lenv) [3];
  O r30 = TAG_INTEGER((irk_int)((irk_string*)(r31))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r29))->data, 1, UNTAG_INTEGER(r30), stdout));
  O r28 = (object *) 14;
  lenv = lenv[1];
  O r23 = (O) &constructed_0;
  O r25 = r23; // reg varref
  O r27 = r23; // reg varref
  O r26 = TAG_INTEGER((irk_int)((irk_string*)(r27))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r25))->data, 1, UNTAG_INTEGER(r26), stdout));
  O r24 = (object *) 14;
  O r22 = ((object*) lenv) [2];
  dump_object (r22, 0);
  O r21 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r21), stdout));
  // dead  = (object *) 14;
  O r20 = (object *) 14;
  lenv = lenv[1];
  O r19 = (object *) -1;
  exit_continuation(r19);
  O r18 = (object *) TC_UNDEFINED;
  IRK_RETURN(18);
}
static void FAIL_1(O r32) {
  // env 26:
  //   int int string
  O r164 = alloc_cleared (TC_ENV, 4);
  r164[1] = lenv; lenv = r164;
  // env 27:
  //   (string int -> int) int (list string)
  O r171 = alloc_cleared (TC_ENV, 4);
  O r173 = allocate (TC_CLOSURE, 2);
  r173[1] = FUN_lambda_3969_i1_0; r173[2] = lenv;
  r171[2] = r173;
  O r174 = (object *) 1;
  r171[3] = r174;
  O r175 = ((object**) lenv) [1][2];
  r171[4] = r175;
  O r172 = top[4];
  O t = allocate (TC_SAVE, 5);
  t[1] = k; t[2] = lenv; t[3] = FUN_search_sexp_list_3078_i37_i66_i67_i72_5; t[4] = r32; t[5] = r164; k = t;
  r171[1] = r172[2]; lenv = r171; FUN_fold_331_0();
}
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_5 (O rr) {
  O r32 = k[4]; O r164 = k[5]; lenv = k[2]; k = k[1];
  O r165 = rr;
  r164[2] = r165;
  O r166 = ((object*) lenv) [2];
  r164[3] = r166;
  O r170 = ((object*) lenv) [3];
  O r169 = TAG_INTEGER((irk_int)string_tuple_length (UNTAG_INTEGER(r170)));
  if (freep + unbox(r169) >= limit) {
    heap_request = unbox(r169);
    heap1[3] = r164;
    heap1[4] = r32;
    gc_flip (2);
    r164 = heap0[3];
    r32 = heap0[4];
  }
  O r168 = ((object*) lenv) [3];
  O r167 = (t=alloc_no_clear (TC_STRING, string_tuple_length (UNTAG_INTEGER(r168))), ((irk_string*)(t))->len = UNTAG_INTEGER(r168), t);
  r164[4] = r167;
  // env 28:
  //   ((list string) int -> string)
  O r53 = alloc_cleared (TC_ENV, 2);
  r53[1] = lenv; lenv = r53;
  O r54 = allocate (TC_CLOSURE, 2);
  r54[1] = FUN_loop_1364_i1_0; r54[2] = lenv;
  r53[2] = r54;
  // env 29:
  //   (list string) int
  O r49 = allocate (TC_ENV, 3);
  O r51 = ((object***) lenv) [1][1][2];
  r49[2] = r51;
  O r52 = (object *) 1;
  r49[3] = r52;
  O r50 = ((object*) lenv) [2];
  O t = allocate (TC_SAVE, 4);
  t[1] = k; t[2] = lenv; t[3] = FUN_search_sexp_list_3078_i37_i66_i67_i72_6; t[4] = r32; k = t;
  r49[1] = r50[2]; lenv = r49; FUN_loop_1364_i1_0();
}
static void FUN_search_sexp_list_3078_i37_i66_i67_i72_6 (O rr) {
  O r32 = k[4]; lenv = k[2]; k = k[1];
  O r48 = rr;
  lenv = lenv[1];
  O r47 = r48;
  lenv = lenv[1];
  O r35 = r47;
  O r34 = r35;
  JUMP_2(r34, r32);
}
static void FUN_loop_1364_i1_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r57 = ((object*) lenv) [2];
  switch (get_case (r57)) {
    case (TC_NIL): { // nil
      O r59 = ((object***) lenv) [1][1][4];
      IRK_RETURN(59);
    } break;
    default: {
      // env 30:
      //   string (list string)
      O r159 = alloc_cleared (TC_ENV, 3);
      r159[1] = lenv; lenv = r159;
      O r163 = ((object**) lenv) [1][2];
      O r160 = UOBJ_GET(r163,0);
      r159[2] = r160;
      O r162 = ((object**) lenv) [1][2];
      O r161 = UOBJ_GET(r162,1);
      r159[3] = r161;
      // env 31:
      //   string int int string int
      O r152 = allocate (TC_ENV, 6);
      r152[1] = lenv; lenv = r152;
      O r153 = ((object**) lenv) [1][2];
      r152[2] = r153;
      O r154 = (object *) 1;
      r152[3] = r154;
      O r158 = ((object**) lenv) [1][2];
      O r155 = TAG_INTEGER((irk_int)((irk_string*)(r158))->len);
      r152[4] = r155;
      O r156 = ((object*****) lenv) [1][1][1][1][4];
      r152[5] = r156;
      O r157 = ((object***) lenv) [1][1][3];
      r152[6] = r157;
      // env 32:
      //   bool
      O r129 = alloc_cleared (TC_ENV, 2);
      r129[1] = lenv; lenv = r129;
      O r131 = (object *) 1;
      O r132 = ((object**) lenv) [1][3];
      if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r131)<=UNTAG_INTEGER(r132))) {
        O r149 = ((object**) lenv) [1][3];
        O r150 = ((object**) lenv) [1][4];
        O r145 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r149)+UNTAG_INTEGER(r150));
        O r147 = ((object**) lenv) [1][3];
        O r148 = r145; // reg varref
        O r146 = IRK_TEST(UNTAG_INTEGER(r147)<=UNTAG_INTEGER(r148));
        O r134 = r146; // reg varref
        if IRK_IS_TRUE(r134) {
          O r142 = ((object**) lenv) [1][3];
          O r143 = ((object**) lenv) [1][4];
          O r136 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r142)+UNTAG_INTEGER(r143));
          O r141 = ((object**) lenv) [1][2];
          O r137 = TAG_INTEGER((irk_int)((irk_string*)(r141))->len);
          O r139 = r136; // reg varref
          O r140 = r137; // reg varref
          O r138 = IRK_TEST(UNTAG_INTEGER(r139)<=UNTAG_INTEGER(r140));
          O r135 = r138; // reg varref
          O r133 = r135;
          JUMP_13(r133, r129);
        } else {
          O r144 = (object *) 6;
          O r133 = r144;
          JUMP_13(r133, r129);
        }
      } else {
        O r151 = (object *) 6;
        O r130 = r151;
        JUMP_12(r130, r129);
      }
    } break;
  }
}
static void JUMP_13(O r133, O r129) {
  O r130 = r133;
  JUMP_12(r130, r129);
}
static void JUMP_12(O r130, O r129) {
  r129[2] = r130;
  O r127 = ((object*) lenv) [2];
  O r128 = (object *) 6;
  O r126 = IRK_TEST(r127==r128);
  lenv = lenv[1];
  O r115 = r126;
  if IRK_IS_TRUE(r115) {
    // env 33:
    //   |String/Range=(product string int int) ...t251|
    O r117 = alloc_cleared (TC_ENV, 2);
    O r119 = allocate (UOTAG(3), 3);
    O r120 = ((object*) lenv) [2];
    r119[1] = r120;
    O r121 = ((object*) lenv) [3];
    r119[2] = r121;
    O r123 = ((object*) lenv) [3];
    O r124 = ((object*) lenv) [4];
    O r122 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r123)+UNTAG_INTEGER(r124));
    r119[3] = r122;
    r117[2] = r119;
    O r118 = top[5];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1364_i1_7; ; k = t;
    r117[1] = r118[2]; lenv = r117; ((pfun)(r118[1]))();
  } else {
    O r125 = (object *) 14;
    JUMP_10();
  }
}
static void FUN_loop_1364_i1_7 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r116 = rr;
  JUMP_10();
}
static void JUMP_10() {
  // env 34:
  //   bool
  O r92 = alloc_cleared (TC_ENV, 2);
  r92[1] = lenv; lenv = r92;
  O r94 = (object *) 1;
  O r95 = ((object**) lenv) [1][6];
  if IRK_IS_TRUE(IRK_TEST(UNTAG_INTEGER(r94)<=UNTAG_INTEGER(r95))) {
    O r112 = ((object**) lenv) [1][6];
    O r113 = ((object**) lenv) [1][4];
    O r108 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r112)+UNTAG_INTEGER(r113));
    O r110 = ((object**) lenv) [1][6];
    O r111 = r108; // reg varref
    O r109 = IRK_TEST(UNTAG_INTEGER(r110)<=UNTAG_INTEGER(r111));
    O r97 = r109; // reg varref
    if IRK_IS_TRUE(r97) {
      O r105 = ((object**) lenv) [1][6];
      O r106 = ((object**) lenv) [1][4];
      O r99 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r105)+UNTAG_INTEGER(r106));
      O r104 = ((object**) lenv) [1][5];
      O r100 = TAG_INTEGER((irk_int)((irk_string*)(r104))->len);
      O r102 = r99; // reg varref
      O r103 = r100; // reg varref
      O r101 = IRK_TEST(UNTAG_INTEGER(r102)<=UNTAG_INTEGER(r103));
      O r98 = r101; // reg varref
      O r96 = r98;
      JUMP_9(r96, r92);
    } else {
      O r107 = (object *) 6;
      O r96 = r107;
      JUMP_9(r96, r92);
    }
  } else {
    O r114 = (object *) 6;
    O r93 = r114;
    JUMP_8(r93, r92);
  }
}
static void JUMP_9(O r96, O r92) {
  O r93 = r96;
  JUMP_8(r93, r92);
}
static void JUMP_8(O r93, O r92) {
  r92[2] = r93;
  O r90 = ((object*) lenv) [2];
  O r91 = (object *) 6;
  O r89 = IRK_TEST(r90==r91);
  lenv = lenv[1];
  O r78 = r89;
  if IRK_IS_TRUE(r78) {
    // env 35:
    //   |String/Range=(product string int int) ...t264|
    O r80 = alloc_cleared (TC_ENV, 2);
    O r82 = allocate (UOTAG(3), 3);
    O r83 = ((object*) lenv) [5];
    r82[1] = r83;
    O r84 = ((object*) lenv) [6];
    r82[2] = r84;
    O r86 = ((object*) lenv) [6];
    O r87 = ((object*) lenv) [4];
    O r85 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r86)+UNTAG_INTEGER(r87));
    r82[3] = r85;
    r80[2] = r82;
    O r81 = top[5];
    O t = allocate (TC_SAVE, 3);
    t[1] = k; t[2] = lenv; t[3] = FUN_loop_1364_i1_8; ; k = t;
    r80[1] = r81[2]; lenv = r80; ((pfun)(r81[1]))();
  } else {
    O r88 = (object *) 14;
    JUMP_6();
  }
}
static void FUN_loop_1364_i1_8 (O rr) {
  ; lenv = k[2]; k = k[1];
  O r79 = rr;
  JUMP_6();
}
static void JUMP_6() {
  O r73 = ((object*) lenv) [5];
  O r74 = ((object*) lenv) [6];
  O r75 = ((object*) lenv) [2];
  O r76 = ((object*) lenv) [3];
  O r77 = ((object*) lenv) [4];
  O r72 = memcpy (((irk_string*)(r73))->data+UNTAG_INTEGER(r74), ((irk_string*)(r75))->data+UNTAG_INTEGER(r76), UNTAG_INTEGER(r77));
  lenv = lenv[1];
  O r65 = ((object*) lenv) [3];
  O r71 = ((object*) lenv) [2];
  O r67 = TAG_INTEGER((irk_int)((irk_string*)(r71))->len);
  O r69 = ((object**) lenv) [1][3];
  O r70 = r67; // reg varref
  O r68 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r69)+UNTAG_INTEGER(r70));
  O r66 = r68; // reg varref
  lenv = ((object *)lenv)[1];
  lenv[2] = r65;
  lenv[3] = r66;
  FUN_loop_1364_i1_0();
}
static void FUN_lambda_3969_i1_0 (void) {
  O r183 = ((object*) lenv) [2];
  O r178 = TAG_INTEGER((irk_int)((irk_string*)(r183))->len);
  O r181 = r178; // reg varref
  O r182 = ((object*) lenv) [3];
  O r180 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r181)+UNTAG_INTEGER(r182));
  IRK_RETURN(180);
}
static void FUN_fold_331_0 (void) {
  if (freep + 0 >= limit) {
    gc_flip (0);
  }
  O r550 = ((object*) lenv) [4];
  switch (get_case (r550)) {
    case (TC_NIL): { // nil
      O r552 = ((object*) lenv) [3];
      IRK_RETURN(552);
    } break;
    default: {
      // env 36:
      //   t55 (list t55)
      O r564 = alloc_cleared (TC_ENV, 3);
      r564[1] = lenv; lenv = r564;
      O r568 = ((object**) lenv) [1][4];
      O r565 = UOBJ_GET(r568,0);
      r564[2] = r565;
      O r567 = ((object**) lenv) [1][4];
      O r566 = UOBJ_GET(r567,1);
      r564[3] = r566;
      O r557 = ((object**) lenv) [1][2];
      // env 37:
      //   t55 t52
      O r560 = allocate (TC_ENV, 3);
      O r562 = ((object*) lenv) [2];
      r560[2] = r562;
      O r563 = ((object**) lenv) [1][3];
      r560[3] = r563;
      O r561 = ((object**) lenv) [1][2];
      O t = allocate (TC_SAVE, 4);
      t[1] = k; t[2] = lenv; t[3] = FUN_fold_331_9; t[4] = r557; k = t;
      r560[1] = r561[2]; lenv = r560; ((pfun)(r561[1]))();
    } break;
  }
}
static void FUN_fold_331_9 (O rr) {
  O r557 = k[4]; lenv = k[2]; k = k[1];
  O r558 = rr;
  O r559 = ((object*) lenv) [3];
  lenv = ((object *)lenv)[1];
  lenv[2] = r557;
  lenv[3] = r558;
  lenv[4] = r559;
  FUN_fold_331_0();
}
static void FUN_length_splat_304_0 (void) {
  O r571 = ((object*) lenv) [2];
  switch (get_case (r571)) {
    case (TC_NIL): { // nil
      O r573 = ((object*) lenv) [3];
      IRK_RETURN(573);
    } break;
    default: {
      O r583 = ((object*) lenv) [2];
      O r576 = UOBJ_GET(r583,1);
      O r579 = r576; // reg varref
      O r581 = (object *) 3;
      O r582 = ((object*) lenv) [3];
      O r580 = TAG_INTEGER((irk_int)UNTAG_INTEGER(r581)+UNTAG_INTEGER(r582));
      lenv[2] = r579;
      lenv[3] = r580;
      FUN_length_splat_304_0();
    } break;
  }
}
static void FUN_base_exception_handler_287_0 (void) {
  O r605 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r605), stdout));
  // dead  = (object *) 14;
  O r600 = (O) &constructed_47;
  O r602 = r600; // reg varref
  O r604 = r600; // reg varref
  O r603 = TAG_INTEGER((irk_int)((irk_string*)(r604))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r602))->data, 1, UNTAG_INTEGER(r603), stdout));
  O r601 = (object *) 14;
  O r595 = (O) &constructed_46;
  O r597 = r595; // reg varref
  O r599 = r595; // reg varref
  O r598 = TAG_INTEGER((irk_int)((irk_string*)(r599))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r597))->data, 1, UNTAG_INTEGER(r598), stdout));
  O r596 = (object *) 14;
  O r590 = (O) &constructed_45;
  O r592 = r590; // reg varref
  O r594 = r590; // reg varref
  O r593 = TAG_INTEGER((irk_int)((irk_string*)(r594))->len);
  TAG_INTEGER((irk_int)fwrite (((irk_string*)(r592))->data, 1, UNTAG_INTEGER(r593), stdout));
  O r591 = (object *) 14;
  O r589 = ((object*) lenv) [2];
  dump_object (r589, 0);
  O r588 = (object *) 655362;
  TAG_INTEGER((irk_int)fputc (GET_CHAR(r588), stdout));
  // dead  = (object *) 14;
  O r587 = (object *) -1;
  exit_continuation(r587);
  O r586 = (object *) TC_UNDEFINED;
  IRK_RETURN(586);
}
object * irk_get_metadata (void) {
  return (object *) constructed_254[0];
}

uint32_t irk_ambig_size = 17;
int32_t G[] = {3, 0, 0, 0, 1, 0, 6, 0, -5, -14, -8, -16, 1, 0, -13, -17, -4, 0};
int32_t V[] = {1014, 1011, 1012, 1018, 1020, 1024, 1021, 1013, 1010, 1017, 1015, 1019, 1026, 1022, 1025, 1023, 1016, 0};
