
#include "pxll.h"

static int lookup_field (int tag, int label);

static
inline
pxll_int
get_typecode (object * ob)
{
  if (IMMEDIATE(ob)) {
    if (IS_INTEGER(ob)) {
      return TC_INT;
    } else {
      return (pxll_int)ob & 0xff;
    }
  } else {
    return (pxll_int)*((pxll_int *)ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
pxll_int
get_case (object * ob)
{
  if (is_immediate (ob)) {
    if (is_int (ob)) {
      return TC_INT;
    } else {
      return (pxll_int) ob;
    }
  } else {
    return (pxll_int)*((pxll_int *)ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
pxll_int
get_case_noint (object * ob)
{
  if (is_immediate (ob)) {
    return (pxll_int) ob;
  } else {
    return (pxll_int) * ((pxll_int*) ob) & 0xff;
  }
}

// for pvcase/nvcase
static
inline
pxll_int
get_case_imm (object * ob)
{
  return (pxll_int)ob;
}

static
inline
pxll_int
get_case_tup (object * ob)
{
  return (pxll_int)*((pxll_int *)ob) & 0xff;
}

static
inline
pxll_int
get_imm_payload (object * ob)
{
  return ((pxll_int) ob) >> 8;
}

static
pxll_int
get_tuple_size (object * ob)
{
  header * h = (header *) ob;
  return (*h)>>8;
}

static
void
indent (int n)
{
  while (n--) {
    fprintf (stdout, "  ");
  }
}

void print_string (object * ob, int quoted);
void print_list (pxll_pair * l);

// this is kinda lame, it's part pretty-printer, part not.
static
object *
dump_object (object * ob, int depth)
{
  // indent (depth);
  if (depth > 100) {
    fprintf (stdout , "...");
    return (object *) PXLL_UNDEFINED;
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
      if ((pxll_int)ob>>8 == 257) {
	// deliberately out-of-range character
	fprintf (stdout, "#\\eof");
      } else {
	char ch = ((char)((pxll_int)ob>>8));
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
      fprintf (stdout, ((pxll_int)ob >> 8) & 0xff ? "#t" : "#f");
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
    case 0: {
      // structure
      header h = (header) (ob[0]);
      int tc = h & 0xff;
      switch (tc) {
      case TC_SAVE: {
	// XXX fix me - now holds saved registers
        pxll_save * s = (pxll_save* ) ob;
        fprintf (stdout, "<save pc=%p\n", s->pc);
        dump_object ((object *) s->lenv, depth+1); fprintf (stdout, "\n");
        dump_object ((object *) s->next, depth+1); fprintf (stdout, ">");
      }
        break;
      case TC_CLOSURE: {
        pxll_closure * c = (pxll_closure *) ob;
        //fprintf (stdout, "<closure pc=%p\n", c->pc);
        //dump_object ((object *) c->lenv, depth+1); fprintf (stdout, ">\n");
	fprintf (stdout, "<closure pc=%p lenv=%p>", c->pc, c->lenv);
      }
        break;
      case TC_TUPLE: {
        pxll_tuple * t = (pxll_tuple *) ob;
        pxll_int n = get_tuple_size (ob);
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
        pxll_vector * t = (pxll_vector *) ob;
        pxll_int n = get_tuple_size (ob);
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
      case TC_VEC16: {
        pxll_vec16 * t = (pxll_vec16 *) ob;
        pxll_int n = t->len;
        int i;
	fprintf (stdout, "#16(");
        for (i=0; i < n; i++) {
	  fprintf (stdout, "%d", t->data[i]);
	  if (i < n-1) {
	    fprintf (stdout, " ");
	  }
        }
        fprintf (stdout, ")");
      }
	break;
      case TC_PAIR:
	print_list ((pxll_pair *) ob);
        break;
      case TC_STRING:
	print_string (ob, 1);
	break;
      case TC_SYMBOL:
	print_string (ob[1], 0);
	break;
      default: {
        pxll_vector * t = (pxll_vector *) ob;
        pxll_int n = get_tuple_size (ob);
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
    case TC_USERIMM:
      // a user immediate unit-type...
      fprintf (stdout, "<u%" PRIuPTR ">", (((pxll_int)ob)>>8));
    }
  }
  return (object *) PXLL_UNDEFINED;
}

// for gdb...
void
DO (object * x)
{
  dump_object (x, 0);
  fprintf (stdout, "\n");
  fflush (stdout);
}

// for debugging
void
stack_depth_indent (object * k)
{
  while (k != PXLL_NIL) {
    k = k[1];
    fprintf (stderr, "  ");
  }
}

void
print_string (object * ob, int quoted)
{
  pxll_string * s = (pxll_string *) ob;
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
	fprintf (stdout, "\\0x%02x", *ps);
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

void
print_list (pxll_pair * l)
{
  fprintf (stdout, "(");
  while (1) {
    object * car = l->car;
    object * cdr = l->cdr;
    dump_object (car, 0);
    if (cdr == PXLL_NIL) {
      fprintf (stdout, ")");
      break;
    } else if (!is_immediate (cdr) && GET_TYPECODE (*cdr) == TC_PAIR) {
      fprintf (stdout, " ");
      l = (pxll_pair *) cdr;
    } else {
      fprintf (stdout, " . ");
      dump_object (cdr, 0);
      fprintf (stdout, ")");
      break;
    }
  }
}

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
    fprintf (stderr, "array reference out of range: %d[%d]\n", length, index);
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

pxll_int verbose_gc = 1;
pxll_int clear_fromspace = 0;
pxll_int clear_tospace = 0;

pxll_int vm (int argc, char * argv[]);

#include "rdtsc.h"

unsigned long long gc_ticks = 0;

static
void
clear_space (object * p, pxll_int n)
{
  while (n--) {
    *p++ = PXLL_NIL;
  }
}

int
main (int argc, char * argv[])
{
  heap0 = malloc (sizeof (object) * heap_size);
  heap1 = malloc (sizeof (object) * heap_size);
  if (!heap0 || !heap1) {
    fprintf (stderr, "unable to allocate heap\n");
    return -1;
  } else {
    unsigned long long t0, t1;
    pxll_int result;
    if (clear_tospace) {
      clear_space (heap0, heap_size);
    }
    t0 = rdtsc();
    result = vm (argc, argv);
    t1 = rdtsc();
    dump_object ((object *) result, 0);
    fprintf (stdout, "\n");
    fprintf (stderr, "{total ticks: %lld gc ticks: %lld}\n", t1 - t0, gc_ticks);
    return (int) result;
  }
}

// CONSTRUCTED LITERALS //

pxll_int
vm (int argc, char * argv[])
{
  register object * lenv = PXLL_NIL;
  register object * k = PXLL_NIL;
// REGISTER_DECLARATIONS //
  object * top = PXLL_NIL; // top-level (i.e. 'global') environment
  object * t = 0; // temp - for swaps & building tuples
  object * result;
  object * limit = heap0 + (heap_size - head_room);
  object * freep = heap0;
  int i; // loop counter
  
#define PXLL_RETURN(d)	result = r##d; goto *k[3]

#include "gc.c"

  // check heap is called at the top of each allocating function.
  //  [by locating the check at the top, we avoid considering any
  //   registers as roots of the gc...]
  void check_heap (int nfree) {
    if (freep >= limit) {
      uint64_t t0, t1;
      t0 = rdtsc();
      gc_flip (nfree);
      t1 = rdtsc();
      gc_ticks += t1 - t0;
    }
  }

  object * allocate (pxll_int tc, pxll_int size) {
    object * save = freep;
    *freep = (object*) (size<<8 | (tc & 0xff));
#if 1
    // at least on the g5, this technique is considerably faster than using memset
    //   in gc_flip() to 'pre-clear' the heap... probably a cache effect...
    while (size--) {
      // this keeps gc from being confused by partially-filled objects.
      *(++freep) = PXLL_NIL;
    }
    ++freep;
#else
    // if you use this version, be sure to set <clear_tospace>!
    freep += size + 1;
#endif
    return save;  
  }

  // this is emitted by the backend for %make-tuple
  object * alloc_no_clear (pxll_int tc, pxll_int size) {
    object * save = freep;
    *freep = (object*) (size<<8 | (tc & 0xff));
    freep += size + 1;
    return save;  
  }

  // gcc inlines/unrolls these nicely, they allow more compact code
  inline object * varref (pxll_int depth, pxll_int index)
  {
    object * walk = lenv;
    while (depth--) {
      walk = walk[1];
    }
    return walk[index+2];
  }

  inline void varset (pxll_int depth, pxll_int index, object * val)
  {
    object * walk = lenv;
    while (depth--) {
      walk = walk[1];
    }
    walk[index+2] = val;
  }

  
  // these could probably be written in irken...
  pxll_int dump_image (char * filename, object * closure) {
    FILE * dump_file = fopen (filename, "wb");
    pxll_int offset;
    pxll_int size;
    object * start;
    // do a gc for a compact dump
    closure = gc_dump (closure);
    // for now, start at the front of the heap
    start = heap0;
    size = freep - start;
    offset = (pxll_int) heap0;
    // XXX add endian indicator...
    fprintf (dump_file, "(pxll image %" PRIuPTR " %p)\n", sizeof (pxll_int), start);
    fwrite (&offset, sizeof(pxll_int), 1, dump_file);
    fwrite (&size, sizeof(pxll_int), 1, dump_file);
    fwrite (start, sizeof(pxll_int), size, dump_file);
    fclose (dump_file);
    return size;
  }

  object * load_image (char * filename) {
    FILE * load_file = fopen (filename, "rb");
    if (!load_file) {
      abort();
    } else {
      object * start, * thunk;
      pxll_int size;
      read_header (load_file);	// XXX verify header...
      fread (&start, sizeof(pxll_int), 1, load_file);
      fread (&size, sizeof(pxll_int), 1, load_file);
      fread (heap1, sizeof(pxll_int), size, load_file);
      fprintf (stderr, "size=%d\n", (int) size);
      // relocate heap0
      gc_relocate (4, heap1, heap1 + size, start - heap1);
      // replace roots
      lenv  = (object *) heap1[0];
      k     = (object *) heap1[1];
      top   = (object *) heap1[2];
      thunk = (object *) heap1[3];
      freep = heap1 + size;
      // swap heaps
      { object * temp = heap0; heap0 = heap1; heap1 = temp; }
      return thunk;
    }
  }

  k = allocate (TC_SAVE, 3);
  k[1] = (object *) PXLL_NIL; // top of stack
  k[2] = (object *) PXLL_NIL; // null environment
  k[3] = &&Lreturn; // continuation that will return from this function.
  // --- BEGIN USER PROGRAM ---
