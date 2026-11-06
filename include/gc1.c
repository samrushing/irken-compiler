
// --------------------------------------------------
// cheney copying garbage collector
// --------------------------------------------------

#include <unistd.h> // sysconf

static
int
sitting_duck (object * p)
{
  return (p >= heap0) && (p < (heap0 + heap_size));
}

static object * scan;
static void grow_heap (int nroots);

static
object *
copy (object * p)
{
  object * pp = (object *) *p;
#if defined(IRK_GC_POISON)
  if (pp == IRK_POISON) {
    fprintf (stderr, "GC: poison (an unset slot) at %p, %d words past the scan pointer\n",
             (void *) p, (int) (p - scan));
    abort();
  }
#endif
  if (is_immediate (pp)) {
    return pp;
  } else if (sitting_duck (pp)) {
    if (*pp == (object) GC_SENTINEL) {
      // pp points into to_space, return the forwarding address
      return (object *) (*(pp+1));
    } else {
      // p points at an object in from_space, copy it
      object * addr = freep;
      irk_int length = GET_TUPLE_LENGTH (*pp);
      irk_int k;
      // copy tag, children
      for (k=0; k < length+1; k++) {
	*freep++ = *pp++;
      }
      // leave a sentinel where the tag was, followed by the forwarding address.
      *(object*)(*p) = (object) GC_SENTINEL;
      *((object*)(*p)+1) = (object) addr;
      return addr;
    }
  } else {
    // pp points outside of the heap
    //fprintf (stderr, "?");
    return pp;
  }
}

static
object
do_gc (int nroots)
{
  int i = 0;

  if (verbose_gc) {
    fprintf (stderr, "[gc...");
  }

  // place our roots
  scan = heap1;
  freep = scan + nroots;

  // copy the roots
  for (i = 0; i < nroots; i++) {
    scan[i] = (object) copy (&(scan[i]));
  }

  // bump scan
  scan += nroots;

  // scan loop

  while (scan < freep) {
    if (IMMEDIATE (*scan)) {
      scan++;
    } else {
      object * p = scan + 1;
      uint16_t tc = GET_TYPECODE (*scan);
      irk_int length = GET_TUPLE_LENGTH (*scan);
      irk_int i;

      switch (tc) {

      case TC_CLOSURE:
	// closure = { tag, pc, lenv }
	p++;			// skip pc
	*p = copy (p); p++;	// lenv
	scan += 3;
	break;

      case TC_SAVE:
	// save = { tag, next, lenv, pc, regs[...] }
	*p = copy (p); p++;		// next
	*p = copy (p); p++;		// lenv
	p++;				// pc
	for (i=3; i < length; i++) {
	  *p = copy (p);
	  p++;
	}
	scan += length + 1;
	break;

      case TC_STRING:
      case TC_BUFFER:
	// skip it all
	scan += length + 1;
	break;

      default:
        // copy everything
        for (i=0; i < length; i++) {
          *p = copy (p); p++;
        }
        scan += length + 1;
        break;
      }
    }
  }
  // swap heaps
  {
    object * temp = heap0; heap0 = heap1; heap1 = temp;
    size_t stemp = heap_size; heap_size = heap1_size; heap1_size = stemp;
  }

  if (clear_fromspace) {
    // zero the from-space
    clear_space (heap1, heap1_size);
  }

#if defined(IRK_GC_POISON)
  // (always: the point is to catch code that depends on clearing.)
  clear_space (freep, heap_size - (freep - heap0));
#else
  // code from older compilers expects the heap to be cleared; code from
  //  newer ones initializes any slot that a collection could see before
  //  it's set, and sets clear_tospace to 0.
  if (clear_tospace) {
    clear_space (freep, heap_size - (freep - heap0));
  }
#endif

  if (verbose_gc) {
    fprintf (stderr, "collected %" PRIuPTR " words]\n", freep - heap0);
  }
  grow_heap (nroots);
  return (object) box (freep - heap0);
}

// a growing heap.
//
// the cost of a collection is proportional to the live data it copies,
//  and the collections come as often as the free space after one runs
//  out.  so if a program's live data fills much of the heap, it spends
//  most of its time collecting: give it a bigger heap.  (the heaps come
//  from calloc, so a heap a program never fills costs little.)
//
// grow when the live data is more than 1/GROW_WHEN of the heap, or what
//  %ensure-heap is about to allocate won't fit; to GROW_TO times the live
//  data (or twice the old size, whichever is bigger).  it never shrinks.

// (in tenths, so they can be fractional; -D to experiment.)
#ifndef GROW_WHEN
#define GROW_WHEN 30
#endif
#ifndef GROW_TO
#define GROW_TO 40
#endif

static
void
heap_exhausted (size_t words)
{
  fprintf (stderr, "Irken: out of memory: couldn't allocate a %" PRIuPTR " MB heap.\n",
           (uintptr_t) (words * sizeof (object) / (1024 * 1024)));
  exit (1);
}

static
object *
new_space (size_t words)
{
  object * p = (object *) calloc (words, sizeof (object));
  if (!p) {
    heap_exhausted (words);
  }
  return p;
}

// growing only for speed stops at 1/8 of physical memory for each space
//  (so the heap never takes more than 1/4 of it).  growing because the
//  live data (or a big allocation) needs the room ignores this.
static
size_t
optional_growth_limit (void)
{
  static size_t limit_words = 0;
  if (limit_words == 0) {
    long pages = sysconf (_SC_PHYS_PAGES);
    long page_size = sysconf (_SC_PAGESIZE);
    if (pages > 0 && page_size > 0) {
      limit_words = (size_t) pages * (size_t) page_size / 8 / sizeof (object);
    } else {
      limit_words = (size_t) -1;
    }
  }
  return limit_words;
}

static
void
grow_heap (int nroots)
{
  size_t live = freep - heap0;
  size_t need = live + heap_request + head_room;
  size_t target;
  heap_request = 0;
  if (need <= heap_size && live * GROW_WHEN <= heap_size * 10) {
    return;
  }
  target = live * GROW_TO / 10 + (need - live);
  // (at least half again as big, so it doesn't grow a little at a time.)
  if (target < heap_size + heap_size / 2) {
    target = heap_size + heap_size / 2;
  }
  if (need <= heap_size) {
    // it should grow (for speed), if there's memory to spare.
    size_t most = optional_growth_limit();
    if (heap_size >= most) {
      return;
    } else if (target > most) {
      target = most;
    }
  }
  {
    if (verbose_gc) {
      fprintf (stderr, "[gc...heap grows to %" PRIuPTR " MB]\n",
               (uintptr_t) (target * sizeof (object) / (1024 * 1024)));
    }
    // copy everything again, into a new, bigger space.  (the roots are
    //  at the front of heap0.)
    free (heap1);
    heap1 = new_space (target);
    heap1_size = target;
    for (int i = 0; i < nroots; i++) {
      heap1[i] = heap0[i];
    }
    do_gc (nroots);
    // and the other space, ready for the next collection.
    free (heap1);
    heap1 = new_space (heap_size);
    heap1_size = heap_size;
  }
}

object
gc_flip (int nregs)
{
  uint64_t t0, t1;
  object nwords;
#if USE_CYCLECOUNTER
  t0 = rdtsc();
#endif
  // copy roots
  heap1[0] = (object) lenv;
  heap1[1] = (object) k;
  heap1[2] = (object) top;
  //assert (freep < (heap0 + heap_size));
  nwords = do_gc (nregs + 3);
  // replace roots
  lenv = (object *) heap0[0];
  k    = (object *) heap0[1];
  top  = (object *) heap0[2];
  // set new limit
  limit = heap0 + (heap_size - 4096);
#if USE_CYCLECOUNTER
  t1 = rdtsc();
  gc_ticks += (t1 - t0);
#endif
  return nwords;
}

// XXX: do we really want to store and restore lenv/k/top?

// exactly the same, except <thunk> is an extra root.
// Warning: dump_image() knows how many roots are used here.
static
object *
gc_dump (object * thunk)
{
  // copy roots
  heap1[0] = (object) lenv;
  heap1[1] = (object) k;
  heap1[2] = (object) top;
  heap1[3] = (object) thunk;
  do_gc (4);
  // replace roots
  lenv  = (object *) heap0[0];
  k     = (object *) heap0[1];
  top   = (object *) heap0[2];
  thunk = (object *) heap0[3];
  // set new limit
  limit = heap0 + (heap_size - 1024);
  return thunk;
}


static
void adjust (object * q, irk_int delta)
{
  if ((*q) && (!IMMEDIATE(*(q)))) {
    // get the pointer arith right
    object ** qq = (object **) q;
    *(qq) -= delta;
  }
}

static
void
gc_relocate (int nroots, object * start, object * finish, irk_int delta)
{
  object * scan = start;
  int i;

  // roots are either immediate values, or pointers to tuples
  // (map adjust roots)

  for (i=0; i < nroots; i++, scan++) {
    adjust (scan, delta);
  }

  while (scan < finish) {
    // There must be a tuple here
    int tc = GET_TYPECODE (*scan);
    irk_int length = GET_TUPLE_LENGTH (*scan);
    object * p = scan + 1;
    int i;

    switch (tc) {

      // XXX if we moved SAVE's <pc> to the front, then these two could be identical...
    case TC_CLOSURE:
      // { tag, pc, lenv }
      p++; // skip pc (XXX: actually, pc will have its own adjustment)
      adjust (p, delta);
      scan += length+1;
      break;
    case TC_SAVE:
      // { tag, next, lenv, pc, regs[...] }
      adjust (p, delta); p++;
      adjust (p, delta); p++;
      p++; // skip pc (XXX: ...)
      scan += length+1;
      break;
    case TC_STRING:
    case TC_BUFFER:
    case TC_FOREIGN: // XXX should probably squeal in this case.
      // skip it all
      scan += length+1;
      break;
    default:
      // adjust everything
      for (i=0; i < length; i++, p++) {
        adjust (p, delta);
      }
      scan += length+1;
      break;
    }
  }
}

// these could probably be written in irken...
irk_int
dump_image (char * filename, object * closure) {
  FILE * dump_file = fopen (filename, "wb");
  irk_int offset;
  irk_int size;
  object * start;
  // do a gc for a compact dump
  closure = gc_dump (closure);
  // for now, start at the front of the heap
  start = heap0;
  size = freep - start;
  offset = (irk_int) heap0;
  // XXX add endian indicator...
  fprintf (dump_file, "(irken image %" PRIuPTR " %p)\n", sizeof (irk_int), start);
  fwrite (&offset, sizeof(irk_int), 1, dump_file);
  fwrite (&size, sizeof(irk_int), 1, dump_file);
  fwrite (start, sizeof(irk_int), size, dump_file);
  fclose (dump_file);
  return size;
}

object *
load_image (char * filename) {
  FILE * load_file = fopen (filename, "rb");
  if (!load_file) {
    abort();
  } else {
    object * start, * thunk;
    irk_int size;
    read_header (load_file);	// XXX verify header...
    fread (&start, sizeof(irk_int), 1, load_file);
    fread (&size, sizeof(irk_int), 1, load_file);
    if ((size_t) size + head_room > heap1_size) {
      free (heap1);
      heap1_size = (size + head_room) * GROW_TO / 10;
      heap1 = new_space (heap1_size);
    }
    fread (heap1, sizeof(irk_int), size, load_file);
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
    {
      object * temp = heap0; heap0 = heap1; heap1 = temp;
      size_t stemp = heap_size; heap_size = heap1_size; heap1_size = stemp;
    }
    if (heap1_size < heap_size) {
      free (heap1);
      heap1 = new_space (heap_size);
      heap1_size = heap_size;
    }
    limit = heap0 + (heap_size - 1024);
    return thunk;
  }
}
