
// --------------------------------------------------
// cheney copying garbage collector
// --------------------------------------------------

static
int
sitting_duck (object * p)
{
  return (p >= heap0) && (p < (heap0 + heap_size));
}

static object * scan;

static
object *
copy (object * p)
{
  object * pp = (object *) *p;
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
      unsigned char tc = GET_TYPECODE (*scan);
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

      case TC_FOREIGN:
        if (length == 1) {
          scan += 2;
        } else {
          *p = copy (p); p++;
          *p = copy (p); p++;
          scan += 3;
        }
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
  { object * temp = heap0; heap0 = heap1; heap1 = temp; }

  if (clear_fromspace) {
    // zero the from-space
    clear_space (heap1, heap_size);
  }

  if (clear_tospace) {
    clear_space (freep, heap_size - (freep - heap0));
  }

  if (verbose_gc) {
    fprintf (stderr, "collected %" PRIuPTR " words]\n", freep - heap0);
  }
  return (object) box (freep - heap0);
}

static
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
static irk_int
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

static
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
    { object * temp = heap0; heap0 = heap1; heap1 = temp; }
    return thunk;
  }
}
