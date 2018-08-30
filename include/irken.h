#ifndef IRK_H
#define IRK_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <inttypes.h>

typedef intptr_t irk_int;
typedef void * object;

const size_t heap_size = 50000000; // about 400MB on 64-bit machine
const size_t head_room = 8192;

object * heap0 = NULL;
object * heap1 = NULL;

/* Type Tags */

#define TC_INT                  (0<<1) // 00000000 00

/* immediate types (multiples of 2 (but not 4!)) */
#define TC_CHAR                 (1<<1) // 00000010 02
#define TC_BOOL                 (3<<1) // 00000110 06
#define TC_NIL                  (5<<1) // 00001010 0a
#define TC_UNDEFINED            (7<<1) // 00001110 0e
#define TC_EMPTY_VECTOR         (9<<1) // 00010010 12
#define TC_USERIMM             (11<<1) // 00010110 16

/* pointer types (multiples of 4) */
#define TC_SAVE                 (1<<2) // 00000100  04
#define TC_CLOSURE              (2<<2) // 00001000  08
#define TC_TUPLE                (3<<2) // 00001100  0c
#define TC_ENV                  (3<<2) // 00001100  0c alias
#define TC_STRING		(4<<2) // 00010000  10
#define TC_VECTOR               (5<<2) // 00010100  14
#define TC_PAIR                 (6<<2) // 00011000  18
#define TC_SYMBOL               (7<<2) // 00011100  1c
#define TC_BUFFER               (8<<2) // 00100000  20
#define TC_FOREIGN              (9<<2) // 00100100  24
#define TC_USEROBJ             (10<<2) // 00101000  28

// alias
#define TC_CONTINUATION TC_SAVE

// the range TC_USEROBJ to 252 is available for variant records,
//   leaving a max of 59 variants in any one type.

// immediate constants
#define IRK_FALSE		(object *) (0x000 | TC_BOOL)
#define IRK_TRUE		(object *) (0x100 | TC_BOOL)
#define IRK_MAYBE		(object *) (0x200 | TC_BOOL)  // just kidding
#define IRK_NIL		  	(object *) (0x000 | TC_NIL)
#define IRK_UNDEFINED		(object *) (0x000 | TC_UNDEFINED)

// XXX make these inline functions rather than macros

#define GET_TYPECODE(p)		(((irk_int)(p))&0xff)
#define GET_PAYLOAD(p)		(((irk_int)(p))>>8)
#define GET_TUPLE_LENGTH(p)	(((irk_int)(p))>>8)
#define GET_ENV_LENGTH(p)	(((irk_int)(p))>>8)
#define TAG_VALUE(tag,value)	((object)((tag&0xff)|(value<<8)))
#define GET_STRING_POINTER(s)   (((irk_string *)(s))->data)

#define IS_INTEGER(p)		(((irk_int)(p)) & 1)
#define TAG_INTEGER(p)		((object)(((p)<<1)|1))
#define UNTAG_INTEGER(p)	(((irk_int)(p))>>1)

#define IMMEDIATE(p)		(((irk_int)(p)) & 3)
#define IS_TYPE(t, p)		(((irk_int)(p)&0xff)==t)
#define IS_CHAR(p)		IS_TYPE (TC_CHAR, p)
#define IS_BOOL(p)		IS_TYPE (TC_BOOL, p)
#define IS_NIL(p)		IS_TYPE (TC_NIL, p)
#define IS_UNDEFINED(p)		IS_TYPE (TC_UNDEFINED, p)

#define GET_CHAR(p)		(((irk_int)(p)>>8))
#define TO_CHAR(ch)		((object)(irk_int)(((ch)<<8)|TC_CHAR))

#define HOW_MANY(x,n)		(((x)+(n)-1)/(n))
#define STRING_TUPLE_LENGTH(n)  HOW_MANY (n + sizeof(int32_t), sizeof(object))
#define STRING_HEADER(n)        STRING_TUPLE_LENGTH(n)<<8|TC_STRING
#define SYMBOL_HEADER           ((1<<8)|TC_SYMBOL)
#define CONS_HEADER             ((2<<8)|TC_PAIR)

// these make the C output more compact & readable
#define IRK_TEST(x)		((x) ? IRK_TRUE : IRK_FALSE)
#define IRK_IS_TRUE(x)		((x) != IRK_FALSE)

#define UOBJ_GET(o,i)           (((irk_vector*)(o))->val[i])
#define UOBJ_SET(o,i,v)         (((irk_vector*)(o))->val[i] = v)

// code output for literals
#define UOTAG(n)                (TC_USEROBJ+((n)<<2))
#define UITAG(n)                (TC_USERIMM+((n)<<8))
#define UPTR(n,o)               ((irk_int)(constructed_##n+o))
#define UPTR0(n)                ((irk_int)(&constructed_##n))
#define UOHEAD(l,n)             ((l<<8)|UOTAG(n))
#define INTCON(p)		((irk_int)TAG_INTEGER(p))

// here we want something that looks like a pointer, but is unlikely,
// i.e. ...111111100
#define GC_SENTINEL		(-4)

// XXX technically this is 'tagging' rather than boxing.  think about renaming them.
static irk_int unbox (object * n) {return (irk_int)n >> 1;}
static object *   box (irk_int n) {return (object *) ((n << 1) | 1);}

// Here's an interesting idea.  Can we store the first item of a multi-item tuple
//   in with the typecode?  Can we avoid storing lengths?  Maybe if the most important
//   variable-length tuple is the environment tuple, we can define its tag in such a way
//   that assumes 8-byte alignment?

// integer/pointer [no length indicator?]
typedef uintptr_t header;

// XXX future path: eventually we'd like to be able to model C types in the
//   Irken type system - in which case we could model these guys directly in
//   irken.

// environment tuple - 'rib'
typedef struct _tuple
{
  header tc;
  struct _tuple * next;
  object * val[0];
} irk_tuple;

// full continuation
typedef struct _save {
  header tc;
  struct _save * next;
  irk_tuple * lenv;
  void * pc;
  object *regs[0];
} irk_save;

typedef struct _vector {
  header tc;
  object * val[0];
} irk_vector;

typedef struct _closure {
  header tc;
  void * pc;
  irk_tuple * lenv;
} irk_closure;

// The layout of strings is actually an endless source of
// hand-wringing.  I can't bring myself to waste an entire 64 bits for
// the length part of this field (especially since the tuple header already
// has *most* of this information), but this one departure from the
// regular layout creates a mess of special-case code for strings...
// Another possible encoding (that's really tempting) would be to store
// a uint8 in the first character that says how many characters of the
// full-word length are junk.  (so that you would compute the length by
// ((tc>>8)*sizeof(object))-((uint8)data[0]))
//
// Another thing to consider - like python, always secretly
// zero-terminate strings.
typedef struct _string {
  header tc;
  uint32_t len;
  // hopefully we get 32-bit alignment here
  char data[];
} irk_string;

typedef struct _pair {
  header tc;
  object * car;
  object * cdr;
} irk_pair;

#define GET_TYPECODE(p) (((irk_int)(p))&0xff)

static int
is_int (object * ob)
{
  return (irk_int) ob & 1;
}

static irk_int
is_immediate (object * ob)
{
  irk_int tc = ((irk_int) ob) & 0xff;
  if (tc & 3) {
    return tc;
  } else {
    return 0;
  }
}

static irk_int
string_tuple_length (irk_int n)
{
  irk_int word_size = sizeof (object);
  irk_int len_size = sizeof (int32_t);
  irk_int nwords = HOW_MANY (n + len_size, word_size);
  return nwords;
}

static object * make_string (irk_int len);

static irk_int
get_safe_typecode (object * ob)
{
  if (is_immediate (ob)) {
    return GET_TYPECODE (ob);
  } else {
    return GET_TYPECODE (*ob);
  }
}

#endif // IRK_H

