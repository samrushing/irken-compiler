#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// it's arguable that there's no need for this include file, that it should
//   just be placed at the top of header.c.

typedef intptr_t pxll_int;
typedef void * object;

const size_t heap_size = 100000;

object * heap0 = NULL;
object * heap1 = NULL;

/* Type Tags */

/* immediate types (multiples of 2 (but not 4!)) */

#define TC_INT                  (0<<1) // 0x00
#define TC_CHAR                 (1<<1) // 0x02
#define TC_BOOL                 (3<<1) // 0x06
#define TC_NIL                  (5<<1) // 0x0a
#define TC_UNDEFINED            (7<<1) // 0x0e

/* pointer types (multiples of 4) */
#define TC_SAVE                 (1<<2)    /* 00000100  04 */
#define TC_CLOSURE              (2<<2)    /* 00001000  08 */
#define TC_TUPLE                (3<<2)    /* 00001100  0c */
#define TC_STRING		(4<<2)	  /* 00010000  10 */
#define TC_VECTOR               (5<<2)    /* 00010100  14 */
#define TC_PAIR                 (6<<2)    /* 00011000  18 */
#define TC_SYMBOL               (7<<2)    /* 00011100  1c */
#define TC_USEROBJ              (8<<2)    /* 00100000  20 */

#define TC_LAST                 TC_USEROBJ

// immediate constants
#define PXLL_FALSE		(object *) (0x000 | TC_BOOL)
#define PXLL_TRUE		(object *) (0x100 | TC_BOOL)
#define PXLL_MAYBE		(object *) (0x200 | TC_BOOL)
#define PXLL_NIL		(object *) (0x000 | TC_NIL)
#define PXLL_UNDEFINED		(object *) (0x000 | TC_UNDEFINED)

// XXX make these inline functions rather than macros

#define GET_TYPECODE(p)		(((pxll_int)(p))&0xff)
#define GET_PAYLOAD(p)		(((pxll_int)(p))>>8)
#define GET_TUPLE_LENGTH(p)	(((pxll_int)(p))>>8)
#define GET_TUPLE_SIZE(p)	((GET_TUPLE_LENGTH(p)+1)<<2)
#define TAG_VALUE(tag,value)	((object)((tag&0xff)|(value<<8)))
#define GET_STRING_POINTER(s)   (((pxll_string *)(s))->data)

#define IS_INTEGER(p)		(((pxll_int)(p)) & 1)
#define BOX_INTEGER(p)		((object)(((p)<<1)|1))
#define UNBOX_INTEGER(p)	((p)>>1)

#define IMMEDIATE(p)		(((pxll_int)(p)) & 3)
#define IS_TYPE(t, p)		(((pxll_int)(p)&0xff)==t)
#define IS_CHAR(p)		IS_TYPE (TC_CHAR, p)
#define IS_BOOL(p)		IS_TYPE (TC_BOOL, p)
#define IS_NIL(p)		IS_TYPE (TC_NIL, p)
#define IS_UNDEFINED(p)		IS_TYPE (TC_UNDEFINED, p)

#define GET_CHAR(p)		(((pxll_int)(p)>>8))
#define TO_CHAR(ch)		((object)(pxll_int)(((ch)<<8)|TC_CHAR))

#define HOW_MANY(x,n)		(((x)+(n)-1)/(n))

// these make the C output more compact & readable
#define PXLL_TEST(x)		((x) ? PXLL_TRUE : PXLL_FALSE)
#define PXLL_IS_TRUE(x)		((x) != PXLL_FALSE)

#define UOBJ_GET(o,i)           (((pxll_vector*)(o))->val[i])
#define UOBJ_SET(o,i,v)         (((pxll_vector*)(o))->val[i] = v)

// here we want something that looks like a pointer, but is unlikely,
// i.e. ...111111100
#define GC_SENTINEL		(-4)

inline pxll_int unbox (object * n) {return (pxll_int)n >> 1;}
inline object *   box (pxll_int n) {return (object *) ((n << 1) | 1);}

// Here's an interesting idea.  Can we store the first item of a multi-item tuple
//   in with the typecode?  Can we avoid storing lengths?  Maybe if the most important
//   variable-length tuple is the environment tuple, we can define its tag in such a way
//   that assumes 8-byte alignment?

// integer/pointer [no length indicator?]
typedef uintptr_t header;

// environment tuple - 'rib'
typedef struct _tuple
{
  header tc;
  struct _tuple * next;
  object * val[0];
} pxll_tuple;

// full continuation
typedef struct _save {
  header tc;
  struct _save * next;
  pxll_tuple * lenv;
  void * pc;
  object *regs[0];
} pxll_save;

typedef struct _vector {
  header tc;
  object * val[0];
} pxll_vector;

typedef struct _closure {
  header tc;
  void * pc;
  pxll_tuple * lenv;
} pxll_closure;

typedef struct _string {
  header tc;
  uint32_t len;
  char data[0];
} pxll_string;

typedef struct _pair {
  header tc;
  object * car;
  object * cdr;
} pxll_pair;

#define GET_TYPECODE(p) (((pxll_int)(p))&0xff)

inline
int
is_int (object * ob)
{
  return (pxll_int) ob & 1;
}

inline
pxll_int
is_immediate (object * ob)
{
  pxll_int tc = ((pxll_int) ob) & 0xff;
  if (tc & 3) {
    return tc;
  } else {
    return 0;
  }
}

inline
pxll_int
string_tuple_length (pxll_int n)
{
  pxll_int word_size = sizeof (object);
  pxll_int len_size = sizeof (int32_t);
  pxll_int nwords = HOW_MANY (n + len_size, word_size);
  return nwords;
}
