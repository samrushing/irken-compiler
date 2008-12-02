

#include <stdio.h>
#include <stdint.h>

uintptr_t thing_tag_val = 0;
uintptr_t zord_tag_val = 0;

int
thing (int x)
{
 thing_tag:
  fprintf (stderr, "[%d]", x);
  if (zord_tag_val) {
    x = x + 1;
    goto *zord_tag_val;
  } else {
    thing_tag_val = (uintptr_t) &&thing_tag;
    zord (x + 1);
  }
}

int
zord (int y)
{
 zord_tag:
  fprintf (stderr, "[%d]", y);
  if (thing_tag_val) {
    y = y + 1;
    goto *thing_tag_val;
  } else {
    zord_tag_val = (uintptr_t) &&zord_tag;
    thing (y + 1);
  }
}


int main (int argc, char * argv[])
{
  thing (19);
}
