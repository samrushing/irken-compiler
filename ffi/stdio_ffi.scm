;; bootstrap version.

(includes "stdio.h" )
(cflags )
(lflags )
(sig fopen ((* char) (* char) -> (* FILE)))
(sig fwrite ((* void) int int (* FILE) -> int))
(sig fread ((* void) int int (* FILE) -> int))
(sig fgetc ((* FILE) -> int))
(sig fclose ((* FILE) -> int))
(sig fflush ((* FILE) -> int))
(sig fdopen (int (* char) -> (* FILE)))
(sig putchar (int -> int))
(sig getchar ( -> int))
