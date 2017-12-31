;; -*- Mode: irken -*-

;; -- do not edit: auto-generated from interface 'stdio.ffi'
;; --   platform: Darwin  machine: x86_64

(includes "stdio.h")
(sig fopen ((* char) (* char) -> (* FILE)))
(sig fwrite ((* char) int int (* FILE) -> int))
(sig fread ((* char) int int (* FILE) -> int))
(sig fgetc ((* FILE) -> int))
(sig fclose ((* FILE) -> int))
(sig fflush ((* FILE) -> int))
(sig fdopen (int (* char) -> (* FILE)))
(sig putchar (int -> int))
(sig getchar ( -> int))
(sig stdout (* FILE))
