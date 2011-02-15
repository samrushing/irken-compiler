;; -*- Mode: Irken -*-

(define (make-context)
  { datatypes = (alist/make)
    macros = (alist/make)
    scc-graph = '()
    regalloc = (make-register-allocator)
    }
  )
