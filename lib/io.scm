;; -*- Mode: Irken -*-

(%backend (c llvm)
  (include "lib/io_c.scm"))
(%backend bytecode
  (include "lib/io_vm.scm"))
