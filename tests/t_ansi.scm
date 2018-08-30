;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(printf "testing, "
        (ansi red "red") " " (ansi blue "blue") " "
        (ansi (bold red) "bold red") " " (ansi (bold blue) "bold blue") "\n"
        (ansi (bold red) (int 99) "->" (int 88)) "\n"
        )
