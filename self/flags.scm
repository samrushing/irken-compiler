
(define CC "clang")
(define CFLAGS "-std=c99 -O3 -fomit-frame-pointer -I./include")
;;; NOTE: if you are tempted to put "-g" in there and your compiler is gcc,
;;;   you *must* add -fno-var-tracking as well or your compiles will never finish.
;;;   See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=56510
