(cinclude "fcntl.h")

(define O_RDONLY (%%cexp int "O_RDONLY"))
(define O_WRONLY (%%cexp int "O_WRONLY"))
(define O_RDWR (%%cexp int "O_RDWR"))

(let ((x O_RDONLY)
      (y #\eof)
      )
  y)
