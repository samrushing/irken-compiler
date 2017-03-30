;; -*- Mode: Irken -*-

;; XXX incomplete.

;; See https://github.com/libffi/libffi
;;
;; FreeBSD, OS X, and Linux seem to have this installed by default.

(include "lib/basis.scm")
(include "lib/malloc.scm")

(cinclude "ffi/ffi.h")

(datatype ffitype
  (:void)
  (:uint8)
  (:sint8)
  (:uint16)
  (:sint16)
  (:uint32)
  (:sint32)
  (:uint64)
  (:sint64)
  (:float)
  (:double)
  (:uchar)
  (:schar)
  (:ushort)
  (:sshort)
  (:uint)
  (:sint)
  (:ulong)
  (:slong)
  (:longdouble)
  (:pointer)
  (:struct (list ffitype))
  )

(define ffi-type-void       (%%cexp (-> int) "&ffi_type_void"))
(define ffi-type-uint8      (%%cexp (-> int) "&ffi_type_uint8"))
(define ffi-type-sint8      (%%cexp (-> int) "&ffi_type_sint8"))
(define ffi-type-uint16     (%%cexp (-> int) "&ffi_type_uint16"))
(define ffi-type-sint16     (%%cexp (-> int) "&ffi_type_sint16"))
(define ffi-type-uint32     (%%cexp (-> int) "&ffi_type_uint32"))
(define ffi-type-sint32     (%%cexp (-> int) "&ffi_type_sint32"))
(define ffi-type-uint64     (%%cexp (-> int) "&ffi_type_uint64"))
(define ffi-type-sint64     (%%cexp (-> int) "&ffi_type_sint64"))
(define ffi-type-float      (%%cexp (-> int) "&ffi_type_float"))
(define ffi-type-double     (%%cexp (-> int) "&ffi_type_double"))
(define ffi-type-uchar      (%%cexp (-> int) "&ffi_type_uchar"))
(define ffi-type-schar      (%%cexp (-> int) "&ffi_type_schar"))
(define ffi-type-ushort     (%%cexp (-> int) "&ffi_type_ushort"))
(define ffi-type-sshort     (%%cexp (-> int) "&ffi_type_sshort"))
(define ffi-type-uint       (%%cexp (-> int) "&ffi_type_uint"))
(define ffi-type-sint       (%%cexp (-> int) "&ffi_type_sint"))
(define ffi-type-ulong      (%%cexp (-> int) "&ffi_type_ulong"))
(define ffi-type-slong      (%%cexp (-> int) "&ffi_type_slong"))
(define ffi-type-longdouble (%%cexp (-> int) "&ffi_type_longdouble"))
(define ffi-type-pointer    (%%cexp (-> int) "&ffi_type_pointer"))

(define get-ffi-type
  (ffitype:void)       -> ffi-type-void
  (ffitype:uint8)      -> ffi-type-uint8
  (ffitype:sint8)      -> ffi-type-sint8
  (ffitype:uint16)     -> ffi-type-uint16
  (ffitype:sint16)     -> ffi-type-sint16
  (ffitype:uint32)     -> ffi-type-uint32
  (ffitype:sint32)     -> ffi-type-sint32
  (ffitype:uint64)     -> ffi-type-uint64
  (ffitype:sint64)     -> ffi-type-sint64
  (ffitype:float)      -> ffi-type-float
  (ffitype:double)     -> ffi-type-double
  (ffitype:uchar)      -> ffi-type-uchar
  (ffitype:schar)      -> ffi-type-schar
  (ffitype:ushort)     -> ffi-type-ushort
  (ffitype:sshort)     -> ffi-type-sshort
  (ffitype:uint)       -> ffi-type-uint
  (ffitype:sint)       -> ffi-type-sint
  (ffitype:ulong)      -> ffi-type-ulong
  (ffitype:slong)      -> ffi-type-slong
  (ffitype:longdouble) -> ffi-type-longdouble
  (ffitype:pointer)    -> ffi-type-pointer
  (ffitype:struct tl)  -> (error "ffitype:struct NYI")
  )

(define sizeof-ffi-cif (%%cexp (-> int) "sizeof(ffi_cif)"))
(define sizeof-ffi-arg (%%cexp (-> int) "sizeof(ffi_arg)"))
(define sizeof-ffi-type (%%cexp (-> int) "sizeof(ffi_type)"))

(define (make-arglist argtypes)
  (let ((nargs (length argtypes))
        (mbuf (malloc (* sizeof-ffi-arg nargs))))
    (for-range i nargs
      (%%cexp (int int int -> undefined)
              "((ffi_type **)%0)[%1] = (ffi_type*)%2"
              (malloc/addr mbuf)
              i
              (get-ffi-type (nth argtypes i))
              ))
    mbuf))

(datatype cif
  (:t malloc)
  )

(define (make-cif rtype argtypes)
  (let ((nargs (length argtypes))
        (argsbuf (make-arglist argtypes))
        (cifbuf (malloc sizeof-ffi-cif)))
    (if (= 0 
           (%%cexp (int int int int -> int)
                   "ffi_prep_cif ((ffi_cif*)%0, FFI_DEFAULT_ABI, %1, (ffi_type *)%2, (ffi_type **)%3)"
                   (malloc/addr cifbuf)
                   nargs
                   (get-ffi-type rtype)
                   (malloc/addr argsbuf)))
        (cif cifbuf)
        (raise (:FFIFailure "ffi_pref_cif")))
    ))


    
    


  
