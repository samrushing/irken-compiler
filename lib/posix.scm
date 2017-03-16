;; -*- Mode: Irken -*-

(cinclude "sys/utsname.h")

(define (uname)
  (%backend (c llvm)
    (let ((utsname (%callocate (struct utsname) 1)))
      (%%cexp ((buffer (struct utsname)) -> int) "uname (%0)" utsname)
      {sysname=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->sysname" utsname))
       nodename= (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->nodename" utsname))
       release=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->release" utsname))
       version=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->version" utsname))
       machine=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->machine" utsname))
       }))
  (%backend bytecode
    (let ((utsname (%%cexp (-> (list string)) "plat")))
      (match utsname with
        (sysname nodename release version machine)
        -> {sysname=sysname
            nodename=nodename
            release=release
            version=version
            machine=machine}
        _ 
        -> (raise (:UnameFailed utsname))
        )))
  )
      

       
