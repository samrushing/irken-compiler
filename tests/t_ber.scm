;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/codecs/hex.scm")
(require "lib/codecs/base64.scm")
(require "lib/asn1/ber.scm")

;; XXX need to write real tests.

(define tests-passed 0)
(defmacro assert2
  (assert2 exp)
  -> (if (not exp)
         (begin
           (printf "assertion failed: " (repr (car (%%sexp exp))) "\n")
           (raise (:AssertionFailed)))
         (set! tests-passed (+ tests-passed 1))))

;; XXX need to do some round-trip tests with all known types.
(define (round-trip ber0)
  (let ((asn1 (ber->asn1 ber0))
        (ber1 (asn1->ber asn1)))
    (pp-ber asn1 0)
    (printf "\n")
    (printf "ber0 " (string->hex ber0) "\n")
    (printf "ber1 " (string->hex ber1) "\n")
    ))

(define (test0)
  (let ((c (collector))
        (ber (ber:SEQUENCE
              (list
               (ber:INTEGER -314159)
               (ber:NULL)
               (ber:STRING (asn1string:OCTET) "testing")
               (ber:OID (list 1 2 840 113549 1))
               (ber:SET (list (ber:INTEGER 3) (ber:INTEGER 5) (ber:INTEGER 7)))
               ))))
    (for-list x '(0 127 128 256 -128 -129 314159 -314159)
      (c.reset)
      (ber/encode c.push (ber:INTEGER x))
      (printf "val: " (lpad 10 (int x)) " " (string (c.val)) "\n")
      (printf (ber-repr (ber->asn1 (c.val))) "\n")
      )
    (printf (ber-repr (ber->asn1 "\x02\x08\x12\x34\x56\x78\xde\xad\xbe\xef")) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:INTEGER 314159265)))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:NULL)))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:STRING (asn1string:OCTET) "testing, testing")))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:OID (list 1 2 840 113549 1))))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:SEQUENCE (list (ber:INTEGER 1) (ber:INTEGER 2) (ber:INTEGER 3)))))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:BOOLEAN #f)))) "\n")
    (printf (ber-repr (ber->asn1 (asn1->ber (ber:BOOLEAN #t)))) "\n")
    (printf "ber = " (string (asn1->ber ber)) "\n")
    (let ((ber0 (ber->asn1 (asn1->ber ber))))
      (printf (ber-repr ber0) "\n")
      (printf (ber-repr ber) "\n")
      (printf " equal? " (bool (eq? (cmp:=) (magic-cmp ber0 ber))) "\n")
      )))

(define der0-b64
  (string-concat
   (list "MIIMMzCCC5ygAwIBAgIRAM4hg/ZYuy6A41W4G43gQtYwDQYJKoZIhvcNAQEFBQAwggKoMQswCQYDVQQG"
         "EwJERTEVMBMGA1UEChMMVmlhVGhpbmtTb2Z0MREwDwYDVQQLEwhSZXNlYXJjaDEzMDEGA1UEAxMqVmlh"
         "VGhpbmtTb2Z0IE9JRC1PdmVyZmxvdyBUZXN0IENlcnRpZmljYXRlMRswGQYDVQQDExJiYWRndXkuZXhh"
         "bXBsZS5jb20xLzAtBgkqhkiG9w0BCQEWIGRhbmllbC1tYXJzY2hhbGxAdmlhdGhpbmtzb2Z0LmRlMTMw"
         "MQYDVQQDEypWaWFUaGlua1NvZnQgT0lELU92ZXJmbG93IFRlc3QgQ2VydGlmaWNhdGUxGzAZBgNVBAMT"
         "EmJhZGd1eS5leGFtcGxlLmNvbTEsMCoGB5CAgIBVBAMTHzMyYml0Lm92ZXJmbG93LmFyYzIuZXhhbXBs"
         "ZS5jb20xLDAqBgdVkICAgAQDEx8zMmJpdC5vdmVyZmxvdy5hcmMxLmV4YW1wbGUuY29tMSwwKgYHVQSQ"
         "gICAAxMfMzJiaXQub3ZlcmZsb3cucm9vdC5leGFtcGxlLmNvbTEzMDEGD5CAgIBVkICAgASQgICAAxMe"
         "MzJiaXQub3ZlcmZsb3cuYWxsLmV4YW1wbGUuY29tMTEwLwYMgoCAgICAgICAVQQDEx82NGJpdC5vdmVy"
         "Zmxvdy5hcmMyLmV4YW1wbGUuY29tMTEwLwYMVYKAgICAgICAgAQDEx82NGJpdC5vdmVyZmxvdy5hcmMx"
         "LmV4YW1wbGUuY29tMTEwLwYMVQSCgICAgICAgIADEx82NGJpdC5vdmVyZmxvdy5yb290LmV4YW1wbGUu"
         "Y29tMUIwQAYegoCAgICAgICAVYKAgICAgICAgASCgICAgICAgIADEx42NGJpdC5vdmVyZmxvdy5hbGwu"
         "ZXhhbXBsZS5jb20wHhcNMTQxMTE1MjM1NDI3WhcNMTQxMTE2MjM1NDI3WjCCAqgxCzAJBgNVBAYTAkRF"
         "MRUwEwYDVQQKEwxWaWFUaGlua1NvZnQxETAPBgNVBAsTCFJlc2VhcmNoMTMwMQYDVQQDEypWaWFUaGlu"
         "a1NvZnQgT0lELU92ZXJmbG93IFRlc3QgQ2VydGlmaWNhdGUxGzAZBgNVBAMTEmJhZGd1eS5leGFtcGxl"
         "LmNvbTEvMC0GCSqGSIb3DQEJARYgZGFuaWVsLW1hcnNjaGFsbEB2aWF0aGlua3NvZnQuZGUxMzAxBgNV"
         "BAMTKlZpYVRoaW5rU29mdCBPSUQtT3ZlcmZsb3cgVGVzdCBDZXJ0aWZpY2F0ZTEbMBkGA1UEAxMSYmFk"
         "Z3V5LmV4YW1wbGUuY29tMSwwKgYHkICAgFUEAxMfMzJiaXQub3ZlcmZsb3cuYXJjMi5leGFtcGxlLmNv"
         "bTEsMCoGB1WQgICABAMTHzMyYml0Lm92ZXJmbG93LmFyYzEuZXhhbXBsZS5jb20xLDAqBgdVBJCAgIAD"
         "Ex8zMmJpdC5vdmVyZmxvdy5yb290LmV4YW1wbGUuY29tMTMwMQYPkICAgFWQgICABJCAgIADEx4zMmJp"
         "dC5vdmVyZmxvdy5hbGwuZXhhbXBsZS5jb20xMTAvBgyCgICAgICAgIBVBAMTHzY0Yml0Lm92ZXJmbG93"
         "LmFyYzIuZXhhbXBsZS5jb20xMTAvBgxVgoCAgICAgICABAMTHzY0Yml0Lm92ZXJmbG93LmFyYzEuZXhh"
         "bXBsZS5jb20xMTAvBgxVBIKAgICAgICAgAMTHzY0Yml0Lm92ZXJmbG93LnJvb3QuZXhhbXBsZS5jb20x"
         "QjBABh6CgICAgICAgIBVgoCAgICAgICABIKAgICAgICAgAMTHjY0Yml0Lm92ZXJmbG93LmFsbC5leGFt"
         "cGxlLmNvbTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAr69++f2X/KX8uuHVUtd3w5A26eePFdjH"
         "c9SklpLwF2aSemAEDXeEHE1m6W23pLrkCkpNGiMKUwPaIBqU1VQKz0KD6/LJhh3WOls/2cx3ESlyNfV9"
         "OR6nUC0YLEmgTQJGsj+U/zeC8CSLfcgWsRtgkF16Lgy3gP0RlCXiBX8UmhUCAwEAAaOCBVcwggVTMB0G"
         "A1UdDgQWBBQ1LbdVMqJ2I4DA+lWOSkUM1OIGDTCCAuoGA1UdIwSCAuEwggLdgBQ1LbdVMqJ2I4DA+lWO"
         "SkUM1OIGDaGCArCkggKsMIICqDELMAkGA1UEBhMCREUxFTATBgNVBAoTDFZpYVRoaW5rU29mdDERMA8G"
         "A1UECxMIUmVzZWFyY2gxMzAxBgNVBAMTKlZpYVRoaW5rU29mdCBPSUQtT3ZlcmZsb3cgVGVzdCBDZXJ0"
         "aWZpY2F0ZTEbMBkGA1UEAxMSYmFkZ3V5LmV4YW1wbGUuY29tMS8wLQYJKoZIhvcNAQkBFiBkYW5pZWwt"
         "bWFyc2NoYWxsQHZpYXRoaW5rc29mdC5kZTEzMDEGA1UEAxMqVmlhVGhpbmtTb2Z0IE9JRC1PdmVyZmxv"
         "dyBUZXN0IENlcnRpZmljYXRlMRswGQYDVQQDExJiYWRndXkuZXhhbXBsZS5jb20xLDAqBgeQgICAVQQD"
         "Ex8zMmJpdC5vdmVyZmxvdy5hcmMyLmV4YW1wbGUuY29tMSwwKgYHVZCAgIAEAxMfMzJiaXQub3ZlcmZs"
         "b3cuYXJjMS5leGFtcGxlLmNvbTEsMCoGB1UEkICAgAMTHzMyYml0Lm92ZXJmbG93LnJvb3QuZXhhbXBs"
         "ZS5jb20xMzAxBg+QgICAVZCAgIAEkICAgAMTHjMyYml0Lm92ZXJmbG93LmFsbC5leGFtcGxlLmNvbTEx"
         "MC8GDIKAgICAgICAgFUEAxMfNjRiaXQub3ZlcmZsb3cuYXJjMi5leGFtcGxlLmNvbTExMC8GDFWCgICA"
         "gICAgIAEAxMfNjRiaXQub3ZlcmZsb3cuYXJjMS5leGFtcGxlLmNvbTExMC8GDFUEgoCAgICAgICAAxMf"
         "NjRiaXQub3ZlcmZsb3cucm9vdC5leGFtcGxlLmNvbTFCMEAGHoKAgICAgICAgFWCgICAgICAgIAEgoCA"
         "gICAgICAAxMeNjRiaXQub3ZlcmZsb3cuYWxsLmV4YW1wbGUuY29tghEAziGD9li7LoDjVbgbjeBC1jAP"
         "BgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBBjCBnQYDVR0lAQH/BIGSMIGPBggrBgEFBQcDAQYI"
         "KwYBBQUHAwIGA1UEAwYHkICAgFUEAwYHVZCAgIAEAwYHVQSQgICAAwYPkICAgFWQgICABJCAgIADBgyC"
         "gICAgICAgIBVBAMGDFWCgICAgICAgIAEAwYMVQSCgICAgICAgIADBh6CgICAgICAgIBVgoCAgICAgICA"
         "BIKAgICAgICAgAMwcwYDVR0RBGwwaoEgZGFuaWVsLW1hcnNjaGFsbEB2aWF0aGlua3NvZnQuZGWGQmh0"
         "dHBzOi8vd3d3LnZpYXRoaW5rc29mdC5kZS9+ZGFuaWVsLW1hcnNjaGFsbC9hc24uMS9vaWRfZmFjdHMu"
         "aHRtbIgCiDcwcwYDVR0SBGwwaoEgZGFuaWVsLW1hcnNjaGFsbEB2aWF0aGlua3NvZnQuZGWGQmh0dHBz"
         "Oi8vd3d3LnZpYXRoaW5rc29mdC5kZS9+ZGFuaWVsLW1hcnNjaGFsbC9hc24uMS9vaWRfZmFjdHMuaHRt"
         "bIgCiDcwgZgGA1UdIASBkDCBjTAFBgNVBAMwCQYHkICAgFUEAzAJBgdVkICAgAQDMAkGB1UEkICAgAMw"
         "EQYPkICAgFWQgICABJCAgIADMA4GDIKAgICAgICAgFUEAzAOBgxVgoCAgICAgICABAMwDgYMVQSCgICA"
         "gICAgIADMCAGHoKAgICAgICAgFWCgICAgICAgIAEgoCAgICAgICAAzANBgkqhkiG9w0BAQUFAAOBgQCR"
         "wSvg6dicI068TSl17LlOzWaoFs5GwR9W1b3QjSlpFL9W1uSfm1SxhNCNVJ62IsEvOEJfo3SFP/uPiIG/"
         "2a6YlQdmOMvn8h0PYfW9Fx1kLElZvcuTa3AR/VG/0vhfkNxdkaDUmOZV72R6gkGicP8yOLx9imt0Qalv"
         "gK8rG256mA==")))

(define der0 (b64-decode der0-b64))
(define der1 (hex->string "603c303a040361626302013202022711020417bc927a3020300602010a020165300602010a020165300602010a020165300602010a0201650101000a0100"))

(define (test2)
  (ber->asn1 der0)
  (ber->asn1 der1)
  )

(define (test1)
  (let ((s (format (repeat 508 "A")))
        (asn1 (ber:STRING (asn1string:OCTET) s))
        (ber0 (asn1->ber asn1))
        (asn2 (ber->asn1 ber0))
        (ber1 (asn1->ber asn2)))
    (printf "ber0 " (string->hex ber0) "\n")
    (printf "ber1 " (string->hex ber1) "\n")
    (assert2 (string=? ber0 ber1))
    ))

(test0)
(test1)
(test2)

(printf "passed " (int tests-passed) " tests.\n")
