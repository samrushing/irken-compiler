;; -*- Mode: Irken -*-

;; PBKDF2-HMAC (lib/crypto/pbkdf2.scm): the SHA-256 vectors from rfc7914
;;  section 11, and two more (checked with python's hashlib).

;; skip: b -- the VM can't reach libsodium through its FFI.

(include "lib/basis.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/pbkdf2.scm")

(printf (string->hex (pbkdf2/sha256 "passwd" "salt" 1 64)) "\n")
(printf (string->hex (pbkdf2/sha256 "Password" "NaCl" 80000 64)) "\n")
;; SCRAM-SHA-256's SaltedPassword (rfc7677's example)
(printf (string->hex (pbkdf2/sha256 "pencil" "salty" 4096 32)) "\n")
(printf (string->hex (pbkdf2/sha512 "password" "salt" 2 64)) "\n")

;; => 55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783
;; => 4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d
;; => 28f9c07ab10d7bba50d5d87f453a6f9bfde6e589133ab75c8356b4969c1d11e8
;; => e1d9c16aa681708a45f5c7c4e215ceb66e011a2e9f0040713f18aefdb866d53cf76cab2868a39b9f7840edce4fef5a82be67335c77a6068e04112754f27ccf4e
