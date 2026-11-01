generate a new p256 key & self-signed cert for localhost:

$ self/compile demo/crypto/gencert.scm
$ demo/crypto/gencert

outputs: key.pem (PKCS#8, mode 0600), cert.pem

options:
  -cn <name>    host name for the CN and subjectAltName [localhost]
                (localhost also gets 127.0.0.1 and ::1)
  -days <n>     validity [365]
  -key <path>   [key.pem]
  -cert <path>  [cert.pem]
  -k            keep the existing key, write only a new cert

the cert is meant to be trusted directly by a browser or OS: it has
subjectAltName, keyUsage digitalSignature, extendedKeyUsage serverAuth
and basicConstraints CA:FALSE.  For a demo cert, keep -days under 398.

with openssl, the rough equivalent is:

$ openssl req -x509 -nodes -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
    -keyout key.pem -out cert.pem -days 365 -subj '/CN=localhost' \
    -addext 'subjectAltName=DNS:localhost'

[lib/crypto/sig.scm reads the PKCS#8 keys that openssl writes, too.]

----------------------------------------------------------------------

describe certificates, and check the chain they form:

$ self/compile demo/crypto/x509tool.scm
$ demo/crypto/x509tool chain.pem

each input file is PEM (any number of certificates) or DER.  for each
cert it prints the names, validity, key, fingerprint and decoded
extensions (SANs, key usages, policies, AIA, CRL DPs, SCTs, ...).
then, for each cert, it finds the issuer among the inputs and checks
the issuer name, key ids, that the issuer is a CA, and the signature.
the exit status is 1 if it found a problem.

options:
  -v   also print keys and signatures in hex
  -a   also dump each certificate's ASN.1

signatures it can check: ECDSA on P-256/P-384 (lib/crypto/ecdsa.scm),
Ed25519, and RSA PKCS#1 v1.5 with sha256/384/512.
