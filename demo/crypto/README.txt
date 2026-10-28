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
