generate a new p256 key & cert for localhost:

$ openssl req -x509 -nodes -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -keyout key.pem -out cert.pem -days 720 -subj '/CN=localhost'

[this writes the key as PKCS#8 wrapping an RFC 5915 EC key, which
 lib/crypto/sig.scm reads as of Oct 2026.]

If you already have a key:

$ openssl req -x509 -nodes -key key.pem -out cert.pem -days 720 -subj '/CN=localhost'

[need to make a genpkey.scm]

outputs: key.pem, cert.pem
