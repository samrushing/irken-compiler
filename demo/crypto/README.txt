generate a new p256 key & cert for localhost:

$ openssl req -x509 -nodes -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -keyout key.pem -out cert.epm -days 720 -subj '/CN=localhost'

[the output key is not parsable by irken. not pkcs#8?]

If you already have a key:

$ openssl req -x509 -nodes -key key.pem -out cert.epm -days 720 -subj '/CN=localhost'

[need to make a genpkey.scm]

outputs: key.pem, cert.pem
