self-signed test certificates for tests/t_x509_verify.scm (the private
keys were thrown away).  ed25519-ber.pem is deliberately not DER: its
serial number has redundant leading zero bytes, and it is signed over
that encoding, so verifying it needs the original tbsCertificate bytes
rather than a re-encoding.

rsa.key and rsa-pss.sig are for tests/t_rsa_pss_verify.scm: a throwaway
2048-bit test key, and an RSASSA-PSS (sha256, salt length 32) signature
of "hello, irken" made with:

  openssl dgst -sha256 -sign rsa.key -sigopt rsa_padding_mode:pss \
    -sigopt rsa_pss_saltlen:digest -out rsa-pss.sig
