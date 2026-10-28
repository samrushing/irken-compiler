self-signed test certificates for tests/t_x509_verify.scm (the private
keys were thrown away).  ed25519-ber.pem is deliberately not DER: its
serial number has redundant leading zero bytes, and it is signed over
that encoding, so verifying it needs the original tbsCertificate bytes
rather than a re-encoding.
