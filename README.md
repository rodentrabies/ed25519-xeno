# ed25519-xeno

**Common Lisp implementation of EdDSA on top of curve isomorphic 
to the twisted Edwards curve -x²+y² = 1 - 121665/121666 x²y² 
over GF(2²⁵⁵ - 19).**

The implementation heavily borrows from the following:

- SUPERCOP reference ["ref10"][ref10] implementation;
- Adam Langley's Golang [ed25519][golang];
- @dalek-cryptography Rust [curve25519-dalek][cdalek]/[ed25519-dalek][edalek].

### Rationale

Main goal is to benchmark performance of the SBCL-optimised low-level 
Common Lisp in the context of cryptographic algorithms, while 
the name is inspired partly by C++ [ed25519-donna][cpp] and 
Rust [ed25519-dalek][rust] libraries and partly by the the xenomorphic
look of those `(() ((())) () (())`:)

### Disclaimer

**This is untested/unreviewed implementation that absolutely should not be used
for anything at all unless it's your new cryptocurrency because it's fun to
watch those fail and it's twice as fun to watch those fail in Common Lisp.**

### TODO

- [x] field element implementation;
- [x] group element implementation;
- [x] scalar implementation;
- [x] generate/sign/verify API;
- [ ] warnings, debugging and testing.

[ref10]: https://bench.cr.yp.to/supercop.html
[golang]: https://github.com/agl/ed25519
[cpp]: https://github.com/floodyberry/ed25519-donna
[cdalek]: https://github.com/dalek-cryptography/curve25519-dalek
[edalek]: https://github.com/dalek-cryptography/ed25519-dalek
