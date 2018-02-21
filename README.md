# ed25519-xeno

**Common Lisp implementation of EdDSA on top of curve isomorphic 
to the twisted Edwards curve -x²+y² = 1 - 121665/121666 x²y² 
over GF(2²⁵⁵ - 19).**

The implementation heavily borrows from the following:

- SUPERCOP reference ["ref10"][ref10] implementation;
- @agl's Golang [ed25519][golang];
- @isislovecruft's Rust [ed25519-dalek][rust].

### Rationale

Main goal is to benchmark performance of the SBCL-optimised low-level 
Common Lisp in the context of cryptographic algorithms, while 
the name is inspired partly by C++ [ed25519-donna][cpp] and 
Rust [ed25519-dalek][rust] libraries and partly by the the xenomorphic
look of those `(() ((())) () (())`:)

### TODO

Currently only field element operations are implemented. Edwards 
representation group element implementation is under construction,
while generate/sign/verify interface will be ready right after group
operations are implemented:

- [x] field element implementation;
- [ ] group element implementation;
- [ ] generate/sign/verify API.

[ref10]: https://bench.cr.yp.to/supercop.html
[golang]: https://github.com/agl/ed25519
[cpp]: https://github.com/floodyberry/ed25519-donna
[rust]: https://github.com/isislovecruft/ed25519-dalek

