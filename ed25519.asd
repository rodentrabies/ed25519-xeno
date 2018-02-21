(defsystem :ed25519
  :description "Common Lisp implementation of EdDSA based on curve25519"
  :version "0.0.1"
  :author "whythat <whythat@protonmail.com>"

  :class :package-inferred-system
  :pathname #P"./"

  :depends-on (:ed25519/core/all
               :ed25519/eddsa/all))
