;; SPDX-License-Identifier: PMPL-1.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for stapeln

(define neurosym-config
  `((version . "1.0.0")
    (symbolic-layer
      ((type . "scheme")
       (reasoning . "minikanren")
       (verification . "idris2-proofs")))
    (neural-layer
      ((embeddings . false)
       (fine-tuning . false)))
    (integration
      ((security-reasoning . "minikanren-planned")
       (validation . "zig-ffi-active")
       (scanning . "hypatia-ci")))))
