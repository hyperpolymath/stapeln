;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for stapeln

(define playbook
  `((version . "1.0.0")
    (procedures
      ((dev-frontend . (("cd" . "frontend")
                        ("build" . "npx rescript build")
                        ("serve" . "deno run --allow-net --allow-read dev-server.js")))
       (dev-backend . (("cd" . "backend")
                       ("deps" . "mix deps.get")
                       ("server" . "mix phx.server")))
       (build-ffi . (("cd" . "ffi/zig")
                     ("build" . "zig build")
                     ("test" . "zig build test")))
       (deploy . ())
       (rollback . ())
       (debug . ())))
    (alerts . ())
    (contacts
      ((maintainer . "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")))))
