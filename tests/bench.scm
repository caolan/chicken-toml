#!/usr/bin/csi -s

(include-relative "../toml.scm")
(import
  scheme
  (only chicken.io read-string)
  toml)

(define raw
  (with-input-from-file "./tests/fixtures/example-v0.4.0.toml" read-string))
;(print (read-toml raw))

(time
  (let lp ((n 1000))
    (when (> n 0)
      (read-toml raw)
      (lp (sub1 n)))))
