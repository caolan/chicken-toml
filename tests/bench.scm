#!/usr/bin/csi -s

(load-relative "../toml.scm")
(import scheme)
(import toml)

(use utils miscmacros)

(define raw (read-all "./tests/fixtures/example-v0.4.0.toml"))
;(print (read-toml raw))

(time (repeat 1000 (read-toml raw)))
