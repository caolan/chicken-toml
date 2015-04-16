#!/usr/bin/csi -s

;; for use with https://github.com/BurntSushi/toml-test
(load-relative "../toml.scm")
(import scheme)
(import toml)
(use medea)

(let ((data (read-toml (current-input-port))))
  (if data
    (print (json->string data))
    (exit 1)))
