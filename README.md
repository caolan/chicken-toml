# TOML parser for CHICKEN Scheme

Built with [comparse](http://wiki.call-cc.org/eggref/4/comparse).

```scheme
(use toml)

(read-toml string/input-port) ;; => data
(write-toml data output-port)
(toml->string data) ;; => string
```

Passes [toml-test](https://github.com/BurntSushi/toml-test) suites for
encoding and decoding.
