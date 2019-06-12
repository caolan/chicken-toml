# TOML parser for CHICKEN Scheme

Built with [comparse](http://wiki.call-cc.org/eggref/4/comparse).

```scheme
(import toml)

(read-toml string/input-port) ;; => data
(write-toml data output-port)
(toml->string data) ;; => string
```

Parsed TOML data is mapped to scheme types as follows:

```
TOML     | Scheme
-------------------
string   | string
integer  | number
float    | number
boolean  | boolean
table    | alist
array    | vector
date     | rfc3339 record
```

Passes [toml-test](https://github.com/BurntSushi/toml-test) suites for
encoding and decoding.
