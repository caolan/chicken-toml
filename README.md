# TOML parser for CHICKEN Scheme

```scheme
(use toml)

(read-toml string/input-port) ;; => data
(write-toml data output-port)
(toml->string data) ;; => string
```
