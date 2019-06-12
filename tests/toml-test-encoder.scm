#!/usr/bin/csi -s

;; for use with https://github.com/BurntSushi/toml-test
;; $ toml-test -encoder ./tests/toml-test-encoder.scm

(include-relative "../toml.scm")
(import scheme)
(import toml)

(import medea rfc3339 vector-lib)

;; so the long float test works
(flonum-print-precision 16)

;; JSON encoding
;;
;; The following JSON encoding applies equally to both encoders and decoders.
;;
;;   * TOML tables correspond to JSON objects.
;;   * TOML table arrays correspond to JSON arrays.
;;   * TOML values correspond to a special JSON object of the form
;;     {"type": "{TTYPE}", "value": {TVALUE}}
;;
;; In the above, TTYPE may be one of:
;;
;;   * string
;;   * integer
;;   * float
;;   * datetime
;;   * bool
;;   * array

(define (test-json->toml data)
  (cond
    ((null? data) '())
    ((vector? data)
     (vector-map (lambda (i x) (test-json->toml x)) data))
    ((pair? data)
     (let ((type (assq 'type data))
           (value (assq 'value data)))
       (if type
         (case (string->symbol (cdr type))
           ((string) (cdr value))
           ((integer float) (string->number (cdr value)))
           ((datetime) (string->rfc3339 (cdr value)))
           ((bool) (string=? (cdr value) "true"))
           ((array) (vector-map (lambda (i x) (test-json->toml x))
                                (cdr value))))
         (map (lambda (x)
                (cons (car x) (test-json->toml (cdr x))))
              data))))))

;; read from stdin and output JSON
(let ((data (read-json (current-input-port))))
  (if data (print (toml->string (test-json->toml data)))
           (exit 1)))
