#!/usr/bin/csi -s

;; for use with https://github.com/BurntSushi/toml-test
;; $ toml-test ./tests/toml-test-decoder.scm

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

(define (toml->test-json data)
  (cond
    ((null? data) '())
    ((symbol? data) data)
    ((pair? data)
     (cons (toml->test-json (car data))
           (toml->test-json (cdr data))))
    ((vector? data)
     (if (and (> (vector-length data) 0)
              (list? (vector-ref data 0)))
       ;; array of tables
       (vector-map (lambda (i x) (toml->test-json x)) data)
       ;; array
       (list '(type . "array")
             (cons 'value
                   (vector-map (lambda (i x) (toml->test-json x))
                               data)))))
    ((number? data)
     (list (cons 'type (if (integer? data) "integer" "float"))
           (cons 'value (number->string data))))
    ((boolean? data)
     (list '(type . "bool") (cons 'value (if data "true" "false"))))
    ((rfc3339? data)
     (list '(type . "datetime") (cons 'value (rfc3339->string data))))
    ((string? data)
     (list '(type . "string") (cons 'value data)))))

;; read from stdin and output JSON
(let ((data (read-toml (current-input-port))))
  (if data (print (json->string (toml->test-json data)))
           (exit 1)))
