(module toml (read-toml)

(import scheme chicken)
(use comparse srfi-14)

(define (read-toml input)
  (parse document (->parser-input input)))

(define char-set:graphic-and-blank
  (char-set-union char-set:graphic char-set:blank))

(define toml-whitespace
  (any-of (is #\space) (is #\tab)))

(define toml-newline
  (any-of (is #\newline) (char-seq "\r\n")))

(define line-end
  (sequence
    (zero-or-more toml-whitespace) ;; allow trailing whitespace
    (any-of toml-newline end-of-input))) ;; allow missing newline at EOF

(define blank-line
  (sequence (zero-or-more toml-whitespace) toml-newline))

(define comment
  (preceded-by (is #\#) (zero-or-more (none-of* toml-newline item))))

(define basic-string
  (enclosed-by
    (is #\")
    (as-string
      (one-or-more
        (none-of* (is #\")
          (in char-set:graphic-and-blank))))
    (is #\")))

(define ((as-symbol parser) input)
  (and-let* ((result+remainder ((as-string parser) input)))
    (cons (string->symbol (car result+remainder))
          (cdr result+remainder))))

(define ((as-pair parse1 parse2) input)
  (and-let* ((a (parse1 input))
             (b (parse2 (cdr a))))
    (cons (cons (car a) (car b)) (cdr b))))

(define key
  (as-symbol (one-or-more (in char-set:graphic))))

(define value
  (any-of basic-string))

(define whitespaces
  (one-or-more toml-whitespace))

(define key-value
  (as-pair key
    (enclosed-by (sequence whitespaces (is #\=) whitespaces)
                 value
                 line-end)))

(define document
  (followed-by
    (zero-or-more
      (none-of* comment
                blank-line
                (any-of key-value)))
    end-of-input))

)
