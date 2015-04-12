(module toml (read-toml)

(import scheme chicken)
(use comparse srfi-14 extras)

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

;; TOML string escape sequences
;;
;; \b         - backspace       (U+0008)
;; \t         - tab             (U+0009)
;; \n         - linefeed        (U+000A)
;; \f         - form feed       (U+000C)
;; \r         - carriage return (U+000D)
;; \"         - quote           (U+0022)
;; \\         - backslash       (U+005C)
;; \uXXXX     - unicode         (U+XXXX)
;; \UXXXXXXXX - unicode         (U+XXXXXXXX)

(define char-set:toml-escape
  (string->char-set "btnfr\"\\"))

(define escape
  (bind (in char-set:toml-escape)
        (lambda (x)
          (result
            (case x
              ((#\b) #\backspace)
              ((#\t) #\tab)
              ((#\n) #\newline)
              ((#\f) #\page)
              ((#\r) #\return)
              ((#\") #\")
              ((#\\) #\\))))))

(define (unicode-scalar-value? code)
  (or (<= 0 code #xD7FF)
      (<= #xE000 code #x10FFFF)))

(define unicode
  (bind
    (any-of
      (preceded-by (is #\u) (as-string (repeated (in char-set:hex-digit) 4)))
      (preceded-by (is #\U) (as-string (repeated (in char-set:hex-digit) 8))))
    (lambda (x)
      (let ((code (string->number x 16)))
        (if (unicode-scalar-value? code)
          (result (##sys#char->utf8-string (integer->char code)))
          fail)))))

(define char
  (any-of (none-of* (is #\") (is #\\) (in char-set:graphic-and-blank))
          (preceded-by (is #\\) (any-of escape unicode))))

(define basic-string
  (enclosed-by
    (is #\")
    (as-string (one-or-more char))
    (is #\")))

(define escaped-whitespace
  (bind (preceded-by (is #\\) toml-newline (zero-or-more toml-whitespace))
        (lambda (x) (result ""))))

(define multi-line-basic-string
  (enclosed-by
    (sequence (char-seq "\"\"\"") (maybe toml-newline))
    (as-string
      (one-or-more (any-of char (in char-set:whitespace) escaped-whitespace)))
    (char-seq "\"\"\"")))

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
  (any-of basic-string
          multi-line-basic-string))

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
