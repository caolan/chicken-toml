; TOML
; ====
;
; Tom's Obvious, Minimal Language.
;
; By Tom Preston-Werner.
;
; Latest tagged version:
; [v0.4.0](https://github.com/mojombo/toml/blob/master/versions/en/toml-v0.4.0.md).
;
; Be warned, this spec is still changing a lot. Until it's marked as 1.0, you
; should assume that it is unstable and act accordingly.
;
; Objectives
; ----------
;
; TOML aims to be a minimal configuration file format that's easy to read due to
; obvious semantics. TOML is designed to map unambiguously to a hash table. TOML
; should be easy to parse into data structures in a wide variety of languages.
;
; Example
; -------
;
; ```toml
; # This is a TOML document.
;
; title = "TOML Example"
;
; [owner]
; name = "Tom Preston-Werner"
; dob = 1979-05-27T07:32:00-08:00 # First class dates
;
; [database]
; server = "192.168.1.1"
; ports = [ 8001, 8001, 8002 ]
; connection_max = 5000
; enabled = true
;
; [servers]
;
;   # Indentation (tabs and/or spaces) is allowed but not required
;   [servers.alpha]
;   ip = "10.0.0.1"
;   dc = "eqdc10"
;
;   [servers.beta]
;   ip = "10.0.0.2"
;   dc = "eqdc10"
;
; [clients]
; data = [ ["gamma", "delta"], [1, 2] ]
;
; # Line breaks are OK when inside arrays
; hosts = [
;   "alpha",
;   "omega"
; ]
; ```

(module toml (read-toml)

(import scheme chicken)
(use comparse srfi-13 srfi-14)

(define (read-toml input)
  (parse document (->parser-input input)))

;; Some convenience functions for our implementation:

;; returns parser result as a symbol
(define ((as-symbol parser) input)
  (and-let* ((result+remainder ((as-string parser) input)))
    (cons (string->symbol (car result+remainder))
          (cdr result+remainder))))

;; returns result of two sequential parser results as a pair
(define ((as-pair parse1 parse2) input)
  (and-let* ((a (parse1 input))
             (b (parse2 (cdr a))))
    (cons (cons (car a) (car b)) (cdr b))))

; Spec
; ----
;
; * TOML is case sensitive.
; * A TOML file must contain only UTF-8 encoded Unicode characters.
; * Whitespace means tab (0x09) or space (0x20).
; * Newline means LF (0x0A) or CRLF (0x0D0A).

(define toml-whitespace
  (any-of (is #\space) (is #\tab)))

(define toml-newline
  (any-of (is #\newline) (char-seq "\r\n")))

;; Some utility parsers built on these

(define whitespaces
  (one-or-more toml-whitespace))

(define line-end
  (sequence
    (zero-or-more toml-whitespace) ;; allow trailing whitespace
    (any-of toml-newline end-of-input))) ;; allow missing newline at EOF


; Comment
; -------
;
; A hash symbol marks the rest of the line as a comment.
;
; ```toml
; # This is a full-line comment
; key = "value" # This is a comment at the end of a line
; ```

(define comment
  (preceded-by (is #\#)
               (zero-or-more (none-of* toml-newline item))
               line-end))

; String
; ------
;
; There are four ways to express strings: basic, multi-line basic, literal, and
; multi-line literal. All strings must contain only valid UTF-8 characters.
;
; **Basic strings** are surrounded by quotation marks. Any Unicode character may
; be used except those that must be escaped: quotation mark, backslash, and the
; control characters (U+0000 to U+001F).
;
; ```toml
; str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."
; ```
;
; For convenience, some popular characters have a compact escape sequence.
;
; ```
; \b         - backspace       (U+0008)
; \t         - tab             (U+0009)
; \n         - linefeed        (U+000A)
; \f         - form feed       (U+000C)
; \r         - carriage return (U+000D)
; \"         - quote           (U+0022)
; \\         - backslash       (U+005C)
; \uXXXX     - unicode         (U+XXXX)
; \UXXXXXXXX - unicode         (U+XXXXXXXX)
; ```

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

; Any Unicode character may be escaped with the `\uXXXX` or `\UXXXXXXXX` forms.
; The escape codes must be valid Unicode [scalar values](http://unicode.org/glossary/#unicode_scalar_value).
;
; All other escape sequences not listed above are reserved and, if used, TOML
; should produce an error.

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

(define char-set:graphic-and-blank
  (char-set-union char-set:graphic char-set:blank))

(define char
  (any-of (none-of* (is #\") (is #\\) (in char-set:graphic-and-blank))
          (preceded-by (is #\\) (any-of escape unicode))))

(define basic-string
  (enclosed-by
    (is #\")
    (as-string (one-or-more char))
    (is #\")))

; Sometimes you need to express passages of text (e.g. translation files) or would
; like to break up a very long string into multiple lines. TOML makes this easy.
; **Multi-line basic strings** are surrounded by three quotation marks on each
; side and allow newlines. A newline immediately following the opening delimiter
; will be trimmed. All other whitespace and newline characters remain intact.
;
; ```toml
; str1 = """
; Roses are red
; Violets are blue"""
; ```
;
; TOML parsers should feel free to normalize newline to whatever makes sense for
; their platform.
;
; ```toml
; # On a Unix system, the above multi-line string will most likely be the same as:
; str2 = "Roses are red\nViolets are blue"
;
; # On a Windows system, it will most likely be equivalent to:
; str3 = "Roses are red\r\nViolets are blue"
; ```
;
; For writing long strings without introducing extraneous whitespace, end a line
; with a `\`. The `\` will be trimmed along with all whitespace (including
; newlines) up to the next non-whitespace character or closing delimiter. If the
; first characters after the opening delimiter are a backslash and a newline, then
; they will both be trimmed along with all whitespace and newlines up to the next
; non-whitespace character or closing delimiter. All of the escape sequences that
; are valid for basic strings are also valid for multi-line basic strings.
;
; ```toml
; # The following strings are byte-for-byte equivalent:
; str1 = "The quick brown fox jumps over the lazy dog."
;
; str2 = """
; The quick brown \
;
;
;   fox jumps over \
;     the lazy dog."""
;
; key3 = """\
;        The quick brown \
;        fox jumps over \
;        the lazy dog.\
;        """
; ```

(define escaped-whitespace
  (bind (preceded-by (is #\\) toml-newline (zero-or-more toml-whitespace))
        (lambda (x) (result ""))))

(define multi-line-basic-string
  (enclosed-by
    (sequence (char-seq "\"\"\"") (maybe toml-newline))
    (as-string
      (one-or-more (any-of char (in char-set:whitespace) escaped-whitespace)))
    (char-seq "\"\"\"")))

; Any Unicode character may be used except those that must be escaped: backslash
; and the control characters (U+0000 to U+001F). Quotation marks need not be
; escaped unless their presence would create a premature closing delimiter.
;
; If you're a frequent specifier of Windows paths or regular expressions, then
; having to escape backslashes quickly becomes tedious and error prone. To help,
; TOML supports literal strings where there is no escaping allowed at all.
; **Literal strings** are surrounded by single quotes. Like basic strings, they
; must appear on a single line:
;
; ```toml
; # What you see is what you get.
; winpath  = 'C:\Users\nodejs\templates'
; winpath2 = '\\ServerX\admin$\system32\'
; quoted   = 'Tom "Dubs" Preston-Werner'
; regex    = '<\i\c*\s*>'
; ```

(define literal-string
  (enclosed-by
    (is #\')
    (as-string
      (one-or-more
        (none-of* (is #\') (in char-set:graphic-and-blank))))
    (is #\')))

; Since there is no escaping, there is no way to write a single quote inside a
; literal string enclosed by single quotes. Luckily, TOML supports a multi-line
; version of literal strings that solves this problem. **Multi-line literal
; strings** are surrounded by three single quotes on each side and allow newlines.
; Like literal strings, there is no escaping whatsoever. A newline immediately
; following the opening delimiter will be trimmed. All other content between the
; delimiters is interpreted as-is without modification.
;
; ```toml
; regex2 = '''I [dw]on't need \d{2} apples'''
; lines  = '''
; The first newline is
; trimmed in raw strings.
;    All other whitespace
;    is preserved.
; '''
; ```
;
; For binary data it is recommended that you use Base64 or another suitable ASCII
; or UTF-8 encoding. The handling of that encoding will be application specific.

(define multi-line-literal-string
  (enclosed-by
    (sequence (char-seq "'''") (maybe toml-newline))
    (as-string (one-or-more (none-of* (char-seq "'''") (in char-set:printing))))
    (char-seq "'''")))

; Integer
; -------
;
; Integers are whole numbers. Positive numbers may be prefixed with a plus sign.
; Negative numbers are prefixed with a minus sign.
;
; ```toml
; int1 = +99
; int2 = 42
; int3 = 0
; int4 = -17
; ```
;
; For large numbers, you may use underscores to enhance readability. Each
; underscore must be surrounded by at least one digit.
;
; ```toml
; int5 = 1_000
; int6 = 5_349_221
; int7 = 1_2_3_4_5     # valid but inadvisable
; ```
;
; Leading zeros are not allowed. Hex, octal, and binary forms are not allowed.
; Values such as "infinity" and "not a number" that cannot be expressed as a
; series of digits are not allowed.
;
; 64 bit (signed long) range expected (−9,223,372,036,854,775,808 to
; 9,223,372,036,854,775,807).

(define integer
  (bind
    (as-string
      (sequence (maybe (in '(#\- #\+)))
                (one-or-more (any-of (in char-set:digit) (is #\_)))))
    (lambda (x)
      (result (string->number (string-delete #\_ x))))))

(define key
  (as-symbol (one-or-more (in char-set:graphic))))

(define value
  (any-of basic-string
          multi-line-basic-string
          literal-string
          multi-line-literal-string
          integer))

(define key-value
  (as-pair key
    (enclosed-by (sequence whitespaces (is #\=) whitespaces)
                 value
                 line-end)))

(define blank-line
  (sequence (zero-or-more toml-whitespace) toml-newline))

(define document
  (enclosed-by
    (zero-or-more (any-of comment blank-line)) ;; ignore these
    (zero-or-more (any-of key-value)) ;; match these
    end-of-input)) ;; make sure we matched the whole document

)
