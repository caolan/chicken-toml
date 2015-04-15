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

(module toml (read-toml merge-table)

(import scheme chicken)
(use comparse srfi-1 srfi-13 srfi-14 rfc3339)

;; Some convenience functions for our implementation:

;; returns parser result as a symbol
(define ((as-symbol parser) input)
  (and-let* ((result+remainder ((as-string parser) input)))
    (cons (string->symbol (car result+remainder))
          (cdr result+remainder))))

;; returns two sequential parser results as a pair
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
  (preceded-by (zero-or-more toml-whitespace)
               (is #\#)
               (zero-or-more (none-of* toml-newline item))))

;; Some utility parsers

(define whitespaces
  (one-or-more toml-whitespace))

(define line-end
  (sequence
    (maybe whitespaces) ;; allow trailing whitespace
    (maybe comment) ;; allow comment at end of lines
    (any-of toml-newline end-of-input))) ;; allow missing newline at EOF

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
      (let ((n (string->number (string-delete #\_ x))))
        (if n (result n) fail)))))

; Float
; -----
;
; A float consists of an integer part (which may be prefixed with a plus or minus
; sign) followed by a fractional part and/or an exponent part. If both a
; fractional part and exponent part are present, the fractional part must precede
; the exponent part.
;
; ```toml
; # fractional
; flt1 = +1.0
; flt2 = 3.1415
; flt3 = -0.01
;
; # exponent
; flt4 = 5e+22
; flt5 = 1e6
; flt6 = -2E-2
;
; # both
; flt7 = 6.626e-34
; ```
; A fractional part is a decimal point followed by one or more digits.
;
; An exponent part is an E (upper or lower case) followed by an integer part
; (which may be prefixed with a plus or minus sign).
;
; Similar to integers, you may use underscores to enhance readability. Each
; underscore must be surrounded by at least one digit.
;
; ```toml
; flt8 = 9_224_617.445_991_228_313
; flt9 = 1e1_000
; ```
;
; 64-bit (double) precision expected.

(define fractional
  (sequence (is #\.)
            (one-or-more (any-of (in char-set:digit) (is #\_)))))

(define exponent
  (sequence (any-of (is #\e) (is #\E))
            (maybe (in '(#\- #\+)))
            (one-or-more (any-of (in char-set:digit) (is #\_)))))

(define float
  (bind
    (as-string
      (sequence (maybe (in '(#\- #\+)))
                (one-or-more (any-of (in char-set:digit) (is #\_)))
                (any-of (sequence (maybe fractional) exponent)
                        fractional)))
    (lambda (x)
      (let ((n (string->number (string-delete #\_ x))))
        (if n (result n) fail)))))

; Boolean
; -------
;
; Booleans are just the tokens you're used to. Always lowercase.
;
; ```toml
; bool1 = true
; bool2 = false
; ```

(define boolean
  (bind (any-of (char-seq "true") (char-seq "false"))
        (lambda (x) (result (string=? x "true")))))

; Datetime
; --------
;
; Datetimes are [RFC 3339](http://tools.ietf.org/html/rfc3339) dates.
;
; ```toml
; date1 = 1979-05-27T07:32:00Z
; date2 = 1979-05-27T00:32:00-07:00
; date3 = 1979-05-27T00:32:00.999999-07:00
; ```

(define date
  (bind
    (as-string
      (sequence
        (repeated (in char-set:digit) 4) (is #\-)
        (repeated (in char-set:digit) 2) (is #\-)
        (repeated (in char-set:digit) 2) (is #\T)
        (repeated (in char-set:digit) 2) (is #\:)
        (repeated (in char-set:digit) 2) (is #\:)
        (repeated (in char-set:digit) 2)
        (any-of
          (is #\Z)
          (sequence
            (maybe (sequence (is #\.) (one-or-more (in char-set:digit))))
            (any-of (is #\+) (is #\-))
            (repeated (in char-set:digit) 2) (is #\:)
            (repeated (in char-set:digit) 2)))))
    (lambda (x)
      (let ((t (string->rfc3339 x)))
        (if x (result t) fail)))))

; Array
; -----
;
; Arrays are square brackets with other primitives inside. Whitespace is ignored.
; Elements are separated by commas. Data types may not be mixed (though all string
; types should be considered the same type).
;
; ```toml
; arr1 = [ 1, 2, 3 ]
; arr2 = [ "red", "yellow", "green" ]
; arr3 = [ [ 1, 2 ], [3, 4, 5] ]
; arr4 = [ "all", 'strings', """are the same""", '''type'''] # this is ok
; arr5 = [ [ 1, 2 ], ["a", "b", "c"] ] # this is ok
; arr6 = [ 1, 2.0 ] # note: this is NOT ok
; ```
;
; Arrays can also be multiline. So in addition to ignoring whitespace, arrays also
; ignore newlines between the brackets. Terminating commas are ok before the
; closing bracket.
;
; ```toml
; arr7 = [
;   1, 2, 3
; ]
;
; arr8 = [
;   1,
;   2, # this is ok
; ]
; ```

(define (toml-type v)
  (cond
    ((string? v) 'string)
    ((number? v) (if (and (exact? v) (integer? v)) 'integer 'float))
    ((boolean? v) 'boolean)
    ((vector? v) 'array)
    ((list? v) 'table)))

(define (same-types? lst)
  (or (null? lst)
      (let ((type (toml-type (car lst))))
        (every (lambda (v) (eq? type (toml-type v)))
               (cdr lst)))))

(define array-ignore
  (zero-or-more (any-of comment (in char-set:whitespace))))

(define array
  (recursive-parser
    (bind
      (enclosed-by
        ;; opening
        (sequence (is #\[) array-ignore)
        ;; values
        (zero-or-more
          (sequence
            value ;; first value
            (zero-or-more
              (preceded-by array-ignore
                           (is #\,)
                           array-ignore
                           ;; subsequent values
                           value))))
        ;; closing
        (sequence array-ignore
                  ;; trailing comma
                  (maybe (sequence (is #\,) array-ignore))
                  (is #\])))
      (lambda (x)
        (let ((arr (cons (caar x) (cadar x))))
          (if (same-types? arr)
            (result (list->vector arr))
            fail))))))

; Table
; -----
;
; Tables (also known as hash tables or dictionaries) are collections of key/value
; pairs. They appear in square brackets on a line by themselves. You can tell them
; apart from arrays because arrays are only ever values.
;
; ```toml
; [table]
; ```
;
; Under that, and until the next table or EOF are the key/values of that table.
; Keys are on the left of the equals sign and values are on the right. Whitespace
; is ignored around key names and values. The key, equals sign, and value must
; be on the same line (though some values can be broken over multiple lines).
;
; Keys may be either bare or quoted. **Bare keys** may only contain letters,
; numbers, underscores, and dashes (`A-Za-z0-9_-`). **Quoted keys** follow the
; exact same rules as basic strings and allow you to use a much broader set of key
; names. Best practice is to use bare keys except when absolutely necessary.
;
; Key/value pairs within tables are not guaranteed to be in any specific order.
;
; ```toml
; [table]
; key = "value"
; bare_key = "value"
; bare-key = "value"
;
; "127.0.0.1" = "value"
; "character encoding" = "value"
; "ʎǝʞ" = "value"
; ```
;
; Dots are prohibited in bare keys because dots are used to signify nested tables!
; Naming rules for each dot separated part are the same as for keys (see above).
;
; ```toml
; [dog."tater.man"]
; type = "pug"
; ```
;
; In JSON land, that would give you the following structure:
;
; ```json
; { "dog": { "tater.man": { "type": "pug" } } }
; ```
;
; Whitespace around dot-separated parts is ignored, however, best practice is to
; not use any extraneous whitespace.
;
; ```toml
; [a.b.c]          # this is best practice
; [ d.e.f ]        # same as [d.e.f]
; [ g .  h  . i ]  # same as [g.h.i]
; [ j . "ʞ" . l ]  # same as [j."ʞ".l]
; ```
;
; You don't need to specify all the super-tables if you don't want to. TOML knows
; how to do it for you.
;
; ```toml
; # [x] you
; # [x.y] don't
; # [x.y.z] need these
; [x.y.z.w] # for this to work
; ```
;
; Empty tables are allowed and simply have no key/value pairs within them.
;
; As long as a super-table hasn't been directly defined and hasn't defined a
; specific key, you may still write to it.
;
; ```toml
; [a.b]
; c = 1
;
; [a]
; d = 2
; ```
;
; You cannot define any key or table more than once. Doing so is invalid.
;
; ```toml
; # DO NOT DO THIS
;
; [a]
; b = 1
;
; [a]
; c = 2
; ```
;
; ```toml
; # DO NOT DO THIS EITHER
;
; [a]
; b = 1
;
; [a.b]
; c = 2
; ```
;
; All table names and keys must be non-empty.
;
; ```toml
; # NOT VALID TOML
; []
; [a.]
; [a..b]
; [.b]
; [.]
;  = "no key name" # not allowed
; ```

(define bare-key
  (as-symbol
    (one-or-more (any-of (in char-set:letter+digit)
                         (is #\_)
                         (is #\-)))))

(define quoted-key
  (as-symbol basic-string))

(define key
  (any-of bare-key quoted-key))

(define value
  (any-of basic-string
          multi-line-basic-string
          literal-string
          multi-line-literal-string
          float
          date
          integer
          boolean
          array))

(define key-value
  (as-pair
    key
    (enclosed-by (sequence whitespaces (is #\=) whitespaces)
                 value
                 line-end)))

(define blank-line
  (sequence (zero-or-more toml-whitespace) toml-newline))

(define table-name
  (bind
    (sequence key (zero-or-more (preceded-by (is #\.) key)))
    (lambda (x)
      (result (cons (car x) (cadr x))))))

(define table-property
  (preceded-by
    (zero-or-more (any-of comment blank-line)) ;; ignore these
    key-value))

(define (table-properties input)
  ;; TODO: re-implement this pattern (based on zero-or-more) as
  ;; a general fold-parser??
  (let loop ((result '())
             (input input))
    (let ((value (table-property input)))
      (if value
          (if (assoc (caar value) result)
            ;; key already exists in property list
            (fail input)
            ;; key does not already exist
            (loop (cons (car value) result)
                  (cdr value)))
          (cons (reverse! result)
                input)))))

;; returns new alist if successful, #f if the key already exists
(define (alist-append-uniq key value alist)
  (and (not (assoc key alist))
       (append alist (list (cons key value)))))

;; removes all instances of key in alist, then appends a new pair
;; for given key/value
(define (alist-replace key value alist)
  (append (alist-delete key alist)
          (list (cons key value))))

(define (merge-table parent name properties)
  (if (null? name)
    ;; at correct level to insert properties
    (and (not parent) properties)
    ;; keep descending through document
    (let ((existing (and parent (assoc (car name) parent))))
      (if existing
        (if (list? (cdr existing))
          ;; replace path with new properties
          (let ((sub (merge-table (cdr existing) (cdr name) properties)))
            (if sub (alist-replace (car name) sub parent) #f))
          ;; conflict at table name level
          #f)
        ;; path doesn't exist yet
        (let ((sub (merge-table #f (cdr name) properties)))
          (if sub (append (or parent '())
                          (list (cons (car name) sub))) #f))))))

(define table
  (bind
    (sequence
      (enclosed-by (is #\[) table-name (sequence (is #\]) line-end))
      (maybe table-properties))
    (lambda (x)
      (result (cons (car x) (cadr x))))))

(define ((tables doc) input)
  (let loop ((result doc)
             (input input))
    (and result ;; if result is #f due to conflict, return immediately
      (let ((value (table input)))
        (if value
            (let ((sub (merge-table result (caar value) (cdar value))))
              (loop sub (cdr value)))
            (cons result input))))))

;; putting it all together

(define document
  (bind table-properties ;; get top-level key value pairs
        (lambda (doc)
          (followed-by
            (tables doc) ;; merge tables with top-level props
            (sequence
              (zero-or-more (any-of comment blank-line)) ;; ignore these
              end-of-input))))) ;; make sure we matched the whole document

(define (read-toml input)
  (parse document (->parser-input input)))

)
