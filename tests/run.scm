(load-relative "../toml.scm")
(import scheme)
(import toml)

(use test utils posix medea rfc3339)

(define example-json (read-all "./tests/fixtures/example.json"))
(define example-toml (read-all "./tests/fixtures/example.toml"))

(test-group "comment"
  (test "comment"
        '()
        (read-toml "# this is a comment\n"))
  (test "leading whitespace"
        '()
        (read-toml "# this is a comment\n"))
  (test "after key-value"
        '((key . "val"))
        (read-toml "key = 'val' # comment\n")))

(test-group "blank lines"
  (test '() (read-toml "\r\n\n")))

(test-group "basic strings"
  (test "basic string"
        '((str . "I'm a string"))
        (read-toml "str = \"I'm a string\"\n"))
  (test "basic string no newline"
        '((str . "I'm a string"))
        (read-toml "str = \"I'm a string\""))
  (test "basic string escaped double quote"
        '((str . "escaped \" double quote"))
        (read-toml "str = \"escaped \\\" double quote\"\n"))
  (test "basic string escaped tab character"
        '((str . "escaped \t tab"))
        (read-toml "str = \"escaped \\t tab\"\n"))
  (test "basic string with unicode"
        '((str . "Name: José"))
        (read-toml "str = \"Name: Jos\\u00E9\"\n")))

(test-group "multi-line basic strings"
  (test "multi-line basic string"
        '((str . "Roses are red\nViolets are blue"))
        (read-toml "str = \"\"\"\nRoses are red\nViolets are blue\"\"\"\n"))
  (test "trimmed whitespace"
        '((str . "The quick brown fox jumps over the lazy dog."))
        (read-toml
          (string-append
            "str = \"\"\"\\\n"
            "       The quick brown \\\n"
            "       fox jumps over \\\n"
            "       the lazy dog.\\\n"
            "       \"\"\""))))

(test-group "literal strings"
  (test "literal string"
        '((winpath . "C:\\Users\\nodejs\\templates"))
        (read-toml "winpath = 'C:\\Users\\nodejs\\templates'"))
  (test "literal string with double quotes"
        '((quoted . "Tom \"Dubs\" Preston-Werner"))
        (read-toml "quoted = 'Tom \"Dubs\" Preston-Werner'\n"))
  (test "multi-line regex example"
        '((regex2 . "I [dw]on't need \\d{2} apples"))
        (read-toml "regex2 = '''I [dw]on't need \\d{2} apples'''"))
  (test "multi-line lines example"
        `((lines . ,(string-append
                      "The first newline is\n"
                      "trimmed in raw strings.\n"
                      "   All other whitespace\n"
                      "   is preserved.\n")))
        (read-toml
          (string-append
            "lines = '''\n"
            "The first newline is\n"
            "trimmed in raw strings.\n"
            "   All other whitespace\n"
            "   is preserved.\n"
            "'''\n"))))

(test-group "integers"
  (test "integer"
        '((int . 42)) (read-toml "int = 42"))
  (test "preceding plus sign"
        '((int . 99)) (read-toml "int = +99"))
  (test "zero"
        '((int . 0)) (read-toml "int = 0"))
  (test "negative"
        '((int . -17)) (read-toml "int = -17"))
  (test "underscores"
        '((int . 5349221)) (read-toml "int = 5_349_221")))

(test-group "floats"
  (test "fractional"
        '((flt . 3.1415)) (read-toml "flt = 3.1415"))
  (test "fractional positive"
        '((flt . 1.0)) (read-toml "flt = +1.0"))
  (test "fractional negative"
        '((flt . -0.01)) (read-toml "flt = -0.01"))
  (test "exponent"
        '((flt . 1e6)) (read-toml "flt = 1e6"))
  (test "exponent positive"
        '((flt . 5e+22)) (read-toml "flt = 5e+22"))
  (test "exponent negative"
        '((flt . -2E-2)) (read-toml "flt = -2E-2"))
  (test "both"
        '((flt . 6.626e-34)) (read-toml "flt = 6.626e-34"))
  (test "underscores"
        '((flt . 9224617.445991228313))
        (read-toml "flt = 9_224_617.445_991_228_313"))
  (test "underscores exponent"
        ;; using e100 instead of e1000 since we'd have to import the
        ;; 'numbers' module in the parser but the expected number range for
        ;; TOML is only stated as 64bit signed long
        '((flt . 1e100))
        (read-toml "flt = 1e1_00")))

(test-group "booleans"
  (test "true"
        '((bool . #t)) (read-toml "bool = true"))
  (test "false"
        '((bool . #f)) (read-toml "bool = false")))

(test-group "dates"
  (test "RFC3339 example 1"
        `((date . ,(make-rfc3339 1979 5 27 07 32 00 0 0)))
        (read-toml "date = 1979-05-27T07:32:00Z"))
  (test "RFC3339 example 2"
        `((date . ,(make-rfc3339 1979 5 27 07 32 00 0 (* 7 60 60))))
        (read-toml "date = 1979-05-27T07:32:00-07:00"))
  (test "RFC3339 example 3"
        `((date . ,(make-rfc3339 1979 5 27 07 32 00 0.999999 (* 7 60 60))))
        (read-toml "date = 1979-05-27T07:32:00.999999-07:00")))

(test-group "arrays"
  (test "array of integers"
        '((arr . #(1 2 3)))
        (read-toml "arr = [ 1, 2, 3 ]"))
  (test "array of strings"
        '((arr . #("red" "yellow" "green")))
        (read-toml "arr = [ \"red\", \"yellow\", \"green\" ]"))
  (test "array of arrays"
        '((arr . #(#(1 2) #(3 4 5))))
        (read-toml "arr = [ [ 1, 2 ], [3, 4, 5] ]"))
  (test "multiple types of string definition"
        '((arr . #("all" "strings" "are the same" "type")))
        (read-toml "arr = [ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''']"))
  (test "array of arrays of different types allowed"
        '((arr . #(#(1 2) #("a" "b" "c"))))
        (read-toml "arr = [ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]"))
  (test "array of different types not allowed"
        #f
        (read-toml "arr = [ 1, 2.0 ]"))
  (test "array over multiple lines"
        '((arr . #(1 2 3)))
        (read-toml "arr = [\n  1, 2, 3\n]"))
  (test "array values over multiple lines with trailing comma"
        '((arr . #(1 2)))
        (read-toml "arr = [\n  1,\n  2,\n]"))
  (test "array values over multiple lines with comment"
        '((arr . #(1 2)))
        (read-toml "arr = [\n  1,\n  2 # this is a comment\n]")))

(test-group "merge-table"
  (test "empty sublevel"
        '((table . ()))
        (merge-table '() '(table) '()))
  (test "empty target, nested source"
        '((foo . ((bar . ()))))
        (merge-table '() '(foo bar) '()))
  (test "merge properties at sub level"
        '((one . 1)
          (two . 2)
          (sub1 . ((id . "something") (price . 1000)))
          (sub2 . ((foo . 123)
                   (bar . 456)
                   (sub2sub2 . ((baz . "qux")))
                   (sub2sub1 . ((wibble . "qwer")
                                (wobble . "asdf")
                                (wubble . "zxcv"))))))
        (merge-table
          '((one . 1)
            (two . 2)
            (sub1 . ((id . "something") (price . 1000)))
            (sub2 . ((foo . 123)
                     (bar . 456)
                     (sub2sub2 . ((baz . "qux"))))))
          '(sub2 sub2sub1)
          '((wibble . "qwer")
            (wobble . "asdf")
            (wubble . "zxcv"))))
  (test "key clash at leaf level"
        #f
        (merge-table
          '((foo . ((bar . 123))))
          '(foo)
          '((bar . 456) (baz . 789))))
  (test "key clash at table name level"
        #f
        (merge-table
          '((foo . ((bar . 123))))
          '(foo bar)
          '((baz . 456))))
  (test "key clash at table name level where target has alist"
        #f
        (merge-table
          '((foo . ((bar . ((asfd . 123))))))
          '(foo bar)
          '((baz . 456))))
  )

(test-group "tables"
  (test "empty table"
        '((table . ()))
        (read-toml "[table]\n"))
  (test "table with single key-value"
        '((table . ((key . "value"))))
        (read-toml "[table]\nkey = 'value'"))
  (test "table with multiple key-values"
        '((table . ((foo . 123) (bar . 456))))
        (read-toml "[table]\nfoo = 123\nbar = 456\n"))
  (test "table with quoted keys in key-value pairs"
        '((table . ((|my key| . "my value"))))
        (read-toml "[table]\n\"my key\" = \"my value\"\n"))
  (test "table with blank lines and comments between key-values"
        '((table . ((foo . 123) (bar . 456))))
        (read-toml "[table]\nfoo = 123\n\n# comment\n\n\nbar = 456\n"))
  (test "nested table"
        '((foo . ((bar . ((baz . 123))))))
        (read-toml "[foo.bar]\nbaz = 123\n"))
  (test "quoted table name"
        '((|foo bar| . ((baz . 123))))
        (read-toml "[\"foo bar\"]\nbaz = 123\n"))
  (test "nested and quoted table name"
        '((parent . ((foo.bar . ((baz . 123))))))
        (read-toml "[parent.\"foo.bar\"]\nbaz = 123\n"))
  (test "repeated keys should not parse"
        #f
        (read-toml "foo = 123\nfoo = 456\n"))
  (test "repeated table names should not parse"
        #f
        (read-toml "[table]\nprop = 'val'\n[table]\nprop2 = 'val2'\n"))
  (test "nested tables with siblings on parent"
        '((foo . 123)
          (bar . ((baz . 456)
                  (qux . ((wibble . #t))))))
        (read-toml "foo = 123\n\n[bar]\nbaz = 456\n[bar.qux]\nwibble = true\n"))
  (test "nested tables out of order (see toml-lang/toml#320)"
        '((b . ((bar . 2)))
          (a . ((foo . 1)
                (c . ((baz . 3))))))
        (read-toml
          (string-append
            "[a]\n"
            "foo = 1\n"
            "\n"
            "[b]\n"
            "bar = 2\n"
            "\n"
            "[a.c]\n"
            "baz = 3\n")))
  (test "repeated table names should not parse"
        #f
        (read-toml
          (string-append
            "[a]\n"
            "b = 1\n"
            "\n"
            "[a]\n"
            "c = 2\n")))
  (test "table name conflicting with property should not parse"
        #f
        (read-toml
          (string-append
            "[a]\n"
            "b = 1\n"
            "\n"
            "[a.b]\n"
            "c = 2\n")))
  (test "table name [] should not parse" #f (read-toml "[]"))
  (test "table name [a.] should not parse" #f (read-toml "[a.]"))
  (test "table name [a..b] should not parse" #f (read-toml "[a..b]"))
  (test "table name [.b] should not parse" #f (read-toml "[.b]"))
  (test "table name [.] should not parse" #f (read-toml "[.]"))
  (test "missing key name should not parse" #f (read-toml " = 'no key name'")))

(test-group "inline tables"
  (test "inline table"
        '((point . ((x . 1) (y . 2))))
        (read-toml "point = {x = 1, y = 2}\n"))
  (test "newlines not allowed in inline tables"
        #f
        (read-toml "point = {\n  x = 1,\n  y = 2\n}\n"))
  (test "repeated keys in inline tables should not parse"
        #f
        (read-toml "point = {x = 1, x = 2}\n"))
  (test "inline table as value inside normal table"
        '((a . ((b . 1) (c . ((foo . "bar") (baz . "qux"))))))
        (read-toml "[a]\nb = 1\nc = { foo = 'bar', baz = 'qux' }\n"))
  )

;(test-group "example"
;  (test (read-json example-json)
;        (read-toml example-toml))
;  )

(test-exit)
