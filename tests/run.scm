(load-relative "../toml.scm")
(import scheme)
(import toml)

(use test utils posix medea rfc3339 alist-lib)

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
        '((str . "中国"))
        (read-toml "str = \"中国\"\n")))

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
            "       \"\"\"")))
  (test "multi-line basic string with unicode"
        '((str . "中国"))
        (read-toml "str = \"\"\"\n中国\\\n\"\"\"\n")))

(test-group "literal strings"
  (test "literal string"
        '((winpath . "C:\\Users\\nodejs\\templates"))
        (read-toml "winpath = 'C:\\Users\\nodejs\\templates'"))
  (test "literal string with unicode"
        '((str . "中国"))
        (read-toml "str = '中国'\n"))
  (test "literal string with double quotes"
        '((quoted . "Tom \"Dubs\" Preston-Werner"))
        (read-toml "quoted = 'Tom \"Dubs\" Preston-Werner'\n")))

(test-group "multi-line literal strings"
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
            "'''\n")))
  (test "multi-line literal string with unicode"
        '((str . "中国"))
        (read-toml "str = '''\n中国'''\n")))

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

(test-group "insert-normal-table"
  (test "empty sublevel"
        '((table . ()))
        (insert-normal-table '() '(table) '()))
  (test "empty target, nested source"
        '((foo . ((bar . ()))))
        (insert-normal-table '() '(foo bar) '()))
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
        (insert-normal-table
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
        (insert-normal-table
          '((foo . ((bar . 123))))
          '(foo)
          '((bar . 456) (baz . 789))))
  (test "key clash at table name level"
        #f
        (insert-normal-table
          '((foo . ((bar . 123))))
          '(foo bar)
          '((baz . 456))))
  (test "key clash at table name level where target has alist"
        #f
        (insert-normal-table
          '((foo . ((bar . ((asfd . 123))))))
          '(foo bar)
          '((baz . 456))))
  (test "insert normal table into array table"
        '((foo . #(((bar . 123) (baz . ((qux . 456)))))))
        (insert-normal-table
          '((foo . #(((bar . 123)))))
          '(foo baz)
          '((qux . 456))))
  (test "insert normal table into empty array table"
        '((foo . #(((baz . ((qux . 456)))))))
        (insert-normal-table
          '((foo . #(())))
          '(foo baz)
          '((qux . 456)))))

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
        (read-toml "[a]\nb = 1\nc = { foo = 'bar', baz = 'qux' }\n")))

(test-group "insert-array-table"
   (test "empty array table"
         '((a . #(())))
         (insert-array-table '() '(a) '()))
   (test "empty array table with property"
         '((a . #(((foo . "bar")))))
         (insert-array-table '() '(a) '((foo . "bar"))))
   (test "insert with existing array table"
         '((a . #(((foo . "bar"))
                  ((foo . "baz")))))
         (insert-array-table '((a . #(((foo . "bar")))))
                             '(a)
                             '((foo . "baz"))))
   (test "insert new nested array table"
         '((a . ((b . #(((c . 123)))))))
         (insert-array-table '() '(a b) '((c . 123))))
   (test "insert existing nested array table with siblings"
         '((a . ((d . #t) (b . #(((c . 456)) ((c . 123)))))))
         (insert-array-table '((a . ((b . #(((c . 456)))) (d . #t))))
                             '(a b)
                             '((c . 123)))))

(test-group "array of tables"
  (test "array table"
        '((a . #(((b . 1)) ((b . 2)))))
        (read-toml "[[a]]\nb = 1\n\n[[a]]\nb = 2\n"))
  (test "empty array table"
        '((a . #(())))
        (read-toml "[[a]]\n"))
  (test "example products array table"
        '((products . #(((name . "Hammer") (sku . 738594937))
                        ()
                        ((name . "Nail") (sku . 284758393) (color . "gray")))))
        (read-toml
          (string-append
            "[[products]]\n"
            "name = \"Hammer\"\n"
            "sku = 738594937\n"
            "\n"
            "[[products]]\n"
            "\n"
            "[[products]]\n"
            "name = \"Nail\"\n"
            "sku = 284758393\n"
            "color = \"gray\"\n")))
  (test "nested arrays of tables"
        '((fruit . #(((name . "apple")
                      (physical . ((color . "red") (shape . "round")))
                      (variety . #(((name . "red delicious"))
                                   ((name . "granny smith")))))
                     ((name . "banana")
                      (variety . #(((name . "plantain"))))))))
        ;; TODO:
        ;; You can create nested arrays of tables as well. Just use the same double bracket syntax on sub-tables. Each double-bracketed sub-table will belong to the most recently defined table element above it.
        ;; SO:
        ;; update insert functions to use last defined array entry when
        ;; descending through paths
        (read-toml
          (string-append
            "[[fruit]]\n"
            "  name = \"apple\"\n"
            "\n"
            "  [fruit.physical]\n"
            "    color = \"red\"\n"
            "    shape = \"round\"\n"
            "\n"
            "  [[fruit.variety]]\n"
            "    name = \"red delicious\"\n"
            "\n"
            "  [[fruit.variety]]\n"
            "    name = \"granny smith\"\n"
            "\n"
            "[[fruit]]\n"
            "  name = \"banana\"\n"
            "\n"
            "  [[fruit.variety]]\n"
            "    name = \"plantain\"\n")))
  (test "normal table with the same name as array table should not parse"
        #f
        (read-toml
          (string-append
            "[[fruit]]\n"
            "  name = \"apple\"\n"
            "\n"
            "  [[fruit.variety]]\n"
            "    name = \"red delicious\"\n"
            "\n"
            "  # This table conflicts with the previous table\n"
            "  [fruit.variety]\n"
            "    name = \"granny smith\"\n")))
  (test "can also use array of inline tables"
        '((points . #(((x . 1) (y . 2) (z . 3))
                      ((x . 7) (y . 8) (z . 9))
                      ((x . 2) (y . 4) (z . 8)))))
        (read-toml
          (string-append
            "points = [ { x = 1, y = 2, z = 3 },\n"
            "           { x = 7, y = 8, z = 9 },\n"
            "           { x = 2, y = 4, z = 8 } ]\n"))))

(test-group "examples"
  (begin
    (define example-json (read-all "./tests/fixtures/example.json"))
    (define example-toml (read-all "./tests/fixtures/example.toml"))
    (let* ((expected (read-json example-json))
           (owner (assoc 'owner expected))
           (dob (assoc 'dob (cdr owner))))
      ;; convert JSON date string to RFC3339 date record for comparison
      (set-cdr! dob (string->rfc3339 (cdr dob)))
      (test "example" expected (read-toml example-toml))))
  (begin
    (define hard-example-json (read-all "./tests/fixtures/hard_example.json"))
    (define hard-example-toml (read-all "./tests/fixtures/hard_example.toml"))
    (test "hard_example"
          (read-json hard-example-json)
          (read-toml hard-example-toml))))

(test-exit)
