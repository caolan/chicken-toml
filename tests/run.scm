(load-relative "../toml.scm")
(import scheme)
(import toml)

(use test utils posix medea)

(define example-json (read-all "./tests/fixtures/example.json"))
(define example-toml (read-all "./tests/fixtures/example.toml"))

(test-group "comment"
  (test '() (read-toml "# this is a comment\n")))

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
            "'''\n")))
  )

;(test-group "example"
;  (test (read-json example-json)
;        (read-toml example-toml))
;  )

(test-exit)
