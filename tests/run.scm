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

(test-group "strings"
  (test '((str . "I'm a string"))
        (read-toml "str = \"I'm a string\"\n"))
  ;(test "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."
  ;      (read-toml "\"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.\""))
  )

;(test-group "example"
;  (test (read-json example-json)
;        (read-toml example-toml))
;  )

(test-exit)
