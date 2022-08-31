#lang info
(define test-include-paths (list "tests.rkt"))

(define test-omit-paths (list #rx".*(?<!tests)\\.rkt"))