#lang racket
(require "flowchart.rkt" "tm-flowchart.rkt" rackunit)

(test-equal? "Check tm-example" (int tm-int `(,tm-example (1 1 1 0 1 0 1))) '(1 1 0 1))
