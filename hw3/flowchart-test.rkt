#lang racket

(require rackunit "flowchart.rkt")

(test-begin
 (let ([state (read-variables '(a b c dd e28 F GGWP228) '(1 2 3 (0 0 0) "e28" F #f))])
   (check = (eval-expression state '(+ a b)) 3)))