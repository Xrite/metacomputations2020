#lang racket

(require "flowchart.rkt" rackunit rackunit/text-ui)

;####################################################################
; TESTS
;####################################################################


(define eval-expression-tests
  (test-suite
   "Tests of eval-expression"
   ;(test-case 
   ; "empty expression"
   ; (check-equal? (eval-expression (empty-state) '()) '())
   (test-case
    "single variable"
    (define st (fc:read-variables '(a b gg wp nested) '(0 (0 0 a) "gg" #f ((1 2) ((3 4) 5)))))
    (check-equal? (fc:eval-expression st 'a) 0)
    (check-equal? (fc:eval-expression st 'b) '(0 0 a))
    (check-equal? (fc:eval-expression st 'gg) "gg")
    (check-equal? (fc:eval-expression st 'wp) #f)
    (check-equal? (fc:eval-expression st 'nested) '((1 2) ((3 4) 5))))
   (test-case
    "arithmetics"
    (define st (fc:read-variables '(a b c d) '(0 1 2 3)))
    (check-equal? (fc:eval-expression st '(+ a b)) 1)
    (check-equal? (fc:eval-expression st '(+ a b c d)) 6)
    (check-equal? (fc:eval-expression st '(+ a (+ b (+ c d)))) 6)
    (check-equal? (fc:eval-expression st '(+ (+ a b) (+ c d) 1)) 7))
   (test-case
    "racket functions"
    (define st (fc:read-variables '(a b c d) '(0 1 2 "ggwp")))
    (check-equal? (fc:eval-expression st '(string-length "ggwp")) 4)
    (check-equal? (fc:eval-expression st '(string-length d)) 4)
    (check-equal? (fc:eval-expression st '(length '(0 1))) 2)
    (check-equal? (fc:eval-expression st '(length (quote (0 1)))) 2)
    (check-equal? (fc:eval-expression st '(length '(a b))) 2)
    (check-equal? (fc:eval-expression st '(+ (string-length "ggwp") (length '(a b)) (length (list 0 1)))) 8))
   (test-case
    "lambdas"
    (define f (lambda (x y) (+ x y)))
    (define st (fc:read-variables '(a b plus) `(3 1 ,f)))
    (check-equal? (fc:eval-expression st '(apply plus (list a b))) 4))))

(run-tests eval-expression-tests)

(define test_state (fc:read-variables '(a b c dd e28 F GGWP228) '(1 2 3 (0 0 0) "e28" F #f)))

(check-equal? (fc:eval-expression test_state '(+ a b)) 3)

(check-equal? (fc:eval-expression test_state '(length dd)) 3)

(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))

(test-equal? "find_name TM test 1" (int find_name '(x (x y z) (1 2 3))) 1)

(test-equal? "find_name TM test 2" (int find_name '(y (x y z) (1 2 3))) 2)

(test-equal? "find_name TM test 3" (int find_name '(z (x y z) (1 2 3))) 3)

