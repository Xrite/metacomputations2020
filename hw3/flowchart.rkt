#lang racket

(require rackunit)
(require rackunit/text-ui)
(require (only-in "aux-functions.rkt" aux-namespace))

(provide int)

(define (validate-read read)
  (match read
    [(cons 'read _) 'ok]
    [_ (error "malformed read" read)]))

; interpret a FlowChart program using namespace
(define (int program data #:namespace [namespace aux-namespace])
    (let ([read (car program)]
          [blocks (cdr program)])
      (validate-read read)
      (define st_0 (set-instr-ptr (read-variables (cdr read) data) 0))
      (define jmp-table (map-labels blocks))
      (define (run-block state)
        (let ([block (get-instr-ptr state)])
          (cond
            [(equal? block -1) (get-return state)]
            [else (run-block (eval-block (list-ref blocks block) jmp-table state #:namespace namespace))])))
      (run-block st_0)))
    
(define empty-state (hash))

(define (set-variable state var val) (hash-set state var val))

(define (get-variable state var) (hash-ref state var))

(define (set-instr-ptr state val) (hash-set state "instr" val))

(define (get-instr-ptr state) (hash-ref state "instr"))

(define (set-return state val) (hash-set (hash-set state "return" val) "instr" -1))

(define (get-return state) (hash-ref state "return"))

(define (is-variable? state var) (hash-has-key? state var)) 

(define (read-variables names data) (make-immutable-hash (map cons names data)))

(define (map-labels blocks)
  (define (iter blocks index acc)
    (cond
      [(empty? blocks) acc]
      [else (iter (cdr blocks) (+ index 1) (hash-set acc (caar blocks) index))]))
  (iter blocks 0 (hash)))

(define (eval-block block jmp-table state #:namespace [namespace (make-base-namespace)])
  (define (iter cmds state)
    (match cmds
      [`(,jump) (eval-jump jump state jmp-table #:namespace namespace)]
      ['() (error "empty block" block)]
      [(cons ass rest) (iter rest (eval-assignment ass state #:namespace namespace))]
      [_ (error "malformed block" block)]))
  (iter (cdr block) state))

(define (eval-assignment assignment state #:namespace [namespace (make-base-namespace)])
  (match assignment
    [`(:= ,x ,expr) (set-variable state x (eval-expression state expr #:namespace namespace))]
    [_ (error "malformed assignment" assignment)]))

(define (eval-jump jump state jmp-table #:namespace [namespace (make-base-namespace)])
  (match jump
    [`(goto ,label) (eval-goto label state jmp-table #:namespace namespace)]
    [`(if ,expr ,true-label ,false-label) (eval-if expr true-label false-label state jmp-table #:namespace namespace)]
    [`(return ,expr) (eval-return expr state #:namespace namespace)]
    [_ (error "malformed jump" jump)]))

(define (eval-goto label state jmp-table #:namespace [namespace (make-base-namespace)])
  (cond
    [(hash-has-key? jmp-table label) (set-instr-ptr state (hash-ref jmp-table label))]
    [else (error "no such label" label)]))

(define (eval-if if-expr true-label false-label state jmp-table #:namespace [namespace (make-base-namespace)])
  (if (eval-expression state if-expr #:namespace namespace)
      (eval-goto true-label state jmp-table #:namespace namespace)
      (eval-goto false-label state jmp-table #:namespace namespace)))

(define (eval-return return-expr state #:namespace [namespace (make-base-namespace)])
  (set-return state (eval-expression state return-expr #:namespace namespace)))

(define (eval-expression state expr #:namespace [namespace (make-base-namespace)])
  (cond
    [(symbol? expr) (get-variable state expr)]
    [(and (cons? expr) (equal? 'quote (car expr))) (cadr expr)]
    [(cons? expr)
     (let ([tail (map (lambda (e) (eval-expression state e #:namespace namespace)) (cdr expr))]
           [head (car expr)])
       ;(display tail)
       (apply (eval head namespace) tail))]
    [else expr]))




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
    (define st (read-variables '(a b gg wp nested) '(0 (0 0 a) "gg" #f ((1 2) ((3 4) 5)))))
    (check-equal? (eval-expression st 'a) 0)
    (check-equal? (eval-expression st 'b) '(0 0 a))
    (check-equal? (eval-expression st 'gg) "gg")
    (check-equal? (eval-expression st 'wp) #f)
    (check-equal? (eval-expression st 'nested) '((1 2) ((3 4) 5))))
   (test-case
    "arithmetics"
    (define st (read-variables '(a b c d) '(0 1 2 3)))
    (check-equal? (eval-expression st '(+ a b)) 1)
    (check-equal? (eval-expression st '(+ a b c d)) 6)
    (check-equal? (eval-expression st '(+ a (+ b (+ c d)))) 6)
    (check-equal? (eval-expression st '(+ (+ a b) (+ c d) 1)) 7))
   (test-case
    "racket functions"
    (define st (read-variables '(a b c d) '(0 1 2 "ggwp")))
    (check-equal? (eval-expression st '(string-length "ggwp")) 4)
    (check-equal? (eval-expression st '(string-length d)) 4)
    (check-equal? (eval-expression st '(length '(0 1))) 2)
    (check-equal? (eval-expression st '(length (quote (0 1)))) 2)
    (check-equal? (eval-expression st '(length '(a b))) 2)
    (check-equal? (eval-expression st '(+ (string-length "ggwp") (length '(a b)) (length (list 0 1)))) 8))
   (test-case
    "lambdas"
    (define f (lambda (x y) (+ x y)))
    (define st (read-variables '(a b plus) `(3 1 ,f)))
    (check-equal? (eval-expression st '(apply plus (list a b))) 4))))

(run-tests eval-expression-tests)

(define test_state (read-variables '(a b c dd e28 F GGWP228) '(1 2 3 (0 0 0) "e28" F #f)))

(check-equal? (eval-expression test_state '(+ a b)) 3)

(check-equal? (eval-expression test_state '(length dd)) 3)

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

