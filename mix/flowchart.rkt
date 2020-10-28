#lang racket

(require rackunit)
(require rackunit/text-ui)
(require (only-in "aux-functions.rkt" aux-namespace))

(provide (combine-out int (prefix-out fc: (all-defined-out))))
 
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
     (let (;[xd (displayln expr)]
           [tail (map (lambda (e) (eval-expression state e #:namespace namespace)) (cdr expr))]
           [head (car expr)])
       ;(display tail)
       (apply (eval head namespace) tail))]
    [else expr]))




