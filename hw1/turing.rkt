#lang racket
(define (run program tape)
  (eval-cmd 0 program tape 0))

(define (find-instruction label instructions)
  (define (impl is i)
    (cond
      [(empty? is) -1]
      [(equal? (caar is) label) i]
      [else (impl (cdr is) (+ i 1))]))
  (impl instructions 0))
    
(define (get-or-fail lst i)
  (if (or (< i 0) (>= i (length lst)))
      'fail
      (list-ref lst i)))

(define (eval-cmd instr-ptr program tape tape-ptr)
  (let ([instr (get-or-fail program instr-ptr)])
    (match instr
      [(list _ 'left)
       (eval-cmd (+ instr-ptr 1) program tape (- tape-ptr 1))]
      [(list _ 'right)
       (eval-cmd (+ instr-ptr 1) program tape (+ tape-ptr 1))]
      [(list _ 'write x)
       (eval-cmd (+ instr-ptr 1) program (list-set tape tape-ptr x) tape-ptr)]
      [(list _ 'goto x)
       (eval-cmd (find-instruction x program) program tape tape-ptr)]
      [(list _ 'if x 'goto y)
       (if (equal? x (list-ref tape tape-ptr))
           (eval-cmd (find-instruction y program) program tape tape-ptr)
           (eval-cmd (+ instr-ptr 1) program tape tape-ptr))]
      [_ tape])))