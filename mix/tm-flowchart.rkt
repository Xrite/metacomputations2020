#lang racket
(require "flowchart.rkt")

(provide tm-int tm-int-division tm-example)

(define (get-instruction instructions i)
  (cond
    [(< i 0) '(fail fail)]
    [(>= i (length instructions)) '(stop stop)]
    [else (list-ref instructions i)]))

(define (find-instruction instructions label)
  (define (impl is i)
    (cond
      [(empty? is) -1]
      [(equal? (caar is) label) i]
      [else (impl (cdr is) (+ i 1))]))
  (impl instructions 0))

(define tm-int
  '((read Q Right)
    (init (:= ptr 0)
          (:= Left '())
          (goto loop))
    (loop (:= instr (get-instruction Q ptr))
          (:= cur_op (cadr instr))
          (goto cont))
    (cont (if (equal? 'right cur_op) do-right cont1))
    (cont1 (if (equal? 'left cur_op) do-left cont2))
    (cont2 (if (equal? 'write cur_op) do-write cont3))
    (cont3 (if (equal? 'goto cur_op) do-goto cont4))
    (cont4 (if (equal? 'if cur_op) do-if cont5))
    (cont5 (if (equal? 'stop cur_op) stop error))
    (do-right (:= Left (cons (car Right) Left))
              (:= Right (cdr Right))
              (goto next))
    (do-left (:= Right (cons (car Left) Right))
             (:= Left (cdr Left))
             (goto next))
    (do-write (:= symb (caddr instr))
              (:= Right (cons symb (cdr Right)))
              (goto next))
    (do-goto (:= next-label (caddr instr))
             (goto jump))
    (do-if (:= next-label (fifth instr))
           (:= symb (third instr)) 
           (if (equal? (car Right) symb) jump next))
    (next (:= ptr (+ ptr 1))
          (goto loop))
    (jump (:= ptr (find-instruction Q next-label))
          (goto loop))
    (stop (return Right))
    (error (return (list 'syntaxerror: instr)))))

(define tm-int-division (set 'Q 'ptr 'instr 'cur_op 'symb 'next-label))

(define tm-example
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)))
