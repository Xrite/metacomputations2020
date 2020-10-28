#lang racket
(require "mix.rkt" "flowchart.rkt" "tm-flowchart.rkt" rackunit)

(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))


(test-equal? "Check tm-int specialization using mix_racket"
             (int (mix_racket tm-int (set 'Q 'ptr 'instr 'cur_op 'symb 'next-label) (hash 'Q tm-example 'ptr 0 'symb '())) '((1 1 1 0 1 0 1)))
             '(1 1 0 1))

(test-equal? "Check tm-int specialization using mix_flowchart"
             (let* ([division (set 'Q 'ptr 'instr 'cur_op 'symb 'next-label)]
                    [vs0 (hash 'Q tm-example 'ptr 0)]
                    [specialized (int mix_flowchart `(,tm-int ,division ,vs0))])
               (int specialized '((1 1 1 0 1 0 1))))
             '(1 1 0 1))

(test-equal? "Check find_name specialization using mix_racket"
             (let* ([division (set 'name 'namelist)]
                    [vs0 (hash 'name 'z 'namelist '(x y z))]
                    [specialized (mix_racket find_name division vs0)])
               (int specialized '((1 2 3))))
             3)

(test-equal? "Check find_name specialization using mix_flowchart"
             (let* ([division (set 'name 'namelist)]
                    [vs0 (hash 'name 'z 'namelist '(x y z))]
                    [specialized (int mix_flowchart `(,find_name ,division ,vs0))])
               (int specialized '((1 2 3))))
             3)
               