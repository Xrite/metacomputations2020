#lang racket

(require "tm-flowchart.rkt")

(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))

(define (initial-point program)
  (first (second program)))

(define (lookup label program)
  (define (impl is)
    (cond
      [(empty? is) (error label "no block with such label")]
      [(equal? (caar is) label) (cdr (car is))]
      [else (impl (cdr is))]))
  (impl (cdr program)))

(define (initial-code pp vs) `(,(list pp vs)))

(define (static-var? division x) (set-member? division x))

(define (static-exp? division exp)
  (cond
    [(symbol? exp) (static-var? division exp)]
    [(and (cons? exp) (equal? 'quote (car exp))) #t]
    [(cons? exp)
     (let ([tail (andmap (lambda (e) (static-exp? division e)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       tail)]
    [else #t]))

(define (set-var vs var val) (dict-set vs var val))

(define (extend s e)
  (when (> (random) 0.9)
      (displayln (length s)))
  (reverse (cons e (reverse s))))

(define (reduce exp vs) exp)

(define (eval-exp exp vs)
  (cond
    [(symbol? exp) (dict-ref vs exp (lambda () (error exp "no such static variable")))]
    [(and (cons? exp) (equal? 'quote (car exp))) (cadr exp)]
    [(cons? exp)
     (let ([tail (map (lambda (e) (eval-exp e vs)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (apply (eval head tm-int-namespace) tail))]
    [else exp]))


(define (mix_racket program division vs0)
  (define pp0 (initial-point program))
  (define pending (set (list pp0 vs0)))
  (define marked (set))
  (define residual (list))
  (let loop_pending ()
    (unless (set-empty? pending)
      (define pp (first (set-first pending)))
      (define vs (second (set-first pending)))
      (displayln (list pp vs))
      (set! pending (set-rest pending))
      (set! marked (set-add marked (list pp vs)))
      (define bb (lookup pp program))
      (define code (initial-code pp vs))
      (let loop_bb ()
        (unless (empty? bb)
          (define cmd (first bb))
          (set! bb (rest bb))
          (match cmd
            [`(:= ,x ,exp)
             (cond
               [(static-var? division x) (set! vs (set-var vs x (eval-exp exp vs)))]
               [else (set! code (extend code `(:= ,x ,(reduce exp vs))))])]
            [`(goto ,next_pp) (set! bb (lookup next_pp program))]
            [`(if ,exp ,pp_true ,pp_false)
             (cond
               [(static-exp? division exp)
                (cond
                  [(eval-exp exp vs) (set! bb (lookup pp_true program))]
                  [else (set! bb (lookup pp_false program))])]
               [else
                (begin
                  (define true_label (list pp_true vs))
                  (define false_label (list pp_false vs))
                  (set! pending (set-union pending (set-subtract (set true_label false_label) marked)))
                  (set! code (extend code `(if ,(reduce exp vs) ,true_label ,false_label))))])]
            [`(return ,exp) (set! code (extend code `(return ,(reduce exp vs))))]
            [else (error cmd "wrong instruction")])
          (loop_bb)))
      (set! residual (extend residual code))
      (loop_pending)))
  residual)

(define (relabel program)
  (define new-label (make-hash))
  (define r
    (for/list ([bb program])
      (define label (car bb))
      (unless (dict-has-key? new-label label)
        (dict-set! new-label label (dict-count new-label)))
      (cons (dict-ref new-label label) (cdr bb))))
  (for/list ([bb r])
    (for/list ([instr bb])
      (match instr
        [`(if ,expr ,l ,r) `(if ,expr ,(dict-ref new-label l) ,(dict-ref new-label r))]
        [`(goto ,l) `(goto ,(dict-ref new-label l))]
        [x x]))))

(define mix_flowchart
  '((read program division vs0)
    (init (:= pending (set (list (initial-point program) vs0)))
          (:= marked (set))
          (goto pending_while_cond))
    (pending_while_cond (if (set-empty? pending) pending_while_end pending_while_start))
    (pending_while_start (:= pp (first (set-first pending)))
                         (:= vs (second (set-first pending)))
                         (:= pending (set-rest pending))
                         (:= marked (set-add marked (list pp vs)))
                         (:= bb (lookup pp program))
                         (:= code (initial-code pp vs))
                         (goto bb_while_cond))
    (bb_while_cond (if (empty? bb) bb_while_end bb_while_start))
    (bb_while_start (:= cmd (first bb))
                    (:= bb (rest bb))
                    (:= op (car cmd))
                    (goto match_op))
    (match_op (if (equal? ':= op) process_ass match_op1))
    (match_op1 (if (equal? 'goto op) process_goto match_op2))
    (match_op2 (if (equal? 'if op) process_if match_op3))
    (match_op3 (if (equal? 'return op) process_return error))
    (process_ass (:= x (second cmd))
                 (:= exp (third cmd))
                 (if (static-var? division x) static_ass dynamic_ass))
    (static_ass (:= vs (set-var vs x (eval-exp exp vs)))
                (goto bb_while_cond))
    (dynamic_ass (:= code (extend code (list ':= x (reduce exp vs))))
                (goto bb_while_cond))
    (process_goto (:= next_pp (second cmd))
                  (:= bb (lookup next_pp program))
                  (goto bb_while_cond))
    (process_if (:= exp (second cmd))
                (:= pp_true (third cmd))
                (:= pp_false (fourth cmd))
                (if (static-exp? division exp) static_cond dynamic_cond))
    (static_cond (if (eval-exp exp vs) compress_true compress_false))
    (compress_true (:= bb (lookup pp_true program))
                   (goto bb_while_cond))
    (compress_false (:= bb (lookup pp_false program))
                    (goto bb_while_cond))
    (dynamic_cond (:= true_label (list pp_true vs))
                  (:= false_label (list pp_false vs))
                  (:= pending (set-union pending (set-subtract (set true_label false_label) marked)))
                  (:= code (extend code (list 'if (reduce exp vs) true_label false_label)))
                  (goto bb_while_cond))
    (process_return (:= code (extend code (list 'return (reduce exp vs))))
                    (goto bb_while_cond))
    (error (return 'error))
    (bb_while_end (goto add_residual_block))
    (add_residual_block (:= residual (extend residual code))
                        (goto pending_while_cond))
    (pending_while_end (return residual)))