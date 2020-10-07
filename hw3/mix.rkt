#lang racket

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

(define (initial-code pp vs) `(,(cons pp vs)))

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
       (apply (eval head) tail))]
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