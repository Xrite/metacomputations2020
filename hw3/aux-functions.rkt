#lang racket

(provide (all-defined-out))

(define-namespace-anchor aux-anchor)

(define aux-namespace (namespace-anchor->namespace aux-anchor))

;########################################################
;                  FUNCTIONS FOR MIX
;########################################################

(define (remove-static-reads read-block division)
  (cons 'read (filter (lambda (x) (not (set-member? division x))) (cdr read-block))))

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
  (when (> (random) 0.9) (displayln (length s)))
  (reverse (cons e (reverse s))))

(define (reduce expr vs)
  (car
   (let proc ([exp expr])
     (cond
       [(symbol? exp) (if (dict-has-key? vs exp)
                          (cons `(quote ,(dict-ref vs exp)) #t)
                          (cons exp #f))]
       [(and (cons? exp) (equal? 'quote (car exp))) (cons exp #t)]
       [(cons? exp)
        (let* ([tailres (map (lambda (e) (proc e)) (cdr exp))]
               ;[xd (displayln tailres)]
               [all-static (andmap cdr tailres)]
               [tail (map car tailres)]
               [head (car exp)])
          ;(display tail)
          (if all-static
              (cons `(quote ,(eval (cons head tail) aux-namespace)) #t)
              (cons (cons head tail) #f)))]
       [else (cons exp #t)]))))

(define (eval-exp exp vs) (eval (reduce exp vs) aux-namespace))

;(define (eval-exp exp vs)
;  (cond
;    [(symbol? exp) (dict-ref vs exp (lambda () (error exp "no such static variable")))]
;    [(and (cons? exp) (equal? 'quote (car exp))) (cadr exp)]
;    [(cons? exp)
;     (let ([tail (map (lambda (e) (eval-exp e vs)) (cdr exp))]
;           [head (car exp)])
;       ;(display tail)
;       (cons 'quote (apply (eval head aux-namespace) tail)))]
;    [else exp]))

(define (program-points program)
  (map car (cdr program)))

;########################################################
;            FUNCTIONS FOR TM INTERPRETER
;########################################################

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