#lang racket
;####################################################################
;                            VS REMOVING
;####################################################################

(provide unvs)

;Substitute all subexpressions in form (eval-exp 'exp vs-name) in expression
(define (remove-eval-exp-from-exp exp vs-name)
  (cond
    [(symbol? exp) exp]
    [(and (cons? exp) (equal? 'quote (car exp))) exp]
    [(match exp [`(eval-exp ,exp ,vs) #:when (equal? vs vs-name) #t] [_ #f]) (second (second exp))]
    [(cons? exp)
     (let ([tail (map (lambda (e) (remove-eval-exp-from-exp e vs-name)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (cons head tail))]
    [else exp]))

;Build an expression that behaves similar to a quasiquote that unquotes only variables in vars
(define (rebuild-reduce exp vars)
  (cond
    [(symbol? exp) (if (set-member? vars exp)
                       `(cons 'quote (list ,exp))
                       `',exp)]
    [(and (cons? exp) (equal? 'quote (car exp))) `',exp]
    [(cons? exp)
     (let ([tail (map (lambda (e) (rebuild-reduce e vars)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (list* 'list `',head tail))]
    [else `',exp]))

;Substitute all subexpressions in form (reduce 'exp vs-name) in expression
(define (remove-reduce-from-exp exp vars vs-name)
  (cond
    [(symbol? exp) exp]
    [(and (cons? exp) (equal? 'quote (car exp))) exp]
    [(match exp [`(reduce ,exp ,vs) #:when (equal? vs vs-name) #t] [_ #f]) (rebuild-reduce (second (second exp)) vars)]
    [(cons? exp)
     (let ([tail (map (lambda (e) (remove-reduce-from-exp e vars vs-name)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (cons head tail))]
    [else exp]))

;Substitute variable with vs-name to a construction of a hash that maps variables in vars
(define (expand-vs-in-exp exp vars vs-name)
  (cond
    [(symbol? exp) (if (equal? exp vs-name)
                       (cons 'make-immutable-hash (list (cons 'list (map (lambda (v) `(cons ',v ,v)) vars))))
                       exp)]
    [(and (cons? exp) (equal? 'quote (car exp))) exp]
    [(cons? exp)
     (let ([tail (map (lambda (e) (expand-vs-in-exp e vars vs-name)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (cons head tail))]
    [else exp]))

;Merge dict of static variables vars with name vs-name to the local variables.
;read-vars is the list of variables that is read from input
;This code is very ad-hoc and bad designed.
(define (unvs program vars read-vars vs-name)
  (define removed-ass
    (for/list ([bb (cdr program)])
      (cons
       (car bb)
       (for/list ([instr (cdr bb)])
         (match instr
           [`(:= ,vs (set-var ,vs ,var-name ,value)) #:when (equal? vs vs-name) `(:= ,(second var-name) ,value)]
           [x x])))))
  (define removed-eval-exp
    (for/list ([bb removed-ass])
      (cons
       (car bb)
       (for/list ([instr (cdr bb)])
         (match instr
           [`(:= ,x ,exp) `(:= ,x ,(remove-eval-exp-from-exp exp vs-name))]
           [`(if ,exp ,l ,r) `(if ,(remove-eval-exp-from-exp exp vs-name) ,l ,r)]
           [`(return ,exp) `(return ,(remove-eval-exp-from-exp exp vs-name))]
           [x x])))))
  (define removed-reduce
    (for/list ([bb removed-eval-exp])
      (cons
       (car bb)
       (for/list ([instr (cdr bb)])
         (match instr
           [`(:= ,x ,exp) `(:= ,x ,(remove-reduce-from-exp exp vars vs-name))]
           [`(if ,exp ,l ,r) `(if ,(remove-reduce-from-exp exp vars vs-name) ,l ,r)]
           [`(return ,exp) `(return ,(remove-reduce-from-exp exp vars vs-name))]
           [x x])))))
   (define removed-vs-ass
    (for/list ([bb removed-reduce])
      (cons
       (car bb)
       (for/fold ([res null]
                  #:result (reverse res))
                 ([instr (cdr bb)])
         (match instr
           [`(:= ,vs ,exp) #:when (equal? vs vs-name) (values (append res (for/list ([var vars]) `(:= ,var (dict-ref ,exp ',var (void))))))]
           [x (values (cons x res))])))))
  (define expanded-vs
    (for/list ([bb removed-vs-ass])
      (cons
       (car bb)
       (for/list ([instr (cdr bb)])
         (match instr
           [`(:= ,x ,exp) `(:= ,x ,(expand-vs-in-exp exp vars vs-name))]
           [`(if ,exp ,l ,r) `(if ,(expand-vs-in-exp exp vars vs-name) ,l ,r)]
           [`(return ,exp) `(return ,(expand-vs-in-exp exp vars vs-name))]
           [x x])))))
  (define read-block (cons 'read read-vars))
  (define init-vars-block
    (cons (car (car expanded-vs))
          (append (for/list ([var (set-subtract vars read-vars)])
      `(:= ,var (void))) (cdr (car expanded-vs)))))
  (list* read-block init-vars-block (cdr expanded-vs)))