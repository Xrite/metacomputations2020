#lang racket

(provide (all-defined-out))

(define-namespace-anchor aux-anchor)

(define aux-namespace (namespace-anchor->namespace aux-anchor))

;########################################################
;                  FUNCTIONS FOR MIX
;########################################################

;Remove all static variables from read block
(define (remove-static-reads read-block division)
  (cons 'read (filter (lambda (x) (not (set-member? division x))) (cdr read-block))))

;Return initial program point in program
(define (initial-point program)
  (first (second program)))

;Find basic block instructions by label
(define (lookup label program)
  (define (impl is)
    (cond
      [(empty? is) (error label "no block with such label")]
      [(equal? (caar is) label) (cdr (car is))]
      [else (impl (cdr is))]))
  (impl (cdr program)))

;Find basic block by label
(define (lookup-block label program)
  (define (impl is)
    (cond
      [(empty? is) (error label "no block with such label")]
      [(equal? (caar is) label) (car is)]
      [else (impl (cdr is))]))
  (impl (cdr program)))

;Create empty basic block with the given label
(define (initial-code pp vs) `(,(list pp vs)))

;Check whether a variable is static by a division
(define (static-var? division x) (set-member? division x))

;Check whether an expression is static by a division
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

;Set a value to the variable in the vs
(define (set-var vs var val) (dict-set vs var val))

;Add an instruction th the basic block
(define (extend s e)
  ;(when (> (random) 0.9) (displayln (length s)))
  (reverse (cons e (reverse s))))

;Reduce an expression using given static variables
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

;Evaluate an expression using given static variable values
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

;Return a list of all program points
(define (program-points program)
  (map car (cdr program)))

;(define blocks-in-pending '())

;(define (reset-blocks-in-pending) (set! blocks-in-pending '()))

;(define (add-to-blocks-in-pending bb) (set! blocks-in-pending (if (member bb blocks-in-pending) blocks-in-pending (cons bb blocks-in-pending))))

;(define (find-blocks-in-pending) blocks-in-pending)

;Find blocks that could be added in pending
(define (find-blocks-in-pending program division)
  (define pps (for/fold ([pps (set (initial-point program))])
            ([bb (cdr program)])
    (let ([instr (last bb)])
      (match instr
        [`(if ,exp ,pp1 ,pp2) #:when (not (static-exp? division exp)) (values (set-union pps (set pp1 pp2)))]
        [_ (values pps)]))))
  (set-map pps (lambda (pp) (lookup-block pp program))))
  ;'((next (:= ptr (+ ptr 1)) (goto loop)) (jump (:= ptr (find-instruction Q next-label)) (goto loop)) (init (:= ptr 0) (:= Left '()) (goto loop))))

;Return all static variables in the expression
(define (vars-in-exp exp division)
  (cond
    [(symbol? exp) (if (static-var? division exp) (set exp) (set))]
    [(and (cons? exp) (equal? 'quote (car exp))) (set)]
    [(cons? exp)
     (let ([tail (map (lambda (e) (vars-in-exp e division)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (apply set-union (cons (set) tail)))]
    [else (set)]))
  
;Perform live varable analysis with given division
(define (find-projections program division)
  (define (next partial)
    (dict-map partial
              (lambda (pp live-vars)
                (cons pp (let f ([bb (lookup pp program)])
                  (let* ([instr (car bb)]
                         [op (car instr)])
                    (match instr
                      [`(:= ,x ,exp) (set-union (set-remove (f (cdr bb)) x) (vars-in-exp exp division))]
                      [`(goto ,l) (dict-ref partial l)]
                      [`(return ,exp) (vars-in-exp exp division)]
                      [`(if ,exp ,l ,r) (set-union (vars-in-exp exp division) (dict-ref partial l) (dict-ref partial r))])))))))
  (define initial (make-immutable-hash (map (lambda (pp) (cons pp (set))) (program-points program))))
  (let fix ([x initial])
    (let ([next_x (next x)])
      (if (equal? x next_x)
          x
          (fix next_x)))))

;Retain live variables from vs
(define (retain-live vs live-vars)
  (make-immutable-hash (filter (lambda (p) (set-member? live-vars (car p))) (dict->list vs))))
                  
(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))


;########################################################
;            FUNCTIONS FOR TM INTERPRETER
;########################################################

;Retrun i-th instruction 
(define (get-instruction instructions i)
  (cond
    [(< i 0) '(fail fail)]
    [(>= i (length instructions)) '(stop stop)]
    [else (list-ref instructions i)]))

;Find instruction position by its label
(define (find-instruction instructions label)
  (define (impl is i)
    (cond
      [(empty? is) -1]
      [(equal? (caar is) label) i]
      [else (impl (cdr is) (+ i 1))]))
  (impl instructions 0))
