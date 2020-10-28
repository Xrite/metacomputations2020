#lang racket

(provide (all-defined-out))

(define (symbol-append a b) (string->symbol (string-append (symbol->string a) (symbol->string b))))

(define (add-suffix-to-division division suffix)
  (for/set ([var division]) (symbol-append var suffix)))

;All variables in program
(define (all-variables program)
  (for/fold ([vars (cdr (car program))])
            ([bb (cdr program)])
    (set-union
     vars
     (for/fold ([in-ass null])
               ([instr (cdr bb)])
       (match instr
         [`(:= ,x ,_) (set-add in-ass x)]
         [_ in-ass])))))

;All variables in read section of the program
(define (read-variables program)
  (cdr (car program)))


;Add suffix to all variables in the program
(define (add-suffix program suffix)
  (define vars (all-variables program))
  (define (rename-in-exp exp)
    (cond
      [(symbol? exp) (if (set-member? vars exp)
                         (symbol-append exp suffix)
                         exp)]
      [(and (cons? exp) (equal? 'quote (car exp))) exp]
      [(cons? exp)
       (let ([tail (map (lambda (e) (rename-in-exp e)) (cdr exp))]
             [head (car exp)])
         ;(display tail)
         (cons head tail))]
      [else exp]))
  (cons
   (cons (car (car program)) (for/list ([v (cdr (car program))]) (symbol-append v suffix)))
   (for/list ([bb (cdr program)])
     (cons
      (car bb)
      (for/list ([instr (cdr bb)])
        (match instr
          [`(:= ,x ,exp) `(:= ,(symbol-append x suffix) ,(rename-in-exp exp))]
          [`(if ,exp ,l ,r) `(if ,(rename-in-exp exp) ,l ,r)]
          [`(return ,exp) `(return ,(rename-in-exp exp))]
          [x x]))))))

(define (relabel program)
  (define new-label (make-hash))
  (define r
    (for/list ([bb (cdr program)])
      (define label (car bb))
      (unless (dict-has-key? new-label label)
        (dict-set! new-label label (dict-count new-label)))
      (cons (dict-ref new-label label) (cdr bb))))
  (cons (car program)
        (for/list ([bb r])
          (for/list ([instr bb])
            (match instr
              [`(if ,expr ,l ,r) `(if ,expr ,(dict-ref new-label l) ,(dict-ref new-label r))]
              [`(goto ,l) `(goto ,(dict-ref new-label l))]
              [x x])))))
    