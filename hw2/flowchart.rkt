#lang racket

(require rackunit)

(define int (lambda (program data) (interpret program data)))

(define (validate-read read)
  (match read
    [(cons 'read _) 'ok]
    [_ (error "malformed read" read)]))

(define (interpret program data)
  (let ([read (car program)]
        [blocks (cdr program)])
    (validate-read read)
    (define st_0 (set-instr-ptr (read-variables (cdr read) data) 0))
    (define jmp-table (map-labels blocks))
    (define (run-block state)
      (let ([block (get-instr-ptr state)])
        (cond
          [(equal? block -1) (get-return state)]
          [else (run-block (eval-block (list-ref blocks block) jmp-table state))])))
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

(define (eval-block block jmp-table state)
  (define (iter cmds state)
    (match cmds
      [`(,jump) (eval-jump jump state jmp-table)]
      ['() (error "empty block" block)]
      [(cons ass rest) (iter rest (eval-assignment ass state))]
      [_ (error "malformed block" block)]))
  (iter (cdr block) state))

(define (eval-assignment assignment state)
  (match assignment
    [`(:= ,x ,expr) (set-variable state x (eval-expression state expr))]
    [_ (error "malformed assignment" assignment)]))

(define (eval-jump jump state jmp-table)
  (match jump
    [`(goto ,label) (eval-goto label state jmp-table)]
    [`(if ,expr ,true-label ,false-label) (eval-if expr true-label false-label state jmp-table)]
    [`(return ,expr) (eval-return expr state)]
    [_ (error "malformed jump" jump)]))

(define (eval-goto label state jmp-table)
  (cond
    [(hash-has-key? jmp-table label) (set-instr-ptr state (hash-ref jmp-table label))]
    [else (error "no such label" label)]))

(define (eval-if if-expr true-label false-label state jmp-table)
  (if (eval-expression state if-expr)
      (eval-goto true-label state jmp-table)
      (eval-goto false-label state jmp-table)))

(define (eval-return return-expr state)
  (set-return state (eval-expression state return-expr)))

(define (eval-expression state expr)
  (cond
    [(symbol? expr) (get-variable state expr)]
    [(cons? expr)
     (let ([tail (map (lambda (e) (eval-expression state e)) (cdr expr))]
           [head (car expr)])
       ;(display tail)
       (apply (eval head) tail))]
    [else expr]))



(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))


(test-begin
 (let ([state (read-variables '(a b c dd e28 F GGWP228) '(1 2 3 '(0 0 0) "e28" F #f))])
   (check-equal? ((eval-expression state '(+ a b)) 3)
   (check-equal? (eval-expression state '(length dd)) 3))))