#lang racket

(require "tm-flowchart.rkt" "flowchart.rkt" "aux-functions.rkt" rackunit)


(define (mix_racket program division vs0)
  (define pp0 (initial-point program))
  (define pending (set (list pp0 vs0)))
  (define marked (set))
  (define residual (list (remove-static-reads (car program) division)))
  (define live-vars (find-projections program division))
  (let loop_pending ()
    (unless (set-empty? pending)
      (define pp (first (set-first pending)))
      (define vs (second (set-first pending)))
      ;(displayln (list pp vs))
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
                ;(displayln exp)
                (cond
                  [(eval-exp exp vs) (set! bb (lookup pp_true program))]
                  [else (set! bb (lookup pp_false program))])]
               [else
                (begin
                  (define true_label (list pp_true (retain-live vs (dict-ref live-vars pp_true))))
                  (define false_label (list pp_false (retain-live vs (dict-ref live-vars pp_false))))
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


(define mix_flowchart
  '((read program division vs)
    (init (:= pending (set (list (initial-point program) vs)))
          (:= marked (set))
          ;(:= bp (reset-blocks-in-pending))
          ;(:= bp (add-to-blocks-in-pending (cadr program)))
          (:= bp (find-blocks-in-pending program division))
          (:= live-vars (find-projections program division))
          (:= residual (list (remove-static-reads (car program) division)))
          ;(:= xd '())
          (goto pending_while_cond))
    (pending_while_cond (if (set-empty? pending) pending_while_end pending_while_start))
    (pending_while_start (:= pp_dyn (first (set-first pending)))
                         (:= vs (second (set-first pending)))
                         ;(:= bbs (cdr program))
                         ;(:= xd xd)
                         ;(:= xd (find-blocks-in-pending))
                         (:= bbs bp)
                         ;(:= debug (displayln blocks-in-pending))
                         (goto find_pp))
    (find_pp (if (empty? bbs) error_pp_not_found find_pp_not_empty_bbs))
    (find_pp_not_empty_bbs (:= bb (cdar bbs))
                           (:= pp (caar bbs))
                           (:= bbs (cdr bbs))
                           (if (equal? pp pp_dyn) pp_found find_pp))
    (error_pp_not_found (return (list 'error_pp_not_found pp_dyn)))
    (pp_found (:= pending (set-rest pending))
              ;(:= marked (set-add marked (list pp vs)))
              (:= marked (set-add marked (list pp (retain-live vs (dict-ref live-vars pp)))))
              (:= code (initial-code pp (retain-live vs (dict-ref live-vars pp))))
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
                 ;(if (static-var? division (second cmd)) static_ass dynamic_ass))
    (static_ass (:= vs (set-var vs x (eval-exp exp vs)))
                ;(:= vs (set-var vs (second cmd) (eval-exp (third cmd) vs)))
                (goto bb_while_cond))
    (dynamic_ass (:= code (extend code (list ':= x (reduce exp vs))))
                 ;(:= code (extend code (list ':= (second cmd) (reduce (third cmd) vs))))
                 (goto bb_while_cond))
    (process_goto (:= next_pp (second cmd))
                  (:= bb (lookup next_pp program))
                  ;(:= bb (lookup (second cmd) program))
                  (goto bb_while_cond))
    (process_if (:= exp (second cmd))
                (:= pp_true (third cmd))
                (:= pp_false (fourth cmd))
                (if (static-exp? division exp) static_cond dynamic_cond))
                ;(if (static-exp? division (second cmd)) static_cond dynamic_cond))
    (static_cond (if (eval-exp exp vs) compress_true compress_false))
                 ;(if (eval-exp (second cmd) vs) compress_true compress_false))
    (compress_true (:= bb (lookup pp_true program))
                   ;(:= bb (lookup (third cmd) program))
                   (goto bb_while_cond))
    (compress_false (:= bb (lookup pp_false program))
                    ;(:= bb (lookup (fourth cmd) program))
                    (goto bb_while_cond))
    (dynamic_cond (:= true_label (list pp_true (retain-live vs (dict-ref live-vars pp_true))))
                  (:= false_label (list pp_false (retain-live vs (dict-ref live-vars pp_false))))
                  (:= pending (set-union pending (set-subtract (set true_label false_label) marked)))
                  (:= code (extend code (list 'if (reduce exp vs) true_label false_label)))
                  ;(:= pending (set-union pending (set-subtract (set (list (third cmd) vs) (list (fourth cmd) vs)) marked)))
                  ;(:= code (extend code (list 'if (reduce (second cmd) vs) (list (third cmd) vs) (list (fourth cmd) vs))))
                  ;(:= bp (add-to-blocks-in-pending (lookup-block pp_true program)))
                  ;(:= bp (add-to-blocks-in-pending (lookup-block pp_false program)))
                  ;(:= debug (displayln (find-blocks-in-pending)))
                  (goto bb_while_cond))
    (process_return (:= exp (second cmd))
                    (:= code (extend code (list 'return (reduce exp vs))))
                    ;(:= code (extend code (list 'return (reduce (second cmd) vs))))
                    (goto bb_while_cond))
    (error (return 'error))
    (bb_while_end (goto add_residual_block))
    (add_residual_block (:= residual (extend residual code))
                        (goto pending_while_cond))
    (pending_while_end (return residual))))

(define mix_flowchart-division (set 'program 'division 'bbs 'bb 'pp 'cmd 'op 'x 'exp 'next_pp 'pp_true 'pp_false 'debug 'live-vars 'xd 'bp))

(define tm-int-division (set 'Q 'ptr 'instr 'cur_op 'symb 'next-label))

(define flowchart-int-division (set 'program 'pp 'bb 'cmd 'op 'x 'exp 'next_pp 'pp_true 'pp_false 'debug))

(define flowchart-int
  '((read program vs)
    (init (:= pp (initial-point program))
          (:= bb (lookup pp program))
          (goto bb_while_cond))
    (bb_while_cond (if (empty? bb) bb_while_end bb_while_start))
    (bb_while_start (:= cmd (first bb))
                    (:= bb (rest bb))
                    (:= op (car cmd))
                    (goto match_op))
    (match_op (if (equal? ':= op) process_ass match_op1))
    (match_op1 (if (equal? 'goto op) process_goto match_op2))
    (match_op2 (if (equal? 'if op) process_if match_op3))
    (match_op3 (if (equal? 'return op) process_return error_wrong_op))
    (process_ass (:= x (second cmd))
                 (:= exp (third cmd))
                 (:= vs (set-var vs x (eval-exp exp vs)))
                 (goto bb_while_cond))
    (process_goto (:= next_pp (second cmd))
                  (:= bb (lookup next_pp program))
                  (goto bb_while_cond))
    (process_if (:= exp (second cmd))
                (:= pp_true (third cmd))
                (:= pp_false (fourth cmd))
                (if (eval-exp exp vs) compress_true compress_false))
    (compress_true (:= bb (lookup pp_true program))
                   (goto bb_while_cond))
    (compress_false (:= bb (lookup pp_false program))
                    (goto bb_while_cond))
    (process_return (:= exp (second cmd))
                    (return (eval-exp exp vs)))
    (error_wrong_op (return (list 'error op)))
    (bb_while_end (goto error_bb_empty))
    (error_bb_empty (return 'error_bb_empty))))

;####################################################################
;                            VS REMOVING
;####################################################################

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

(define (rebuild-reduce exp vars)
  (cond
    [(symbol? exp) (if (set-member? vars exp)
                       exp
                       `',exp)]
    [(and (cons? exp) (equal? 'quote (car exp))) `',exp]
    [(cons? exp)
     (let ([tail (map (lambda (e) (rebuild-reduce e vars)) (cdr exp))]
           [head (car exp)])
       ;(display tail)
       (list* 'list `',head tail))]
    [else `',exp]))

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
  
;####################################################################
;                            RENAMING
;####################################################################  

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

(define (add-suffix program suffix)
  (define vars (all-variables program))
  (define (symbol-append a b) (string->symbol (string-append (symbol->string a) (symbol->string b))))
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
    


;####################################################################
;                             TESTS
;####################################################################
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
               
               