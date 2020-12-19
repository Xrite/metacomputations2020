#lang racket

(require "flowchart.rkt" "aux-functions.rkt" )

(provide (all-defined-out))

;Mixed evaluator written in racket.
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


;Mixed evaluator written in FlowChart.
;The division is a set of static variable names.
;The vs is a hash map from variable name to variable value.
;It uses live/dead variable analysis and blocks-in-pending.
(define mix_flowchart
  '((read program division vs)
    (init (:= pending (set (list (initial-point program) vs)))
          (:= marked (set))
          (:= bp (find-blocks-in-pending program division))
          (:= live-vars (find-projections program division))
          (:= residual (list (remove-static-reads (car program) division)))
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

;FlowChart interpreter written in FlowChart.
;The vs is a hash map from variable name to variable value.
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

(define flowchart-int-division (set 'program 'pp 'bb 'cmd 'op 'x 'exp 'next_pp 'pp_true 'pp_false 'debug))



(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))
               