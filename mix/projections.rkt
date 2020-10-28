#lang racket
(require "mix.rkt" "tm-flowchart.rkt" "flowchart.rkt" "vs-removing.rkt" "renaming.rkt")

(provide (all-defined-out))

;First Futamura projection for TM. Q is the source.
(define (first-projection_tm Q)
  (relabel (mix_racket tm-int tm-int-division (hash 'Q Q))))

;First Futamura projection for FlowChart.
(define (first-projection_flowchart source)
  (define renamed-flowchart-int (add-suffix flowchart-int '_fc-int))
  (define renamed-flowchart-int-division (add-suffix-to-division flowchart-int-division '_fc-int))
  (define variables-source (all-variables source))
  (define read-variables-source (read-variables source))
  (define with-vs (relabel (mix_racket renamed-flowchart-int renamed-flowchart-int-division (hash 'program_fc-int source))))
  (unvs with-vs variables-source read-variables-source 'vs_fc-int))

;Second Futamura projection for TM.
(define (second-projection_tm)
  (define renamed-mix_flowchart (add-suffix mix_flowchart '_mix-fc))
  (define renamed-mix_flowchart-division (add-suffix-to-division mix_flowchart-division '_mix-fc))
  (define vars-tm-int (set->list tm-int-division))
  (define read-vars-tm-int (read-variables tm-int))
  (define with-vs (relabel (mix_racket renamed-mix_flowchart renamed-mix_flowchart-division (hash 'program_mix-fc tm-int 'division_mix-fc tm-int-division))))
  (unvs with-vs vars-tm-int '(Q) 'vs_mix-fc))

;Second Futamura projection for FlowChart.
(define (second-projection_flowchart)
  (define renamed-fc-int (add-suffix flowchart-int '_fc-int))
  (define renamed-fc-int-division (add-suffix-to-division flowchart-int-division '_fc-int))
  (define renamed-mix_fc (add-suffix mix_flowchart '_mix-fc))
  (define renamed-mix_fc-division (add-suffix-to-division mix_flowchart-division '_mix-fc))
  (define vars-fc-int (set->list (add-suffix-to-division flowchart-int-division '_fc-int)))
  (define read-vars-fc-int (read-variables renamed-fc-int))
  (define with-vs (relabel (mix_racket renamed-mix_fc renamed-mix_fc-division (hash 'program_mix-fc renamed-fc-int 'division_mix-fc renamed-fc-int-division))))
  (unvs with-vs vars-fc-int '(program_fc-int) 'vs_mix-fc))

;Third Futamura projection for FlowChart
(define (third-projection)
  (define mix_fc_1 (add-suffix mix_flowchart '_mix-fc_1))
  (define mix_fc-division_1 (add-suffix-to-division mix_flowchart-division '_mix-fc_1))
  (define mix_fc_2 (add-suffix mix_flowchart '_mix-fc_2))
  (define mix_fc-division_2 (add-suffix-to-division mix_flowchart-division '_mix-fc_2))
  (define vars-mix_fc_2 (set->list (add-suffix-to-division mix_flowchart-division '_mix-fc_2)))
  (define read-vars-mix_fc_2 (read-variables mix_fc_2))
  (define with-vs (relabel (mix_racket mix_fc_1 mix_fc-division_1 (hash 'program_mix-fc_1 mix_fc_2 'division_mix-fc_1 mix_fc-division_2))))
  (unvs with-vs vars-mix_fc_2 '(program_mix-fc_2 division_mix-fc_2) 'vs_mix-fc_1))

;Compile TM source code using second Futamura projection
(define (compile_tm source)
  (relabel (int (second-projection_tm) `(,source))))

;Compile FlowChart source code using second Futamura projection
(define (compile_flowchart source)
  (define vars (all-variables source))
  (define read-vars (read-variables source))
  (define with-vs (relabel (int (second-projection_flowchart) `(,source))))
  (unvs with-vs vars read-vars 'vs_fc-int)) 

;Generate compiler by interpreter written in FlowChart.
;The first argument of the given interpreter must be the source code.
(define (generate-compiler int-source int-division)
  (define input-var (second (car int-source)))
  (define vars (set->list int-division))
  (define read-vars (read-variables int-source))
  (define with-vs (relabel (int (third-projection) `(,int-source ,int-division))))
  (unvs with-vs vars `(,input-var) 'vs_mix-fc_2))