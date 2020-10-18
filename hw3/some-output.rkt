#lang racket
(require "projections.rkt" "mix.rkt" "tm-flowchart.rkt" "renaming.rkt" "flowchart.rkt" "vs-removing.rkt")
(displayln "Compiled find_name using first projection")
(pretty-print (first-projection_flowchart find_name))

(displayln "")

(displayln "Compiled tm-example using first projection")
(pretty-print (first-projection_tm tm-example))

(displayln "")

(displayln "Compiled find_name using second projection")
(pretty-print (compile_flowchart find_name))

(displayln "")

(displayln "Compiled tm-example using second projection")
(pretty-print (compile_tm tm-example))

(displayln "")

(displayln "Compiled find_name using third projection")
(pretty-print (unvs
               (relabel (int (relabel (generate-compiler flowchart-int flowchart-int-division)) `(,find_name)))
               (all-variables find_name)
               (read-variables find_name)
               'vs))

(displayln "")

(displayln "Compiled tm-example using third projection")
(pretty-print (relabel (int (relabel (generate-compiler tm-int tm-int-division)) `(,tm-example))))

