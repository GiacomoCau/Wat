
; from: https://github.com/amirouche/seed/blob/hello-seeder/src/benchmarks/abacus/abacus.scm

; (load "varie/abacus.lsp")

(def\ (evalExpr expr)
  (match expr
    [('+ a b) (+ (evalExpr a) (evalExpr b))]
    [('- a b) (- (evalExpr a) (evalExpr b))]
    [('* a b) (* (evalExpr a) (evalExpr b))]
    [('/ a b) (/ (evalExpr a) (evalExpr b))]
    [(#: Number n) n]
    [else (error "invalid expr")] ))

(def\ (makeSumTree n)
  (let build ([i 1])
    (if (== i n) n (list '+ i (build (1+ i)))) ))

(def\ (makeBalancedTree d)
  (if (0? d) 1
    (list '+
      (makeBalancedTree (-1+ d))
      (makeBalancedTree (-1+ d)) )))

(def\ (string->number str) (@valueOf Integer str) )

(def ABACUS-DEPTH
  (let ([envVal (getEnv "SEED_ABACUS_DEPTH")])
    (if (null? envVal) 14 (string->number envVal))))

(def\ (run-benchmark)
  (print "Arithmetic Evaluator Benchmark (SRFI-241 match catamorphism)\n")

  ;; --- Correctness tests ---
  (print "Simple:  (+ 1 2) = " (evalExpr '(+ 1 2) ))
  (print "Nested:  (* (+ 3 4) (- 10 2)) = " (evalExpr '(* (+ 3 4) (- 10 2)) ))
  (print "Division: (/ (* 100 (+ 2 3)) (- 20 10)) = " (evalExpr '(/ (* 100 (+ 2 3)) (- 20 10)) ))

  ;; --- Deep right-leaning: sum 1..100 ---
  (print "Sum 1..100 (depth 99): " (evalExpr (makeSumTree 100)))

  ;; --- Balanced trees ---
  (print "Balanced tree depth 10 (1024 leaves): " (evalExpr (makeBalancedTree 10)))

  (let ([tree (time 1 (makeBalancedTree ABACUS-DEPTH))])
    (print "Balanced tree depth " ABACUS-DEPTH ": " (time 1 (evalExpr tree))))
)