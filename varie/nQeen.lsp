
; frmo: https://github.com/amirouche/seed/blob/hello-seeder/src/benchmarks/n-queen/n-queen.scm

; (load "varie/nQeen.lsp")

(def\ (list-tabulate n proc)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (-1+ i) (cons (proc i) acc)))))

(def\ (n-queens n)
  (letrec\
    ( ((place-initial-row)
         (list-tabulate n (\ (col) (list (cons 0 col)))))
      ((invalid? soln-so-far row col)
         (any? (\ (posn)
                 (|| (== col (cdr posn))
                     (== (abs (- row (car posn)))
                         (abs (- col (cdr posn))) )))
               soln-so-far ))
       ((place-on-row soln-so-far row)
          (let try-col ((col 0) (res '()))
            (if (== col n) res
              (try-col (1+ col)
                (if (invalid? soln-so-far row col) res
                  (cons (cons (cons row col) soln-so-far)
                        res ))))))
       ((solve res row)
          (if (== row n) res
            (solve (apply append 
                     (map (\ (soln) (place-on-row soln row))
                       res ))
                   (+ 1 row) ))) )
    (solve (place-initial-row) 1) ))

(def\ (string->number str) (@valueOf Integer str) )

(def NQUEEN-N
  (let ([env-val (getEnv "SEED_NQUEEN")])
    (if (null? env-val) 8 (string->number env-val))))

(def\ (run-benchmark . n)
  (time (optDft n 1) (print (length (n-queens NQUEEN-N)) " solutions")) #inert)

(assert (length (n-queens 8)) 92)
