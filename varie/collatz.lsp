
; from: https://github.com/amirouche/seed/blob/hello-seeder/src/benchmarks/collatz/collatz.scm

; (load "varie/collatz.lsp")

(def\ (collatzLength n)
  (let count ([x n] [steps 0])
    (if (1? x) steps
      (count (if (even? x) (/ x 2) (1+ (* 3 x))) (1+ steps)) )))

(def\ (collatzLength n)
  ((rec\ (count x steps)
     (if (1? x) steps
       (count (if (even? x) (/ x 2) (1+ (* 3 x))) (1+ steps)) )) n 0 ))

(def\ (findLongestCollatz limit)
  (let search ([n 1] [bestN 1] [bestLen 0])
    (if (>= n limit) (list bestN bestLen)
      (let ([len (collatzLength n)])
        (if (> len bestLen)
          (search (1+ n) n len)
          (search (1+ n) bestN bestLen))))))

(def\ (countSpecial limit)
  (let loop ([n 1] [count 0])
    (if (>= n limit) count
      (loop (1+ n) (if (^ (0? (% n 3)) (0? (% n 7))) count (1+ count))) )))

(def\ (string->number str) (@valueOf Integer str) )

(def COLLATZ-LIMIT
  (let1 [envVal (getEnv "SEED_COLLATZ")]
    (if (null? envVal) 200 #;20_000_000 (string->number envVal))))
(def SPECIAL-LIMIT
  (let1 [envVal (getEnv "SEED_SPECIAL")]
    (if (null? envVal) 400 #;40_000_000 (string->number envVal))))

(def\ (runBenchmark)
  (time 1
    (print
       "Collatz: " (findLongestCollatz COLLATZ-LIMIT) "\n"
       "Special: " (countSpecial SPECIAL-LIMIT) "\n" )
    #inert))

(assert (findLongestCollatz 200) (171 124))
(assert (countSpecial 400) 247)
