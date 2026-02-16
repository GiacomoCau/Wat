
(def\ (fib n) ((rec\ (fib a b) (if (0? (-- n)) a (fib b (+ a b)))) 1 1))