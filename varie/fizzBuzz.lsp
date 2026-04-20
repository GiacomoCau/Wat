
(def\ (mod? n m) (0? (% n m)))
(def\ (fizzbuzz n) (if (mod? n 15) 'fizzbuzz (mod? n 5) 'fizz (mod? n 3) 'buzz n) )
(map fizzbuzz (iota 15))

(forEach# (\ (n) (log (fizzbuzz n))) (iota 15))
(forEach# (compose log fizzbuzz) (iota 15))
(def\ (b* f g) (\ x (f (apply g x))))
(forEach# (b* log fizzbuzz) (iota 15))
