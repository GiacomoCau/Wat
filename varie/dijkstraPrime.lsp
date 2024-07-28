
#| original

	def dijkstraPrimes(n):
	    pool = [[4,2]]
	    primes = [2]
	    for i in range(3, n):
	        if min(pool)[0] > i:
	            pool.append([i**2,i])
	            primes+=[i]
	        else:
	            for pair in pool:
	                while pair[0] <= i:
	                    pair[0] += pair[1]
	    return primes
|#

(def\ (primes max)
  (let ( (primes (2))
         (pool ((4 2))) )
    (for (n 3 (1+ n)) (<= n max)
      (if (all? (\ (x) (< n (car x))) pool)
        (then 
          (set! primes (append primes (cons n)))
          (set! pool (append pool (cons (list (* n n) n)))) )
        (forEach (\ (x) (while (<= (car x) n) (@setCar x (apply + x)))) pool) ))
     primes ))
     
(def\ (primes max)
  (let next ( (n 3) (primes (2)) (pool ((4 2))) )
    (if (> n max) primes
      (let1 (t (all? (\ (x) (< n (car x))) pool))
        (next (+ 1 n)
          (if t (append primes (cons n)) primes)
          (if t (append pool (cons (list (* n n) n))) (forEach (\ (e) (while (<= (car e) n) (@setCar e (apply + e)))) pool)) )))))

(def\ (primes max)
  (let next ( (n 3) (primes (2)) (pool ((4 2))) )
    (if (> n max) primes
      (all? (\ (x) (< n (car x))) pool)
        (next (1+ n) (append primes (cons n)) (append pool (cons (list (* n n) n))) )
        (next (1+ n) primes (forEach (\ (e) (while (<= (car e) n) (@setCar e (apply + e)))) pool)) )))

(primes 50)

(def\ (primes2 max)
  (let next ( (n 3) (primes (2)) (pool ((4 2))) )
    (if (> n max) primes
      (< n (caar pool))
        (next (1+ n) (append primes (cons n)) (append pool (cons (list (* n n) n))) )
        (next (1+ n) primes (sort (forEach (\ (e) (while (<= (car e) n) (@setCar e (apply + e)))) pool) :key car)) )))

(primes2 50)
