;;; fast fourier trasformation

(def\ (cx r . i) (list :complex r (opt? i 0d)))

(def\ (c+ (:complex rx ix) (:complex ry iy))
  (cx (+ rx ry) (+ ix iy)) )

(def\ (c- (:complex rx ix) (:complex ry iy))
  (cx (- rx ry) (- ix iy)) )

(def\ (c* (:complex rx ix) (:complex ry iy))
  (cx
    (- (* rx ry) (* ix iy))
    (+ (* ix ry) (* rx iy)) ))

(def\ (c/ (:complex rx ix) (:complex ry iy))
  (let1 (den (+ (* ry ry) (* iy iy)))
    (cx
      (/ (+ (* rx ry) (* ix iy)) den)
      (/ (- (* ix ry) (* rx iy)) den) )))

(def exp (let1 (exp (@getMethod Math "exp" &double)) (\ (x) (exp #null x))))
(def cos (let1 (cos (@getMethod Math "cos" &double)) (\ (x) (cos #null x))))
(def sin (let1 (sin (@getMethod Math "sin" &double)) (\ (x) (sin #null x))))

(def\ (cexp (:complex r i))
  (cx
   (* (exp r) (cos i))
   (* (exp r) (sin i)) ))

(def pi (.PI Math))

(def\ (evens lst)
  (if (null? lst) #null
    (cons (car lst) (evens (cddr lst))) ))

(def\ (odds lst)
  (if (null? lst) #null
    (cons (cadr lst) (odds (cddr lst))) ))

(def\ (fft x)
  (if (== (len x) 1) x 
    (let*
      ( (even (fft (evens x)))
        (odd (fft (odds x)))
        (k -1) ;(k (box -1))
        (aux (map (\ (j) (c* j (cexp (c/ (c* (cx 0d -2) (cx (* pi (++ k)))) (cx (len x)))))) odd)) )
      (append (map c+ even aux) (map c- even aux)) )))

(def\ (fftR . rs) (fft (map cx rs)))

(assert (fftR 1 1 1 1 0 0 0 0)
 '((:complex 4.0 0.0)
   (:complex 1.0 -2.414213562373095)
   (:complex 0.0 0.0)
   (:complex 1.0 -0.4142135623730949)
   (:complex 0.0 0.0)
   (:complex 0.9999999999999999 0.4142135623730949)
   (:complex 0.0 0.0)
   (:complex 0.9999999999999997 2.414213562373095) ))
