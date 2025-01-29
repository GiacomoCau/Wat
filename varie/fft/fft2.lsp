(def\ cx (r i) (list* :complex r i))

(def\ c+ ((:complex rx . ix) (:complex ry . iy))
  (cx (+ rx ry) (+ ix iy)) )

(def\ c- ((:complex rx . ix) (:complex ry . iy))
  (cx (- rx ry) (- ix iy)) )

(def\ c* ((:complex rx . ix) (:complex ry . iy))
  (cx
    (- (* rx ry) (* ix iy))
    (+ (* ix ry) (* rx iy)) ))

(def\ c/ ((:complex rx . ix) (:complex ry . iy))
  (let1 (den (+ (* ry ry) (* iy iy)))
    (cx
      (/ (+ (* rx ry) (* ix iy)) den)
      (/ (- (* ix ry) (* rx iy)) den) )))

(def exp (let1 (exp (@getMethod &java.lang.Math "exp" &double)) (\ (x) (exp #null x))))
(def cos (let1 (cos (@getMethod &java.lang.Math "cos" &double)) (\ (x) (cos #null x))))
(def sin (let1 (sin (@getMethod &java.lang.Math "sin" &double)) (\ (x) (sin #null x))))

(def\ exp (x) (@exp &java.lang.Math x))
(def\ cos (x) (@cos &java.lang.Math x))
(def\ sin (x) (@sin &java.lang.Math x))

(def\ cexp ((:complex r . i))
  (cx
   (* (exp r) (cos i))
   (* (exp r) (sin i)) ))

(def\ cabs ((:complex r . i))
  (sqrt (+ (* r r) (* i i))) )

(def pi (.PI &java.lang.Math))

(def\ evens (lst)
  (if (null? lst) #null
    (cons (car lst) (evens (cddr lst)))))

(def\ odds (lst)
  (if (null? lst) #null
      (cons (cadr lst) (odds (cddr lst)))))

(def\ fft (x)
  (if (== (len x) 1) x 
    (let*
      ( (even (fft (evens x)))
        (odd (fft (odds x)))
        (k -1)
        ;(++k (\ () (@set (%theEnv) 'k (+ k 1)) k))
        (++ (vau (k) e (eval (list 'begin (list 'def k (list '+ k 1)) k) (.parent e))))
        (aux (map (\ (j) (c* (cexp (c/ (c* (cx 0 -2) (cx (* pi (++ k)) 0)) (cx (len x) 0))) j)) odd)) )
      (append (map c+ even aux) (map c- even aux)) )))

(def\ fftR r (fft (map (\ (x) (cx x 0.0)) r)))

(assert (fftR 1 1 1 1 0 0 0 0)
  '((:complex 4.0 . 0.0)
    (:complex 1.0 . -2.414213562373095)
    (:complex 0.0 . 0.0)
    (:complex 1.0 . -0.4142135623730949)
    (:complex 0.0 . 0.0)
    (:complex 0.9999999999999999 . 0.4142135623730949)
    (:complex 0.0 . 0.0)
    (:complex 0.9999999999999997 . 2.414213562373095) ))



