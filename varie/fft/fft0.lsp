(def\ cx (x y) (cons x y))

(def\ c+ (x y)
  (cx (+ (car x) (car y)) (+ (cdr x) (cdr y))) )

(def\ c- (x y)
  (cx (- (car x) (car y)) (- (cdr x) (cdr y))) )

(def\ c* (x y)
  (cx
    (- (* (car x) (car y)) (* (cdr x) (cdr y)))
    (+ (* (cdr x) (car y)) (* (car x) (cdr y))) ))

(def\ c/ (x y)
  (let1 (den (+ (* (car y) (car y)) (* (cdr y) (cdr y))))
    (cx
      (/ (+ (* (car x) (car y)) (* (cdr x) (cdr y))) den)
      (/ (- (* (cdr x) (car y)) (* (car x) (cdr y))) den) )))

(def exp (let1 (exp (@getMethod &java.lang.Math "exp" &double)) (\ (x) (exp #null x))))
(def cos (let1 (cos (@getMethod &java.lang.Math "cos" &double)) (\ (x) (cos #null x))))
(def sin (let1 (sin (@getMethod &java.lang.Math "sin" &double)) (\ (x) (sin #null x))))


(def\ exp (x) (@exp &java.lang.Math x))
(def\ cos (x) (@cos &java.lang.Math x))
(def\ sin (x) (@sin &java.lang.Math x))

(def\ cexp (x)
  (cx
   (* (exp (car x)) (cos (cdr x)))
   (* (exp (car x)) (sin (cdr x)))))

(def\ cabs (x)
  (sqrt (+ (* (car x) (car x)) (* (cdr x) (cdr x)))))

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
  '((4.0 . 0.0)
    (1.0 . -2.414213562373095)
    (0.0 . 0.0)
    (1.0 . -0.4142135623730949)
    (0.0 . 0.0)
    (0.9999999999999999 . 0.4142135623730949)
    (0.0 . 0.0)
    (0.9999999999999997 . 2.414213562373095) ))



