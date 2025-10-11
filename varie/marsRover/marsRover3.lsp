
;; Cerimonies

(def\ (<- env . keys)
  (if (null? keys) env
    (apply** <- (env (car keys)) (cdr keys)))) 

(defVau (clse extend . bindings) env
  (def\ (obje bindings super)
    (def this (newEnv super))
    (forEach (\ ((name . #ignore)) (this name #inert)) bindings)
    (forEach (\ ((name . forms))
      (if (cons? name)
        (this (car name) (eval (list* '\ (cdr name) forms) this)) ;; method definition
        (this name (eval (cons 'begin forms) this)) )) ;; attribute definition
      bindings )
    (this :cnt :this this) ) ;; anaphoric this
  (\ args
    (unless (null? extend) (set! env (newe (eval extend env))) )
    (def obj (obje bindings env))
    (let1 (new (@remove (.map obj) "new")) (unless (null? new) (apply new args obj))) ;; new invocation
    obj ))

(def\ (newe cls . args) (apply cls args)) 

(let ()
  (def A (clse () (a 1) (B (clse () ((new b) (this :b b)) (b 2) ((f c) (+ a b c))))))
  (def objA (newe A))
  (def objB (newe (objA :B) 5)) 
  (assert ((objB :f) 4) 10)
)
(let ()  
  (def C (clse () (a 1)))
  (def D (clse C (b 2)))
  (def E (clse D (c 3) ((f d) (+ a b c d))))
  (def objC (newe C))
  (def objD (newe D))
  (def objE (newe E))
  (assert ((objE :f) 4) 10)
)


;; Mars Rover Kata 

(def Plateau (clse ()
  (mxX 0) (mxY 0) (mnX 0) (mnY 0)
  ((new mxX mxY) (this :mxX mxX :mxY mxY))
  (Rover (clse ()
    (x 0) (y 0) (i 0) ;; i: 0:Nord 1:Est 2:Sud 3:Ovest
    ((new x y d) (this :x x :y y :i (->i d)))
    (set (case\
      ((x y d)
       	(if (|| (< x mnX) (> x mxX)) (throw (@new &java.lang.IllegalStateException "illegal x!")))
		(if (|| (< y mnY) (> y mxY)) (throw (@new &java.lang.IllegalStateException "illegal y!")))
        (this :x x :y y :i (->i d)) )
      ((xi yi di s xf yf df) (set xi yi di) (cmd s) (chk xf yf df) (toString)) ))
    ((m) (case i
      (0 (if (< y mxY) (+= y 1)))
      (1 (if (< x mxX) (+= x 1)))
      (2 (if (> y mnY) (-= y 1)))
      (3 (if (> x mnX) (-= x 1))) ))
    ((toString) ($ x " " y " " (->d i)))
    (log #f)
    ((cmd s)
      (def s (@toLowerCase s))
      (for1 (i 0 (1+ i)) (< i (@length s))
        (case (def c :rhs (@charAt s i)) (#\l (l)) (#\r (r)) (#\m (m)))
        (when log (print (if (== c #\ ) "  " ($ c ":")) " " (toString))) )
      (toString) )
    ((chk xn yn dn)
      (unless (&& (== x xn) (== y yn) (== i (->i dn)))
        (print "c: " xn " " yn " " dn ": " #f) ))
    ((l) (set! i (if (== i 0) 3 (-1+ i))))
    ((r) (set! i (if (== i 3) 0 (1+ i))))
    ((->i d) (prog1 (def i :rhs (@indexOf "NESO" d)) (when (== i -1) (throw (@new &java.lang.IllegalStateException "illegal d!")))))
    ((->d i) (@charAt "NESO" i)) )) ))

(def pl (newe Plateau 5 5))
(def rr (newe (pl :Rover) 1 2 "N"))
((rr :cmd) "LMLMLMLMM")
((rr :chk) 1 3 "N")
((rr :set) 3 3 "E" "MMRMMRMRRM" 5 1 "E")
