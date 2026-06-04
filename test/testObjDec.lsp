
(assert ((\ ((a b)) (+ a b)) (newObj :a 1 :b 2)) 3)
(assert ((\ (((#: Integer a) b)) (+ a b)) (newObj :a 1 :b 2)) 3)
(assert ((\ ((a b)) (+ a b)) (array 1 2 3)) 3)
(assert ((\ ((a b .length)) (+ a b length)) (array 1 2 3)) 6)
(assert ((\ ((@getMessage)) getMessage) (new Error "abc")) "abc")
(assert ((\ ((@doubleValue)) doubleValue) 1) 1.0)
(let1 (o (newObj :a 1 :b 2)) (assert ((\ ((a b . o)) o) o) o))
(assert ((\ ((a b . o)) o) (array 1 2 3)) (array 3))

(assert ((\ ((#: Obj a b)) (+ a b)) (newObj :a 1 :b 2)) 3)
(assert ((\ ((#: Obj (#: Integer a) b)) (+ a b)) (newObj :a 1 :b 2)) 3)
(assert ((\ ((#: Object[] a b)) (+ a b)) (array 1 2 3)) 3)

(assert ((\ ((#: Integer @intValue @doubleValue)) doubleValue) 1) 1.0)
(assert ((\ ((@intValue @doubleValue)) doubleValue) 1) 1.0)

(assert ((\ ((@intValue @doubleValue . i)) i) 1) 1)
(assert ((\ ((#: Integer @intValue @doubleValue . i)) i) 1) 1)

(begenv 
  (def obj (newObj :a 1 :b 2 :c 3))
  
  (assert ((\ ((#: Obj b c)) (+ b c)) obj) 5)
  (assert ((\ ((#: (matchType? Obj) b c)) (+ b c)) obj) 5)
  (assert ((\ ((#: (matchType? Obj :a 1) b c)) (+ b c)) obj) 5)
  (assert ((\ ((#: (matchType? Obj :a (and Integer (>= 1))) b c)) (+ b c)) obj) 5)

  (assert ((\ ((#: Obj b c . o)) o) obj) obj)
  (assert ((\ ((#: (matchType? Obj) b c . o)) o) obj) obj)
  (assert ((\ ((#: (matchType? Obj :a 1) b c . o)) o) obj) obj)
  (assert ((\ ((#: (matchType? Obj :a (and Integer (>= 1))) b c . o)) o) obj) obj)

  (assert ((\ ((#: (matchType? Obj :a (and Integer (>= 1))) b c . o)) (+ b c)) obj) 5)
  
;(def\ (p? o e f v) (f (o e) v))
;(assert ((\ ((#: (and Obj (f? :a >= 1)) a . o)) a) (newObj :a 1)) 1)

;(def\ (p? o e) (check? (o e) (and Integer (>= 1))))
;(assert ((\ ((#: (and Obj (p? :a)) a . o)) a) (newObj :a 1)) 1)

;  ((\ ((#: (and Obj (f? :a (\ (e) (: (and Integer (>= 1)) e)))) a . o)) a) (newObj :a 1))
  
  (def box (newBox 1))
  (assert ((\ ((#: Box a . b)) a)  box) 1)
  (assert ((\ ((#: (matchType? Box) a . b)) a)  box) 1)
  (assert ((\ ((#: (matchType? Box (>= 1)) a . b)) a)  box) 1)
  (assert ((\ ((#: (matchType? Box (>= 1)) a . b)) b)  box) box)
)
