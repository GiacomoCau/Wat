
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