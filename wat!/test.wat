;; -*- mode: scheme -*-

;;;; Utilities

(def\ (exit v)
  (takeSubcont %rootPrompt #ignore v) )

;;;;; Wat Test Suite

(assert (\))
(assert (\ 12 12))
(assert (\ "foo" "bar"))
(assert (def))
(assert (def 12))
(assert (def 12 12))

(assert (begin) #inert)
(assert (begin 1) 1)
(assert (begin 1 2) 2)


;;;; Delimited Control Operators Tests

;; adapted from 

(def\ (newPrompt) (list #null))

(def\ (abortPrompt p e)
  (takeSubcont p #ignore e))

(test test1
  (let ((p (newPrompt)))
    (pushPrompt p 1))
  1)
  
(test test2
  (let ((p (newPrompt)))
    (+ (pushPrompt p (pushPrompt p 5))
      4))
  9)
  
(test test3
  (let ((p (newPrompt)))
    (+ (pushPrompt p (+ (abortPrompt p 5) 6))
      4))
  9)

(test test31
  (let ((p (newPrompt)))
    (+ (pushPrompt p (pushPrompt p (+ (abortPrompt p 5) 6)))
      4))
  9)

(test test32
  (let ((p (newPrompt)))
    (let ((v (pushPrompt p
	       (let* ((v1 (pushPrompt p (+ (abortPrompt p 5) 6)))
                  (v1 (abortPrompt p 7)))
             (+ v1 10) ))))
      (+ v 20) ))
  27)

(test test33
  (let ((p (newPrompt)))
    (let ((v (pushPrompt p
           (let* ((v1 (pushPrompt p (+ 6 (abortPrompt p 5))))
                  (v1 (abortPrompt p 7)) )
             (+ v1 10) ))))
      (abortPrompt p 9)
      (+ v 20) ))
  )
; gives prompt not found: (#null)

;; (testCheck test331
;;   (let ((p (newPrompt)))
;;     (let ((v (pushPrompt p
;; 	       (let* ((v1 (pushPrompt p (+ (abortPrompt p 5) 6)))
;;            (v1 (abortPrompt p 7)))
;;       (+ v1 10)))))
;;       (promptSet? p))) ; give unbound: promptSet?
;;   #f)

(test test4
  (let ((p (newPrompt)))
    (+ (pushPrompt p 
	     (+ (takeSubcont p sk (pushPrompt p (pushSubcont sk 5)))
	        10) )
       20) )
  35 )

(def\ (shift p f) 
  (takeSubcont p sk
    (pushPrompt p
      (f (\ (c)
           (pushDelimSubcont p sk (c)) )))))

(test test5
  (+ (pushPrompt 'p0
       (+ (shift 'p0 (\ (sk)
                       (+ 100 (sk (\ () (sk (\ () 3))))) ))
          2))
     10)
  117)

(test test51
  (+ 10 (pushPrompt 'p0
          (+ 2 (shift 'p0 (\ (sk)
                            (sk (\ () (+ 3 100))))))))
  115)

(def\ (abortSubcont prompt value)
  (takeSubcont prompt #ignore value))

(test test52
  (+ (pushPrompt 'p0
       (+ (shift 'p0 (\ (sk)
                       (+ (sk (\ ()
                                (pushPrompt 'p1
                                  (+ 9 (sk (\ ()
                                             (abortSubcont 'p1 3)))))))
                          100)))
          2))
     10)
  115)

(test test53
  (+ (pushPrompt 'p0
       (let ((v (shift 'p0 (\ (sk)
                             (+ (sk (\ ()
                                      (pushPrompt 'p1
                                        (+ 9 (sk (\ ()
                                                   (abortSubcont 'p1 3)))))))
                                100)))))
         (+ v 2)))
     10)
  115)

(test test54
  (+ (pushPrompt 'p0
       (let ((v (shift 'p0 (\ (sk)
                             (+ (sk (\ ()
                                      (pushPrompt 'p1
                                        (+ 9 (sk (\ ()
                                                   (abortSubcont 'p0 3)))))))
                                100)))))
         (+ v 2)))
     10)
  124)

(test test6
  (+ (let ((pushTwice (\ (sk)
              (pushSubcont sk (pushSubcont sk 3)))))
       (pushPrompt 'p1
         (pushPrompt 'p2
           (+ (takeSubcont 'p1 sk
                (pushTwice sk))
              1))))
     10)
  15)

(test test7
  (+ (let ((pushTwice (\ (sk)
              (pushSubcont sk
                (pushSubcont sk
                  (takeSubcont 'p2 sk2
                    (pushSubcont sk2
                      (pushSubcont sk2 3))))))))
       (pushPrompt 'p1
         (+ (pushPrompt 'p2
              (+ 10 (pushPrompt 'p3
                      (takeSubcont 'p1 sk (pushTwice sk)))))
            1)))
     100)
  135)

(test test71
  (+ (let ((pushTwice (\ (sk)
              (sk (\ ()
                    (sk (\ ()
                          (shift 'p2 (\ (sk2)
                                       (sk2 (\ ()
                                              (sk2 (\ () 3)))))))))))))
       (pushPrompt 'p1
         (+ (pushPrompt 'p2
              (+ 10 (pushPrompt 'p3
                      (shift 'p1 (\ (sk) (pushTwice sk))))))
            1)))
     100)
  135)


;;;; Dynamic Binding Tests

(test defdynamic.1
  (begin
    (ddef* (x y) 1 (+ 1 1))
    (assert (dval x) 1)
    (assert (dval y) 2)
    (dlet* ((x 3))
      (assert (dval x) 3)
      (assert (dval y) 2)
      (dlet* ((y 4))
        (assert (dval x) 3)
        (assert (dval y) 4))
      (assert (dval x) 3)
      (assert (dval y) 2))
    (assert (dval x) 1)
    (assert (dval y) 2))
  #t)
  
(test defdynamic.redefine
  (begin  
    (ddef a (+ 1 1))
    (def oa a)
    (assert (dval a) 2)
    (assert (dval oa) 2)
    (ddef a (+ 2 2))
    (assert (dval a) 4)
    (assert (dval oa) 4)
    (assert (eq? oa a) #t)
    (ddef a #null)
    (assert (dval a) #null)
    (assert (eq? oa a) #t) )
  #t )

(test progv.1
  (begin
    (ddef* (*x* *y*) 1 2)
    (assert (dval *x*) 1)
    (assert (dval *y*) 2)
    (progv (*x*) (3)
      (assert (dval *x*) 3)
      (assert (dval *y*) 2)
      (progv (*y*) (4)
        (assert (dval *x*) 3)
        (assert (dval *y*) 4) )
      (assert (dval *x*) 3)
      (assert (dval *y*) 2) )
    (assert (dval *x*) 1)
    (assert (dval *y*) 2) )
  #t )
    
(test dynamic.1
  (begin
    (ddef foo)
    (assert (== (.value foo) #null) #t)
    (assert (== (dval foo) #null) #t)
    (assert (== (foo) #null) #t)
    (assert (type? foo DVar) #t)
    (assert (type? foo Object) #t)
    (assert (subClass? DVar Object) #t) )
  #t )

(test set-dynamic.1
  (begin
    (ddef *bar* #null)
    (dlet ((*bar* 1))
      (dval *bar* 2)
      (assert (dval *bar*) 2)
      (dlet ((*bar* 3))
        (assert (dval *bar*) 3) )
      (assert (dval *bar*) 2)
      (dval *bar* 4)
      (assert (dval *bar*) 4) )
    (assert (dval *bar*) #null) )
  #t )

(test dynamic-let*.1
  (dlet* () (+ 1 1))
  2)

(test dynamic-let*.2
  (begin
    (ddef *x* 1)
    (dlet* ((*x* 2)) (+ 1 (dval *x*))))
  3)

(test dynamic-let*.2
  (begin
    (ddef* (*x* *y*) 1 0)
    (dlet* ((*x* 2) (*y* (+ (dval *x*) 1)))
      (list (dval *x*) (dval *y*))))
  '(2 3))

(test dynamic-let-sanity-check
  (begin
    (ddef* (*x* *y*) 1 0)
    (dlet ((*x* 2) (*y* (+ (dval *x*) 1)))
      (list (dval *x*) (dval *y*))))
  '(2 2) )

(assert (catchTag 'a (throwTag 'a)) #inert)
(assert (catchTag 'a (throwTag 'a 2)) 2)

(assert (atEnd (== 1 1)) Error :type 'match :operands# +1)
(assert (atEnd () (== 1 1)) #t)

(assert (finally (== 1 1)) #t)
(assert (begin (+ (finally 1 2 3 (def x 10)) x)) 11)
(assert (+ (catchTag 'a (finally (throwTag 'a 1) 2 3 (def x 10))) 10) 11)
(assert (catchTag 'a (finally 1 2 3 (throwTag 'a 4))) 4)
(assert (catchTag 'a (finally (throwTag 'a 1) 2 3 (throwTag 'a (+ 2 2)))) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def\ (combine cmb ops) (apply (wrap cmb) ops))

(assert (combine and (list (== 1 1) (== 2 2))) #t)
(assert (combine and (list (!= 1 1) (== 2 2))) #f)

(assert (apply (\ (x) x) (list 2)) 2)

(let1 (v (vau () #ignore))  (%assert (unwrap v) v))


#| TODO da rivedere
(let ((obj (object ("x" 1))))
  (set (.x obj) 2)
  (assertEqual 2 (.x obj))
  ;(set (@ obj "x") 3) ; give not a combiner: [object Undefined] in: (3 obj "x")
  (set (.x obj) 3)
  (assertEqual 3 (.x obj)) )

(assertEqual &x #undefined)
(set &x 2)
(assertEqual &x 2)
|#

(assert (* 1 2 3 4) 24)
(assert (*) 1)
(assert (* 3) 3)
(assert (+ 1 2 3 4) 10)
(assert (+) 0)
(assert (+ 1) 1)

(assert (- 5) -5)
(assert (- 10 5 2) 3)
(assert (/ 5) (/ 1 5))
(assert (/ 54 2 3) 9)

(assert (toString (reverse (list 3 2 1))) (toString (list 1 2 3)))

(assert (log "logging" 1 2 3) "logging")

(assert (and (== 1 1) (== 4 4) (== 5 5)) #t)
(assert (and (== 1 1) (== 4 4) (== 5 10)) #f)
(assert (or (== 1 1) (== 4 10) (== 5 5)) #t)
(assert (or (== 1 10) (== 4 10) (== 5 5)) #t)

(assert (== 4 (+ 2 2) (- 6 2)) #t)
(assert (< 1 2 3 4 5) #t)
(assert (< 1 2 3 4 5 1) #f)
(assert (<= 1 1 2 3 4 5 5) #t)
(assert (< 1 1 2 3 4 5 5) #f)

(exit "finito")

#| TODO da rivedere
(let ((x (cell 0)))
  (while (< (ref x) 10)
    (++ (ref x)))
  (assertEqual 10 (ref x)) )
|#
