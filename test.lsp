;; -*- mode: scheme -*-

;;;; Utilities

(define (exit v)
  (takeSubcont %rootPrompt #ignore v) )

;;;;; Wat Test Suite

(assert (lambda))
(assert (lambda 12 12))
(assert (lambda "foo" "bar"))
(assert (define))
(assert (define 12))
(assert (define 12 12))

(assert (begin) #inert)
(assert (begin 1) 1)
(assert (begin 1 2) 2)

;;;; Delimited Dynamic Binding Tests

;; adapted from 

(define (newPrompt) (list #null))

(define (abortPrompt p e)
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
		 (+ v1 10)))))
      (+ v 20)))
  27)

(test test33
  (let ((p (newPrompt)))
    (let ((v (pushPrompt p (let*
	    ((v1 (pushPrompt p (+ 6 (abortPrompt p 5))))
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
;; 		      (v1 (abortPrompt p 7)))
;; 		 (+ v1 10)))))
;;       (promptSet? p))) ; give unbound: promptSet?
;;   #f)

(test test4
  (let ((p (newPrompt)))
    (+ (pushPrompt p 
	     (+ (takeSubcont p sk (pushPrompt p (pushSubcont sk 5)))
	        10) )
       20) )
  35 )

(define (shift p f) 
  (takeSubcont p sk
    (pushPrompt p
      (f (lambda (c)
           (pushPromptSubcont p sk (c)) )))))

(test test5
  (+ (pushPrompt 'p0
       (+ (shift 'p0 (lambda (sk)
                       (+ 100 (sk (lambda () (sk (lambda () 3))))) ))
          2))
     10)
  117)

(test test51
  (+ 10 (pushPrompt 'p0
          (+ 2 (shift 'p0 (lambda (sk)
                            (sk (lambda () (+ 3 100))))))))
  115)

(define (abortSubcont prompt value)
  (takeSubcont prompt #ignore value))

(test test52
  (+ (pushPrompt 'p0
       (+ (shift 'p0 (lambda (sk)
                       (+ (sk (lambda ()
                                (pushPrompt 'p1
                                  (+ 9 (sk (lambda ()
                                             (abortSubcont 'p1 3)))))))
                          100)))
          2))
     10)
  115)

(test test53
  (+ (pushPrompt 'p0
       (let ((v (shift 'p0 (lambda (sk)
                             (+ (sk (lambda ()
                                      (pushPrompt 'p1
                                        (+ 9 (sk (lambda ()
                                                   (abortSubcont 'p1 3)))))))
                                100)))))
         (+ v 2)))
     10)
  115)

(test test54
  (+ (pushPrompt 'p0
       (let ((v (shift 'p0 (lambda (sk)
                             (+ (sk (lambda ()
                                      (pushPrompt 'p1
                                        (+ 9 (sk (lambda ()
                                                   (abortSubcont 'p0 3)))))))
                                100)))))
         (+ v 2)))
     10)
  124)

(test test6
  (+ (let ((pushTwice (lambda (sk)
              (pushSubcont sk (pushSubcont sk 3)))))
       (pushPrompt 'p1
         (pushPrompt 'p2
           (+ (takeSubcont 'p1 sk
                (pushTwice sk))
              1))))
     10)
  15)

(test test7
  (+ (let ((pushTwice (lambda (sk)
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
  (+ (let ((pushTwice (lambda (sk)
              (sk (lambda ()
                    (sk (lambda ()
                          (shift 'p2 (lambda (sk2)
                                       (sk2 (lambda ()
                                              (sk2 (lambda () 3)))))))))))))
       (pushPrompt 'p1
         (+ (pushPrompt 'p2
              (+ 10 (pushPrompt 'p3
                      (shift 'p1 (lambda (sk) (pushTwice sk))))))
            1)))
     100)
  135)

|#
(defineOperative (block blockName . forms) env
  (let ((tag (list #null))) ; cons up a fresh object as tag
    (let ((escape (lambda (value) (throwTag tag value))))
      (catchTag tag
        (eval (list (list* lambda (list blockName) forms)
                    escape)
              env)))))

(define (returnFrom blockName . value?)
    (blockName (optional value?)))

(define optional (lambda value? (if (== value? ()) () (car value?))))
(assert (optional) ())
(assert (optional 1) 1)

(define unwindProtect finally)
#|

(assert (catchTag a (throwTag a)) #inert)
(assert (catchTag a (throwTag a 2)) 2)

(assert (finally (== 1 1)) #t)
(assert (begin (+ (finally 1 2 3 (define x 10)) x)) 11)
(assert (+ (catchTag a (finally (throwTag a 1) 2 3 (define x 10))) x) 11)
(assert (catchTag a (finally 1 2 3 (throwTag a 4))) 4)
(assert (catchTag a (finally (throwTag a 1) 2 3 (throwTag a (+ 2 2)))) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (combine cmb ops) (apply (wrap cmb) ops))

(assert (combine and (list (== 1 1) (== 2 2))) #t)
(assert (combine and (list (!= 1 1) (== 2 2))) #f)

(assert (apply (lambda (x) x) (list 2)) 2)

(assert (unwrap ($vau () #ignore)))

|#
(let ((obj (object ("x" 1))))
  (set (.x obj) 2)
  (assertEqual 2 (.x obj))
  ;(set (@ obj "x") 3) ; give not a combiner: [object Undefined] in: (3 obj "x")
  (set (.x obj) 3)
  (assertEqual 3 (.x obj)) )

(assertEqual &x #undefined)
(set &x 2)
(assertEqual &x 2)
#|

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

(assert (toString (reverseList (list 3 2 1))) (toString (list 1 2 3)))

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

(define (testTco n) (if (<= n 0) n (testTco (- n 1))))
(assert (testTco 400) 0)

(exit "finito")

|#
(let ((x (cell 0)))
  (while (< (ref x) 10)
    (++ (ref x)))
  (assertEqual 10 (ref x)) )
#|
