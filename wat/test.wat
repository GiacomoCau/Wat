;; -*- mode: scheme -*-

;;;; Utilities

(define (exit v)
  (take-subcont root-prompt #ignore v) )

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

(define (new-prompt) (list #null))

(define (abort-prompt p e)
  (take-subcont p #ignore e))

(test 'test1
  (let ((p (new-prompt)))
    (push-prompt p 1))
  1)
  
(test 'test2
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p 5))
      4))
  9)
  
(test 'test3
  (let ((p (new-prompt)))
    (+ (push-prompt p (+ (abort-prompt p 5) 6))
      4))
  9)

(test 'test3-1
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p (+ (abort-prompt p 5) 6)))
      4))
  9)

(test 'test3-2
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abort-prompt p 5) 6)))
		      (v1 (abort-prompt p 7)))
		 (+ v1 10)))))
      (+ v 20)))
  27)

(test 'test3-3
  (let ((p (new-prompt)))
    (let ((v (push-prompt p (let*
	    ((v1 (push-prompt p (+ 6 (abort-prompt p 5))))
		 (v1 (abort-prompt p 7)) )
		(+ v1 10) ))))
      (abort-prompt p 9)
      (+ v 20) ))
  )
; gives prompt not found: (#null)

;; (test-check 'test3-3-1
;;   (let ((p (new-prompt)))
;;     (let ((v (push-prompt p
;; 	       (let* ((v1 (push-prompt p (+ (abort-prompt p 5) 6)))
;; 		      (v1 (abort-prompt p 7)))
;; 		 (+ v1 10)))))
;;       (prompt-set? p))) ; give unbound: prompt-set?
;;   #f)

(test 'test4
  (let ((p (new-prompt)))
    (+ (push-prompt p 
	     (+ (take-subcont p sk (push-prompt p (push-subcont sk 5)))
	        10) )
       20) )
  35 )

(define (shift p f) 
  (take-subcont p sk
    (push-prompt p
      (f (lambda (c)
           (push-prompt-subcont p sk (c)) )))))

(test 'test5
  (+ (push-prompt 'p0
       (+ (shift 'p0 (lambda (sk)
                       (+ 100 (sk (lambda () (sk (lambda () 3))))) ))
          2))
     10)
  117)

(test 'test5-1
  (+ 10 (push-prompt 'p0
          (+ 2 (shift 'p0 (lambda (sk)
                            (sk (lambda () (+ 3 100))))))))
  115)

(define (abort-subcont prompt value)
  (take-subcont prompt #ignore value))

(test 'test5-2
  (+ (push-prompt 'p0
       (+ (shift 'p0 (lambda (sk)
                       (+ (sk (lambda ()
                                (push-prompt 'p1
                                  (+ 9 (sk (lambda ()
                                             (abort-subcont 'p1 3)))))))
                          100)))
          2))
     10)
  115)

(test 'test5-3
  (+ (push-prompt 'p0
       (let ((v (shift 'p0 (lambda (sk)
                             (+ (sk (lambda ()
                                      (push-prompt 'p1
                                        (+ 9 (sk (lambda ()
                                                   (abort-subcont 'p1 3)))))))
                                100)))))
         (+ v 2)))
     10)
  115)

(test 'test5-4
  (+ (push-prompt 'p0
       (let ((v (shift 'p0 (lambda (sk)
                             (+ (sk (lambda ()
                                      (push-prompt 'p1
                                        (+ 9 (sk (lambda ()
                                                   (abort-subcont 'p0 3)))))))
                                100)))))
         (+ v 2)))
     10)
  124)

(test 'test6
  (+ (let ((push-twice (lambda (sk)
              (push-subcont sk (push-subcont sk 3)))))
       (push-prompt 'p1
         (push-prompt 'p2
           (+ (take-subcont 'p1 sk
                (push-twice sk))
              1))))
     10)
  15)

(test 'test7
  (+ (let ((push-twice (lambda (sk)
              (push-subcont sk
                (push-subcont sk
                  (take-subcont 'p2 sk2
                    (push-subcont sk2
                      (push-subcont sk2 3))))))))
       (push-prompt 'p1
         (+ (push-prompt 'p2
              (+ 10 (push-prompt 'p3
                      (take-subcont 'p1 sk (push-twice sk)))))
            1)))
     100)
  135)

(test 'test7-1
  (+ (let ((push-twice (lambda (sk)
              (sk (lambda ()
                    (sk (lambda ()
                          (shift 'p2 (lambda (sk2)
                                       (sk2 (lambda ()
                                              (sk2 (lambda () 3)))))))))))))
       (push-prompt 'p1
         (+ (push-prompt 'p2
              (+ 10 (push-prompt 'p3
                      (shift 'p1 (lambda (sk) (push-twice sk))))))
            1)))
     100)
  135)

|#
(define-operative (block block-name . forms) env
  (let ((tag (list #null))) ; cons up a fresh object as tag
    (let ((escape (lambda (value) (throw-tag tag value))))
      (catch-tag tag
        (eval (list (list* lambda (list block-name) forms)
                    escape)
              env)))))

(define (return-from block-name . value?)
    (block-name (optional value?)))

(define optional (lambda value? (if (== value? ()) () (car value?))))
(assert (optional) ())
(assert (optional 1) 1)

(define unwind-protect finally)
#|

(assert (catch-tag a (throw-tag a)) #inert)
(assert (catch-tag a (throw-tag a 2)) 2)

(assert (finally (== 1 1)) #t)
(assert (begin (+ (finally 1 2 3 (define x 10)) x)) 11)
(assert (+ (catch-tag a (finally (throw-tag a 1) 2 3 (define x 10))) x) 11)
(assert (catch-tag a (finally 1 2 3 (throw-tag a 4))) 4)
(assert (catch-tag a (finally (throw-tag a 1) 2 3 (throw-tag a (+ 2 2)))) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (combine cmb ops) (apply (wrap cmb) ops))

(assert (combine and (list (== 1 1) (== 2 2))) #t)
(assert (combine and (list (!= 1 1) (== 2 2))) #f)

(assert (apply (lambda (x) x) (list 2)) 2)

(assert (unwrap ($vau () #ignore)))

|#
(let ((obj (object ("x" 1))))
  (set (.x obj) 2)
  (assert-equal 2 (.x obj))
  ;(set (@ obj "x") 3) ; give not a combiner: [object Undefined] in: (3 obj "x")
  (set (.x obj) 3)
  (assert-equal 3 (.x obj)) )

(assert-equal &x #undefined)
(set &x 2)
(assert-equal &x 2)
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

(assert (toString (reverse-list (list 3 2 1))) (toString (list 1 2 3)))

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

(define (test-tco n) (if (<= n 0) n (test-tco (- n 1))))
(assert (test-tco 400) 0)

(exit "finito")

|#
(let ((x (cell 0)))
  (while (< (ref x) 10)
    (++ (ref x)))
  (assert-equal 10 (ref x)) )
#|
