;

(%assert () #null)
(%assert #null ())

(%assert #ignore #_)
(%assert #_ #ignore)

(%assert (%' (a b)) '(a b))
(%assert '(a b) (%' (a b)))

(%assert (%list-) ())
(%assert (%list- ()) ())
(%assert (%list- 1) '(1))
(%assert (%list- 1 2) '(1 2))
(%assert (%list- 1 2 ()) '(1 2))
(%assert (%list- 1 2 '(3 4)) '(1 2 (3 4)))

(%assert (%def) &Wat.Vm$Error :type 'match :operands +2)
(%assert (%def a) &Wat.Vm$Error :type 'match :operands +1) 
(%assert (%def a 1) #inert) ; a=1
(%assert (%def 1 1) &Wat.Vm$Error :type 'type :datum 1 :expected '(or Symbol Cons))
(%assert (%def "a" 1) &Wat.Vm$Error :type 'type :datum "a" :expected '(or Symbol Cons))
(%assert (%def a 1 2) &Wat.Vm$Error :type 'type :datum 1 :expected '(or Null Inert :rhs :prv))
 
(%assert (%begin (%def (a) (%list 1)) a)         1)
(%assert (%begin (%def (a b) (%list 1 2)) b)     2)
(%assert (%begin (%def (a . b) (%list 1 2)) b)   '(2))
(%assert (%begin (%def (a . b) (%list 1 2 3)) b) '(2 3))

(%assert (%def (a))           ) ; throw
(%assert (%def (a) 1)         ) ; throw
(%assert (%def (a) 1 2)       ) ; throw
(%assert (%def (a b) 1 2)     ) ; throw
(%assert (%def (a . b) 1 2)   ) ; throw       
(%assert (%def (a . b) 1 2 3) ) ; throw
(%assert (%def (a . a) 1 2 3) ) ; throw

(%assert (%begin)     #inert)
(%assert (%begin 1)   1)
(%assert (%begin 1 2) 2)

(%assert (%if)           ) ;throw
(%assert (%if #t)        ) ;throw
(%assert (%if #t 1)     1)
(%assert (%if #f 1)     #inert)
(%assert (%if #t 1 2)   1)
(%assert (%if #f 1 2)   2)
(%assert (%if #f 1 2 3) 3)

(%assert ((%vau))                 ) ;throw
(%assert ((%vau a))               ) ;throw
(%assert ((%vau a #ignore)) #inert)

(%assert ((%vau a #ignore a))     ())
(%assert ((%vau a #ignore a) 1)   '(1))
(%assert ((%vau a #ignore a) 1 2) '(1 2))
(%assert ((%vau a #ignore b))     ) ;throw
(%assert ((%vau (a) #ignore a) 1) 1)
(%assert ((%vau a #ignore 1 a) 1) '(1))

(%assert ((%vau 1 #ignore 1))   ) ;throw
(%assert ((%vau "a" #ignore 1)) ) ;throw
(%assert ((%vau a a 1))         ) ;throw
(%assert ((%vau (a . a) e 1))   ) ;throw
(%assert ((%vau a #ignore 1 2 3 4 5 a) 6) '(6))
(%assert ((%vau (a) #ignore 1 2 3 4 5 a) 6) 6)

(%assert ((%\ (m)  ) 1) #inert)
(%assert ((%\ (m) m) 1) 1)
(%assert ((%\ x x) 1)   '(1))

(%if (ctApv)
  (%begin 
    (%assert (%catchTagWth))
    (%assert (%catchTagWth #null))
    (%assert (%catchTagWth #null () (%\ () 1)) 1)
    (%assert (%catchTagWth #null (%\ a 2) (%\ () 1)))
    (%assert (%catchTagWth #null (%\ (a) 2) (%\ () 1)) 1)

    (%assert (%catchTagWth #_ () (%\ () (%throwTag #ignore))) #inert)
    (%assert (%catchTagWth #_ () (%\ () (%throwTag 'a))) #inert)
    (%assert (%catchTagWth 'a () (%\ () (%throwTag 'a))) #inert)
    (%assert (%catchTagWth #_ () (%\ () (%throwTag #ignore 1))) 1)
    (%assert (%catchTagWth #_ () (%\ () (%throwTag 'a 1))) 1)
    (%assert (%catchTagWth 'a () (%\ () (%throwTag 'a 1))) 1)
    (%assert (%catchTagWth #_ (%\ (x) 2) (%\ () (%throwTag #ignore 1))) 2)
    (%assert (%catchTagWth #_ (%\ (x) 2) (%\ () (%throwTag 'a 1))) 2)
    (%assert (%catchTagWth 'a (%\ (x) 2) (%\ () (%throwTag 'a 1)) ) 2)

    (%assert (%catchTagWth #ignore () (%\ () (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #ignore) (%def x (%+ x 1))))))) #inert)
    (%assert (%catchTagWth #ignore () (%\ () (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #ignore x) (%def x (%+ x 1))))))) 10)
    (%assert (%catchTagWth #ignore () (%\ () (%def x 0) (%loop (%if (%== x 10) (%throwTag #ignore x) (%def x (%+ x 1)))))) 10)
    (%assert (%catchTagWth #ignore () (%\ () (%def x 0) (%loop (%if (%== x 10) (%throwTag #ignore x)) (%def x (%+ x 1))))) 10) )
  
  (%begin 
    (%assert (%catchTagWth))
    (%assert (%catchTagWth #null))
    (%assert (%catchTagWth #null #null 1) 1)
    (%assert (%catchTagWth #null #null 1 2) 2)
    (%assert (%catchTagWth #null (%\ a 2) 1) 1)

    (%assert (%catchTagWth #ignore #null (%throwTag #ignore)) #inert)
    (%assert (%catchTagWth #ignore #null (%throwTag 'a)) #inert)
    (%assert (%catchTagWth 'a #null (%throwTag 'a)) #inert)
    (%assert (%catchTagWth #ignore #null (%throwTag #ignore 1)) 1)
    (%assert (%catchTagWth #ignore #null (%throwTag 'a 1)) 1)
    (%assert (%catchTagWth 'a #null (%throwTag 'a 1)) 1)
    (%assert (%catchTagWth #ignore (%\ (x) 2) (%throwTag #ignore 1)) 2)
    (%assert (%catchTagWth #ignore (%\ (x) 2) (%throwTag 'a 1)) 2)
    (%assert (%catchTagWth 'a (%\ (x) 2) (%throwTag 'a 1)) 2)

    (%assert (%catchTagWth #ignore #null (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #ignore) (%def x (%+ x 1))))))) #inert)
    (%assert (%catchTagWth #ignore #null (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #ignore x) (%def x (%+ x 1))))))) 10)
    (%assert (%catchTagWth #ignore #null (%begin (%def x 0) (%loop (%if (%== x 10) (%throwTag #ignore x) (%def x (%+ x 1)))))) 10)
    (%assert (%catchTagWth #ignore #null (%begin (%def x 0) (%loop (%if (%== x 10) (%throwTag #ignore x)) (%def x (%+ x 1))))) 10) ))

(%def testTco (%\ (n) (%if (%<= n 0) n (testTco (%- n 1)))))
(%if (doTco) (%assert (testTco 400) 0))