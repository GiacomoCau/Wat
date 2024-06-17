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

(%assert (%def)             &Wat.Vm$Error :type 'match :operands# +2)
(%assert (%def 1)           &Wat.Vm$Error :type 'match :operands# +1) 
(%assert (%def 1 1)         &Wat.Vm$Error :type 'type :datum 1 :expected '(or Symbol Cons))
(%assert (%def a 1)         #inert)
(%assert (%def a 1 2)       &Wat.Vm$Error :type 'type :datum 1 :expected '(or #inert #ignore :rhs :prv :cnt))
(%assert (%def a #inert 1)  #inert)
(%assert (%def a #ignore 1) (%if (%== (bndRes) #inert) #inert (%if (%== (bndRes) :rhs) 1 #null)))
(%assert (%def a :prv 1)    #null)
(%assert (%def a :rhs 1)    1)
(%assert (%def a :rhs 1 2)  &Wat.Vm$Error :type 'match :operands# -1)
 
(%assert (%begin (%def (a) (1)) a)         1)
(%assert (%begin (%def (a b) (1 2)) b)     2)
(%assert (%begin (%def (a . b) (1 2)) b)   (2))
(%assert (%begin (%def (a . b) (1 2 3)) b) (2 3))

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
(%assert (%if #f 1 2 3) &Wat.Vm$Error @getMessage "not a Boolean: 2" :type 'type :datum 2 :expected 'Boolean)
(%assert (%if #f 1 #t 3)      3)
(%assert (%if #f 1 #f 3)      #inert)
(%assert (%if #f 1 #f 3 4)    4)
(%assert (%if #f 1 #f 3 #t 5) 5)

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

  
(%assert (%catchTagWth))
(%assert (%catchTagWth #_))
(%assert (%catchTagWth #_ #_) #inert)
(%assert (%catchTagWth #_ #_ 1) 1)
(%assert (%catchTagWth #_ #_ 1 2) 2)
(%assert (%catchTagWth #_ (%\ a 2) 1) 1)

(%assert (%catchTagWth #_ #_ (%throwTag #_)) #inert)
(%assert (%catchTagWth #_ #_ (%throwTag 'a)) #inert)
(%assert (%catchTagWth 'a #_ (%throwTag 'a)) #inert)
(%assert (%catchTagWth #_ #_ (%throwTag #_ 1)) 1)
(%assert (%catchTagWth #_ #_ (%throwTag 'a 1)) 1)
(%assert (%catchTagWth 'a #_ (%throwTag 'a 1)) 1)
(%assert (%catchTagWth #_ (%\ (x) 2) (%throwTag #_ 1)) 2)
(%assert (%catchTagWth #_ (%\ (x) 2) (%throwTag 'a 1)) 2)
(%assert (%catchTagWth 'a (%\ (x) 2) (%throwTag 'a 1)) 2)

(%assert (%catchTagWth #_ #_ (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #_) (%def x (%+ x 1))))))) #inert)
(%assert (%catchTagWth #_ #_ (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throwTag #_ x) (%def x (%+ x 1))))))) 10)
(%assert (%catchTagWth #_ #_ (%begin (%def x 0) (%loop (%if (%== x 10) (%throwTag #_ x) (%def x (%+ x 1)))))) 10)
(%assert (%catchTagWth #_ #_ (%begin (%def x 0) (%loop (%if (%== x 10) (%throwTag #_ x)) (%def x (%+ x 1))))) 10)

(%def testTco (%\ (n) (%if (%<= n 0) n (testTco (%- n 1))))) ; (prTrc 3)(testTco 5)(prTrc 0)
(%def stackDeep ((%\ (stackDeep) (%\ () (stackDeep #null))) (@getMethod &Wat.Utility "stackDeep")))

(%if (doTco)
  (%begin
    (%assert (%== (stackDeep) (%begin (stackDeep))) #t)
    (%assert (%== (stackDeep) (%if #t (stackDeep))) #t)
    (%assert (%== (stackDeep) (%if #f #inert (stackDeep))) #t)
    (%assert (%== (stackDeep) ((%vau () #_ (stackDeep)))) #t)
    (%assert (%== (stackDeep) (%eval '(stackDeep) (%theEnv))) #t)
    (%assert (%== (stackDeep) ((%\ () (stackDeep)))) #t)
    ( (%\ (key . lst)
        (%def loop (%\ (lst) (%if (%null? lst) #t (%== (%car lst) key) (loop (%cdr lst)) #f)))
        (%if (%! (loop lst)) (%apply log lst)) )
      (stackDeep)
      (%begin (stackDeep))
      (%if #t (stackDeep))
      (%if #f #inert (stackDeep))
      ((%vau () #_ (stackDeep)))
      (%eval '(stackDeep) (%theEnv))
      ((%\ () (stackDeep))) )
    (%assert (testTco 400) 0) ))
