;

(%assert () #null)
(%assert #null ())

(%assert #ignore #_)
(%assert #_ #ignore)

(%assert (%quote (a b)) '(a b))
(%assert '(a b) (%quote (a b)))

(%assert (%def)             ) ; throw
(%assert (%def a)           ) ; throw 
(%assert (%def a 1)   #inert) ; a=1
(%assert (%def 1 1)         ) ; throw
(%assert (%def "a" 1)       ) ; throw
(%assert (%def a 1 2)       ) ; throw
 
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

(%if (ctapv)
  (%begin 
    (%assert (%catch))
    (%assert (%catch #null))
    (%assert (%catch #null () (%\ () 1)) 1)
    (%assert (%catch #null (%\ a 2) (%\ () 1)))
    (%assert (%catch #null (%\ (a) 2) (%\ () 1)) 1)

    (%assert (%catch #_ () (%\ () (%throw #ignore))) #inert)
    (%assert (%catch #_ () (%\ () (%throw 'a))) #inert)
    (%assert (%catch 'a () (%\ () (%throw 'a))) #inert)
    (%assert (%catch #_ () (%\ () (%throw #ignore 1))) 1)
    (%assert (%catch #_ () (%\ () (%throw 'a 1))) 1)
    (%assert (%catch 'a () (%\ () (%throw 'a 1))) 1)
    (%assert (%catch #_ (%\ (x) 2) (%\ () (%throw #ignore 1))) 2)
    (%assert (%catch #_ (%\ (x) 2) (%\ () (%throw 'a 1))) 2)
    (%assert (%catch 'a (%\ (x) 2) (%\ () (%throw 'a 1)) ) 2)

    (%assert (%catch #ignore () (%\ () (%def x 0) (%loop (%begin (%if (%== x 10) (%throw #ignore) (%def x (%+ x 1))))))) #inert)
    (%assert (%catch #ignore () (%\ () (%def x 0) (%loop (%begin (%if (%== x 10) (%throw #ignore x) (%def x (%+ x 1))))))) 10)
    (%assert (%catch #ignore () (%\ () (%def x 0) (%loop (%if (%== x 10) (%throw #ignore x) (%def x (%+ x 1)))))) 10)
    (%assert (%catch #ignore () (%\ () (%def x 0) (%loop (%if (%== x 10) (%throw #ignore x)) (%def x (%+ x 1))))) 10) )
  
  (%begin 
    (%assert (%catch))
    (%assert (%catch #null))
    (%assert (%catch #null #null 1) 1)
    (%assert (%catch #null #null 1 2) 2)
    (%assert (%catch #null (%\ a 2) 1) 1)

    (%assert (%catch #ignore #null (%throw #ignore)) #inert)
    (%assert (%catch #ignore #null (%throw 'a)) #inert)
    (%assert (%catch 'a #null (%throw 'a)) #inert)
    (%assert (%catch #ignore #null (%throw #ignore 1)) 1)
    (%assert (%catch #ignore #null (%throw 'a 1)) 1)
    (%assert (%catch 'a #null (%throw 'a 1)) 1)
    (%assert (%catch #ignore (%\ (x) 2) (%throw #ignore 1)) 2)
    (%assert (%catch #ignore (%\ (x) 2) (%throw 'a 1)) 2)
    (%assert (%catch 'a (%\ (x) 2) (%throw 'a 1)) 2)

    (%assert (%catch #ignore #null (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throw #ignore) (%def x (%+ x 1))))))) #inert)
    (%assert (%catch #ignore #null (%begin (%def x 0) (%loop (%begin (%if (%== x 10) (%throw #ignore x) (%def x (%+ x 1))))))) 10)
    (%assert (%catch #ignore #null (%begin (%def x 0) (%loop (%if (%== x 10) (%throw #ignore x) (%def x (%+ x 1)))))) 10)
    (%assert (%catch #ignore #null (%begin (%def x 0) (%loop (%if (%== x 10) (%throw #ignore x)) (%def x (%+ x 1))))) 10) ))

(%def testTco (%\ (n) (%if (%<= n 0) n (testTco (%- n 1)))))
(%if (dotco) (%assert (testTco 400) 0))