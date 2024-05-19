;

#|
 
 
  a        [a b ...] a
 ()(...)   [(...) (...) ...] (...) () 

check(Object o, Object chk)
  if (chk instanceof Object[] chks)
    if (chks[0] instanceof List)
      if (o != null && o != List) throw error
      for (i=0; i<chks.lenght; i+=1)
      	try return check(o, chks[i])    
      throw error
    else
      for (i=0; i<chks.lenght; i+=1)
      	try check(o, chks[i]); return 1   
      throw error
  else if (chk instanceof List chkl)
    if (o != null && o != List) throw error
    for (i=0; i<chkl.lenght; i+=1)
      i2 = check(o[i], chkl[i])
      if (len > 1) return i + i2  
    return i
  else if (chk instanceof Class chkc)
    if (!(o instacheof chkc) || o instanceof Class oc && !(chkc isAssegnable oc) throw error
    return 1
  else
   if !(o.equals(chk)) throw error
   return 1 
    
check(List o, List chk)             

(load "wat!/vm.wat")(load "testChk.lsp")

(let1 (xx '(Error @getMessage "foo")) (eval (list 'the '(or Class (1 oo Class (or Symbol Keyword At) Any)) (map (\ (x) (eval x)) xx))))
(eval (list 'the '(or Class (1 oo Class (or Symbol Keyword At) Any)) (list Error @getMessage "foo")) (theEnv))
(eval (list 'the '(Class At String) '(Error @getMessage "foo")) (theEnv))
(the (Class At String) (Error @getMessage "foo"))
(the At @getMessage) ; ok
(type? (%at "getMessage") At) ; ok

|#

(assert (def (#! Integer i)  1) #inert)
(assert (def (#! Integer i) 'a)       ) 
(assert (def (#! Integer i) 'a)  Error :type 'type :datum 'a :expected 'Integer)
(assert#t (signalsError? (def (#! Integer i) 'a)  Error :type 'type :datum 'a :expected 'Integer))

(assert (def* ((#! Integer i)(#! String b)) 1 "d") #inert)
(assert (def* ((#! Integer i)(#! String b)) 1 'd) Error :type 'type :datum 'd :expected 'String)

(assert (check   () (or () (1 'a))) 0)
(assert (check ('a) (or () (1 'a))) 1)

(assert (check   'a (or 'a 'b)) 0)

(assert ((\ ((#! (or () (1 'a)) x)) x) '(a)) '(a))
(assert ((\ ((#! (or () (1 'a)) x)) x) ()) ())


; test check e #!

(assert (check       () (or () (2))) 0)
(assert (check     '(1) (or () (2))) Error :type 'type :datum (1) :expected '(or () (2)))
(assert (check   '(1 2) (or () (2))) 2)
(assert (check '(1 2 3) (or () (2))) Error :type 'type :datum (1 2 3) :expected '(or () (2)))

(assert (check*       () or () (2)) 0)
(assert (check*     '(1) or () (2)) Error :type 'type :datum (1) :expected '(or () (2)))
(assert (check*   '(1 2) or () (2)) 2)
(assert (check* '(1 2 3) or () (2)) Error :type 'type :datum (1 2 3) :expected '(or () (2)))

(assert ((\ ((#! (or () (2)) x)) x)      ()) ())
(assert ((\ ((#! (or () (2)) x)) x)     (1)) Error :type 'type :datum (1) :expected '(or () (2)))
(assert ((\ ((#! (or () (2)) x)) x)   (1 2)) (1 2))
(assert ((\ ((#! (or () (2)) x)) x) (1 2 3)) Error :type 'type :datum (1 2 3) :expected '(or () (2)))


(assert (check   '(b 3) (or ('b 3) ('a 1 2))) 2)
(assert (check   '(b 2) (or ('b 3) ('a 1 2))) Error :type 'type :datum '(b 2) :expected '(or (b 3) (a 1 2)))
(assert (check '(a 1 2) (or ('b 3) ('a 1 2))) 3)
(assert (check '(a 1 3) (or ('b 3) ('a 1 2))) Error :type 'type :datum '(a 1 3) :expected '(or (b 3) (a 1 2)))

(assert (check*   '(b 3) or ('b 3) ('a 1 2)) 2)
(assert (check*   '(b 2) or ('b 3) ('a 1 2)) Error :type 'type :datum '(b 2) :expected '(or (b 3) (a 1 2)))
(assert (check* '(a 1 2) or ('b 3) ('a 1 2)) 3)
(assert (check* '(a 1 3) or ('b 3) ('a 1 2)) Error :type 'type :datum '(a 1 3) :expected '(or (b 3) (a 1 2)))

(assert (check   ('b 3) (or ('b 3) ('a 1 2))) 2)
(assert (check   ('b 2) (or ('b 3) ('a 1 2))) Error :type 'type :datum '(b 2) :expected '(or (b 3) (a 1 2)))
(assert (check ('a 1 2) (or ('b 3) ('a 1 2))) 3)
(assert (check ('a 1 3) (or ('b 3) ('a 1 2))) Error :type 'type :datum '(a 1 3) :expected '(or (b 3) (a 1 2)))


(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x)   ('b 3)) ('b 3))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x)   ('b 2)) Error :type 'type :datum '(b 2) :expected '(or (b 3) (a 1 2)))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x) ('a 1 2)) ('a 1 2))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x) ('a 1 3)) Error :type 'type :datum '(a 1 3) :expected '(or (b 3) (a 1 2)))


;#| test new
(assert (new) Error :type 'match :operands# +1)
(assert (new Box) Box #null)
(assert (new Box 1) Box 1)
(assert (new Obj :a) Error :type 'type :datum (&Wat.Vm$Obj :a) :expected '(or (1 2 Box) (1 oo Obj (or ((or Symbol Keyword String) Any) (1 oo Throwable (or Symbol Keyword String) Any) (1 oo String (or ((or Symbol Keyword String) Any) (1 oo Throwable (or Symbol Keyword String) Any)))))))
(assert (new Obj :a 1 :b) Error :type 'type :datum (&Wat.Vm$Obj :a 1 :b) :expected '(or (1 2 Box) (1 oo Obj (or ((or Symbol Keyword String) Any) (1 oo Throwable (or Symbol Keyword String) Any) (1 oo String (or ((or Symbol Keyword String) Any) (1 oo Throwable (or Symbol Keyword String) Any)))))))

(new Obj) ;-> ok {&Wat.Vm.Obj}
(new Box 1) ;-> ok {&Box 1}
(new Obj :a 1) ;-> ok {&Wat.Vm.Obj :a=1}
(new Obj :a 1 :b 2) ;-> ok {&Wat.Vm.Obj :a=1 :b=2}


(assert (check* '(Obj)                   1 oo (or (2 'Box) (1 oo 'Obj (or Symbol Keyword) Any))) 1)
(assert (check* '(Box 1)                 1 oo (or (2 'Box) (1 oo 'Obj (or Symbol Keyword) Any))) 2)
(assert (check* '(Obj :a 1)              1 oo (or (2 'Box) (1 oo 'Obj (or Symbol Keyword) Any))) 3)
(assert (check* '(Obj :a 1 :b 3)         1 oo (or (2 'Box) (1 oo 'Obj (or Symbol Keyword) Any))) 5)

(assert (check* '(&Wat.Vm$Obj)           1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 1)
(assert (check* '(&Wat.Vm$Box 1)         1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 2)
(assert (check* '(&Wat.Vm$Obj :a 1)      1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 3)
(assert (check* '(&Wat.Vm$Obj :a 1 :b 2) 1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 5)

(assert (check* ('&Wat.Vm$Obj)           1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 1)
(assert (check* ('&Wat.Vm$Box 1)         1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 2)
(assert (check* ('&Wat.Vm$Obj :a 1)      1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 3)
(assert (check* ('&Wat.Vm$Obj :a 1 :b 2) 1 oo (or (2  Box) (1 oo  Obj (or Symbol Keyword) Any))) 5)


;test #! the

(assert ((\ ((#! (or 1 2) x)) x) 1) 1)
(assert ((\ ((#! (or 1 2) x)) x) 3) Error :type 'type :datum 3 :expected '(or 1 2))

(assert (the (or 1 2) 1) 1)
(assert (the (or 1 2) 3) Error :type 'type :datum 3 :expected '(or 1 2))


(assert ((\ ((#! (or () (1 'a)) x)) x) ()) ()) 
(assert ((\ ((#! (or () (1 'a)) x)) x) '(a)) '(a))
(assert ((\ ((#! (or () (2 'a)) x)) x) '(a a)) '(a a))

(assert (the (or () (1 'a)) ()) ())
(assert (the (or () (1 'a)) ('a)) ('a))
(assert (the (or () (2 'a)) ('a 'a)) ('a 'a))


(assert ((\ ((#! (or () (2)) x)) x)      ()) ())
(assert ((\ ((#! (or () (2)) x)) x)     (1)) Error :type 'type :datum (1) :expected '(or () (2)))
(assert ((\ ((#! (or () (2)) x)) x)   (1 2)) (1 2))
(assert ((\ ((#! (or () (2)) x)) x) (1 2 3)) Error :type 'type :datum (1 2 3) :expected '(or () (2)))

(assert (the (or () (2))      ()) ())
(assert (the (or () (2))     (1)) Error :type 'type :datum (1) :expected '(or () (2)))
(assert (the (or () (2))   (1 2)) (1 2))
(assert (the (or () (2)) (1 2 3)) Error :type 'type :datum (1 2 3) :expected '(or () (2)))


(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x)   ('b 3)) ('b 3))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x)   ('b 2)) Error :type 'type :datum ('b 2) :expected '(or (b 3) (a 1 2)))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x) ('a 1 2)) ('a 1 2))
(assert ((\ ((#! (or ('b 3) ('a 1 2)) x)) x) ('a 1 3)) Error :type 'type :datum ('a 1 3) :expected '(or (b 3) (a 1 2)))

(assert (the (or ('b 3) ('a 1 2))   ('b 3)) ('b 3)) 
(assert (the (or ('b 3) ('a 1 2))   ('b 2)) Error :type 'type :datum ('b 2) :expected '(or (b 3) (a 1 2)))
(assert (the (or ('b 3) ('a 1 2)) ('a 1 2)) ('a 1 2))
(assert (the (or ('b 3) ('a 1 2)) ('a 1 3)) Error :type 'type :datum ('a 1 3) :expected '(or (b 3) (a 1 2)))


(assert ((\ ((#! Integer i)) i) 1) 1)
(assert ((\ ((#! (Integer) i)) i) (1)) (1))
(assert ((\ ((#! (Integer) i)) i) (1 2)) (1 2))
(assert ((\ ((#! (Integer) i)) i) (1 2 3)) (1 2 3))
(assert ((\ ((#! (2 Integer) i)) i) (1 2 3)) Error :type 'match :operands# -1)
(assert ((\ ((#! (3 Integer) i)) i) (1 2 3)) (1 2 3))
(assert ((\ ((#! (Integer String) i)) i) (1 "2")) (1 "2"))
(assert ((\ ((#! (Integer String) i)) i) (1 "2" 3)) Error :type 'match :operands# +1)
(assert ((\ ((#! (Integer String) i)) i) (1 "2" 3 "4")) (1 "2" 3 "4"))
(assert ((\ ((#! (Integer String) i)) i) (1 2 3)) Error :type 'type :datum 2 :expected 'String)
(assert ((\ ((#! (2 Integer String) i)) i) (1 "2" 3 "4")) Error :type 'match :operands# -2)
(assert ((\ ((#! (2 Integer String) i)) i) (1 "2")) (1 "2"))
(assert ((\ ((#! (Integer String Integer) i)) i) (1 "2" 3)) (1 "2" 3))

(assert (the Integer 1) 1)
(assert (the Integer (1)) Error :type 'type :datum (1) :expected 'Integer)
(assert (the (Integer) 1) Error :type 'type :datum 1 :expected '(or () List))
(assert (the (Integer) (1)) (1)) 
(assert (the (Integer) (1 2)) (1 2)) 
(assert (the (Integer) (1 2 3)) (1 2 3)) 
(assert (the (2 Integer) (1 2 3)) Error :type 'match :operands# -1) 
(assert (the (3 Integer) (1 2 3)) (1 2 3)) 
(assert (the (Integer String) (1 "2")) (1 "2"))
(assert (the (Integer String) (1 "2" 3)) Error :type 'match :operands# +1)
(assert (the (Integer String) (1 "2" 3 "4")) (1 "2" 3 "4"))
(assert (the (Integer String) (1 2 3)) Error :type 'type :datum 2 :expected 'String)
(assert (the (2 Integer String) (1 "2" 3 "4")) Error :type 'match :operands# -2)
(assert (the (2 Integer String) (1 "2")) (1 "2"))
(assert (the (Integer String Integer) (1 "2" 3)) (1 "2" 3))
