;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctapv #f)
(prtrc 0)
(bndres 0)
(boxdft #null)
(aquote #t)
(ttrue 1)

(%def Null #null)
(%def Any &Wat.Vm$Any)
(%def Apv &Wat.Vm$Apv)
(%def At &Wat.Vm$At)
(%def Box &Wat.Vm$Box)
(%def Boolean &java.lang.Boolean)
(%def Class &java.lang.Class)
(%def Combinable &Wat.Vm$Combinable)
(%def Condition &Wat.Vm$Condition)
(%def Cons &Wat.Vm$Cons)
(%def Date &java.util.Date)
(%def Dot &Wat.Vm$Dot)
(%def Double &java.lang.Double)
(%def DVar &Wat.Vm$DVar)
(%def Env &Wat.Vm$Env)
(%def Error &Wat.Vm$Error)
(%def Ignore &Wat.Vm$Ignore)
(%def Inert &Wat.Vm$Inert)
(%def Intern &Wat.Vm$Intern)
(%def Integer &java.lang.Integer)
(%def Keyword &Wat.Vm$Keyword)
(%def JFun &Wat.Vm$JFun)
(%def List &Wat.Vm$List)
(%def Math &java.lang.Math)
(%def Number &java.lang.Number)
(%def Object &java.lang.Object)
(%def Opv &Wat.Vm$Opv)
(%def Keyword &Wat.Vm$Keyword)
(%def Obj &Wat.Vm$Obj)
(%def Object &java.lang.Object)
(%def Opv &Wat.Vm$Opv)
(%def Symbol &Wat.Vm$Symbol)
(%def System &java.lang.System)
(%def String &java.lang.String)
(%def Throwable &java.lang.Throwable)
(%def Inert &Wat.Vm$Inert)
(%def Ignore &Wat.Vm$Ignore)
(%def Utility &Wat.Utility)

(%def %zero? (%\ (v) (%== v 0)))
(%def %inert? (%\ (v) (%== v #inert)))
(%def %nth (%\ (i l) (%the Integer i) (@car (%the Cons l) i)))
(%def %nthCdr (%\ (i l) (%the Integer i) (%if (%zero? i) l (@cdr (%the Cons l) (%- i 1)))))
(%def %take (%\ (i l) (%if (%zero? i) #null (%cons (%car l) (%take (%- i 1) (%cdr l))))))
(%def %listSubseq (%\ (l s e) (%the Integer s) (%if (%! (%inert? e)) (%the Integer e)) (%def tail (%nthCdr s l)) (%if (%inert? e) tail (%take (%- e s) tail))))
(%def %stringSubseq (%\ (seq start end) (%if (%inert? end) (@substring (%the String seq) (%the Integer start)) (@substring (%the String seq) (%the Integer start) (%the Integer end)))))

(load "wat!/boot.wat");


;;;; Test

(load "wat!/test.wat");

(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
(%$ "vm started in " (%$ milli "ms"))