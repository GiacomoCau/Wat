;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctApv #f)
(prTrc 0)
(bndRes 0)
(boxDft #null)
(aQuote #t)
(tTrue 1)
(hdlAny #t)

(%def Null #null)
(%def Any &Wat.Vm$Any)
(%def Apv &Wat.Vm$Apv)
(%def At &Wat.Vm$At)
(%def AtDot &Wat.Vm$AtDot)
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
(%def Vm &Wat.Vm)
(%def Parser &List.Parser)

(%def %zero? (%\ (v) (%== v 0)))
(%def %inert? (%\ (v) (%== v #inert)))

(%def %nth (%\ (i l) (%the Integer i) (@car (%the Cons l) i)))
(%def %nthCdr (%\ (i l) (%the Integer i) (%if (%zero? i) l (@cdr (%the Cons l) (%- i 1)))))

(%def %take (%\ (i l) (%if (%zero? i) #null (%cons (%car l) (%take (%- i 1) (%cdr l))))))
(%def %subList (%\ (l s e) (%the Integer s) (%if (%! (%inert? e)) (%the Integer e)) (%def tail (%nthCdr s l)) (%if (%inert? e) tail (%take (%- e s) tail))))
(%def %subString (%\ (seq start end) (%if (%inert? end) (@substring (%the String seq) (%the Integer start)) (@substring (%the String seq) (%the Integer start) (%the Integer end)))))

;(%def %pushPrompt ((%\ (%pushPrompt) (%wrap %pushPrompt)) %pushPrompt))

(%def %className (%\ (class) (%intern (@getSimpleName (%the Class class)))))
;(%def %newClass ((%\ (%newClass) (%\ (name superclass) (%newClass (%intern (@capitalize Utility (@camelize Utility (%$ "" name) "-"))) superclass))) %newClass))

(%def %getSlot (%\ (obj slot) ((%the Obj obj) (%the Intern slot)) ))
(%def %setSlot (%\ (obj slot value) ((%the Obj obj) (%the Intern slot) value) ))
(%def %slotBound? (%\ (obj slot) (@isBound (%the Obj obj) (%the Intern slot)) ))

(%def %check
  ( (%\ (%check)
      (%vau (o ck) env
        (%def evl
          (%\ (ck)
            (%if (%== ck '+) (.MAX_VALUE Integer)
              (%if (%! (%cons? ck)) (%eval ck env)
              ( (%\ (ckcar)
                  (%if (%== ckcar 'or) (%list->array (evl (%cdr ck)))
                    (%if (%== ckcar '%') (cadr ck)
                      (%if (%== ckcar 'quote) (%cadr ck)
                        (evm ck) ))))
                (%car ck) ) ))))
        (%def evm (%\ (lst) (%if (%null? lst) #null (%cons (evl (%car lst)) (evm (%cdr lst))))))
        (%check o (evl ck)) ))
    %check ))


;;; Boot

(load "wat!/boot.wat");


;;;; Test

(load "wat!/test.wat");

(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
(%$ "vm started in " (%$ milli "ms"))