;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(prTrc 0)
(bndRes #inert)
(boxDft #null)
(aQuote #t)
(typeT 1)
(hdlAny #t)

(%def Null #null)
(%def Any &Wat.Vm$Any)
(%def Apv &Wat.Vm$Apv)
(%def Array &java.lang.reflect.Array)
(%def Arrays &java.util.Arrays)
(%def At &Wat.Vm$At)
(%def AtDot &Wat.Vm$AtDot)
(%def BiConsumer &java.util.function.BiConsumer)
(%def BiFunction &java.util.function.BiFunction)
(%def Box &Wat.Vm$Box)
(%def Boolean &java.lang.Boolean)
(%def CaseMacro &Wat.Vm$CaseMacro)
(%def CaseOpv &Wat.Vm$CaseOpv)
(%def Class &java.lang.Class)
(%def Class[] &java.lang.Class[])
(%def Combinable &Wat.Vm$Combinable)
(%def Condition &Wat.Vm$Condition)
(%def Constructor &java.lang.reflect.Constructor)
(%def Consumer &java.util.function.Consumer)
(%def Cons &Wat.Vm$Cons)
(%def Date &java.util.Date)
(%def Dot &Wat.Vm$Dot)
(%def Double &java.lang.Double)
(%def DVar &Wat.Vm$DVar)
(%def Env &Wat.Vm$Env)
(%def Error &Wat.Vm$Error)
(%def Executable &java.lang.reflect.Executable)
(%def Field &java.lang.reflect.Field)
(%def Function &java.util.function.Function)
(%def Ignore &Wat.Vm$Ignore)
(%def Inert &Wat.Vm$Inert)
(%def Intern &Wat.Vm$Intern)
(%def Integer &java.lang.Integer)
(%def Keyword &Wat.Vm$Keyword)
(%def JFun &Wat.Vm$JFun)
(%def List &Wat.Vm$List)
(%def Long &java.lang.Long)
(%def Macro &Wat.Vm$Macro)
(%def Math &java.lang.Math)
(%def Method &java.lang.reflect.Method)
(%def Number &java.lang.Number)
(%def Object &java.lang.Object)
(%def Opv &Wat.Vm$Opv)
(%def Keyword &Wat.Vm$Keyword)
(%def Obj &Wat.Vm$Obj)
(%def ObjEnv &Wat.Vm$ObjEnv)
(%def Object &java.lang.Object)
(%def Object[] &java.lang.Object[])
(%def Opv &Wat.Vm$Opv)
(%def Symbol &Wat.Vm$Symbol)
(%def System &java.lang.System)
(%def String &java.lang.String)
(%def Supplier &java.util.function.Supplier)
(%def Throwable &java.lang.Throwable)
(%def Utility &Wat.Utility)
(%def Vm &Wat.Vm)
(%def Parser &List.Parser)
(%def DPtEp &Wat.Vm$DPtEp)

(%def %0? (%\ (v) (%== v 0)))
(%def %1? (%\ (v) (%== v 1)))
(%def %-1? (%\ (v) (%== v -1)))
(%def %inert? (%\ (v) (%== v #inert)))
(%def %ignore? (%\ (v) (%== v #ignore)))

(%def %nth
  (%\ ([#: (and Integer (>= 0)) i] [#: (or () Cons) lst])
    (%if (%cons? lst) (@car lst i)
      (error (%new Error "cannot get car" :type 'outOfBounds))) ))
(%def %nthCdr
  (%\ ([#: (and Integer (>= 0)) i] [#: (or () Cons) lst])
    (%if (%0? i) lst
      (%if (%cons? lst) (@cdr lst (%- i 1))
        (error (%new Error "cannot get cdr" :type 'outOfBounds)) ))))
(%def %take
  (%\ ([#: (and Integer (>= 0)) i] [#: (or () Cons) lst])
    (%if (%0? i) #null
      (%if (%cons? lst) (%cons (%car lst) (%take (%- i 1) (%cdr lst)))
        (error (%new Error "cannot take" :type 'outOfBounds)) ))))
(%def %subList
  (%\ ([#: (or () Cons) lst] [#: (and Integer (>= 0)) start] . [#: (or () (1 (and Integer (>= 0)))) end])
    (%def tail (%nthCdr start lst))
    (%if (%null? end) tail
      (%take (%- (car end) start) tail) )))    
(%def %subString
  (%\ ([#: String str] [#: (and Integer (>= 0)) start] . [#: (or () (1 (and Integer (>= 0)))) end])
    (%catchTagWth #ignore 
      (%\ (thw)
        (%error 
          (%if (%type? (@getCause thw) &java.lang.StringIndexOutOfBoundsException)
            (%new Error "cannot subString" thw :type 'outOfBounds)
            thw )))
      (%apply** @substring str start end) )))
(%def %subArray
  (%\ ([#: Object[] obj] [#: (and Integer (>= 0)) start] . [#: (or () (1 (and Integer (>= 0)))) end])
    (%catchTagWth #ignore 
      (%\ (thw)
        (%error
          (if (%matchType? thw (Error :type outBounds)) thw 
            (%new Error "cannot copy range" thw :type 'outOfBounds) )))
      (%if (%if (== end #null) #t (<= (%car end) (.length obj)) #t #f)
        (%apply* @copyOfRange Arrays obj start (%if (%null? end) (.length obj) (%car end)))
        (%error (%new Error "cannot copy range, out of bound!" :type 'outBounds))
        ;(%error (@new &java.lang.RuntimeException "out of bound!"))
        ))))


;(%def %pushPrompt ((%\ (%pushPrompt) (%wrap %pushPrompt)) %pushPrompt))

(%def %className (%\ ((#: Class class)) (%intern (@getSimpleName class))))
;(%def %newClass ((%\ (%newClass) (%\ (name superclass) (%newClass (%intern (@capitalize Utility (@camelize Utility (%$ "" name) "-"))) superclass))) %newClass))

(%def %getValue (%\ ((#: ObjEnv obj) (#: Intern key)) (obj key)))
(%def %keyBound? (%\ ((#: ObjEnv obj) (#: Intern key)) (%bound? key obj)))


;;; Boot

(load "wat!/boot.wat")

(def SimpleError
  #|(type Class)
   |(extends Error)
   |
   |Class for simple errors with a MESSAGE.
   |For minimal lispx test compatibility
   |#
  Error )

(def\ simpleError (message)
  #|($nm message)
   |(type function)
   |
   |Signal a simple error with a MESSAGE.
   |For minimal lispx test compatibility
   |#
  (error message :type 'simple) )


;;;; User error break routine, called to print stacktrace and throw or debug

(def userBreak
  #|Define userBreak function invocked from Vm when signals an error
   |#
  printStacktrace )


;;;; Test Util


(defMacro defTest (name expression . expected?)
  (list* 'test name expression (if (null? expected?) '(#t) expected?)))

(defMacro defTest* (name . forms)
  (list defTest name (list* prog1 #t forms)) )

(defVau defSuite (name . forms) env
  (apply begin forms env) )

(def signalsError? assert)


;;;; Test

(load "wat!/test.wat")

(def milli (- (@currentTimeMillis &java.lang.System) milli))
($ "vm started in " (%$ milli "ms"))
