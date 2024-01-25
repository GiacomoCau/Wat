;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctApv #f)
(prTrc 0)
(bndRes #inert)
(boxDft #null)
(aQuote #t)
(typeT 1)
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
(%def PTree &Wat.Vm$PTree)

(%def %zero? (%\ (v) (%== v 0)))
(%def %inert? (%\ (v) (%== v #inert)))
(%def %ignore? (%\ (v) (%== v #ignore)))

(%def %nth
  (%\ ([#! (and Integer (>= 0)) i] [#! (or () Cons) lst])
    (%if (%!null? lst) (@car lst i)
      (error (%new Error "cannot get car" :type 'outOfBounds))) ))
(%def %nthCdr
  (%\ ([#! (and Integer (>= 0)) i] [#! (or () Cons) lst])
    (%if (%zero? i) lst
      (%if (%!null? lst) (@cdr lst (%- i 1))
        (error (%new Error "cannot get cdr" :type 'outOfBounds)) ))))
(%def %take
  (%\ ([#! (and Integer (>= 0)) i] [#! (or () Cons) lst])
    (%if (%zero? i) #null
      (%if (%cons? lst) (%cons (%car lst) (%take (%- i 1) (%cdr lst)))
        (error (%new Error "cannot take" :type 'outOfBounds)) ))))
(%def %subList
  (%\ ([#! (or () Cons) lst] [#! (and Integer (>= 0)) start] . [#! (or () (1 (and Integer (>= 0)))) end])
    (%def tail (%nthCdr start lst))
    (%if (%null? end) tail
      (%take (%- (car end) start) tail) )))    
(%def %subString
  (%\ ([#! String str] [#! (and Integer (>= 0)) start] . [#! (or () (1 (and Integer (>= 0)))) end])
    (%catchTagWth #ignore 
      (%\ (thw)
        (%error 
          (%if (%type? (@getCause thw) &java.lang.StringIndexOutOfBoundsException)
            (%new Error "cannot subString" thw :type 'outOfBounds)
            thw )))
      (apply** @substring str start end)  )))


;(%def %pushPrompt ((%\ (%pushPrompt) (%wrap %pushPrompt)) %pushPrompt))

(%def %className (%\ (class) (%intern (@getSimpleName (%the Class class)))))
;(%def %newClass ((%\ (%newClass) (%\ (name superclass) (%newClass (%intern (@capitalize Utility (@camelize Utility (%$ "" name) "-"))) superclass))) %newClass))

(%def %getSlot (%\ (obj slot) ((%the Obj obj) (%the Intern slot)) ))
(%def %setSlot (%\ (obj slot value) ((%the Obj obj) (%the Intern slot) value) ))
(%def %slotBound? (%\ (obj slot) (@isBound (%the Obj obj) (%the Intern slot)) ))

(%def %makeCheckEvl
  (%\ (check)
    (%def %=*
      (%vau (key . lst) env
        (%def key (%eval key env))
        ((%def loop :rhs (%\ (lst) (%if (%null? lst) #f (%if (%== (%car lst) key) #t (loop (%cdr lst)))))) lst)) )
    (%vau (o ck) env
      (%def evl
        (%\ (ck)
          (%if (%== ck 'oo) (.MAX_VALUE &java.lang.Integer)
            (%if (%! (%cons? ck)) (%eval ck env)
            ( (%\ (ckcar)
                (%if (%== ckcar 'or) (%list->array (evl (%cdr ck)))
                  (%if (%=* ckcar %' quote) (%cadr ck)
                    (%if (%=* ckcar < <= >= >) (%cons ckcar (%cons (%eval ckcar env) (evl (%cdr ck))))
                      (%if (%== ckcar 'and) (cons 'and (evl (%cdr ck)))
                        (evm ck) )))))
              (%car ck) ) ))))
      (%def evm (%\ (lst) (%if (%null? lst) #null (%cons (evl (%car lst)) (evm (%cdr lst))))))
      (check o (evl ck)) )))

(%def %check (%makeCheckEvl %check))
(%def %checkO (%makeCheckEvl %checkO))


;;; Boot

(load "wat!/boot.wat");

;;;; Error break routine, called by VM to print stacktrace and throw

(def\ (printFrames k)
  (let1 (k (.nxt k))
    (unless (null? k) (printFrames k)) )
  (log "v" k) )

(def\ (printStacktrace)
  (takeSubcont rootPrompt k
  	(printFrames k) (pushPrompt rootPrompt (pushSubcont k)) ))

(def\ (userBreak err)
  (when (prStk) (log "-" (@getMessage err)) (printStacktrace))
  (throw err) )


;;;; Test

(load "wat!/test.wat");


; for minimal lispx test compatibility

(defMacro deftest (name expression . expected?)
  (list* '%test name expression (if (null? expected?) '(#t) expected?)))

(defMacro deftest* (name . forms)
  (list deftest name (list* prog1 #t forms)) )

(defVau defsuite (name . forms) env
  (apply begin forms env) )

(def signalsError? assert)

(def SimpleError Error)
(def\ simpleError (message)
  (error message :type 'simple) )


(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
(%$ "vm started in " (%$ milli "ms"))