;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def

(%def def %def)	

;; Rename bindings that will be used as provided by VM

(def \ %\)

(def == %==)
(def != %!=)

(def $ %$)
(def + %+)
(def * %*)
(def - %-)
(def / %/)
(def % %%)
  
(def ! %!)
(def !! %!!)
(def < %<)
(def > %>)
(def <= %<=)
(def >= %>=)
  
(def ~ %~)
(def & %&)
;(def | %|) ;; TODO lo vedrebbe come unescaped symbol!
(def ^ %^)
(def << %<<)
(def >> %>>)
(def >>> %>>>)

(def append %append)
(def array->list %array->list)
(def assert %assert)
(def begin %begin)
(def cons %cons)
(def cons? %cons?)
;(def ddef %dDef)
;(def ddef* %dDef*)
(def dval %dVal)
(def dvar %dVar)
(def eq? %eq?)
(def error %error)
(def eval %eval)
(def finally %finally)
(def if %if)
(def instanceof? %instanceof?)
(def len %len)
(def list? %list?)
(def list* %list*)
(def list->array %list->array)
(def loop %loop)
(def makeEnv %makeEnv)
(def null? %null?)
(def not %!)
(def null? %null?)
(def obj %obj)
(def reverse %reverse)
(def rootPrompt %rootPrompt)
(def symbol %symbol)
(def symbolName %internName)
(def symbol? %symbol?)
(def keywordName %internName)
(def keyword? %keyword?)
(def test %test)
(def type? %type?)
(def unwrap %unwrap)
(def wrap %wrap)


;;;; Basic type

(def Apv &Wat.Vm$Apv)
(def Opv &Wat.Vm$Opv)
(def Box &Wat.Vm$Box)
(def Cons &Wat.Vm$Cons)
(def DVar &Wat.Vm$DVar)
(def JFun &Wat.Vm$JFun)
(def Obj &Wat.Vm$Obj)
(def Error &Wat.Vm$Error)
(def Symbol &Wat.Vm$Symbol)
(def Keyword &Wat.Vm$Keyword)
(def Boolean &java.lang.Boolean)
(def Date &java.util.Date)
(def Number &java.lang.Number)
(def Integer &java.lang.Integer)
(def Double &java.lang.Double)
(def Object &java.lang.Object)
(def String &java.lang.String)
(def Math &java.lang.Math)
(def System &java.lang.System)


;;;; Important utilities

(def vau %vau)
(def quote (vau (x) #ignore x))
(def quote %quote)
(def list (wrap (vau elts #ignore elts)))
(def list %list)
(def lambda (vau (formals . body) env (wrap (eval (list* vau formals #ignore body) env))))
(def lambda %\)
(def theEnv (vau () e e))
(def theEnv %theEnv)

(def car (\ ((x . #ignore)) x))
(def car %car)
(def cdr (\ ((#ignore . x)) x))
(def cdr %cdr)

(def compose (\ (f g) (\ (arg) (f (g arg)))))

(def caar (compose car car))
(def cadr (compose car cdr))
(def cdar (compose cdr car))
(def cddr (compose cdr cdr))


;;;; Macro

(def evalMacro (dvar #t))

(def makeMacro
  (wrap
    (vau (expander) #ignore
      (vau operands env
        (def !evalMacro (! (evalMacro)))
        (if !evalMacro (evalMacro #t))
        (def exp (eval (cons expander operands) (makeEnv)))
        (if !evalMacro exp (eval exp env)) ))))

(def macro
  (makeMacro
    (vau (params . body) #ignore
      (list 'makeMacro (list* 'vau params #ignore body)) )))

; defMacro defVau def\ def*\ rec\ e letrec\ permettono le definizioni con le seguenti due sintassi
;    (... name parameters . body)
;    (... (name . parameters) . body)
; rec rec\ letrec e letrec\ inizializzano a #inert le definizioni prima della valutazione

(def defMacro
  (macro (lhs . rhs)
    (if (symbol? lhs)
      (list 'def lhs (list* 'macro (car rhs) (cdr rhs)))
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs)) )))

(defMacro (expand macro)
  (list 'begin (list 'evalMacro #f) macro))

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))

(defMacro (defVau lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* 'vau (car rhs) (cadr rhs) (cddr rhs)))
    (list 'def (car lhs) (list* 'vau (cdr lhs) (car rhs) (cdr rhs))) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%quote +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%quote +) n 1) env))))

(defMacro (def* lhs . rhs)
  (list 'def lhs (list* 'list rhs)) )

(defMacro (def\ lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '\ (car rhs) (cdr rhs)))
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )

(defMacro (wrau pt env . body)
  (list 'wrap (list* 'vau pt env body)))

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))    


;;;; Basic value test

(def\ (zero? n) (== n 0))
(def\ (inert? o) (== o #inert))
 

;;;; Wrap incomplete VM forms

(def* (then else) begin begin)

(if (ctapv)
  (then
    (defMacro (catch exp . hdl) (list* '%catch #ignore (list '\ () exp) hdl) )
    (defMacro (throw . val) (list* '%throw #ignore val) )

    (assert (catch (throw)) #inert)
    (assert (catch (throw (\ () 1))) 1)
    (assert (catch (throw (\ () 1)) (\ (x) (+ x 1))) 2)
    
    (defMacro (catchTag tag exp . hdl) (list* '%catch tag (list '\ () exp) hdl) )
    (defMacro (throwTag tag . val) (list* '%throw tag val) )
    (def throwTag %throw)
  )
  (else
    (defMacro (catch exp . hdl) (list* '%catch #ignore exp hdl))
    (defMacro (throw . val) (list* '%throw #ignore val))

    (assert (catch (throw)) #inert)
    (assert (catch (throw 1)) 1)
    (assert (catch (throw 1) (\ (x) (+ x 1))) 2)

    (defVau (catchTag tag exp . hdl) env (eval (list* '%catch (eval tag env) exp hdl) env) )
    (defVau (throwTag tag . val) env (eval (list* '%throw (eval tag env) val) env) )
  )
)

(def finally %finally)
(def takeSubcont %takeSubcont)

(defVau (pushPrompt prompt . body) env
  (eval (list '%pushPrompt (eval prompt env) (list* 'begin body)) env) )

(defMacro (pushPromptSubcont p k . body)
  (list '%pushPromptSubcont p k (list* '\ () body)) )

(defMacro (pushSubcont k . body)
  (list '%pushPromptSubcont #ignore k (list* '\ () body)) )


;;;; Basic macros and functions

(def\ (apply appv args . env)
  (def env (if (null? env) (makeEnv) ((\ ((env)) env) env))) 
  (if (%jFun? appv)
    (@combine (@new JFun vm appv) env args)
    (eval (cons (unwrap appv) args) env) ))

(defMacro (rec lhs . rhs)
  (list (list '\ () (list 'def lhs #inert) (list* 'def lhs :rhs rhs))) )

(assert ((rec f (\ (l) (if (null? l) "" ($ (car l) (f (cdr l)))))) '(1 2 3)) "123")
(assert ((rec f (\ l (if (null? l) "" ($ (car l) (apply f (cdr l)))))) 1 2 3) "123")

(defMacro (rec\ lhs . rhs)
  (if (symbol? lhs)
    (list 'rec lhs (list* '\ (car rhs) (cdr rhs)))
    (list 'rec (car lhs) (list* '\ (cdr lhs) rhs)) )) 

(assert ((rec\ (f l)   (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\  f (l)  (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\ (f . l) (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")
(assert ((rec\  f l    (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")

(def\ (map f . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (map lst) (if (null? lst) #null (cons (f (car lst)) (map (cdr lst))) )) (car lst*))
    ((rec\ (map* lst*) (if (null? (car lst*)) #null (cons (apply f (map car lst*)) (map* (map cdr lst*))) )) lst*) ))

(assert (map car  '((a 1)(b 2))) '(a b))
(assert (map cdr  '((a 1)(b 2))) '((1) (2)))
(assert (map cadr '((a 1)(b 2))) '(1 2))
(assert (map (\ (a b) (+ a b)) '(1 2) '(3 4)) '(4 6)) 

(defMacro (def*\ lhs* . rhs*)
  (list* 'def*
    (map (\ (lhs) (if (symbol? lhs) lhs (car lhs))) lhs*)
    (map (\ (lhs rhs) (if (symbol? lhs) (list* '\ (car rhs) (cdr rhs)) (list* '\ (cdr lhs) rhs))) lhs* rhs*) )) 

(assert (expand (def*\ ((a n) (b n)) ((+ n 1)) ((+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )
(assert (expand (def*\ (a b) ((n) (+ n 1)) ((n) (+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )


;;;; Lexical bindings

(def\ (->inert binding) #inert)
(def\ (->1expr binding) ((\ ((#_ cadr)) cadr) binding))
(def\ (->begin binding) ((\ ((#_ cadr . exps)) (if (null? exps) cadr (list* 'begin cadr exps))) binding))

#|
(defMacro (let1\ . args)
  (if (symbol? (car args))
    (def (name binding . body) args)
    (def ((name binding) . body) args))
  (list
    (list* 'rec\ name (list (car binding)) body)
    (->1expr binding) ))

(assert (let1\  f (a 2)  (if (zero? a) 'end (f (- a 1)))) 'end)
(assert (let1\ (f (a 2)) (if (zero? a) 'end (f (- a 1)))) 'end)
|#

(defMacro (let\ . args)
  (if (symbol? (car args))
    (def (name bindings . body) args)
    (def ((name . bindings) . body) args))
  (list*
    (list* 'rec\ name (map car bindings) body)
    (map ->1expr bindings) ))

(def letLoop let\)

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))
(assert (letLoop (sum (a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))

(defMacro (let1 lhs . rhs)
  (if (symbol? lhs)
    ;(list* 'let1\ lhs (car rhs) (cdr rhs))
    (list* 'let\ lhs (list (car rhs)) (cdr rhs))
    (list (list* '\ (list (car lhs)) rhs)
      (->1expr lhs) )))
      
(assert (let1 (a 1) a) 1)
;(assert (let1 (a 1 2) a) 2) ; need ->begin
(assert (let1 f (a 2) (if (zero? a) 'end (f (- a 1)))) 'end)

(defMacro (let lhs . rhs)
  (if (symbol? lhs)
    (list* 'let\ lhs rhs)
    (list* (list* '\ (map car lhs) rhs)
      (map ->1expr lhs) )))

(assert (let ((a 1)) a) 1)
    
(defMacro (let* bindings . body)
  (if (null? bindings)
      (list* 'let () body)
      (list 'let
        (list (car bindings))
        (list* 'let* (cdr bindings) body) )))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)

(defMacro (letrec bindings . body)
  (list* 'let ()
    (list* 'def* (map car bindings) (map ->inert bindings))
    (list* 'def* (map car bindings) (map ->1expr bindings))
    body ))

(assert (letrec ( (even? (\ (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (zero? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . body)
  (list* 'let ()
    (list* 'def* (map (\ ((lhs . rhs)) (if (symbol? lhs) lhs (car lhs))) bindings) (map ->inert bindings))
    (list* 'def*\ (map car bindings) (map cdr bindings))
    body ))

(def labels letrec\)

(assert (labels ( ((even? n) (if (zero? n) #t (odd? (- n 1)))) ((odd? n) (if (zero? n) #f (even? (- n 1)))) ) (even? 88) ) #t)
(assert (labels ( (even? (n) (if (zero? n) #t (odd? (- n 1)))) (odd? (n) (if (zero? n) #f (even? (- n 1)))) ) (even? 88) ) #t)

#| TODO definizioni alternative, eliminare
(defMacro (letLoop . args)
  (if (symbol? (car args))
    (def (name bindings . body) args)
    (def ((name . bindings) . body) args))
  (list 'letrec\
    (list (list* name (map car bindings) body))
    (list* name (map ->1expr bindings)) ))

(defMacro (letLoop . args)
  (if (symbol? (car args))
    (def (name bindings . body) args)
    (def ((name . bindings) . body) args))
  (list 'let ()
    (list 'def name #inert)
    (list* 'def\ name (map car bindings) body)
    (list* name (map ->1expr bindings)) ))
|#


;;;; Simple control

(defMacro (when test . body)
  (list 'if test (list* 'begin body)))

(defMacro (unless test . body)
  (list* 'when (list 'not test) body))


;;;; Bind CaseVau Case\ Match

(def bind? %bind?)

(defVau (ifbind? (pt exp) then . else) env
  (let1 (env+ (makeEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (let1 ((exp) else) exp) env) ))))

(defVau (caseVau . clauses) env 
  (vau values #ignore
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert ;(error ($ "not a match form for: " values))
        (let ( (env+ (makeEnv env))
               (((bindings . forms) . clauses) clauses) )
          (if (if (== bindings 'else) #t (bind? env+ bindings values)) ; or!
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
            
#| TODO sostituiti dal seguente, eliminare
(defVau (case\ . clauses) env
  (wrap
    (vau values #ignore
      (let1 loop (clauses clauses)
        (if (null? clauses) #inert
          (let ( (env+ (makeEnv env))
                 (((bindings . forms) . clauses) clauses) )
            (if (if (== bindings 'else) #t (bind? env+ bindings values)) ; or!
              (eval (list* 'begin forms) env+)
              (loop clauses) )))))))

(defVau (case\ . clauses) env
  (\ values
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ( (env+ (makeEnv env))
               (((bindings . forms) . clauses) clauses) )
          (if (if (== bindings 'else) #t (bind? env+ bindings values)) ; or!
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
|#
(defMacro (case\ . clauses)
  (list 'wrap (list* 'caseVau clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))

#| TODO sostituito dal seguente, eliminare
(defVau (match exp . clauses) env
  (let1 (exp (eval exp env))
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ((env+ (makeEnv env))
              (((bindings . forms) . clauses) clauses) )
          (if (if (== bindings 'else) #t (bind? env+ bindings exp)) ; or!
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
(defMacro (match exp . clauses)
  (list* (list* 'case\ clauses) (list exp)) )  
(defVau (match exp . clauses) env
  (eval (list* (list 'caseVau clauses) (eval exp env)) env) )
|#
(defMacro (match exp . clauses)
  (list (list* 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )  

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match   '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match     '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match        1 ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match        4 ((a) 1) ((a b) 2)    (a a)) 4)


;;;; Cond And Or
 
#| TODO sostituito dal seguente, eliminare
(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) cla uses)
      (if (== test 'else)
        (apply (wrap begin) body env) 
        (if (eval test env)
          (apply (wrap begin) body env)
          (apply (wrap cond) clauses env) )))))

(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) cla uses)
      (if (if (== test 'else) #t (eval test env))
        (apply (wrap begin) body env)
        (apply (wrap cond) clauses env) ))))

(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if test
            (if (null? body) test
              (if (== (car body) '=>)
                (let1 ((apd1) (cdr body))
                  ((eval apd1 env) test) )
                (apply (wrap begin) body env) ))
            (apply (wrap cond) clauses env) ))))))

(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if (instanceof? test Boolean)
            (if test
              (if (null? body) test
                (if (== (car body) '=>)
                  (let1 ((apd1) (cdr body)) ((eval apd1 env) test) )
                  (apply (wrap begin) body env) ))
              (apply (wrap cond) clauses env) )
            (let1 ((guard '=> apd1) body)
              (if ((eval guard env) test)
                ((eval apd1 env) test)
                (apply (wrap cond) clauses env) )) ))))))

(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if (instanceof? test Boolean)
            (if test
              (if (null? body) test
                (ifbind? (('=> apd1) body)
                  ((eval apd1 env) test) 
                  (apply (wrap begin) body env) ))
              (apply (wrap cond) clauses env) )
            (let1 ((guard '=> apd1) body)
              (if ((eval guard env) test)
                ((eval apd1 env) test)
                (apply (wrap cond) clauses env) )) ))))))
(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if (instanceof? test Boolean)
            (if test
              (match body
                (() test)
                (('=> apd1) ((eval apd1 env) test))
                (else (apply (wrap begin) body env) ))
              (apply (wrap cond) clauses env) )
            (let1 ((guard '=> apd1) body)
              (if ((eval guard env) test)
                ((eval apd1 env) test)
                (apply (wrap cond) clauses env) )) ))))))
|#
(defVau (cond . clauses) env
  (if (null? clauses) #inert
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if (instanceof? test Boolean)
            (if test
              (apply (wrap begin) body env)
              (apply (wrap cond) clauses env) )
            (match body
              (() test)
              (('=> apv1) ((eval apd1 env) test))
              ((guard '=> apd1)
                 (if ((eval guard env) test)
                   ((eval apd1 env) test)
                   (apply (wrap cond) clauses env) ))
              (else (apply (wrap cond) clauses env)) )))))))

(assert (begin (def a 1) (cond (a (\(a) (== a 1)) => (\(a) (+ a 1))))) 2) 

(defVau (and . x) e
  (cond ((null? x)         #t)
        ((null? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(def && and)

(defVau (or . x) e
  (cond ((null? x)         #f)
        ((null? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(def || or)


;;;; Finding

(def\ (member item lst)
  (let\ (loop (lst lst))
     (if (null? lst) #null
       (if (== item (car lst)) lst
         (loop (cdr lst)) ))))

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

(def\ (assoc item lst)
  (let\ (loop (lst lst))
     (if (null? lst) #null
       (if (== item (caar lst)) (car lst)
         (loop (cdr lst)) ))))

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (get? key lst)
  (let\ (loop (lst lst)) 
     (if (null? lst) #null
       (let1 ((k v . lst) lst)
         (if (== k key) (list v) (loop lst)) ))))

(assert (get? :b '(:a 1 :b 2 :c 3)) '(2)) 
(assert (get? 'b '(a 1 b 2 c 3)) '(2)) 


;;; Case CaseType

(defVau (case exp . clauses) env
  (let1 (value (eval exp env))
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((values . forms) . clauses) clauses)
          (if (or (== values 'else) (cons? (member value values)))
            (if (== (car forms) '=>)
              (let1 ((apd) (cdr forms)) ((eval apd env) value))  
              (eval (list* 'begin forms) env) )
            (loop clauses) ))))))

(assert (case 3 ((2 4 6 8) 'pair) ((1 3 5 7 9) 'odd)) 'odd) 

#| TODO sostituito dal seguente, eliminare
(defVau (caseType keyform . clauses) env
    (let1 (key (eval keyform env))
      (let1 loop (clauses clauses)
        (if (null? clauses) #inert ;(error ($ "type of " key " not " Object))
          (let1 (((class . forms) . clauses) clauses)
            (if (or (== class 'else) (type? key (eval class env)))
              (eval (list* 'begin forms) env)
              (loop clauses) ))))))
|#

(def checkSlot 
  (\ (c slot)
    (let1 next (slot slot)
      (if (null? slot) #t
        (let1 ((name value . slot) slot)
          (if (or (! (@isBound c name)) (! (eq? (c name) value))) #f
            (next slot) ))))))

(def\ (evlis env lst)
  (map (\ (elt) (eval elt env)) lst))

(defVau (caseType keyform . clauses) env
  (let1 (key (eval keyform env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert ;(error ($ "type of " key " not " Object))
        (let1 (((test . forms) . clauses) clauses)
          (if (== test 'else)
            (eval (list* 'begin forms) env)
            (let* ( (symbol? (%symbol? test))
                    (className (if symbol? test (car test)))
                    (class (eval className env)) )
              (if (&& (type? key class) (|| symbol? (&& (type? key Obj) (checkSlot key (evlis env (cdr test))))))
                (eval (list* 'begin forms) env)
                (next clauses) ))))))))

(assert (caseType 2.0 (else 3)) 3)
(assert (caseType 2.0 (String "string") (Double "double")) "double")
(assert (caseType (obj Obj :a 1) (Double "double") ((Obj :a 1) "Obj :a 1")) "Obj :a 1")


;;;; Options

(defVau (opt? exp dft) env
  (let1 (exp (eval exp env))
    (if (cons? exp)
      (let1 ((val) exp) val)
      (eval dft env) ))) 

(assert (opt? '(2) 10) 2)
(assert (opt? #null 10) 10)

(defVau (ifOpt? (pt exp) then . else) env
  (let1 (exp (eval exp env))
    (if (cons? exp)
      (eval (list* (list 'vau (if (symbol? pt) (list pt) pt) #ignore then) exp) env)
      (if (null? else) #null
        (eval (let1 ((exp) else) exp) env) ))))

(assert (ifOpt? (a ()) (+ 1 a)) #null)
(assert (ifOpt? (a '(2)) (+ 1 a)) 3)
(assert (ifOpt? (a '(2 2)) (+ 1 a)))

(assert (ifOpt? ((a) ()) (+ 1 a)) #null)
(assert (ifOpt? ((a) ()) (+ 1 a) 0) 0)
(assert (ifOpt? ((a) ()) (+ 1 a) 0 1))
(assert (ifOpt? ((a) '(2)) (+ 1 a)) 3)
(assert (ifOpt? ((a) '(2 3)) (+ 1 a)))
(assert (ifOpt? ((a b) '(2 3)) (+ 1 b)) 4)

(defMacro whenOpt? ((pt opt?) form . forms)
  (list ifOpt? (list pt opt?) (list* progn form forms)))

(defMacro unlessOpt? (opt? form . forms)
  (list ifOpt? (list #ignore opt?) #null (list* progn form forms)))

(defVau (caseOpt? exp . clauses) env
  (let1 (exp (eval exp env))
    (if (null? exp) #null
      (let1 loop (clauses clauses)
        (if (null? clauses) #inert ;(error "not a valid opt? form! ")
          (let ((env+ (makeEnv env))
                (((bindings . forms) . clauses) clauses) )
            (if (or (== bindings 'else) (bind? env+ bindings exp))
              (eval (list* 'begin forms) env+)
              (loop clauses) )))))))

(assert (caseOpt? () ((a) 1) ((a b) (+ a b))) ())
(assert (caseOpt? '(2) ((a) 1) ((a b) (+ a b))) 1)
(assert (caseOpt? '(1 2) ((a) 1) ((a b) (+ a b))) 3)
(assert (caseOpt? '(1 2 3) ((a) 1) ((a b) (+ a b))) #inert)

(defVau (defOpt? pt exp) env
  (def exp (eval exp env))
  (unless (null? exp) 
    (eval (list 'def pt exp) env) )) 


;;; Loop

(def\ (callWithEscape fun)
  (let1 (fresh (list #null))
    (catch (fun (\ opt? (throw (list fresh (ifOpt? ((val) opt?) opt?) ))))
      (\ (exc)
        (if (and (cons? exc) (== (car exc) fresh))
          (let1 ((#ignore opt?) exc) (if (cons? opt?) (car opt?)))
          (throw exc))))))

(defMacro (label name . body)
  (list 'callWithEscape (list* '\ (list name) body)))

(assert (label return (return)) #inert)
(assert (label return (return 3)) 3)
(assert (label return (return 3 4)))

(defVau (while test . body) env
  (let ((body (list* 'begin body))
        (break (list #null))
        (continue (list #null)) )
    (eval (list 'def 'break (\ v (throwTag break (if (! (null? v)) (if (null? (cdr v)) (car v) v))))) env)
    (eval (list 'def 'continue (\ () (throwTag continue))) env)
    (catchTag break
      (loop
        (catchTag continue
          (if (eval test env) 
            (eval body env)
            (throwTag break) ))))))

(defMacro (-- n) 
  (list 'def n :rhs (list '- n 1)) ) 
(defMacro (++ n) 
  (list 'def n :rhs (list '+ n 1)) ) 

(assert (let1 (c 2) (while (> c 0) (-- c)) c) 0)
(assert (let1 (c 2) (while #t (if (zero? c) (break (+ 5 5)) (-- c)))) 10)
(assert (let ((c 10) (r #null)) (while #t (if (zero? c) (break r)) (if (zero? (% (-- c) 2)) (continue)) (def r (cons c r)) )) '(1 3 5 7 9))

(defMacro (until test-form . forms)
  (list* while (list '! test-form) forms) )

(defVau (block blockName . forms) env
  (let* ((tag (list #inert))
         (escape (\ (value) (throwTag tag value))) )
    (catchTag tag
      (eval (list (list* '\ (list blockName) forms) escape) env) )))

(assert (block exit (def x 1) (loop (if (== x 4) (exit 7)) (def x (+ x 1)))) 7)

(def\ (returnFrom blockName . value?)
  (blockName (unless (null? value?) (car value?))) )

(assert (block ciclo (def x 1) (loop (if (== x 4) (returnFrom ciclo 7)) (def x (+ x 1)))) 7)


;;;; Type parameters check type\

(defMacro (type\ params . body)
  (letrec ((typedParams->namesAndChecks
            (\ (ps)
              (if (cons? ps)
                  (let* (((p . restPs) ps)
                         ((names . checks) (typedParams->namesAndChecks restPs)))
                    (if (cons? p)
                        (let* (((name type) p)
                               (check (list 'the type name)))
                          (cons (cons name names) (cons check checks)) )
                        (cons (cons p names) checks) ))
                  (cons ps ()) ))))
    (let1 ((names . checks) (typedParams->namesAndChecks params))
      (list* '\ names (if (null? checks) body (list* (list* 'begin checks) body))) )))

(defMacro (the type obj)
  (list 'if (list 'type? obj type) obj (list 'error (list '$ obj " is not a: " type))) )

(assert (expand (type\ (a) (+ a 1))) '(\ (a) (+ a 1)))
(assert (expand (type\ ((a Integer)) (+ a 1))) '(\ (a) (begin (the Integer a)) (+ a 1)))

(defMacro (define lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (car rhs))
    (list 'def (car lhs) (list* 'type\ (cdr lhs) rhs)) ))


;;;; List utilities

(def\ (forEach f . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) (car lst*))
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) lst*) ))
  
#|
(forEach (\ (a) (log a)) '(1 2))
(forEach (\ (a b) (log a b)) '(1 2) '(3 4))
|#

(def\ (filter f . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (cons (car lst) (filter (cdr lst))) (filter (cdr lst))))) (car lst*))
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (cons cars (filter* (map cdr lst*))) (filter* (map cdr lst*)) )))) lst*) ))

(def\ (even n) (== (% n 2) 0))
(def\ (odd n)  (== (% n 2) 1))

(assert (filter even '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (filter != '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(def\ (reduceL f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (reduce acc lst) (if (null? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init (car lst*))
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (reduce* (%apply* f acc (map car lst*)) (map cdr lst*)) )) init lst*) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def\ (reduceR f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (reduce acc lst) (if (null? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init (car lst*))
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (%apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init lst*) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (foldl acc lst) (if (null? lst) acc (foldl (f (car lst) acc) (cdr lst)) )) init (car lst*))
    ((rec\ (foldl* acc lst*) (if (null? (car lst*)) acc (foldl* (%apply* f (map car lst*) acc) (map cdr lst*)) )) init lst*) ))

(assert (foldL cons () '(1 2 3 4)) '(4 3 2 1))

(def\ (foldR f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (foldr acc lst) (if (null? lst) acc (f (car lst) (foldr acc (cdr lst)) ) )) init (car lst*))
    ((rec\ (foldr* acc lst*) (if (null? (car lst*)) acc (%apply* f (map car lst*) (foldr* acc (map cdr lst*)) ) )) init lst*) ))

(assert (foldR cons () '(1 2 3 4)) '(1 2 3 4))

(def\ (make\* n f)
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))))
  
(assert ((make\* 2 (\(a b) b)) 1 2 3 4 5) '(2 3 4 5))


;;;; Dynamic Binding

#| TODO sostituite dalle seguenti, eliminare
(defVau (progv dyns vals . forms) env
  (eval
    (list* '%dLet 
      (cons
        (eval (list* 'list dyns) env)
        (eval (list* 'list vals) env) )
      forms )
    env ))

(assert (begin (ddef a 1) (progv (a) (2) (assert (dval a) 2)) (assert (dval a) 1)) #t)

(defVau (dlet bindings . forms) env
  (eval
    (list* '%dLet
      (cons
        (map (\ ((name #ignore))  (eval name env)) bindings)
        (map (\ ((#ignore value)) (eval value env)) bindings) )
      forms )
    env ))

(assert (begin (ddef* (a b) 1 2) (dlet ((a 2)) (assert (dval a) 2)) (dval b)) 2)

(def d\
  (vau (var* . body) #ignore
    (wrau val* env
        (def\ (ckdvar var)
            (def lkp (@get env var))
            (def ndv (.value lkp))
            ;(if (or (and (null? body) (null? ndv)) (instanceof? ndv DVar)) ndv
            ;  (error ($ "not " (if (null? body) "null or " "") "a dynamic value: " var)) )
            (if (or (and (null? body) (! (.isBound lkp))) (instanceof? ndv DVar)) ndv
              (error ($ "not " (if (null? body) "unbound or " "") "a dynamic value: " var)) ))
        (def ndv* (map ckdvar var*))
        (unless (null? body) (def old* (map (\ (ndv) (if (null? ndv) ndv (ndv))) ndv*)))
        (forEach (\ (ndv var val) (if (instanceof? ndv DVar) (ndv val) (@def env var (dvar val)) )) ndv* var* (if (null? val*) (map (\ (var) #null) var*) val*))
        (unless (null? body)
          (finally
            (eval (list* 'begin body) env) 
            (forEach (\ (ndv old) (ndv old)) ndv* old*) )))))

;((d\ (d e) (print e)) 4 5)     
;((d\ (d e)) 6 7)
|#

(defMacro (ddef var . val)
  (list (list '%d\ (list (the Symbol var))) (opt? val #null) ))

(defMacro (ddef* var* . val*)
  (list* (list '%d\ var*) val*) )

(defMacro (progv var* val* exp . exps)
  (list* (list* '%d\ var* exp exps) val*) )

(defMacro (dlet bindings exp . exps)
  (list* (list* '%d\ (map car bindings) exp exps) (map cadr bindings)) )  

(defMacro (dlet* bindings . body)
  (if (null? bindings)
    (list* 'begin body)
    (list 'dlet
	  (list (car bindings))
      (list* 'dlet* (cdr bindings) body) )))
      
(def a (dvar 1))
(assert (expand (ddef a 1)) '((%d\ (a)) 1) )
(assert (expand (ddef* (a b) 1 2)) '((%d\ (a b)) 1 2) )  
(assert (expand (progv (a b) (3 4)  (+ (a) (b)))) '((%d\ (a b) (+ (a) (b))) 3 4) )
(assert (expand (dlet ((a 3) (b 4)) (+ (a) (b)))) '((%d\ (a b) (+ (a) (b))) 3 4) )
(ddef* (a b) 1 2)
(assert (progv (a b) (3 4)  (+ (a) (b))) 7)
(assert (dlet ((a 3) (b 4)) (+ (a) (b))) 7)
(assert (begin (ddef a 1) (progv (a) (2) (assert (dval a) 2)) (assert (dval a) 1)) #t)
(assert (begin (ddef* (a b) 1 2) (dlet ((a 2)) (assert (dval a) 2)) (dval b)) 2)
(assert (begin (ddef* (a) 1) (dlet* ((a (+ 1 (dval a))) (a (+ 1 (dval a)))) (dval a))) 3)


;;;; Class Object Method

(defMacro (defClass className . superClass? #| slot-specs . properties |# )
  (list 'def className (list* '%class (list 'quote className) superClass?)))

(def obj %obj) 

(defMacro (defObj name class . attr)
  (list 'def name (list* obj class attr)) )

;; receiver e parameters dei defMethod dovrebbero corrispondere a quelli del corrispondente defGeneric con quel nome

(defVau (defGeneric . args) env
  (if (symbol? (car args))
    (def (name (receiver . parameters) . properties) args)
    (def ((name receiver . parameters) . properties) args) )
  (def\ (generic . args) (apply (%getMethod (%classOf (car args)) name) args))
  (eval (list 'def name generic) env) )

(defVau (defMethod . args) env
  (if (symbol? (car args))
    (def (name ((receiver class) . parameters) . forms) args)
    (def ((name (receiver class) . parameters) . forms) args) )
  (def method (eval (list* '\ (list* receiver parameters) forms) env))
  (%addMethod (eval class env) name method)
  #inert )

(assert 
  (begin
    (defClass Foo)
    (defClass Bar Foo)
    (defGeneric g1 (obj p))
    (defMethod g1 ((foo Foo) p) (+ p 100))

    (defObj foo Foo)
    (defObj bar Bar :a 1 :b (+ 2 3))

    (assert (g1 foo (+ 1 1)) 102)
    (assert (g1 bar (+ 2 3)) 105)

    (defMethod (g1 (bar Bar) p) (+ p (bar :b)))

    (assert (bar :b) 5)
    (assert (g1 bar 2) 7)
    (assert (g1 bar (* 2 (bar :b))) 15)
    (assert (bar :b :prv 6) 5)
    (assert (bar :b) 6) )
  #t )
  
#|
(defVau (object . pairs) env
  (let ((obj (%jsMakeObject)))
    (map (\ ((name value))
                (set ((jsGetter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(def\ (elt object key)
  ((jsGetter key) object))

(defMacro (set (getter . args) new-val)
  (list* (list 'setter getter) new-val args))

(set (setter elt)
  (type\ (newVal object key)
    (set ((jsGetter key) object) newVal) ))
|#


;;;; Modules

(defVau (provide symbols . body) env
  (eval
    (list 'def symbols
      (list 'let ()
        (list* 'begin body)
        (list* 'list symbols) ))
    env ))

(assert (begin (provide (x) (define x 10)) x) 10)

(defVau (module exports . body) env
  (let ((env (makeEnv env)))
    (eval (list* 'provide exports body) env)
    (makeEnv env) ))

(assert (begin (define m (module (x) (define x 10))) (eval 'x m)) 10)

(defMacro (defModule name exports . body)
  (list 'def name (list* 'module exports body)) )

(assert (begin (defModule m (x) (define x 10)) (eval 'x m)) 10)

(defVau (import module imports) env
  (let* ((module (eval module env))
         (values (map (\ (import) (eval import module)) imports)) )
    (eval (list* 'def* imports values) env) ))

(assert (begin (defModule m (x) (define x 10)) (import m (x)) x) 10)


;;;; Java

(def\ (relationalOp binop)
  (rec\ (op arg1 arg2 . rest)
    (if (binop arg1 arg2)
      (if (null? rest) #t
        (apply op (list* arg2 rest)))
      #f )))

(def == (relationalOp ==))
(def < (relationalOp <))
(def > (relationalOp >))
(def <= (relationalOp <=))
(def >= (relationalOp >=))

(assert (== 1 1 1) #t)
(assert (< 1 2 3) #t)
(assert (> 3 2 1) #t)
(assert (<= 1 2 2 3) #t)
(assert (>= 3 2 2 1) #t)

(def\ (!= . args) (not (apply == args)))

(def\ (positiveOp binop unit)
  (\ args (reduceL binop unit args)))

(def $ (positiveOp $ ""))
(def + (positiveOp + 0))
(def * (positiveOp * 1))

(assert ($ 1 2 3) "123")
(assert (+ 1 2 3) 6)
(assert (* 1 2 3) 6)

(def\ (negativeOp binop unit)
  (\ (arg1 . rest)
    (if (null? rest)
      (binop unit arg1)
      (reduceL binop arg1 rest) )))

(def - (negativeOp - 0))
(def / (negativeOp / 1))


#| TODO da rivedere
(def\ (jsCallback fun)
  (%jsFunction (\ args (pushPrompt rootPrompt (apply fun args)))) )

(defMacro (jsLambda params . body)
  (list 'jsCallback (list* 'type\ params body)))

(def\ (log x . xs)
  (apply @log (list* &console x xs))
  x)
|#


;;;; Greatest Common Divisor e Lowest Common Multiple

(def\ (gcd a b . more)
  (if (null? more) 
    (if (zero? b) a (gcd b (% a b)))
    (gcd a (apply gcd (cons b more))) ))

(def abs (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))  

(assert (gcd 8 108) 4)
(assert (gcd 108 216 432) 108)

(def abs (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))  

(def\ (lcm a b . more)
  (if (null? more)
    (if (or (zero? a) (zero? b)) 0
      (abs (* b (/ a (gcd a b)))) )
    (lcm a (apply lcm (cons b more))) ))

(assert (lcm 8 108) 216)
(assert (lcm 3 4 5 6) 60)


#|
;;;; Cells

(defPrototype Cell Object (value))
(define (cell value) (new Cell value))
(define (ref (c Cell)) (.value c))
(set (setter ref) (type\ (newVal (c Cell)) (set (.value c) newVal)))
|#


;;;; Box

(def box %box)

(defMacro (defBox name . value?)
  (list 'def name (list 'box (opt? value? #null)) ))


;;;; Auto Increment/Decrement and Assignement Operator

;(defVau ($set! exp1 formals exp2) env
;  (eval (list 'def formals (list '(unwrap eval) exp2 env)) (eval exp1 env)))

(defVau (++ plc . args) env
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (+ (val) 1))))
    (Obj (let1 ((fld) args) (val fld :rhs (+ (val fld) 1)))) 
    (Number (let1 (() args) (eval (list '%set! plc :rhs (+ val 1)) env)))
    (else   (error ($ "not valid type: " val))) )) 

(defVau (-- plc . args) env
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (- (val) 1))))
    (Obj (let1 ((fld) args) (val fld :rhs (- (val fld) 1)))) 
    (Number (let1 (() args) (eval (list '%set! plc :rhs (- val 1)) env)))
    (else   (error ($ "not valid type: " val))) ))

(assert (begin (def obj (obj Obj :a 1)) (++ obj :a) (++ obj :a) (-- obj :a)) 2)
(assert (begin (def box (box 1)) (++ box) (++ box) (-- box)) 2)
(assert (begin (def n 1) (++ n) (++ n) (-- n)) 2)

(def\ (assignOp op)
  (vau (plc . args) env
    (def lval (eval plc env))
    (caseType lval
      (Box (match args 
        ((rval) (lval (op (lval) (eval rval env)))) 
        ((key rval) (lval key (op (lval) (eval rval env)))) ))
      (Obj (match args
        ((fld rval) (lval fld (op (lval fld) (eval rval env))))
        ((fld key rval) (lval fld key (op (lval fld) (eval rval env)))) ))
      (Object (match args
        ((rval) (eval (list 'def plc (op lval (eval rval env))) env))
        ((key rval) (eval (list '%set! plc key (op lval (eval rval env))) env)) ))))) 

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (box 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (obj Obj :fld 1)) (+= a :fld :rhs 3)) 4)


;;;; Utilities

(def\ (array . args) (list->array args))

(def Array &java.lang.Object[])

(define (arrayMap fun (arr Array))
  (list->array (map fun (array->list arr))) )

(define (arrayFilter pred (arr Array))
  (list->array (filter pred (array->list arr))) )

(defVau (time exp) env
  (let1 (currentTime (@getMethod System "currentTimeMillis"))
    (def milli (currentTime #null))
    (def result (eval exp env))
    (def milli (- (currentTime #null) milli))
    (log ($ "time " exp ": " milli  "ms"))
    result ))


;;;; Error break routine, called by VM to print stacktrace and throw

(def\ (printStacktrace)
  (def\ (printFrame k)
    (let1 (k (.nxt k)) (unless (null? k) (printFrame k) ))
    (log "v" k) )
  (takeSubcont rootPrompt k
  	(printFrame k) (pushPrompt rootPrompt (pushSubcont k)) ))

(def\ (userBreak err)
  (when (prstk) (log "-" (@getMessage err)) (printStacktrace))
  (throw err) )
