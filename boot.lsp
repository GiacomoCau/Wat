;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def

(%def def %def)
(%def $define! %def)


;; Rename bindings that will be used as provided by VM

(def == %==)
(def != %!=)

(def $ %$)
(def + %+)
(def * %*)
(def - %-)
(def / %/)
(def % %%)
  
(def ! %!)
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

(def \ %lambda)
(def append %append)
(def array->list %array->list)
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
(def list* %list*)
(def list->array %list->array)
(def loop %loop)
(def makeEnv %makeEnv)
(def nil? %null?)
(def null? %null?)
(def not !)
(def reverse %reverse)
(def rootPrompt %rootPrompt)
(def string->symbol %string->symbol)
(def symbolName %internName)
(def symbol? %symbol?)
(def type? %type?)
(def unwrap %unwrap)
(def wrap %wrap)


;;;; Basic type

(def Box &Wat.Vm$Box)
(def Cons &Wat.Vm$Cons)
(def DVar &Wat.Vm$DVar)
(def JFun &Wat.Vm$JFun)
(def StdObj &Wat.Vm$StdObj)
(def Symbol &Wat.Vm$Symbol)
(def Keyword &Wat.Vm$Keyword)
(def Boolean &java.lang.Boolean)
(def Date &java.util.Date)
(def Number &java.lang.Number)
(def Integer &java.lang.Integer)
(def Double &java.lang.Double)
(def Object &java.lang.Object)
(def String &java.lang.String)


;;;; Important utilities

(def $vau %vau)
(def quote ($vau (x) #ignore x))
(def quote %quote)
(def list (wrap ($vau elts #ignore elts)))
(def list %list)
(def $lambda ($vau (formals . body) env (wrap (eval (list* $vau formals #ignore body) env))))
(def $lambda %lambda)
(def theEnv ($vau () e e))
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

(def evm (dvar #t))

(def makeMacro
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
        (def !evm (! (evm)))
        (if !evm (evm #t))
        (def exp (eval (cons expander operands) (makeEnv)))
        (if !evm exp (eval exp env)) ))))

(def macro
  (makeMacro
    ($vau (params . body) #ignore
      (list 'makeMacro (list* '$vau params #ignore body)) )))

; defMacro defVau def\ def*\ rec\ e letrec\ permettono le definizioni con le seguenti due sintassi
;    (name parameters . body)
;    ((name . parameters) . body)
; rec rec\ letrec e letrec\ inizializzano a #inert le definizioni prima della valutazione

(def defMacro
  (macro (lhs . rhs)
    (if (symbol? lhs)
      (list 'def lhs (list* 'macro (car rhs) (cdr rhs)))
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs)) )))

(defMacro (expand macro)
  (list 'begin (list 'evm #f) macro))

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))

(defMacro (defVau lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '$vau (car rhs) (cadr rhs) (cddr rhs)))
    (list 'def (car lhs) (list* '$vau (cdr lhs) (car rhs) (cdr rhs))) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ ($vau (n) env (eval (list (%quote +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ ($vau (n) env (eval (list (%quote +) n 1) env))))

(defMacro (def* lhs . rhs)
  (list 'def lhs (list* 'list rhs)) )

(defMacro (def\ lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '\ (car rhs) (cdr rhs)))
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


;;;; Basic value test

(def\ (zero? n) (== n 0))
(def\ (inert? o) (== o #inert))
 

;;;; Wrap incomplete VM forms

(def* (then else) %begin %begin)

(if (ctapv)
  (then
    (defMacro (catch exp . hdl)
      (list* '%catch #ignore (list '\ () exp) hdl) )
    (defMacro (throw . val)
      (list* '%throw #ignore val) )

    (assert (catch (throw)) #inert)
    (assert (catch (throw (\ () 1))) 1)
    (assert (catch (throw (\ () 1)) (\ (x) (+ x 1))) 2)
    
    (defMacro (catchTag tag exp . hdl)
      (list* '%catch tag (list '\ () exp) hdl) )
    (defMacro (throwTag tag . val)
      (list* '%throw tag val) )
    (def throwTag %throw)
  )
  (else
    (defMacro (catch x . hdl)
      (list* '%catch #ignore x hdl))
    (defMacro (throw . x)
      (list* '%throw #ignore x))

    (assert (catch (throw)) #inert)
    (assert (catch (throw 1)) 1)
    (assert (catch (throw 1) (\ (x) (+ x 1))) 2)

    (defVau (catchTag tag exp . hdl) env
      (eval (list* '%catch (eval tag env) exp hdl) env) )
    (defVau (throwTag tag . val) env
      (eval (list* '%throw (eval tag env) val) env) )
  )
)

(defVau (finally protected . cleanup) env
  (eval (list '%finally protected (list* 'begin cleanup)) env) )

(defMacro (takeSubcont prompt k . body)
  (list '%takeSubcont prompt (list* '\ (list k) body)) )

(defVau (pushPrompt prompt . body) env
  (eval (list '%pushPrompt (eval prompt env) (list* 'begin body)) env) )

(defMacro (pushPromptSubcont p k . body)
  (list '%pushPromptSubcont p k (list* '\ () body)) )

(defMacro (pushSubcont k . body)
  (list '%pushPromptSubcont #ignore k (list* '\ () body)) )


;;;; Important macros and functions

(def\ (apply appv args . optEnv)
  (def env (if (null? optEnv) (makeEnv) (car optEnv))) 
  (if (%jFun? appv)
    (@combine (@new JFun vm appv) env args)
    (eval (cons (unwrap appv) args) env) ))

(defMacro (rec lhs . rhs)
  (list (list '\ () (list 'def lhs #inert) (list* 'def lhs rhs) lhs)) )

(defMacro (rec\ lhs . rhs)
  (def sym (if (symbol? lhs) lhs (car lhs)))
  (list (list '\ () (list 'def sym #inert) (list* 'def\ lhs rhs) sym)) )

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

(defMacro (let bindings . body)
  (if (symbol? bindings)
    (list* 'letLoop bindings body)
    (list* (list* '\ (map car bindings) body)
      (map cadr bindings) )))

(assert (let ((a 1)) a) 1)

(defMacro (let1 binding . body)
  (if (symbol? binding)
    (list* 'letLoop binding (list (car body)) (cdr body))
    (list (list* '\ (list (car binding)) body)
      (cadr binding) )))

(assert (let1 (a 1) a) 1)
    
(defMacro (let* bindings . body)
  (if (nil? bindings)
      (list* 'let () body)
      (list 'let
        (list (car bindings))
        (list* 'let* (cdr bindings) body) )))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)

(defMacro (letrec bindings . body)
  (list* 'let ()
    (list* 'def* (map car bindings) (map (\ (#ignore) #inert) bindings))
    (list* 'def* (map car bindings) (map cadr bindings))
    body ))

(assert (letrec ( (even? (\ (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (zero? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . body)
  (list* 'let ()
    (list* 'def* (map (\ ((lhs . rhs)) (if (symbol? lhs) lhs (car lhs))) bindings) (map (\ (#ignore) #inert) bindings))
    (list* 'def*\ (map car bindings) (map cdr bindings))
    body ))

(def labels letrec\)

(assert (labels ( ((even? n) (if (zero? n) #t (odd? (- n 1)))) ((odd? n) (if (== n 0) #f (even? (- n 1)))) ) (even? 88) ) #t)
(assert (labels ( (even? (n) (if (zero? n) #t (odd? (- n 1)))) (odd? (n) (if (== n 0) #f (even? (- n 1)))) ) (even? 88) ) #t)

(defMacro (letLoop name bindings . body)
  (list 'letrec
    (list (list name (list* '\ (map car bindings) body)))
    (list* name (map cadr bindings)) ))

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (nil? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))

(def\ (member item lst)
  ((rec\ (loop lst)
     (if (null? lst) #null
       (if (== item (car lst)) lst
         (loop (cdr lst)) )))
   lst ))

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

(def\ (assoc item lst)
  ((rec\ (loop lst)
     (if (null? lst) #null
       (if (== item (caar lst)) (car lst)
         (loop (cdr lst)))))
   lst ))

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (get? key lst)
  ((rec\ (loop lst) 
     (if (null? lst) #null
       (let1 ((k v . lst) lst)
         (if (== k key) (list v) (loop lst)))))
   lst ))

(assert (get? :b '(:a 1 :b 2 :c 3)) '(2)) 


;;;; Bind

;(def bind %bind)
(def bind? %bind?)

(defVau (ifBind (pt exp) then . else) env
  (let1 (env+ (makeEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (let1 ((exp) else) exp) env) ))))


;;;; Options

(defVau (opt? exp dft) env
  (let1 (exp (eval exp env))
    (if (cons? exp)
      ;(car exp) ; unchecked lenght
      (let1 ((val) exp) val) ; check length 1
      ;((\ ((val)) val) exp) ; check length 1
      ;(def (val) :rhs exp) ; check length 1
      (eval dft env) ))) 

(assert (opt? '(2) 10) 2)
(assert (opt? #null 10) 10)

(defVau (ifOpt? (pt exp) then . else) env
  (let1 (exp (eval exp env))
    (if (cons? exp)
      (eval (list* (list '$vau (if (symbol? pt) (list pt) pt) #ignore then) exp) env)
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

(defMacro whenOpt? ((name opt?) form . forms)
  (list ifOpt? (list name opt?) (list* progn form forms)))

(defMacro unlessOpt? (opt? form . forms)
  (list ifOpt? (list #ignore opt?) #null (list* progn form forms)))

(defVau (caseOpt? exp . clauses) env
  (let1 (exp (eval exp env))
    (if (! (cons? exp)) #null
      (let1 loop (clauses clauses)
        (if (null? clauses) (error "not a valid opt? form! ")
          (let ((env+ (makeEnv env))
                (((bindings . forms) . clauses) clauses) )
            (if (bind? env+ bindings exp)
              (eval (list* 'begin forms) env+)
              (loop clauses) )))))))

(assert (caseOpt? () ((a) 1) ((a b) (+ a b))) ())
(assert (caseOpt? '(2) ((a) 1) ((a b) (+ a b))) 1)
(assert (caseOpt? '(1 2) ((a) 1) ((a b) (+ a b))) 3)
(assert (caseOpt? '(1 2 3) ((a) 1) ((a b) (+ a b))))

(defVau (defOpt? pt exp) env
  (def exp (eval exp env))
  (unless (null? exp) 
    (eval (list 'def pt exp) env) )) 


;;;; Type parameters check lambda

(defMacro (lambda params . body)
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

(assert (expand (lambda (a) (+ a 1))) '(\ (a) (+ a 1)))
(assert (expand (lambda ((a Integer)) (+ a 1))) '(\ (a) (begin (the Integer a)) (+ a 1)))

(defMacro (define lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (car rhs))
    (list 'def (car lhs) (list* 'lambda (cdr lhs) rhs)) ))


;;;; Simple control

(defMacro (when test . body)
  (list 'if test (list* 'begin body)))

(defMacro (unless test . body)
  (list* 'when (list 'not test) body))

(defVau (cond . clauses) env
  (if (nil? clauses) #inert
    (let ((((test . body) . clauses) clauses))
      (if (== test 'else)
        (apply (wrap begin) body env) 
        (if (eval test env)
          (apply (wrap begin) body env)
          (apply (wrap cond) clauses env) )))))

(defVau (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(def && and)

(defVau (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(def || or)

(def\ (callWithEscape fun)
  (let1 (fresh (list #null))
    (catch (fun (\ opt? (throw (list fresh opt?))))
      (\ (exc)
        (if (and (cons? exc) (== (car exc) fresh))
          (let1 (opt? (cadr exc)) (if (cons? opt?) (car opt?)))
          (throw exc))))))

(defMacro (label name . body)
  (list 'callWithEscape (list* '\ (list name) body)))

(assert (label return (return)) #inert)
(assert (label return (return 3)) 3)

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

(defVau (block block-name . forms) env
  (let* ((tag (list #inert))
         (escape (\ (value) (throwTag tag value))) )
    (catchTag tag
      (eval (list (list* '\ (list block-name) forms) escape) env) )))

(def\ (returnFrom block-name . value?)
  (block-name (unless (null? value?) (car value?))) )


;;; Case

(defVau (case exp . clauses) env
  (let1 (value (eval exp env))
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((values . forms) . clauses) clauses)
          (if (or (== values 'else) (cons? (member value values)))
            (eval (list* 'begin forms) env)
            (loop clauses) ))))))

(assert (case 3 ((2 4 6 8) 'pair) ((1 3 5 7 9) 'odd)) 'odd) 

(defVau (caseVau . clauses) env 
  ($vau values #ignore
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ((env+ (makeEnv env))
              (((bindings . forms) . clauses) clauses) )
          (if (bind? env+ bindings values)
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))

#|
(defVau (case\ . clauses) env
  (wrap
    ($vau values #ignore
      (let1 loop (clauses clauses)
        (if (null? clauses) #inert
          (let ((env+ (makeEnv env))
                (((bindings . forms) . clauses) clauses) )
            (if (bind? env+ bindings values)
              (eval (list* 'begin forms) env+)
              (loop clauses) )))))))

(defVau (case\ . clauses) env
  (\ values
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ((env+ (makeEnv env))
              (((bindings . forms) . clauses) clauses) )
          (if (bind? env+ bindings values)
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
|#

(defMacro (case\ . clauses)
  (list 'wrap (list* 'caseVau clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))

#|
(defVau (match exp . clauses) env
  (let1 (exp (eval exp env))
    (let1 loop (clauses clauses)
      (if (null? clauses) (error "not a valid match form! ")
        (let ((env+ (makeEnv env))
              (((bindings . forms) . clauses) clauses) )
          (if (or (== bindings 'else) (bind? env+ bindings exp))
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
|#

(defMacro (match exp . clauses)
  (list (list* 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )  

;(defMacro (match exp . clauses)
;  (list* (list* 'case\ clauses) (list exp)) )  
;(defVau (match exp . clauses) env
;  (eval (list* (list 'caseVau clauses) (eval exp env)) env) )

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match 1 ((a) 1) ((a b) 2) (else 3)) 3)

(def\ makeCaseType (default)
  ($vau (keyform . clauses) env
    (let1 (key (eval keyform env))
      (let1 loop (clauses clauses)
        (if (null? clauses) (default key)
          (let1 (((class . forms) . clauses) clauses)
            (if (type? key (eval class env))
              (eval (list* 'begin forms) env)
              (loop clauses) )))))))

(def caseType (makeCaseType (\ (#ignore) #inert)))
(def eCaseType (makeCaseType (\ (key) (error "type of " key " not" Object))))

(assert (caseType 2.0 (String "string") (Double "double")) "double")


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
    ((rec\ (reduce acc lst) (if (nil? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init (car lst*))
    ((rec\ (reduce* acc lst*) (if (nil? (car lst*)) acc (reduce* (%apply* f acc (map car lst*)) (map cdr lst*)) )) init lst*) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def\ (reduceR f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (reduce acc lst) (if (nil? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init (car lst*))
    ((rec\ (reduce* acc lst*) (if (nil? (car lst*)) acc (%apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init lst*) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (foldl acc lst) (if (nil? lst) acc (foldl (f (car lst) acc) (cdr lst)) )) init (car lst*))
    ((rec\ (foldl* acc lst*) (if (nil? (car lst*)) acc (foldl* (%apply* f (map car lst*) acc) (map cdr lst*)) )) init lst*) ))

(assert (foldL cons () '(1 2 3 4)) '(4 3 2 1))

(def\ (foldR f init . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec\ (foldr acc lst) (if (nil? lst) acc (f (car lst) (foldr acc (cdr lst)) ) )) init (car lst*))
    ((rec\ (foldr* acc lst*) (if (nil? (car lst*)) acc (%apply* f (map car lst*) (foldr* acc (map cdr lst*)) ) )) init lst*) ))

(assert (foldR cons () '(1 2 3 4)) '(1 2 3 4))

(def\ (call* n f)
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))))
  
(assert ((call* 2 (\(a b) b)) 1 2 3 4 5) '(2 3 4 5))

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
|#

(def d\
  (%vau (var* . body) #ignore
    (wrap ($vau val* env
        (def ckdvar (\ (var)
            (def lkp (@get env var))
            (def dv (.value lkp))
            ;(if (or (and (null? body) (null? dv)) (instanceof? dv DVar)) dv
            ;  (error ($ "not " (if (null? body) "null or " "") "a dynamic value: " var)) )))
            (if (or (and (null? body) (! (.isBound lkp))) (instanceof? dv DVar)) dv
              (error ($ "not " (if (null? body) "unbound or " "") "a dynamic value: " var)) )))
        (def dv* (map ckdvar var*))
        (unless (null? body) (def old* (map (\ (dv) (dv)) dv*)))
        (forEach (\ (dv var val) (if (instanceof? dv DVar) (dv val) (@put env var (dvar val)) )) dv* var* val*)
        (unless (null? body)
          (finally
            (eval (list* 'begin body) env) 
            (forEach (\ (dv old) (dv old)) dv* old*) ))))))

;((d\ (d e) (print e)) 4 5)     
;((d\ (d e)) 6 7)

(defMacro (ddef var val)
  (list (list 'd\ (list var)) val) )

(defMacro (ddef* var* . val*)
  (list* (list 'd\ var*) val*) )

(defMacro (progv var* val* exp . exps)
  (list* (list* 'd\ var* exp exps) val*) )

(defMacro (dlet bindings exp . exps)
  (list* (list* 'd\ (map car bindings) exp exps) (map cadr bindings)) )  

(defMacro (dlet* bindings . body)
  (if (nil? bindings)
    (list* 'begin body)
    (list 'dlet
	  (list (car bindings))
      (list* 'dlet* (cdr bindings) body) )))
      
(def a (dvar 1))
(assert (expand (ddef a 1)) '((d\ (a)) 1))
(assert (expand (ddef* (a b) 1 2)) '((d\ (a b)) 1 2))  
(assert (expand (progv (a b) (3 4)  (+ (a) (b)))) '((d\ (a b) (+ (a) (b))) 3 4))
(assert (expand (dlet ((a 3) (b 4)) (+ (a) (b)))) '((d\ (a b) (+ (a) (b))) 3 4))
(ddef* (a b) 1 2)
(assert (progv (a b) (3 4)  (+ (a) (b))) 7)
(assert (dlet ((a 3) (b 4)) (+ (a) (b))) 7)
(assert (begin (ddef a 1) (progv (a) (2) (assert (dval a) 2)) (assert (dval a) 1)) #t)
(assert (begin (ddef* (a b) 1 2) (dlet ((a 2)) (assert (dval a) 2)) (dval b)) 2)
(assert (begin (ddef* (a) 1) (dlet* ((a (+ 1 (dval a))) (a (+ 1 (dval a)))) (dval a))) 3)


;;;; Class

(defMacro (defClass className . superClass? #| slot-specs . properties |# )
  (list 'def className (list* '%class (list 'quote className) superClass?)))

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

    (def foo (%obj Foo))
    (def bar (%obj Bar :a 1 :b (+ 2 3)))

    (assert (g1 foo (+ 1 1)) 102)
    (assert (g1 bar (+ 2 3)) 105)

    (defMethod g1 ((bar Bar) p) (+ p (bar :b)))

    (assert (bar :b) 5)
    (assert (g1 bar 2) 7)
    (assert (g1 bar (* 2 (bar :b))) 15)
    (assert (bar :b :prv 6) 5)
    (assert (bar :b) 6)
  )
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
  (lambda (newVal object key)
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

(def\ (relationalOp %binop)
  (let ((binop %binop))
    (letrec ((op (\ (arg1 arg2 . rest)
                   (if (binop arg1 arg2)
                       (if (nil? rest) #t
                           (apply op (list* arg2 rest)))
                       #f))))
      op)))

(def == (relationalOp ==))
(def < (relationalOp <))
(def > (relationalOp >))
(def <= (relationalOp <=))
(def >= (relationalOp >=))

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
    (if (nil? rest)
      (binop unit arg1)
      (reduceL binop arg1 rest) )))

(def - (negativeOp - 0))
(def / (negativeOp / 1))


#| TODO da rivedere
(def\ (jsCallback fun)
  (%jsFunction (\ args (pushPrompt rootPrompt (apply fun args)))) )

(defMacro (jsLambda params . body)
  (list 'jsCallback (list* 'lambda params body)))

(def\ (log x . xs)
  (apply @log (list* &console x xs))
  x)


;;;; Cells

(defPrototype Cell Object (value))
(def\ (cell value) (new Cell value))
(def\ (ref (c Cell)) (.value c))
(set (setter ref) (lambda (newVal (c Cell)) (set (.value c) newVal)))
|#

(defVau (++ plc . args) env
  (def val (eval plc env))
  (eCaseType val
    (Box    (let1 (() args) (val :rhs (+ (val) 1))))
    (StdObj (let1 ((fld) args) (val fld :rhs (+ (val fld) 1)))) 
    (Number (let1 (() args) (eval (list 'def plc :rhs (+ val 1)) env))) )) 

(defVau (-- plc . args) env
  (def val (eval plc env))
  (eCaseType val
    (Box    (let1 (() args) (val :rhs (- (val) 1))))
    (StdObj (let1 ((fld) args) (val fld :rhs (- (val fld) 1)))) 
    (Number (let1 (() args) (eval (list 'def plc :rhs (- val 1)) env))) )) 

(assert (begin (def obj (%obj StdObj :a 1)) (++ obj :a) (++ obj :a) (-- obj :a)) 2)
(assert (begin (def box (%box 1)) (++ box) (++ box) (-- box)) 2)
(assert (begin (def n 1) (++ n) (++ n) (-- n)) 2)

(def\ (assignOp op)
  ($vau (plc . args) env
    (def lval (eval plc env))
    (eCaseType lval
      (Box (match args 
        ((rval) (lval (op (lval) (eval rval env)))) 
        ((key rval) (lval key (op (lval) (eval rval env)))) ))
      (StdObj (match args
        ((fld rval) (lval fld (op (lval fld) (eval rval env))))
        ((fld key rval) (lval fld key (op (lval fld) (eval rval env)))) ))
      (Object (match args
        ((rval) (eval (list 'def plc (op lval (eval rval env))) env))
        ((key rval) (eval (list 'def plc key (op lval (eval rval env))) env)) ))))) 

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (%box 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (%obj StdObj :fld 1)) (+= a :fld :rhs 3)) 4)


;;;; Utilities

(def\ (array . args) (list->array args))

(def Array &java.lang.Object[])

(lambda (arrayMap fun (arr Array))
  (list->array (map fun (array->list arr))) )

(lambda (arrayFilter pred (arr Array))
  (list->array (filter pred (array->list arr))) )

(defVau (time exp) env
  (let1 (currentTime (@getMethod &java.lang.System "currentTimeMillis"))
    (def milli (currentTime #null))
    (def result (eval exp env))
    (def milli (- (currentTime #null) milli))
    (log ($ "time " exp ": " milli  "ms"))
    result ))


;;;; Error break routine, called by VM to print stacktrace and throw

(def\ (printStacktrace)
  (def\ (printFrame k)
    ;(log "--" k)
    (if (!= (.next k) #null)
      (printFrame (.next k)) )
    (log "--" k) )
  (takeSubcont rootPrompt k
  	(printFrame k) (pushPrompt rootPrompt (pushSubcont k) )) ;old need 2+ vau args
    ;(pushPrompt rootPrompt (pushSubcont k (printFrame k) )) ;new with 3+ vau args
)

(def\ (userBreak err)
  ;(log "==" err)
  (when (stack) (log "++" err) (printStacktrace))
  (throw err) )
