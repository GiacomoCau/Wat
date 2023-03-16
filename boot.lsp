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

(def DVar &Wat.Vm$DVar)
(def Symbol &Wat.Vm$Symbol)


;;;; Macro

(def evm (dvar #t))

(def makeMacro
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
        (def !evm (! (evm)))
        (if !evm (evm #t))
        (def expr (eval (cons expander operands) (makeEnv)))
        (if !evm expr (eval expr env)) ))))

(def macro
  (makeMacro
    ($vau (params . body) #ignore
      (list 'makeMacro (list* '$vau params #ignore body)) )))

; defMacro def\ defVau rec def*\ e labels permettono le definizioni con le seguenti due sintassi
;    (name parameters . body)
;    ((name . parameters) . body)
; ma solo rec letrec e labels inizializzano a #inert le definizioni prima di valutarne il valore

(def defMacro
  (macro (lhs . rhs)
    (if (symbol? lhs)
      (list 'def lhs (list* 'macro (car rhs) (cdr rhs)))
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs)) )))

(defMacro (expand macro)
  (list 'begin (list 'evm #f) macro))

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))

(defMacro (def* lhs . rhs)
  (list 'def lhs (list* 'list rhs)) )

(defMacro (def\ lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '\ (car rhs) (cdr rhs)))
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )

(defMacro (defVau lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '$vau (car rhs) (cadr rhs) (cddr rhs)))
    (list 'def (car lhs) (list* '$vau (cdr lhs) (car rhs) (cdr rhs))) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ ($vau (n) env (eval (list (%quote +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ ($vau (n) env (eval (list (%quote +) n 1) env))))


;;;; Basic value test

(def\ (zero? n) (== n 0))
(def\ (inert? a) (== a #inert))
 

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
  (if (instanceof? appv &java.util.function.Function)
    (@apply appv (list->array args))
    (eval (cons (unwrap appv) args)
      (if (nil? optEnv) (makeEnv) (car optEnv)) )))

(defMacro (rec lhs . rhs)
  (def fn (if (symbol? lhs) lhs (car lhs)))
  (list (list '\ () (list 'def fn #inert) (list* 'def\ lhs rhs) fn)) )

(assert ((rec (f l)   (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec f (l)   (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec (f . l) (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")
(assert ((rec f l     (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")

(def\ (map f . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec (map lst) (if (null? lst) #null (cons (f (car lst)) (map (cdr lst))) )) (car lst*))
    ((rec (map* lst*) (if (null? (car lst*)) #null (cons (apply f (map car lst*)) (map* (map cdr lst*))) )) lst*) ))

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
    (list* 'def* (map car bindings) (map (\ (b) #inert) bindings))
    (list* 'def* (map car bindings) (map cadr bindings))
    body ))

(assert (letrec ( (even? (\ (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (zero? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (labels bindings . body)
  (list* 'let ()
    (list* 'def* (map (\ (lhs) (if (symbol? (car lhs)) (car lhs) (caar lhs))) bindings) (map (\ (b) #inert) bindings))
    (list* 'def*\ (map car bindings) (map cdr bindings))
    body ))

(assert (labels ( ((even? n) (if (zero? n) #t (odd? (- n 1)))) ((odd? n) (if (== n 0) #f (even? (- n 1)))) ) (even? 88) ) #t)
(assert (labels ( (even? (n) (if (zero? n) #t (odd? (- n 1)))) (odd? (n) (if (== n 0) #f (even? (- n 1)))) ) (even? 88) ) #t)

(defMacro (letLoop name bindings . body)
  (list 'letrec
    (list (list name (list* '\ (map car bindings) body)))
    (list* name (map cadr bindings)) ))

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (nil? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))

(def\ (member item lst)
  ((rec (loop lst)
     (if (null? lst) #null
       (if (== item (car lst)) lst
         (loop (cdr lst)) )))
   lst ))

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

(def\ (assoc item lst)
  ((rec (loop lst)
     (if (null? lst) #null
       (if (== item (caar lst)) (car lst)
         (loop (cdr lst)))))
   lst ))

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (get? key lst)
  ((rec (loop lst) 
     (if (null? lst) #null
       (let1 ((k v . lst) lst)
         (if (== k key) (list v) (loop lst)))))
   lst ))

(defVau (opt? val def) env (let1 (val (eval val env)) (if (!= val #null) (car val) (eval def env)))) 

(assert (get? :b '(:a 1 :b 2 :c 3 :d 4)) '(2)) 
(assert (opt? (get? :b '(:a 1 :b 2 :c 3 :d 4)) 10) 2)
(assert (opt? (get? :f '(:a 1 :b 2 :c 3 :d 4)) 10) 10)


;;;; Type parameters check lambda

(def Boolean &java.lang.Boolean)
(def Date &java.util.Date)
(def Number &java.lang.Number)
(def Integer &java.lang.Integer)
(def Double &java.lang.Double)
(def Object &java.lang.Object)
(def String &java.lang.String)

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
    (%bind env 'break (\ v (throwTag break (if (! (null? v)) (if (null? (cdr v)) (car v) v)))))
    (%bind env 'continue (\ () (throwTag continue)))
    (catchTag break
      (loop
        (catchTag continue
          (if (eval test env) 
            (eval body env)
            (throwTag break) ))))))

(defMacro (-- n) 
  (list 'begin (list 'def n (list '- n 1)) n)) 
(defMacro (++ n) 
  (list 'begin (list 'def n (list '+ n 1)) n)) 

(assert (let1 (c 2) (while (> c 0) (-- c)) c) 0)
(assert (let1 (c 2) (while #t (if (zero? c) (break (+ 5 5)) (-- c)))) 10)
(assert (let ((c 10) (r #null)) (while #t (if (zero? c) (break r)) (if (zero? (% (-- c) 2)) (continue)) (def r (cons c r)) )) '(1 3 5 7 9))

(defMacro (when test . body)
  (list 'if test (list* 'begin body)))

(defMacro (unless test . body)
  (list* 'when (list 'not test) body))

(defMacro (set (getter . args) newVal)
  (list* (list 'setter getter) newVal args))

(def\ makeTypecase (default)
  ($vau (keyform . clauses) env
    (let1 (key (eval keyform env))
      (let1 typecase (clauses clauses)
        (if (null? clauses) (default key)
          (let1 (((class . forms) . clauses) clauses)
            (if (type? key (eval class env))
              (eval (list* 'begin forms) env)
              (typecase clauses) )))))))

(def typecase (makeTypecase (\ (#ignore) #inert)))
(def etypecase (makeTypecase (\ (key) (error "type of " key " not" Object))))

(assert (typecase 2.0 (String "string") (Double "double")) "double")

(defVau (case\ . clauses) env
  (\ values
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ((env (makeEnv env))
              (((bindings . forms) . clauses) clauses) )
          (if (%bind env bindings values)
            (eval (list* 'begin forms) env)
            (loop clauses) ))))))

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)


;;;; List utilities

(def\ (forEach f . lst*)
  (if (null? lst*) (error "none lists"))
  (if (null? (cdr lst*))
    ((rec (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) (car lst*))
    ((rec (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) lst*) ))
  
#|
(forEach (\ (a) (log a)) '(1 2))
(forEach (\ (a b) (log a b)) '(1 2) '(3 4))
|#

(def\ (filter p lst)
  (if (nil? lst) ()
    (if (p (car lst))
      (cons (car lst) (filter p (cdr lst)))
      (filter p (cdr lst)) )))

(def\ (reduce f init lst)
  ((rec (reduce init lst) (if (nil? lst) init (reduce (f init (car lst)) (cdr lst)) )) init lst) )

(def\ (split n lst)
  (let loop ((n n) (h ()) (t lst))
    (if (or (null? t) (<= n 0)) (cons (reverse h) (list t))
      (loop (- n 1) (cons (car t) h) (cdr t)) )))

(def\ (resize n lst)
  (let loop ((n n) (h ()) (t lst))
    (if (null? t) (reverse h)
      (if (<= n 1)
        (reverse (cons (if (null? (cdr t)) (car t) t) h))
        (loop (- n 1) (cons (car t) h) (cdr t)) ))))


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

(defMacro (progv var* val* . body)
  (list* (list* 'd\ var* body) val*) )

(defMacro (dlet bindings . body)
  (list* (list* 'd\ (map car bindings) body) (map cadr bindings)) )  

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


#|
;;;; Prototypes

(defVau (defPrototype name superName propNames) env
  (eval (list 'def name (makePrototype name superName propNames env)) env))

(def\ (makePrototype name superName propNames env)
  (let ((p (apply %jsMakePrototype (list* (symbolName name) (map symbolName propNames))))
        (super (eval superName env)))
    (set (.prototype p) (@create &Object (.prototype super)))
    (set (.constructor (.prototype p)) super)
    p ))

(defMacro (defineGeneric (name . #ignore))
  (list 'def name (lambda args (apply ((jsGetter name) (car args)) args))))

(defMacro (defineMethod (name (self ctor) . args) . body)
  (list 'putMethod ctor (symbolName name) (list* 'lambda (list* self args) body)))

(def\ (putMethod ctor name fun)
  (set ((jsGetter name) (.prototype ctor)) fun))
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


;;;; JavaScript

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
  (\ args (reduce binop unit args)))

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
      (reduce binop arg1 rest) )))

(def - (negativeOp - 0))
(def / (negativeOp / 1))


#| TODO da rivedere
(def % (%jsBinop "%"))
(def not (%jsUnop "!"))
(def in (%jsBinop "in"))
(def typeof (%jsUnop "typeof"))
(def instanceof (%jsBinop "instanceof"))

(def bitand (%jsBinop "&"))
(def bitor (%jsBinop "|"))
(def bitxor (%jsBinop "^"))
(def bitnot (%jsUnop "~"))
(def bitshiftl (%jsBinop "<<"))
(def bitshiftr (%jsBinop ">>"))
(def bitshiftr0 (%jsBinop ">>>"))

(defVau (object . pairs) env
  (let ((obj (%jsMakeObject)))
    (map (\ ((name value))
                (set ((jsGetter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(def\ (elt object key)
  ((jsGetter key) object))

(set (setter elt)
  (lambda (newVal object key)
    (set ((jsGetter key) object) newVal) ))

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

(defMacro (++ place)
  (list 'set place (list '+ place 1)) )
(defMacro (-- place)
  (list 'set place (list '- place 1)) )


;;;; Options

(defPrototype Option Object ())
(defPrototype Some Option (value))
(defPrototype None Option ())
(def\ (some value) (new Some value))
(def\ none (new None))
(defVau (ifOption (optionName optionExpr) then else) env
  (let ((option (the Option (eval optionExpr env))))
    (if (type? option Some)
        (eval (list (list 'lambda (list optionName) then) (.value option)) env)
        (eval else env) )))
|#


;;;; Utilities

(def\ (array . args) (list->array args))

(def Array &java.lang.Object[])

(lambda (arrayMap fun (arr Array))
  (list->array (map fun (array->list arr))) )

(lambda (arrayFilter pred (arr Array))
  (list->array (filter pred (array->list arr))) )

(defVau (time expr) env
  (let1 (currentTime (@getMethod &java.lang.System "currentTimeMillis"))
    (def milli (currentTime #null))
    (def result (eval expr env))
    (def milli (- (currentTime #null) milli))
    (log ($ "time " expr ": " milli  "ms"))
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
