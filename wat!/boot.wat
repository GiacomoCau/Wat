;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def

(%def def
  %def)


;;; Built-Ins - rename bindings that will be used as provided by VM

(def ==
  %==)
(def != %!=)

(def $ %$)
(def + %+)
(def * %*)
(def - %-)
(def / %/)
(def % %%)
  
(def !
  %!)
(def !! %!!)
(def < %<)
(def > %>)
(def <= %<=)
(def >= %>=)
  
(def ~ %~)
(def & %&)
(def \| %\|)
(def ^ %^)
(def << %<<)
(def >> %>>)
(def >>> %>>>)

(def append
  %append)

(def array->list %array->list)
(def assert %assert)
(def begin
  %begin)
(def bind? %bind?)
(def cons
  %cons)
(def cons?
  %cons?)
(def classOf
  %classOf)

(def dval %dVal)
(def dvar %dVar)
(def eq? %eq?)
(def error %error)
(def eval
  %eval)

(def finally
  %finally)

(def if
  %if)
(def instanceOf? %instanceOf?)

(def keyword %keyword)
(def keyword? %keyword?)
(def keywordName %internName)
(def len %len)
(def list*
  %list*)
(def list?
  %list?)

(def list->array %list->array)

(def loop
  %loop)
(def makeEnv
  %makeEnv)
(def null?
  %null?)
(def not
  !)
(def obj %obj)
(def reverse
  %reverse)
(def rootPrompt %rootPrompt)
(def set! %set!)
(def symbol %symbol)
(def symbol? %symbol?)
(def symbolName
  %internName)

(def test %test)
(def type?
  %type?)
(def unwrap
  %unwrap)
(def wrap
  %wrap)


;;; Core Forms

(def vau (%vau (pt ep . forms) env (eval (list '%vau pt ep (list* 'begin forms)) env)))
(def vau
  %vau)

(def theEnv (vau () e e))
(def theEnv
  %theEnv)

(def quote (vau (x) #ignore x))
(def quote
  %quote)

(def list (wrap (vau x #ignore x)))
(def list
  %list)

(def lambda (vau (formals . body) env (wrap (eval (list* vau formals #ignore body) env))))
(def lambda %\)
(def \
  %\)

(def car (\ ((car . #_)) car))
(def car
  %car)

(def cadr (\ ((#_ . (cadr . #_))) cadr))
(def cadr (\ ((#_ cadr . #_)) cadr))
(def cadr
  %cadr)

(def cdr (\ ((#_ . cdr)) cdr))
(def cdr
  %cdr)

(def cddr (\ ((#_ . (#_ . cddr))) cddr))
(def cddr (\ ((#_ #_ . cddr)) cddr))
(def cddr
  %cddr)

(def caar
  (\ (x) (car (car x))))

(def cdar
  (\ (x) (cdr (car x))))

(def compose
  (\ (f g) (\ (x) (f (g x)))))

(def identity
  (\ (x) x))


;;; Macro

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
    (vau (pt . forms) #ignore
      (list 'makeMacro (list* 'vau pt #ignore forms)) )))


; defMacro defVau def\ def*\ rec\ let1\ let1rec\ let\ letrec\ permettono la definizione con due sintassi
;
;    (_ name parameters . body)
;    (_ (name . parameters) . body)
;
; rec rec\ let1rec let1rec\ letrec letrec\ inizializzano a #inert le definizioni prima della valutazione

(def defMacro
  (macro (lhs . rhs)
    (if (symbol? lhs)
      (list 'def lhs (list* 'macro (car rhs) (cdr rhs)))
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs)) )))

(defMacro (expand macro)
  (list 'begin (list 'evalMacro #f) macro) )

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%quote +) n 1))))

(defMacro (defVau lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* 'vau (car rhs) (cadr rhs) (cddr rhs)))
    (list 'def (car lhs) (list* 'vau (cdr lhs) (car rhs) (cdr rhs))) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%quote +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%quote +) n 1) env))))

(defMacro defConstant (name value . docstring?)
  (list 'def name value) )

(defMacro (wrau pt ep . body)
  (list 'wrap (list* 'vau pt ep body)))

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))    

(defMacro (def* lhs . rhs)
  (list 'def lhs (list* 'list rhs)) )

(defMacro (def\ lhs . rhs)
  (if (symbol? lhs)
    (list 'def lhs (list* '\ (car rhs) (cdr rhs)))
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


;;;; Basic value test

(def\ (inert? o) (== o #inert))
(def\ (zero? n) (== n 0))
(def\ (even? n) (== (% n 2) 0))
(def\ (odd? n)  (== (% n 2) 1))


 

;;; Wrap incomplete VM forms

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


;;; Delimited Control Operators

;; These operators follow the API put forth in the delimcc library
;; at URL `http://okmij.org/ftp/continuations/implementations.html'.

(defVau (pushPrompt prompt . body) env
  (eval (list '%pushPrompt (eval prompt env) (list* 'begin body)) env) )

(def takeSubcont %takeSubcont)

(defMacro (pushDelimSubcont p k . body)
  (list '%pushDelimSubcont p k (list* '\ () body)) )

(defMacro (pushSubcont k . body)
  (list '%pushDelimSubcont #ignore k (list* '\ () body)) )

(defMacro pushSubcontBarrier forms
  (list* '%pushSubcontBarrier (%makeEnv) forms))


;;; Basic macros and functions

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

(def\ (map f lst . lst*)
  (if (null? lst*)
    ((rec\ (map lst) (if (null? lst) #null (cons (f (car lst)) (map (cdr lst))) )) lst)
    ((rec\ (map* lst*) (if (null? (car lst*)) #null (cons (apply f (map car lst*)) (map* (map cdr lst*))) )) (cons lst lst*)) ))

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


;;; Lexical Bindings

(def\ (->inert binding) #inert)
(def\ (->1expr binding) ((\ ((#_ cadr)) cadr) binding))
(def\ (->begin binding) ((\ ((#_ cadr . cddr)) (if (null? cddr) cadr (list* 'begin cadr cddr))) binding))
(def\ (->name  lhs) (if (symbol? lhs) lhs (car lhs)))
(def\ (->name\ (lhs . rhs)) (if (symbol? lhs) (list lhs (list* '\ (car rhs) (cdr rhs))) (list (car lhs) (list* '\ (cdr lhs) rhs))))


(defMacro (let1Loop . args)
  (if (symbol? (car args))
    (def (name binding . body) args)
    (def ((name . binding) . body) args))
  (list
    (list* 'rec\ name (list (car binding)) body)
    (->1expr binding) ))

(assert (let1Loop add1 (a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))
(assert (let1Loop (add1 a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))

(defMacro (let1 lhs . rhs)
  (if (symbol? lhs)
    (list* 'let1Loop lhs (car rhs) (cdr rhs))
    (list (list* '\ (list (car lhs)) rhs)
      (->1expr lhs) )))
      
(assert (let1 (a 1) a) 1)
;(assert (let1 (a 1 2) a) 2) ; need ->begin
(assert (let1 f (a 2) (if (zero? a) 'end (f (- a 1)))) 'end)

(defMacro (let1\ binding . body)
  (list* 'let1 (->name\ binding) body))

(assert (let1\ (f (a) a) (f 5)) 5)
(assert (let1\ ((f a) a) (f 5)) 5)

#| TODO non sembra utile
(defMacro (let1rec binding . body)
  (list* 'let ()
    (list 'def (car binding) (->inert binding))
    (list 'def (car binding) (->1expr binding))
    body ))
|#
    
(defMacro (let1rec\ binding . body)
  (list* 'let ()
    (list 'def (->name (car binding)) #inert)
    (list* 'def\ (car binding) (cdr binding))
    body ))


(defMacro (letLoop . args)
  (if (symbol? (car args))
    (def (name bindings . body) args)
    (def ((name . bindings) . body) args))
  (list*
    (list* 'rec\ name (map car bindings) body)
    (map ->1expr bindings) ))

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))
(assert (letLoop (sum (a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))

(defMacro (let lhs . rhs)
  (if (symbol? lhs)
    (list* 'letLoop lhs rhs)
    (list* (list* '\ (map car lhs) rhs)
      (map ->1expr lhs) )))

(assert (let ((a 1)) a) 1)
    
(defMacro (let\ bindings . body)
  (list* 'let (map ->name\ bindings) body))
  
(assert (expand (let\ ((f1 (a) a) ((f2 b) b)) 1)) '(let ((f1 (\ (a) a)) (f2 (\ (b) b))) 1))
    
#| TODO non sembra utile
(defMacro (letrec bindings . body)
  (list* 'let ()
    (list* 'def* (map car bindings) (map ->inert bindings))
    (list* 'def* (map car bindings) (map ->1expr bindings))
    body ))

(assert (letrec ( (even? (\ (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (zero? n) #f (even? (- n 1))))) ) (even? 88)) #t)
|#

(defMacro (letrec\ bindings . body)
  (list* 'let ()
    (list* 'def* (map (\ (binding) (->name (car binding))) bindings) (map ->inert bindings))
    (list* 'def*\ (map car bindings) (map cdr bindings))
    body ))

(def labels
  letrec\)

(assert (labels ( ((even? n) (if (zero? n) #t (odd? (- n 1)))) (odd? (n) (if (zero? n) #f (even? (- n 1)))) ) (even? 88) ) #t)


(defMacro (let* bindings . body)
  (if (null? bindings)
      (list* 'let () body)
      (list 'let
        (list (car bindings))
        (list* 'let* (cdr bindings) body) )))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)


;;;; Simple control

(defMacro (when test . body)
  (list 'if test (list* 'begin body)))

(defMacro (unless test . body)
  (list* 'if test #inert body))


;;;; Bind? IfBind? CaseVau Case\ Match

(defVau (ifBind? (pt exp) then . else) env
  (let1 (env+ (makeEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (let1 ((exp) else) exp) env) ))))

(defVau (caseVau . clauses) env 
  (vau values #ignore
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let ( (env+ (makeEnv env))
               (((bindings . forms) . clauses) clauses) )
          (if (if (== bindings 'else) #t (bind? env+ bindings values)) ; or!
            (eval (list* 'begin forms) env+)
            (loop clauses) ))))))
            
(defMacro (case\ . clauses)
  (list 'wrap (list* 'caseVau clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))

(defMacro (match exp . clauses)
  (list (list* 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )  

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match   '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match     '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match        1 ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match        4 ((a) 1) ((a b) 2)    (a a)) 4)


;;; Cond And Or

(defVau (cond . clauses) env
  (unless (null? clauses)
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply (wrap begin) body env)
        (let1 (test (eval test env))
          (if (instanceOf? test Boolean)
            (if test
              (apply (wrap begin) body env)
              (apply (wrap cond) clauses env) )
            (match body
              (() test)
              (('=> apv1) ((eval apv1 env) test))
              ((guard '=> apv1)
                 (if ((eval guard env) test)
                   ((eval apv1 env) test)
                   (apply (wrap cond) clauses env) ))
              (else (apply (wrap cond) clauses env)) )))))))

(assert (begin (def a 1) (cond (a (\ (a) (== a 1)) => (\ (a) (+ a 1))))) 2) 

(defVau (and . x) e
  (cond ((null? x)        #t)
        ((null? (cdr x))  (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(def && and)

(defVau (or . x) e
  (cond ((null? x)        #f)
        ((null? (cdr x))  (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(def || or)


;;;; Member Assoc Get

(def\ (member item lst)
  (letLoop (loop (lst lst))
     (if (null? lst) #null
       (if (== item (car lst)) lst
         (loop (cdr lst)) ))))

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

(def\ (assoc item lst)
  (letLoop (loop (lst lst))
     (if (null? lst) #null
       (if (== item (caar lst)) (car lst)
         (loop (cdr lst)) ))))

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (get? key lst)
  (letLoop (loop (lst lst)) 
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
              (let1 ((apv) (cdr forms)) ((eval apv env) value))  
              (eval (list* 'begin forms) env) )
            (loop clauses) ))))))

(assert (case 3 ((2 4 6 8) 'pair) ((1 3 5 7 9) 'odd)) 'odd) 

; vedi signalsError in vm.lispx (o test-util.lispx) per codice simile
(defVau (caseType key . clauses) env
  (let1 (key (eval key env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((test . forms) . clauses) clauses)
          (if (|| (== test 'else)
                  (let* ( (symbol? (%symbol? test))
                          (class (eval (if symbol? test (car test)) env)) )
                    (&& (type? key class) (|| symbol? (&& (type? key Obj) (checkSlots key (map (\ (x) (eval x env)) (cdr test))) ))) ))
            (if (== (car forms) '=>)
              (let1 ((apv) (cdr forms)) ((eval apv env) key))  
              (eval (list* 'begin forms) env) )
            (next clauses) ))))))

(def\ (checkSlots obj slots)
  (let1 next (slots slots)
    (if (null? slots) #t
      (let1 ((name value . slots) slots)
        (if (or (! (@isBound obj name)) (! (eq? (obj name) value))) #f
          (next slots) )))))

(assert (caseType 2.0 (else 3)) 3)
(assert (caseType (+ 2 2) (else => (\(v) v))) 4)
(assert (caseType 2.0 (String "string") (Double "double")) "double")
(assert (caseType (obj Obj :a 1) (Double "double") ((Obj :a 1) "Obj :a 1")) "Obj :a 1")


;;; Options ~= if!#null or ifList

;; An option is either nil ("none"), or a one-element list ("some").
;; Variables holding options are conventionally suffixed with "?".

(def\ some (value)
  #|Create a one-element list from the VALUE.
   |#
  (list value))

(def\ (\01+ forms) 
  (if (null? forms) #null (if (null? (cdr forms)) (car forms) (list* 'begin forms))) )

(defVau (ifOpt? (pt opt?) then . else) env
  #|Destructure the OPTION?.  If it's non-nil, evaluate the THEN form
   |with the NAME bound to the contents of the option.  If it's nil,
   |evaluate the ELSE form.
   |#
   ;; (Idea from Taylor R. Campbell's blag.)
  (let1 (opt? (eval opt? env))
    (if (null? opt?)
      (if (null? else) #null
        (eval (\01+ else) env))
      (if (list? opt?)  
        (eval (list* (list 'vau (list pt) #ignore then) opt?) env)
        (error ($ "not (or Null List): " opt?)) ))))

(assert (ifOpt? (a ()) (+ a 1)) #null)
(assert (ifOpt? (a '(2)) (+ a 1)) 3)
(assert (ifOpt? (a '(2 2)) (+ a 1)))

(defVau (ifOpt*? (pt opt?) then . else) env
  (let1 (opt? (eval opt? env))
    (if (null? opt?)
      (if (null? else) #null
        (eval (\01+ else) env))
      (if (list? opt?)  
        (eval (list* (list 'vau pt #ignore then) opt?) env)
        (error ($ "not (or Null List): " opt?)) ))))

(assert (ifOpt*? ((a) ()) (+ 1 a)) #null)
(assert (ifOpt*? ((a) ()) (+ 1 a) 0) 0)
(assert (ifOpt*? ((a) ()) (+ 1 a) 0 1) 1)
(assert (ifOpt*? ((a) '(2)) (+ a 1)) 3)
(assert (ifOpt*? ((a) '(2 3)) (+ a 1)))
(assert (ifOpt*? ((a b) '(2 3)) (+ a b)) 5)

(defMacro whenOpt? ((pt opt?) . forms)
  #|Destructure the OPTION?.  If it's non-nil, evaluate the FORMS with
   |the NAME bound to the contents of the option.  If it's nil, return nil.
   |#
  (list 'ifOpt? (list pt opt?) (\01+ forms)) )

(defMacro unlessOpt? (opt? . forms)
  #|Destructure the OPTION?.  If it's nil, evaluate the FORMS.  If it's
   |non-nil, return nil.
   |#
  (list* 'ifOpt? (list #ignore opt?) #null (\01+ forms)) )

(defVau (caseOpt? exp . clauses) env
  (let1 (exp (eval exp env))
    (if (null? exp) #null
      (let1 loop (clauses clauses)
        (if (null? clauses) #null
          (let ((env+ (makeEnv env))
                (((bindings . forms) . clauses) clauses) )
            (if (or (== bindings 'else) (bind? env+ bindings exp))
              (eval (list* 'begin forms) env+)
              (loop clauses) )))))))

(assert (caseOpt? () ((a) 1) ((a b) (+ a b))) ())
(assert (caseOpt? '(2) ((a) 1) ((a b) (+ a b))) 1)
(assert (caseOpt? '(1 2) ((a) 1) ((a b) (+ a b))) 3)
(assert (caseOpt? '(1 2 3) ((a) 1) ((a b) (+ a b))) #null)
  
(defMacro (opt? exp . dft)
  #|Return the contents of the OPTION?, or the DEFAULT? if the option
   |is nil.  The default itself defaults to void.  The DEFAULT? is
   |evaluated lazily, only when the OPTION? is nil.
   |#
  (list* ifOpt? (list 'exp exp) 'exp dft))

(assert (opt? () 10) 10)
(assert (opt? '(2) 10) 2)
(assert (opt? '(2 3) 10))

(defVau opt*? (lst . dft) env
  #|Similar to `opt', but provides DEFAULTS for any number of
   |elements of LIST.  This is useful for implementing functions that take
   |multiple opt? arguments.  Each default is evaluated lazily, only when needed.
   |#
  (let loop ((lst (eval lst env)) (dft dft))
    (if (null? lst) 
      (if (null? dft) #null
         (cons (eval (car dft) env) (loop #null (cdr dft))) )
      (if (null? (car lst))
        (if (null? dft)
          (cons #null (loop (cdr lst) #null))
          (cons (eval (car dft) env) (loop (cdr lst) (cdr dft))) )
        (cons (car lst)
          (loop (cdr lst) (if (null? dft) #null (cdr dft)))) ))))

(assert (opt*? '(1 () 3) 1 2 3 4) '(1 2 3 4))

(def\ getOpt? (option?)
  #|Returns the contents of the OPTION? or signals an error if it is nil.
   |#
  (opt? option? (error "Option is nil")))

(defVau (defOpt? pt exp) env
  (def exp (eval exp env))
  (unless (null? exp) 
    (eval (list 'def pt exp) env) )) 


;;; Loop

(def\ (callWithEscape fun)
  (let1 (fresh (list #null))
    (catch (fun (\ opt? (throw (list fresh (ifOpt? (val opt?) opt?) ))))
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
  (let1rec\ ( (typedParams->namesAndChecks ps)
              (if (cons? ps)
                  (let* ( ((p . ps) ps)
                          ((names . checks) (typedParams->namesAndChecks ps)) )
                    (if (cons? p)
                        (let* ( ((name type) p)
                                (check (list 'the type name)))
                          (cons (cons name names) (cons check checks)) )
                        (cons (cons p names) checks) ))
                  (cons ps ()) ))
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

(def\ ck|| o (list->array o))
(def ck+ (.MAX_VALUE Integer))
(def\ check (op o . ck) (@check vm op o ck))
(assert (check 'pp '(1 (:a 1 :b 2) c 3) 1 ck+ Integer (list Keyword Integer) Symbol (ck|| 3 4)) 4)
(assert (check 'pp '(a 1 2) 'a 1 2) 3)
(assert (check 'pp '(a 1 2) (ck|| '(b 3) '(a 1 2))) 3)
(assert (check 'pp '(a #null 1) 2 3 Symbol (ck|| Any (list 2 (ck|| Null Inert :prv :rhs)))) 3)
(assert (check 'pp '(a :prv 1)  2 3 Symbol (ck|| Any (list 2 (ck|| Null Inert :prv :rhs)))) 3)
(assert (check 'pp '(a 1)       2 3 Symbol (ck|| Any (list 2 (ck|| Null Inert :prv :rhs)))) 2)

;;; Lists

(def\ (forEach f lst . lst*)
  (if (null? lst*)
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) lst)
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) (cons lst lst*)) ))

#|
(forEach (\ (a) (log a)) '(1 2))
(forEach (\ (a b) (log a b)) '(1 2) '(3 4))
|#

(def\ (forEach f lst . lst*)
  (if (null? lst*)
    (let1 (res lst) ((rec\ (forEach lst) (if (null? lst) res (f (car lst)) (forEach (cdr lst)))) lst))
    (let1 (res (cons lst lst*)) ((rec\ (forEach* lst*) (if (null? (car lst*)) res (apply f (map car lst*)) (forEach* (map cdr lst*)) )) res) )) )

(def\ maplist (f lst)
  (if (null? lst) #null
      (append (f (car lst)) (maplist f (cdr lst))) ))

(def\ (filter f lst . lst*)
  (if (null? lst*)
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (cons (car lst) (filter (cdr lst))) (filter (cdr lst))))) lst)
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (cons cars (filter* (map cdr lst*))) (filter* (map cdr lst*)) )))) (cons lst lst*)) ))

(assert (filter even? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (filter != '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

;; TODO costruire in termini di filter
(def\ (remove f lst . lst*)
  (if (null? lst*)
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (filter (cdr lst)) (cons (car lst) (filter (cdr lst))) ))) lst)
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (filter* (map cdr lst*)) (cons cars (filter* (map cdr lst*))) )))) (cons lst lst*)) ))

(assert (remove odd? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (remove == '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(def\ (reduceL f init lst . lst*)
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (reduce* (%apply* f acc (map car lst*)) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def\ (reduceR f init lst . lst*)
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (%apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init (cons lst lst*)) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init lst . lst*)
  (if (null? lst*)
    ((rec\ (foldl acc lst) (if (null? lst) acc (foldl (f (car lst) acc) (cdr lst)) )) init lst)
    ((rec\ (foldl* acc lst*) (if (null? (car lst*)) acc (foldl* (%apply* f (map car lst*) acc) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (foldL cons () '(1 2 3 4)) '(4 3 2 1))

(def\ (foldR f init lst . lst*)
  (if (null? lst*)
    ((rec\ (foldr acc lst) (if (null? lst) acc (f (car lst) (foldr acc (cdr lst)) ) )) init lst)
    ((rec\ (foldr* acc lst*) (if (null? (car lst*)) acc (%apply* f (map car lst*) (foldr* acc (map cdr lst*)) ) )) init (cons lst lst*)) ))

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

#| TODO non più necessaria, eliminare
(def %d\
  (vau (var* . body) #ignore
    (wrau val* env
        (def\ (ckdvar var)
            (def lkp (@get env var))
            (def ndv (.value lkp))
            ;(if (or (and (null? body) (null? ndv)) (instanceOf? ndv DVar)) ndv
            ;  (error ($ "not " (if (null? body) "null or " "") "a dynamic value: " var)) )
            (if (or (and (null? body) (! (.isBound lkp))) (instanceOf? ndv DVar)) ndv
              (error ($ "not " (if (null? body) "unbound or " "") "a dynamic value: " var)) ))
        (def ndv* (map ckdvar var*))
        (unless (null? body) (def old* (map (\ (ndv) (if (null? ndv) ndv (ndv))) ndv*)))
        (forEach (\ (ndv var val) (if (instanceOf? ndv DVar) (ndv val) (@def env var (dvar val)) )) ndv* var* (if (null? val*) (map (\ (var) #null) var*) val*))
        (unless (null? body)
          (finally
            (eval (list* 'begin body) env) 
            (forEach (\ (ndv old) (ndv old)) ndv* old*) )))))

;((d\ (d e) (print e)) 4 5)     
;((d\ (d e)) 6 7)
|#

(defMacro (ddef var . val)
  (list* (list '%d\ (list var)) val) )

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
  
#| TODO da rivedere
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


#| TODO da rivedere
;;;; Cells

(defPrototype Cell Object (value))
(define (cell value) (new Cell value))
(define (ref (c Cell)) (.value c))
(set (setter ref) (type\ (newVal (c Cell)) (set (.value c) newVal)))
|#


;;;; Box

(def box %box)

(defMacro (defBox name . value?)
  (list 'def name (list* 'box value?)) )


;;;; Auto Increment/Decrement and Assignement Operator

(defVau ($set! exp1 formals exp2) env
  (eval (list 'def formals (list '(unwrap eval) exp2 env)) (eval exp1 env)))

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
