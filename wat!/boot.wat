;;;                                                     -*- mode: Scheme -*-
;;; Wat Bootstrap
;;;

;; Copyright (c) 2021, 2022 Manuel J. Simoni

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def

(%def def
  %def)


;;; Built-Ins - rename bindings that will be used as provided by VM

(def assert %assert)
(def error %error)
(def test %test)

(def ==
  %==)
(def != %!=)

(def string? %string?)
(def $ %$)

(def number? %number?)
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
(def \| %|)
(def ^ %^)
(def << %<<)
(def >> %>>)
(def >>> %>>>)

(def append
  %append)

(def apply
  %apply)

(def apply*
  %apply*)

(def apply**
  %apply**)

(def array->list %array->list)
(def begin
  %begin)
(def bind? %bind?)
(def bound?
  %bound?)
(def newBox
  %newBox)
(def cons
  %cons)
(def cons?
  %cons?)
(def classOf
  %classOf)

(def dval %dVal)
(def newDVar %newDVar)
(def eq? %eq?)
(def eval
  %eval)

(def finally
  %finally)

(def if
  %if)
(def intern
  %intern)
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
(def newEnv
  %newEnv)
(def null?
  %null?)
(def not
  !)

(def nth
  %nth)

(def nthCdr
  %nthCdr)

(def new
  %new)
(def reverse
  %reverse)
(def rootPrompt %rootPrompt)
(def set! %set!)
(def symbol %symbol)
(def symbol? %symbol?)
(def symbolName
  %internName)
(def subClass?
  %subClass?)

(def the %the)
(def type?
  %type?)
(def value %value)
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
  %')

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
  (\ (f g) (\ args (f (apply g args)))) )

(def identity
  (\ (x) x))


;;; Macro

(def evalMacro (newDVar #t))

(def makeMacro
  (wrap
    (vau (expander) #ignore
      (vau operands env
        (def !evalMacro (! (evalMacro)))
        (if !evalMacro (evalMacro #t))
        (def exp (eval (cons expander operands) (newEnv)))
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


#| TODO probabilmente da eliminare, superato da (#! ... )
(def symdef?
  (\ (lhs rhs)
    (if (and (symbol? lhs) (> (len rhs) 1)) #t
      (if (and (cons? lhs) (symbol? (car lhs))) #f
        (error "not (or (Symbol pt . forms) ((Symbol . pt) . forms))") ))))

(def symdef?
  (\ (lhs rhs)
    (if (and (symbol? lhs) (cons? rhs)) #t
      (if (and (cons? lhs) (symbol? (car lhs))) #f
        (error "not (or (Symbol pt . forms) ((Symbol . pt) . forms))") ))))

(def symdef?
  (\ (lhs rhs)
    (if (cons? lhs)
       (if (symbol? (car lhs)) #f
         (error "not ((Symbol . pt) . forms)") )
       (if (and (symbol? lhs) (cons? rhs)) #t
         (error "not (Symbol pt . forms)") ))))

(def dec ((\ (f) (def\ (f n) (if (zero? n) 0 (f (- n 1)))) f) #inert))
|#
	
(def defMacro
  (macro (lhs . rhs)
    (if (cons? lhs)
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs))
      (list 'def lhs (list* 'macro rhs)) )))

(defMacro (expand macro)
  (list 'begin (list 'evalMacro #f) macro) )

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))

(defMacro (defVau lhs . rhs)
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'vau (cdr lhs) rhs))
    (list 'def lhs (list* 'vau rhs)) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))

(def defConstant
  def)

(defMacro (wrau pt ep . body)
  (list 'wrap (list* 'vau pt ep body)))

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))

(defMacro (def* lhs . rhs)
  (list 'def lhs (list* 'list rhs)) )

(defMacro (def\ lhs . rhs)
  (if (cons? lhs)
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'def lhs (list* '\ rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


;;;; Basic value test

(def\ (inert? o) (== o #inert))
(def\ (zero? n) (== n 0))
(def\ (even? n) (== (% n 2) 0))
(def\ (odd? n)  (== (% n 2) 1))


;;; Wrap incomplete VM forms

(def* (then else) begin begin)

(defMacro (throw . val) (list* '%throwTag #_ val) )
(def throwTag
  %throwTag)

;; ctApv non andrebbe mai cambiato dopo il boot, anche la riesecuzione di quanto segue potrebbe non bastare!
(if (ctApv)
  (then
    (defMacro (catch . forms)
      (list '%catchTagWth #_ () (list* '\ () forms)) )
    (defMacro (catchWth hdl . forms)
      (list '%catchTagWth #_ hdl (list* '\ () forms)) )
    (defMacro (catchTag tag . forms)
      (list '%catchTagWth tag () (list* '\ () forms)) )
    (defMacro (catchTagWth tag hdl . forms)
      (list '%catchTagWth tag hdl (list* '\ () forms)) )
  )
  (defMacro (catch . forms)
    (list* '%catchTagWth #_ () forms))
  (defMacro (catchWth hdl . forms)
    (list* '%catchTagWth #_ hdl forms))
  (defMacro (catchTag tag . forms)
    (list* '%catchTagWth tag () forms) )
  (def catchTagWth
    %catchTagWth)
)

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catchWth (\ (x) (+ x 1)) (throw 1) ) 2)
(assert (catchTag 'a (throwTag 'a)) #inert)
(assert (catchTag 'a (throwTag 'a 1)) 1)
(assert (catchTagWth 'a (\ (x) (+ x 1)) (throwTag 'a 1) ) 2)


;;; Delimited Control Operators

;; These operators follow the API put forth in the delimcc library
;; at URL `http://okmij.org/ftp/continuations/implementations.html'.

(defVau (pushPrompt prompt . forms) env
  (eval (list '%pushPrompt (eval prompt env) (list* 'begin forms)) env) )

(def takeSubcont
  %takeSubcont)

(defMacro (pushDelimSubcont prompt continuation . forms)
  (list '%pushDelimSubcont prompt continuation (list* '\ () forms)) )

(defMacro (pushSubcont continuation . forms)
  (list* 'pushDelimSubcont #ignore continuation forms) )

(defMacro (pushSubcont continuation . forms)
  (list '%pushDelimSubcont #ignore continuation (list* '\ () forms)) )

(defMacro pushSubcontBarrier forms
  (list* '%pushSubcontBarrier (%newEnv) forms))


;;; Basic macros and functions

(defMacro (rec lhs . rhs)
  (list (list '\ (list lhs) (list* 'def lhs :rhs rhs)) #inert) )

(assert ((rec f (\ (l) (if (null? l) "" ($ (car l) (f (cdr l)))))) '(1 2 3)) "123")
(assert ((rec f (\ l (if (null? l) "" ($ (car l) (apply f (cdr l)))))) 1 2 3) "123")

(defMacro (rec\ lhs . rhs)
  (if (cons? lhs)
    (list 'rec (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'rec lhs (list* '\ rhs)) ))

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
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map (\ (lhs rhs) (cons '\ (if (cons? lhs) (cons (cdr lhs) rhs) rhs))) lhs* rhs*) ))

(assert (expand (def*\ ((a n) (b n)) ((+ n 1)) ((+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )
(assert (expand (def*\ (a b) ((n) (+ n 1)) ((n) (+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )


;;; Lexical Bindings

(def\ (->1expr binding) ((\ ((#_ cadr)) cadr) binding))
(def\ (->begin binding) ((\ ((#_ cadr . cddr)) (if (null? cddr) cadr (list* 'begin cadr cddr))) binding))
(def\ (->name+#inert (lhs . #_)) (list (if (cons? lhs) (car lhs) lhs) #inert))
(def\ (->name+lambda (lhs . rhs)) (if (cons? lhs) (list (car lhs) (list* '\ (cdr lhs) rhs)) (list lhs (list* '\ rhs)) ))
;(def\ (->name+lambda (lhs . rhs)) (list (if (cons? lhs) (car lhs) lhs) (list* '\ (if (cons? lhs) (cons (cdr lhs) rhs) rhs))))
;(def\ (->name+lambda (lhs . rhs)) (def t (cons? lhs)) (list (if t (car lhs) lhs) (list* '\ (if t (cons (cdr lhs) rhs) rhs))))
;(def\ (->name+lambda (lhs . rhs)) (list (if (def t :rhs (cons? lhs)) (car lhs) lhs) (list* '\ (if t (cons (cdr lhs) rhs) rhs))))


(defMacro (let1Loop lhs . rhs)
  (if (cons? lhs)
    (def* ((name . binding) body) lhs rhs)
    (def* (name (binding . body)) lhs rhs) )
  (list
    (list* 'rec\ name (list (car binding)) body)
    (->begin binding) ))

(assert (let1Loop add1 (a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))
(assert (let1Loop (add1 a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))

(defMacro (let1 lhs . rhs)
  (if (symbol? lhs)
    (list* 'let1Loop lhs rhs)
    (list (list* '\ (list (car lhs)) rhs)
      (->begin lhs) )))

(assert (let1 (a 1) a) 1)
(assert (let1 (a 1 2) a) 2) ; need ->begin
(assert (let1 f (a 2) (if (zero? a) 'end (f (- a 1)))) 'end)

(defMacro (let1\ binding . body)
  (list* 'let1 (->name+lambda binding) body))

(assert (let1\ (f (a) a) (f 5)) 5)
(assert (let1\ ((f a) a) (f 5)) 5)

(defMacro (let1rec binding . body)
  (def name (car binding))
  (list* 'let1 (list name #inert)
    (list 'def (car name (->begin binding))
      body )))

(defMacro (let1rec\ binding . body)
  (list* 'let1 (->name+#inert binding)
    (list* 'def\ binding)
    body ))

(defMacro (letLoop lhs . rhs)
  (if (cons? lhs)
    (def* ((name . bindings) body) lhs rhs)
    (def* (name (bindings . body)) lhs rhs) )
  (list*
    (list* 'rec\ name (map car bindings) body)
    (map ->begin bindings) ))

(%assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))
(%assert (letLoop (sum (a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))

(defMacro (let lhs . rhs)
  (if (symbol? lhs)
    (list* 'letLoop lhs rhs)
    (list* (list* '\ (map car lhs) rhs)
      (map ->begin lhs) )))

(assert (let ((a 1)) a) 1)

(defMacro (let\ bindings . body)
  (list* 'let (map ->name+lambda bindings) body))

(assert (expand (let\ ((f1 (a) a) ((f2 b) b)) 1)) '(let ((f1 (\ (a) a)) (f2 (\ (b) b))) 1))

(defMacro (letrec bindings . body)
  (def names (map car bindings))
  (list* 'let (map (\ (name) (list name #inert)) names)
    (list* 'def* names (map ->begin bindings))
    body ))

(assert (letrec ( (even? (\ (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (zero? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . body)
  (list* 'let (map ->name+#inert bindings)
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


;;;; Simple Control

(defVau prog1 (form . forms) env
  (let1 (result (eval form env))
    (eval (list* begin forms) env)
    result))

(defMacro (when test . forms)
  (list 'if test (list* 'begin forms)))

(defMacro (unless test . forms)
  (list* 'if test #inert forms))

(defVau set (ep dt value) env
  (eval
    (list 'def dt (list (unwrap eval) value env))
    (eval ep env)))

(defVau if* clauses env
  (if (null? clauses) #inert
    (if (null? (cdr clauses)) (eval (car clauses) env)
      (let1 ((test then . else) clauses)
        (if (eval test env) (eval then env)
          (apply if* else env) )))))

(%assert (if*) #inert)
(%assert (if* 1) 1)
(%assert (if* #t 2) 2)
(%assert (if* #f 2 3) 3)
(%assert (if* #f 1 #f 2) #inert)
(%assert (if* #f 1 #f 2 3) 3)


;;; And Or

(defVau and ops env
  (if (null? ops) #t
    (if (eval (car ops) env)
      (apply and (cdr ops) env)
      #f )))

(def && and)

(defVau or ops env
  (if (null? ops) #f
    (if (eval (car ops) env) #t
      (apply or (cdr ops) env) )))

(def || or)


;;;; Bind? IfBind? CaseVau Case\ Match

(defVau (ifBind? (pt exp) then . else) env
  (let1 (env+ (newEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (let1 ((exp) else) exp) env) ))))

(defVau (caseVau . clauses) env
  (vau values #ignore
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((bindings . forms) . clauses) clauses)
          (if (== bindings 'else)
            (if (== (car forms) '=>)
              (let1 ((apv) (cdr forms)) ((eval apv env) values))
              (eval (list* 'begin forms) env) )
            (let1 (env+ (newEnv env)) 
              (if (bind? env+ bindings values)
                (eval (list* 'begin forms) env+)
                (loop clauses) ))))))))

(defMacro (defCaseVau sym . clauses)
  (list 'def sym (list* 'caseVau clauses)) )

(defMacro (case\ . clauses)
  (list 'wrap (list* 'caseVau clauses)) )

(defMacro (defCase\ sym . clauses)
  (list 'def sym (list* 'case\ clauses)) )

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


;;; Quasiquote

;; (Idea from Alf Petrofsky http://scheme-reports.org/mail/scheme-reports/msg00800.html)
(defVau %` (x) env
  (defCase\ qq
    ( ((('%,@ x) . y) #f . d) (append (map (\ (x) (list '%,@ x)) (apply** qq (list x) d)) (apply** qq y #f d)) )
    ( ((('%,@ x) . y) . d)    (append (eval x env) (apply** qq y d)) )
    ( ((('%, x) . y) #f . d)  (append (map (\ (x) (list '%, x))   (apply** qq (list x) d)) (apply** qq y #f d)) )
    ( (('%, x) #f . d)        (list '%, (apply** qq x d)) )
    ( (('%, x) . d)           (eval x env) )
    ( (('%` x) . d)           (list '%` (apply** qq x #f d)) )
    ( ((x . y) . d)           (cons (apply** qq x d) (apply** qq y d)) )
    ( (x . d)                 x) )
  (qq x))

(assert (let ((a 1) (b 2) (c '(3 4))) `(,@c ,a (,a) (,@c) b ,@c)) '(3 4 1 (1) (3 4) b 3 4))
(assert (let1 (x '(a b c)) ``(,,x ,@,x ,,@x ,@,@x)) '`(,(a b c) ,@(a b c) ,a ,b ,c ,@a ,@b ,@c))
(assert ``(,,@'() ,@,@(list)) '`())
(assert `````(a ,(b c ,@,,@,@'(a b c))) '````(a ,(b c ,@,,@a ,@,,@b ,@,,@c)))


;;; Cond

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


;;;; Member Assoc Get

(def\ (member key lst)
  (let1 loop (lst lst)
     (if (null? lst) #null
       (if (== key (car lst)) lst
         (loop (cdr lst)) ))))

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

#|
(def\ (member key lst . keywords)
  (let ( (test (opt? (get? :test keywords) ==))
         (fkey (opt? (get? :fkey keywords) identity)) )
    (let1 loop (lst lst)
      (if (null? lst) #null
        (if (test (fkey (car lst)) key) lst
          (loop (cdr lst)))))))
|#

(def\ (assoc key lst)
  (let1 loop (lst lst)
     (if (null? lst) #null
       (let1 ((kv . lst) lst)
         (if (== (car kv) key) kv
           (loop lst) )))))

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (get? key lst)
  (let1 loop (lst lst)
    (if (null? lst) #null
      (let1 ((k v . lst) lst)
        (if (== k key) (list v)
          (loop lst)) ))))

(assert (get? :b '(:a 1 :b 2 :c 3)) '(2))
(assert (get? 'b '(a 1 b 2 c 3)) '(2))


;;; Case CaseType

(defVau (case exp . clauses) env
  (let1 (value (eval exp env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((values . forms) . clauses) clauses)
          (if (or (== values 'else) (== value values) (and (cons? values) (cons? (member value values))))
            (if (== (car forms) '=>)
              (let1 ((apv) (cdr forms)) ((eval apv env) value))
              (eval (list* 'begin forms) env) )
            (next clauses) ))))))

(assert (case 3 ((2 4 6 8) 'pair) ((1 3 5 7 9) 'odd)) 'odd)

; vedi signalsError? in vm.lispx (o test-util.lispx) per codice simile
(defVau (caseType key . clauses) env
  (let1 (key (eval key env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((test . forms) . clauses) clauses)
          (if (|| (== test 'else)
                  (let* ( (symbol? (symbol? test))
                          (class (eval (if symbol? test (car test)) env)) )
                    (if symbol? (type? key class) (&& (type? key Obj) (matchObjSlots? key (eval (list* 'list (cdr test)) env)))) ))
            (if (== (car forms) '=>)
              (let1 ((apv) (cdr forms)) ((eval apv env) key))
              (eval (list* 'begin forms) env) )
            (next clauses) ))))))

#| TODO sostituito dal seguente, eliminare
(def\ (matchObjSlots? obj slots)
  (let1 next (slots slots)
    (if (null? slots) #t
      (let1 ((name value . slots) slots)
        (if (&& (@isBound obj name) (eq? (obj name) value)) (next slots)
          #f )))))
|#
(def\ (matchObjSlots? obj slots)
  (let1 next (slots slots)
    (if (null? slots) #t
      (let1 ((name value . slots) slots)
        (if (eq? (if (type? name AtDot) (name obj) (obj name)) value) (next slots)
          #f )))))

(def\ (matchObj? obj class . slots)
  (if (type? obj class) (matchObjSlots? obj slots) #f) )

(assert (caseType 2.0 (else 3)) 3)
(assert (caseType (+ 2 2) (else => (\(v) v))) 4)
(assert (caseType 2.0 (String "string") (Double "double")) "double")
(assert (caseType (new Obj :a 1) (Double "double") ((Obj :a 1) "Obj :a 1")) "Obj :a 1")


;;; Options

;; An option is either nil ("none"), or a one-element list ("some").
;; Variables holding options are conventionally suffixed with "?".

(def\ some (value)
  #|Create a one-element list from the VALUE.
   |#
  (list value))

(def\ (\01+ forms)
  (if (null? forms) #null (if (null? (cdr forms)) (car forms) (list* 'begin forms))) )

;; (Idea from Taylor R. Campbell's blag. https://mumble.net/~campbell/blag.txt)
(defVau (ifOpt? (pt opt?) then . else) env
  #|Destructure the OPTION?.  If it's non-nil, evaluate the THEN form
   |with the NAME bound to the contents of the option.  If it's nil,
   |evaluate the ELSE form.
   |#
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
          (let ((env+ (newEnv env))
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

(defVau block (blockName . forms) env
  (let* ( (tag (list #inert)) ; cons up a fresh object as tag
          (escape (\ (value) (throwTag tag value))) )
    (catchTag tag
      (eval (list (list* '\ (list blockName) forms) escape) env) )))

#|
(defVau block (blockName . forms) env
  (def tag (list #inert)) ; cons up a fresh object as tag
  (def\ (escape value) (throwTag tag value))
  (catchTag tag
    (eval (list (list* '\ (list blockName) forms) escape)
          env )))
|#
(assert (block exit (exit 7)) 7)
(assert (block exit (def x 1) (loop (if (== x 4) (exit 7)) (def x (+ x 1)))) 7)

(def\ returnFrom (blockName . value?)
  (blockName (opt? value? #inert)) )

(assert (block ciclo (def x 1) (loop (if (== x 4) (returnFrom ciclo 7)) (def x (+ x 1)))) 7)

(defVau while (testForm . forms) env
  (let ((forms (list* 'begin forms)))
    (block exit
      (loop
        (if (eval testForm env)
            (eval forms env)
            (returnFrom exit #inert))))))

(defVau while (testForm . forms) env
  (let ((forms (list* 'begin forms))
        (break (list #null))
        (continue (list #null)) )
    (eval (list 'def 'break (\ v (throwTag break (if (! (null? v)) (if (null? (cdr v)) (car v) v))))) env)
    (eval (list 'def 'continue (\ () (throwTag continue))) env)
    (catchTag break
      (loop
        (catchTag continue
          (if (eval testForm env)
            (eval forms env)
            (throwTag break) ))))))

(defMacro (-- n)
  (list 'def n :rhs (list '- n 1)) )
(defMacro (++ n)
  (list 'def n :rhs (list '+ n 1)) )

(assert (let1 (c 2) (while (> c 0) (-- c)) c) 0)
(assert (let1 (c 2) (while #t (if (zero? c) (break (+ 5 5)) (-- c)))) 10)
(assert (let ((c 10) (r #null)) (while #t (if (zero? c) (break r)) (if (zero? (% (-- c) 2)) (continue)) (def r (cons c r)) )) '(1 3 5 7 9))

(defMacro until (testForm . forms)
  (list* while (list '! testForm) forms) )

(defMacro dotimes ((var countForm . resultForm?) . bodyForms)
  (let\ ((dotimes (n body result)
           (let ((i (newBox 0)))
             (while (< (i) n)
               (body (i))
               (i (+ (i) 1)))
             (result (i)))))
    (list dotimes
          countForm
          (list* '\ (list var) bodyForms)
          (list* '\ (list var) resultForm?))))

(def\ (withEscape fun)
  (let1 (fresh (list #null))
    (catchWth
      (\ (exc)
        (if (and (cons? exc) (== (car exc) fresh))
          (let1 ((#ignore opt?) exc) (if (cons? opt?) (car opt?)))
          (throw exc)))
      (fun (\ opt? (throw (list fresh (ifOpt? (val opt?) opt?) )))) )))

(defMacro (label name . body)
  (list 'withEscape (list* '\ (list name) body)))

(assert (label return (return)) #inert)
(assert (label return (return 3)) 3)
(assert (label return (return 3 4)))


;;; Type Checks

#| TODO sostituito dal seguente
(defMacro (the type obj)
  (list 'if (list 'type? obj type) obj (list 'error (list '$ obj " is not a: " type))) )
|#

(def the
  %the)

(defMacro (the\ parms . body)
  (let1rec\
    ( (parms->names.checks ps)
      (if (cons? ps)
        (let* ( ((p . ps) ps)
                ((names . checks) (parms->names.checks ps)) )
          (if (cons? p)
            (let* ( ((name type) p)
                    (check (list 'the type name)) )
              (cons (cons name names) (cons check checks)) )
                    (cons (cons p names) checks) ))
        (cons ps ()) ))
    (let1 ((names . checks) (parms->names.checks parms))
      (list* '\ names (if (null? checks) body (list* (list* 'begin checks) body))) )))

(assert (expand (the\ (a) (+ a 1))) '(\ (a) (+ a 1)))
(assert (expand (the\ ((a Integer)) (+ a 1))) '(\ (a) (begin (the Integer a)) (+ a 1)))

(defMacro (define lhs . rhs)
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'the\ (cdr lhs) rhs))
    (list 'def lhs (car rhs)) ))

(defMacro (the\ pt . body)
  (let1rec\
    ( (pt->pt.cks pt)
      (if (cons? pt)
        (let* ( ((lhs . rhs) pt)
                ((ptRhs . cksRhs) (pt->pt.cks rhs)) )
          (if (cons? lhs)
            (if (== (car lhs) #!)  
              (let* ( ((#_ type name) lhs)
                      (check (list 'the type name)) )
                (cons (cons name ptRhs) (cons check cksRhs)) )
              (let1 ((ptLhs . cksLhs) (pt->pt.cks lhs))
                (cons (cons ptLhs ptRhs) (append cksLhs cksRhs)) ) )
            (let1 ((ptRhs . cksRhs) (pt->pt.cks rhs))
              (cons (cons lhs ptRhs) cksRhs)) ))
        (cons pt ()) ) )
    (let1 ((pt . cks) (pt->pt.cks pt))
      (list* '\ pt (if (null? cks) body (list* (list* 'begin cks) body))) )))

(%assert (expand (the\ (((#! Integer b) . #_)(#! Integer a)) (+ a b))) '(\ ((b . #ignore) a) (begin (the Integer b) (the Integer a)) (+ a b)))
;(%assert ((the\ (((#! Integer b) . #_)(#! (or 3 4) a)) (+ a b)) '(1 2) 3) 4)


; evlis: (map (\ (x) (eval x env)) xs) <=> (eval (list* 'list xs) env)

#| TODO eliminare dopo verifica
(defVau (check o . cks) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) '|| (\ o (list->array o)) 'or (\ o (list->array o)) ))
    (apply* @check vm "check" ((\ (o) (if (or (null? o) (list? o)) o (list o))) (eval o env)) (eval (list* 'list cks) env+)) ))

; rende check pari #! ma torna sempre 1 piuttosto che la lunghezza della lista come piacerebbe
(defVau (check o . cks) env
  (let* (  (lta  (\ o (list->array o)))
           (env+ (newEnv env '+ (.MAX_VALUE Integer) '|| lta 'or lta )) )
    (apply* @check vm "check"
      (list (eval o env))
      (list (eval (if (== (car cks) 'or) cks (list* 'list cks)) env+)) )))

; un list di troppo? però ci vuole il controllo per null e list

(defVau (check o . cks) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) 'or (\ o (list->array o)) ))
    (apply* @check vm "check"
      (eval o env)
      (eval (if (== (car cks) 'or) cks (list* 'list cks)) env+)) ))

(def\ (ev x env) 
  (or (! (list? x) (== (car x) 'or)) (eval x env) (map (\ (x) (ev x env)) x)))

(defVau (check* o . cks) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) 'or (\ o (list->array o)) ))
    (%check (eval o env) (eval (list* 'list cks) env+)) ))

(defVau (check o ck) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) 'or (\ o (list->array o)) ))
    (%check (eval o env) (eval ck env+)) ))


(defVau (check o ck) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) 'or (\ o (list->array o)) ))
    (%check (eval o env) (ev ck env+)) ))

(def\ (ev x env+) 
  (if (or (! (list? x)) (== (car x) 'or)) (eval x env+)
    (if (or (== (car x) '%') (== (car x) 'quote)) (cadr x) (map (\ (x) (ev x env+)) x)) ))

(defVau (check o ck) env
  (let1 (env+ (newEnv env '+ (.MAX_VALUE Integer) 'or (\ o (list->array o)) ))
    (let1rec\
      (ev (x)
        (if (! (cons? x))
          (eval x env+)
          (if (== (car x) 'or)
            (list->array (log (ev (cdr x))))
            (if (or (== (car x) '%') (== (car x) 'quote))
              (cadr x)
              (map (\ (x) (ev x)) x) )))) 
      (%check (eval o env) (log (ev ck))) )))

(defVau (check o ck) env
  (let1rec\
    (ev (ck)
      (if (== ck '+) (.MAX_VALUE Integer)
        (if (! (cons? ck)) (eval ck env)
          (if (== (car ck) 'or) (list->array (ev (cdr ck)))
            (if (or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck)
              (map (\ (ck) (ev ck)) ck) )))))
    (%check (eval o env) (ev ck)) ))

(defVau (check0 o ck) env
  (let1rec\
    (ev (ck)
      (if (== ck '+) (.MAX_VALUE Integer)
        (if (! (cons? ck)) (eval ck env)
          (if (== (car ck) 'or) (list->array (ev (cdr ck)))
            (if (or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck)
              (map (\ (ck) (ev ck)) ck) )))))
    (%check o (ev ck)) ))

(defVau (check o ck) env
  (let1rec\
    (ev (ck)
      (cond
        ((== ck '+) (.MAX_VALUE Integer))
        ((! (cons? ck)) (eval ck env))
        ((== (car ck) 'or) (list->array (ev (cdr ck))))
        ((or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck))
        (else (map (\ (ck) (ev ck)) ck)) ))
    (%check (eval o env) (ev ck)) ))
|#

#| TODO definito in vm, eliminare
(def %check
  (let1 (%check %check)
    (vau (o ck) env
      (let1rec\
        (ev (ck)
          (if (== ck '+) (.MAX_VALUE Integer)
            (if (! (cons? ck)) (eval ck env)
              (if (== (car ck) 'or) (list->array (ev (cdr ck)))
                (if (or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck)
                  (map (\ (ck) (ev ck)) ck) )))))
        (%check o (ev ck)) ))))

(def %check
  (let1 (%check %check)
    (vau (o ck) env  
      (let1rec\
        (ev (ck)
          (cond
            ((== ck '+) (.MAX_VALUE Integer))
            ((! (cons? ck)) (eval ck env))
            ((== (car ck) 'or) (list->array (ev (cdr ck))))
            ((or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck))
            (else (map (\ (ck) (ev ck)) ck)) ))
        (%check o (ev ck)) ))))

(def %check
  (let1 (%check %check)
    (vau (o ck) env
      (let1rec\
        (ev (ck)
          (if*
            (== ck '+) (.MAX_VALUE Integer)
            (! (cons? ck)) (eval ck env)
            (== (car ck) 'or) (list->array (ev (cdr ck)))
            (or (== (car ck) '%') (== (car ck) 'quote)) (cadr ck)
            (map (\ (ck) (ev ck)) ck) ))
        (%check o (ev ck)) ))))
|#

(defVau (check o ck) env
  ((wrap %check) (eval o env) ck) )

(defMacro (check* o . cks)
    (list 'check o cks) )

(%assert (check* '(1 (:a 1 :b 2) c 3) 1 + Integer (Keyword Integer) Symbol (or 3 4)) 4)
(%assert (check* '(a 1 2) 'a 1 2) 3)
(%assert (check* '(a) (or '(b) '(a))) 1)
(%assert (check* '(a 1 2) (or '(b 3) '(a 1 2))) 3)
(%assert (check* '(a #null 1) 2 3 Symbol (or Any (2 (or Null Inert :prv :rhs)))) 3)
(%assert (check* '(a :prv 1)  2 3 Symbol (or (1 Any) (2 (or Null Inert :prv :rhs)))) 3)
(%assert (check* '(a 1)       2 3 Symbol (or (1 Any) (2 (or Null Inert :prv :rhs)))) 2)

(defVau match? args env (catchWth #f (apply check args env) #t))

(defMacro (the+ ck obj) (list 'let1 (list 'obj obj) (list 'check 'obj ck) 'obj))

(%assert (the+ Integer 1) 1)
(%assert (the+ Integer "1") Error :type 'type :datum "1" :expected 'Integer)
(%assert (the+ (or 1 2) 1) 1)
(%assert (the+ (or 1 2) 2) 2)
(%assert (the+ (or 1 2) 3) Error :type 'type :datum 3 :expected '(or 1 2))

; TODO non è più cosi costosa la conversione, si può fare
; (def the the+)


;;; Lists

(def\ (any? f lst . lst*)
  (if (null? lst*)
    ((rec\ (any? lst) (if (null? lst) #f (if (f (car lst)) #t (any? (cdr lst)))) ) lst)
    ((rec\ (any*? lst*) (if (null? (car lst*)) #f (if (apply f (map car lst*)) #t (any*? (map cdr lst*))))) (cons lst lst*)) ))

(assert (any? null? (1 2 3 4)) #f)
(assert (any? null? (1 2 () 4)) #t)
(assert (any? > '(1 2) '(3 4)) #f)
(assert (any? < '(1 2) '(3 4)) #t)

(defMacro (any?* f . lst) (list 'any? f lst))

(def\ (all? f lst . lst*)
  (if (null? lst*)
    ((rec\ (all? lst) (if (null? lst) #t (if (f (car lst)) (all? (cdr lst)) #f))) lst)
    ((rec\ (all*? lst*) (if (null? (car lst*)) #t (if (apply f (map car lst*)) (all*? (map cdr lst*)) #f))) (cons lst lst*)) ))

(assert (all? number? (1 2 3 4)) #t)
(assert (all? number? (1 2 () 4)) #f)
(assert (all? > '(1 2) '(3 4)) #f)
(assert (all? < '(1 2) '(3 4)) #t)

(defMacro (all?* f . lst) (list 'all? f lst))

(def\ (forEach f lst . lst*)
  (if (null? lst*)
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) lst)
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) (cons lst lst*)) ))

(assert (forEach (\ (#ignore)) '(1 2)) #inert)
(assert (forEach (\ (#ignore #ignore)) '(1 2) '(3 4)) #inert)
(assert (let1 (n 1) (forEach (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ (forEach f lst . lst*)
  (if (null? lst*)
    (let1 (res lst) ((rec\ (forEach lst) (if (null? lst) res (f (car lst)) (forEach (cdr lst)))) res))
    (let1 (res* (cons lst lst*)) ((rec\ (forEach* lst*) (if (null? (car lst*)) res* (apply f (map car lst*)) (forEach* (map cdr lst*)) )) res*) )) )

(assert (forEach (\ (#ignore)) '(1 2)) '(1 2))
(assert (forEach (\ (#ignore #ignore)) '(1 2) '(3 4)) '((1 2) (3 4)))
(assert (let1 (n 1) (forEach (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ maplist (f lst . lst*)
  (if (null? lst*)
    ((rec\ (maplist lst) (if (null? lst) #null (append (f (car lst)) (maplist (cdr lst))))) lst)
    ((rec\ (maplist* lst*) (if (null? (car lst*)) #null (append (apply f (map car lst*)) (maplist* (map cdr lst*))))) (cons lst lst*)) ))

(assert (maplist (\ (x) (list x)) '(1 2 3 4)) '(1 2 3 4))

(def\ (filter f lst . lst*)
  (if (null? lst*)
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (cons (car lst) (filter (cdr lst))) (filter (cdr lst))))) lst)
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (cons cars (filter* (map cdr lst*))) (filter* (map cdr lst*)) )))) (cons lst lst*)) ))

(assert (filter even? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (filter != '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(defMacro (remove f lst . lst*)
  (list* 'filter (list 'compose '! f) lst lst*) )

(assert (remove odd? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (remove == '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(def\ (reduceL f init lst . lst*)
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (reduce* (apply* f acc (map car lst*)) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def reduce
  reduceL)

(def\ (reduceR f init lst . lst*)
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init (cons lst lst*)) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init lst . lst*)
  (if (null? lst*)
    ((rec\ (foldl acc lst) (if (null? lst) acc (foldl (f (car lst) acc) (cdr lst)) )) init lst)
    ((rec\ (foldl* acc lst*) (if (null? (car lst*)) acc (foldl* (apply* f (map car lst*) acc) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (foldL cons () '(1 2 3 4)) '(4 3 2 1))

(def\ (foldR f init lst . lst*)
  (if (null? lst*)
    ((rec\ (foldr acc lst) (if (null? lst) acc (f (car lst) (foldr acc (cdr lst)) ) )) init lst)
    ((rec\ (foldr* acc lst*) (if (null? (car lst*)) acc (apply* f (map car lst*) (foldr* acc (map cdr lst*)) ) )) init (cons lst lst*)) ))

(assert (foldR cons () '(1 2 3 4)) '(1 2 3 4))

(defMacro dolist ((var listForm . resultForm?) . bodyForms)
  (let1rec\
    (dolist (list body result)
      (if (null? list) (result list)
        (body (car list))
        (dolist (cdr list) body result)))
    (list dolist
          listForm
          (list* '\ (list var) bodyForms)
          (list* '\ (list var) resultForm?))))

(def\ (make\* n f)
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))))

(assert ((make\* 2 (\(a b) b)) 1 2 3 4 5) '(2 3 4 5))


;;; Dynamic Binding

#| TODO primitiva non più necessaria, eliminare
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
        (forEach (\ (ndv var val) (if (instanceOf? ndv DVar) (ndv val) (@def env var (newDVar val)) )) ndv* var* (if (null? val*) (map (\ (var) #null) var*) val*))
        (unless (null? body)
          (finally
            (eval (list* 'begin body) env)
            (forEach (\ (ndv old) (ndv old)) ndv* old*) )))))

;((d\ (d e) (print e)) 4 5)
;((d\ (d e)) 6 7)
|#

(defMacro (ddef var . val?)
  (list* (list '%d\ (list var)) val?) )

(defMacro (ddef* var* . val*)
  (list* (list '%d\ var*) val*) )

(def\ (dget dvar)
  (dvar))

(def\ (dset dvar value)
  (dvar value))

(defMacro (dlet bindings exp . exps)
  (list* (list* '%d\ (map car bindings) exp exps) (map cadr bindings)) )

(defMacro (progv var* val* exp . exps)
  (list* (list* '%d\ var* exp exps) val*) )

(defMacro (dlet* bindings . forms)
  (if (null? bindings)
    (list* 'begin forms)
    (list 'dlet
      (list (car bindings))
      (list* 'dlet* (cdr bindings) forms) )))

(def a (newDVar 1))
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


;;;; Box

(def newBox %newBox)

(defMacro (defBox name . value?)
  (list 'def name (list* 'newBox value?)) )


;;; Classes

(def\ findClass (name env)
  (eval (the Symbol name) env))

(defMacro defClass (name . superClass #|slotSpecs . properties|#)
  (list 'def name (list* '%newClass (list 'quote name) superClass?)))

(defVau defClass (name superclass? slotSpecs . properties) env
   ;; Slot-specs are ignored for now, but check that they are symbols nevertheless.
  (dolist (slotSpec slotSpecs) (the Symbol slotSpec))
  (let1 (superclass (findClass (opt? superclass? 'Obj) env))
    (eval (list def name (%newClass name superclass)) env)) )

#|
(defVau defClass (name superclass? (#! (Symbol) slotSpecs) . properties) env
  ;; Slot-specs are ignored for now, but check that they are symbols nevertheless.
  (let1 (superclass (findClass (opt? superclass? 'Obj) env))
    (eval (list def name (%newClass name superclass)) env)) )
|#

;;; Objects

(def new
  %new)

(defMacro (defObj name class . attr)
  (list 'def name (list* 'new class attr)) )


;;; Generic Functions

;; receiver e parameters dei defMethod dovrebbero corrispondere a quelli del corrispondente defGeneric con quel nome

(defVau (defGeneric . args) env
  (if (cons? (car args))
    (def ((name receiver . parameters) . properties) args)
    (def (name (receiver . parameters) . properties) args) )
  (def\ generic args (apply (%getMethod (classOf (car args)) name) args))
  (eval (list 'def name generic) env) )

(defVau (defMethod . args) env
  (if (cons? (car args))
    (def ((name (receiver class) . parameters) . forms) args)
    (def (name ((receiver class) . parameters) . forms) args) )
  (def method (eval (list* '\ (list* receiver parameters) forms) env))
  (def prv (%addMethod (eval class env) name method))
  (case (bndRes) ((#inert) #inert) ((:rhs) method) ((:prv) prv)) )

(assert
  (begin
    (defClass Foo () ())
    (defClass Bar (Foo) (a b))
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
    (assert (bar :prv :b 6) 5)
    (assert (bar :b) 6) )
  #t )


;;; Modules

(defVau (provide symbols . body) env
  (eval
    (list 'def symbols
      (list 'let ()
        (list* 'begin body)
        (list* 'list symbols) ))
    env ))

(assert (begin (provide (x) (def x 10)) x) 10)

(defVau (module exports . body) env
  (let1 (env (newEnv env))
    (eval (list* 'provide exports body) env)
    (newEnv env) ))

(assert (begin (def m (module (x) (def x 10))) (eval 'x m)) 10)

(defMacro (defModule name exports . body)
  (list 'def name (list* 'module exports body)) )

(assert (begin (defModule m (x) (def x 10)) (eval 'x m)) 10)

(defVau (import module imports) env
  (let* ((module (eval module env))
         (values (eval (list* 'list imports) module)) )
    (eval (list* 'def* imports values) env) ))

(assert (begin (defModule m (x) (def x 10)) (import m (x)) x) 10)


;;; Relational Operators

;; Note that unlike in Common Lisp, these operators currently require
;; at least two arguments.  This will be improved in the future.

(def\ (relationalOp binop)
  (rec\ (op arg1 arg2 . rest)
    (if (binop arg1 arg2)
      (if (null? rest) #t
        (apply op (list* arg2 rest)))
      #f )))

(def == (relationalOp ==))

(def eq?
  (relationalOp eq?) )

(def <
  (relationalOp <) )

(def >
  (relationalOp >) )

(def <=
  (relationalOp <=) )

(def >=
  (relationalOp >=) )

(assert (== 1 1 1) #t)
(assert (< 1 2 3) #t)
(assert (> 3 2 1) #t)
(assert (<= 1 2 2 3) #t)
(assert (>= 3 2 2 1) #t)

(def\ (!= . args) (! (apply == args)))

(def\ /= (arg . args)
  (if (null? args) #t
    (if (cons? (member arg args :test eq?)) #f
      (apply /= args) )))


;;; Numbers

;; The terms thetic (for + and *) and lytic (for - and /) are due to Hankel.

(def\ (theticOp binOp unit)
  (\ args (reduceL binOp unit args)) )

(def +
  (theticOp + 0) )
(def *
  (theticOp * 1) )
(def $
  (theticOp $ "") )

(assert (+ 1 2 3) 6)
(assert (* 1 2 3) 6)
(assert ($ 1 2 3) "123")

(def\ (lyticOp binOp unit)
  (\ (arg1 . rest)
    (if (null? rest)
      (binOp unit arg1)
      (reduceL binOp arg1 rest) )))

(def -
  (lyticOp - 0) )
(def /
  (lyticOp / 1) )


;;;; Greatest Common Divisor e Lowest Common Multiple

(def\ (gcd a b . more)
  (if (null? more)
    (if (zero? b) a (gcd b (% a b)))
    (gcd a (apply gcd (cons b more))) ))

(def abs (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))

(assert (gcd 8 108) 4)
(assert (gcd 108 216 432) 108)

(def\ (lcm a b . more)
  (if (null? more)
    (if (or (zero? a) (zero? b)) 0
      (abs (* b (/ a (gcd a b)))) )
    (lcm a (apply lcm (cons b more))) ))

(assert (lcm 8 108) 216)
(assert (lcm 3 4 5 6) 60)


;;; Sequences

(defGeneric length (sequence)
  #|Return the number of elements in a sequence.|#)

(defMethod length ((seq List))
  (%len seq))

(defMethod length ((seq Null))
  (%len seq))

(defGeneric elt (sequence index)
  #|Return the sequence element at the specified index.|#)

(defMethod elt ((seq List) index)
  (nth index seq))

(defGeneric subSeq (sequence start . end?)
  #|Create a sequence that is a copy of the subsequence
   |of the SEQUENCE bounded by START and optional END?.  If END?  is not
   |supplied or void, the subsequence stretches until the end of the list
   |#)

(defMethod subSeq ((seq List) start . end?)
  (%subList seq start (opt? end? #inert)))

(defMethod subSeq ((seq Null) start . end?)
  (%subList seq start (opt? end? #inert)))

(defMethod subSeq ((seq String) start . end?)
  (%subString seq start (opt? end? #inert)))


;;; Coroutines

(defConstant coroutinePrompt
  #|This prompt is used for general coroutine-like use of continuations.
   |#
  'coroutine-prompt)

(defMacro coroutine forms
  #|Evaluate the FORMS in a context in which `yield' can be used to pause execution.
   |#
  (list* 'pushPrompt 'coroutinePrompt forms))

(defMacro yield (name . forms)
  #|Pause the current coroutine.  In the place where the enclosing
   |`coroutine' (or `resume') was called, evaluate the FORMS with NAME
   |bound to the paused coroutine.  `resume' can later be used to restart
   |execution inside the coroutine.
   |#
  (list* 'takeSubcont 'coroutinePrompt name forms))

(defMacro resume (k . forms)
  #|Resume the paused coroutine K and evaluate FORMS in the place where
   |`yield' was called in the coroutine.
   |#
  (list* 'pushDelimSubcont 'coroutinePrompt k forms))


;;; Fibers

;; The following implementation of fibers follows the one at URL
;; `http://okmij.org/ftp/continuations/implementations.html#dget-wind'
;;
;; We're calling them fibers instead of coroutines so as to not
;; conflict with the built-in coroutine operators.
;;
;; We use it for testing that built-in operators properly suspend and
;; resume.

(defConstant fiberPrompt
  'fiber-prompt)

(defClass YieldRecord ()
  (value continuation) )

(def\ makeYieldRecord (v k)
  (new YieldRecord :value v :continuation k))

(def\ fiberYield v?
  (takeSubcont fiberPrompt k
    (makeYieldRecord (opt? v? #inert) k)))

(def\ fiberResume (yieldRecord . v?)
  (pushDelimSubcont fiberPrompt (yieldRecord 'continuation)
    (opt? v? #inert)))

(defMacro fiber body
  (list* pushPrompt 'fiberPrompt body))

#| TODO non più necessarie, eliminare
(def\ runFiber (thunk)
  (let1 run (result (fiber (thunk)))
    (if (type? result YieldRecord)
        (cons (result 'value) (run (fiberResume result)))
        (list result))))

(def\ runFiberWithValues (thunk values)
  (let run ((result (fiber (thunk))) (values values))
    (if (type? result YieldRecord)
        (cons (result 'value)
              (run (fiberResume result (car values)) (cdr values)))
        (list result))))
|#

(def\ runFiber* (thunk . values)
  (let run ((result (fiber (thunk))) (values values))
    (if (type? result YieldRecord)
      (cons (result 'value)
        (if (null? values)
          (run (fiberResume result) #null)
          (run (fiberResume result (car values)) (cdr values)) ))
      (list result) )))

(%assert (runFiber* (\ () (fiberYield 1) (fiberYield 2) 3)) '(1 2 3))

(%assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t) '(1 2 #inert))
(%assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t #_) '(1 2 #ignore))
(%assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t 4) '(1 2 4))
(%assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #f) '(1 3))

(%assert (runFiber* (\ () ((\ (a b) (+ a b)) (fiberYield 1) (fiberYield 2)) ) 3 4) '(1 2 7))

(defMacro (runFiberWithValues f args) (list 'eval (list 'list* 'runFiber* f args))) 
(defMacro (runFiberWithValues f args) (list 'apply 'runFiber* (list 'list* f args))) 
(defMacro (runFiberWithValues f args) (list 'apply** 'runFiber* f args)) 

(%assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) '(#inert 3)) (1 2 3))
(%assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) (#inert 3)) (1 2 3))

(def runFiber runFiber*) 


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
  (the\ (newVal object key)
    (set ((jsGetter key) object) newVal) ))

(def\ (jsCallback fun)
  (%jsFunction (\ args (pushPrompt rootPrompt (apply fun args)))) )

(defMacro (jsLambda parms . body)
  (list 'jsCallback (list* 'the\ parms body)))

(def\ (log x . xs)
  (apply @log (list* &console x xs))
  x)

(defPrototype Cell Object (value))

(define (cell value) (new Cell value))

(define (ref (c Cell)) (.value c))

(set (setter ref) (the\ (newVal (c Cell)) (set (.value c) newVal)))
|#


;;;; Auto Increment/Decrement and Assignement Operator

(defVau (++ plc . args) env
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (+ (val) 1))))
    (Obj    (let1 ((fld) args) (val :rhs fld (+ (val fld) 1))))
    (Number (let1 (() args) (eval (list 'set! plc :rhs (+ val 1)) env)))
    (else   (error ($ "not valid type: " val))) ))

(defVau (-- plc . args) env
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (- (val) 1))))
    (Obj    (let1 ((fld) args) (val :rhs fld (- (val fld) 1))))
    (Number (let1 (() args) (eval (list 'set! plc :rhs (- val 1)) env)))
    (else   (error ($ "not valid type: " val))) ))

(assert (begin (def obj (new Obj :a 1)) (++ obj :a) (++ obj :a) (-- obj :a)) 2)
(assert (begin (def box (newBox 1)) (++ box) (++ box) (-- box)) 2)
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
        ((key fld rval) (lval key fld (op (lval fld) (eval rval env)))) ))
      (Object (match args
        ((rval) (eval (list 'def plc (op lval (eval rval env))) env))
        ((key rval) (eval (list 'set! plc key (op lval (eval rval env))) env)) )))))

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (newBox 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (new Obj :fld 1)) (+= a :rhs :fld 3)) 4)


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
    (log ($ "time " exp ": " milli "ms"))
    result ))


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

(def SimpleError Error)
(def\ simpleError (message) (error (%apply* @new Error message :type 'simple #|:message message|#)))

