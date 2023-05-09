;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def
(%def $define! %def)

;; Rename bindings that will be used as provided by VM
($define! array->list %array->list)
($define! begin %begin)
($define! catch-tag %catch)
($define! cons %cons)
($define! cons? %cons?)
($define! dvar %dVar)
($define! dref %dRef)
($define! error %error)
($define! eval %eval)
($define! if %if)
($define! list* %list*)
($define! list->array %list->array)
($define! loop %loop)
($define! make-environment %makeEnvironment)
($define! nil? %null?)
($define! not !)
($define! reverse-list %reverseList)
($define! root-prompt %rootPrompt)
($define! string->symbol %string->symbol)
($define! symbol-name %symbolName)
($define! symbol? %symbol?)
($define! throw-tag %throw)
($define! unwrap %unwrap)
($define! wrap %wrap)

;; Important utilities
($define! $vau %vau)
($define! quote ($vau (x) #ignore x))
($define! quote %quote)
($define! list (wrap ($vau elts #ignore elts)))
($define! list %list)
($define! $lambda ($vau (formals . body) env (wrap (eval (list* $vau formals #ignore body) env))))
($define! $lambda %\)
($define! the-environment ($vau () e e))

;;;; Macro

($define! make-macro-expander
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
        (eval (eval (cons expander operands) (make-environment)) env) ))))

($define! macro
  (make-macro-expander
    ($vau (params . body) #ignore
      (list make-macro-expander (list* $vau params #ignore body)) )))

($define! define-macro
  (macro ((name . params) . body)
    (list $define! name (list* macro params body)) ))

(define-macro (define-operative (name . params) envparam . body)
  (list $define! name (list* $vau params envparam body)) )

;;;; Wrap incomplete VM forms

(define-macro (catch x . handler)
  (list* catch-tag #ignore x handler))

(define-macro (throw . x)
  (list* throw-tag #ignore x))

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catch (throw 1) (%\ (x) 2)) 2)

(define-operative (finally protected . cleanup) env
  (eval (list %finally protected (list* begin cleanup)) env) )

(define-macro (take-subcont prompt k . body)
  (list %takeSubcont prompt (list* $lambda (list k) body)) )

(define-operative (push-prompt prompt . body) env
  (eval (list %pushPrompt (eval prompt env) (list* begin body)) env) )

(define-macro (push-prompt-subcont p k . body)
  (list %pushPromptSubcont p k (list* $lambda () body)) )

(define-macro (push-subcont k . body)
  (list %pushPromptSubcont #ignore k (list* $lambda () body)) )

;;;; List utilities

($define! compose ($lambda (f g) ($lambda (arg) (f (g arg)))))

($define! car ($lambda ((x . #ignore)) x))
($define! cdr ($lambda ((#ignore . x)) x))
($define! caar (compose car car))
($define! cadr (compose car cdr))
($define! cdar (compose cdr car))
($define! cddr (compose cdr cdr))

;;;; Important macros and functions

($define! map-list
  ($lambda (f lst)
    (if (nil? lst) ()
        (cons (f (car lst)) (map-list f (cdr lst))) )))

(assert (map-list car  '((a 1)(b 2))) '(a b))
(assert (map-list cdr  '((a 1)(b 2))) '((1) (2)))
(assert (map-list cadr '((a 1)(b 2))) '(1 2))
(assert (($vau l e (list* '$define! (map-list car l) (list (list 'quote (map-list cadr l))))) (a 1)(b 2)) '($define! (a b) (quote (1 2))))

($define! list-for-each
  ($lambda (f lst)
    (if (nil? lst) ()
        (begin (f (car lst)) (list-for-each f (cdr lst))) )))

($define! list-keep
  ($lambda (p lst)
    (if (nil? lst) ()
        (if (p (car lst))
            (cons (car lst) (list-keep p (cdr lst)))
            (list-keep p (cdr lst)) ))))

($define! fold-list
  ($lambda (f init lst)
    (if (nil? lst) init
        (fold-list f (f init (car lst)) (cdr lst)) )))

(define-macro (let bindings . body)
  (if (symbol? bindings)
      (list* let-loop bindings body)
      (list* (list* $lambda (map-list car bindings) body)
             (map-list cadr bindings) )))

(assert (let ((a 1)) a) 1)

(define-macro (let-loop name bindings . body)
  (list letrec
		(list (list name (list* $lambda (map-list car bindings) body)))
        (list* name (map-list cadr bindings) )))

(define-macro (let* bindings . body)
  (if (nil? bindings)
      (list* let () body)
      (list let
			(list (car bindings))
            (list* let* (cdr bindings) body) )))

(assert (let* () 1) 1)
(assert (let* ((a 1)(b a)) b) 1)

(define-macro (letrec bindings . body)
  (list* let ()
         (list $define!
               (map-list car bindings)
               (list* list (map-list cadr bindings)))
         body))

(assert (letrec ( (a 1)) a) 1)
(assert (letrec ( (a ($lambda () 1)) ) (a)) 1)

(assert (let-loop l ((a 1)) a) 1)

(define-macro (lambda params . body)
  (letrec ((typed-params->names-and-checks
            ($lambda (ps)
              (if (cons? ps)
                  (let* (((p . rest-ps) ps)
                         ((names . checks) (typed-params->names-and-checks rest-ps)))
                    (if (cons? p)
                        (let* (((name type) p)
                               (check (list the type name)))
                          (cons (cons name names) (cons check checks)) )
                        (cons (cons p names) checks) ))
                  (cons ps ()) ))))
    (let ( ((untyped-names . type-checks) (typed-params->names-and-checks params)) )
      (list* $lambda untyped-names (list* begin type-checks) body) )))

(define-macro (define lhs . rhs)
  (if (cons? lhs)
    (list $define! (car lhs) (list* lambda (cdr lhs) rhs))
    (list $define! lhs (car rhs))))

(define (apply appv arg . opt)
  (if (%instanceof? appv &java.util.function.Function)
      (@apply appv (list->array arg))
      (eval (cons (unwrap appv) arg)
            (if (nil? opt)
                (make-environment)
                (car opt)))))

;;;; Simple control

(define-operative (cond . clauses) env
  (if (nil? clauses)
      #inert
      (let ((((test . body) . clauses) clauses))
        (if (eval test env)
            (apply (wrap begin) body env)
            (apply (wrap cond) clauses env) ))))

(define else #t)

(define-operative (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(define-operative (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(define (call-with-escape fun)
  (let ((fresh (list #null)))
    (catch (fun ($lambda opt-arg (throw (list fresh opt-arg))))
      ($lambda (exc)
        (if (and (cons? exc) (eq? fresh (car exc)))
            (let ((opt-arg (cadr exc)))
              (if (cons? opt-arg) (car opt-arg) #null))
            (throw exc))))))

(define-macro (label name . body)
  (list call-with-escape (list* $lambda (list name) body)))

(define-operative (while test . body) env
  (let ((body (list* begin body)))
    (label return
      (loop
        (if (eval test env)
          (eval body env)
          (return))))))

(define-macro (when test . body)
  (list if test (list* begin body) #null))

(define-macro (unless test . body)
  (list* when (list not test) body))

(define-macro (set (getter . args) new-val)
  (list* (list setter getter) new-val args))

;;;; Dynamic Binding

;; Evaluate right hand sides before binding all dynamic variables at once.
(define-operative (dlet bindings . body) env
  (eval
    (let process-bindings ((bs bindings))
       (if (nil? bs)
           (list* begin body)
           (let* ( (((name expr) . rest-bs) bs)
                   (value (eval expr env)) )
             (list %dLet name value (process-bindings rest-bs)) )))
    env ))

(assert (begin (define a (dvar 1)) (dlet ((a 2)) (assert (dref a) 2)) (dref a)) 1)

|#
;;;; Prototypes

(define-operative (define-prototype name super-name prop-names) env
  (eval (list $define! name (make-prototype name super-name prop-names env)) env))

(define (make-prototype name super-name prop-names env)
  (let ((p (apply %js-make-prototype (list* (symbol-name name) (map-list symbol-name prop-names))))
        (super (eval super-name env)))
    (set (.prototype p) (@create &Object (.prototype super)))
    (set (.constructor (.prototype p)) super)
    p ))

(define-macro (define-generic (name . #ignore))
  (list $define! name (lambda args (apply ((js-getter name) (car args)) args))))

(define-macro (define-method (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name) (list* lambda (list* self args) body)))

(define (put-method ctor name fun)
  (set ((js-getter name) (.prototype ctor)) fun))
#|

;;;; Modules

(define-operative (provide symbols . body) env
  (eval
    (list $define! symbols
      (list let ()
        (list* begin body)
        (list* list symbols) ))
    env ))

(assert (begin (provide (x) (define x 10)) x) 10)

(define-operative (module exports . body) env
  (let ((menv (make-environment env)))
    (eval (list* provide exports body) menv)
    (make-environment menv) ))

(assert (begin (define m (module (x) (define x 10))) (eval 'x m)) 10)

(define-macro (define-module name exports . body)
  (list $define! name (list* module exports body)) )

(assert (begin (define-module m (x) (define x 10)) (eval 'x m)) 10)

(define-operative (import module imports) env
  (let* ((m (eval module env))
         (values (map-list ($lambda (import) (eval import m)) imports)))
    (eval (list $define! imports (list* list values)) env) ))

(assert (begin (define-module m (x) (define x 10)) (import m (x)) x) 10)

;;;; JavaScript

(define (relational-op %binop)
  (let ((binop %binop))
    (letrec ((op (lambda (arg1 arg2 . rest)
                   (if (binop arg1 arg2)
                       (if (nil? rest) #t
                           (apply op (list* arg2 rest)))
                       #f))))
      op)))

(define == (relational-op ==))
;(define === (relational-op "==="))
(define < (relational-op <))
(define > (relational-op >))
(define <= (relational-op <=))
(define >= (relational-op >=))

(define (!= . args) (not (apply == args)))

(define *
  (let ((vm* *))
    (lambda args
      (fold-list vm* 1 args) )))

(assert (* 1 2 3) 6)

;; Can't simply use 0 as unit or it won't work with strings
(define +
  (let ((vm+ +))
	(lambda args
	  (if (nil? args) 0
	      (fold-list vm+ (car args) (cdr args)) ))))

(define (negative-op binop unit)
  (lambda (arg1 . rest)
    (if (nil? rest)
        (binop unit arg1)
        (fold-list binop arg1 rest) )))

(define - (negative-op - 0))
(define / (negative-op / 1))

#|
(define % (%js-binop "%"))
(define not (%js-unop "!"))
(define typeof (%js-unop "typeof"))
(define in (%js-binop "in"))
(define instanceof (%js-binop "instanceof"))

(define bitand (%js-binop "&"))
(define bitor (%js-binop "|"))
(define bitxor (%js-binop "^"))
(define bitnot (%js-unop "~"))
(define bitshiftl (%js-binop "<<"))
(define bitshiftr (%js-binop ">>"))
(define bitshiftr0 (%js-binop ">>>"))

(define-operative (object . pairs) env
  (let ((obj (%js-make-object)))
    (map-list ($lambda ((name value))
                (set ((js-getter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(define (elt object key)
  ((js-getter key) object))

(set (setter elt)
  (lambda (new-val object key)
    (set ((js-getter key) object) new-val) ))

(define (array . args) (list->array args))

(define (js-callback fun)
  (%js-function ($lambda args (push-prompt root-prompt (apply fun args)))) )

(define-macro (js-lambda params . body)
  (list js-callback (list* lambda params body)))

(define-macro (type? obj type)
  (list %type? obj type (symbol-name type)))

(define-macro (the type obj)
  (list if (list type? obj type) obj (list error (list + obj " is not a: " type))) )

(define Array &Array)
(define Boolean &Boolean)
(define Date &Date)
(define Function &Function)
(define Number &Number)
(define Object &Object)
(define RegExp &RegExp)
(define String &String)

(define (log x . xs)
  (apply @log (list* &console x xs))
  x)

;;;; Cells

(define-prototype Cell Object (value))
(define (cell value) (new Cell value))
(define (ref (c Cell)) (.value c))
(set (setter ref) (lambda (new-val (c Cell)) (set (.value c) new-val)))

(define-macro (++ place)
  (list set place (list + place 1)) )
(define-macro (-- place)
  (list set place (list - place 1)) )

;;;; Utilities

;; ugh
(define (map-array fun (arr Array))
  (list->array (map-list fun (array->list arr))) )

(define (array-keep pred (arr Array))
  (list->array (list-keep pred (array->list arr))) )

(define-operative (time expr) env
  (let ((n (@getTime (new Date)))
        (result (eval expr env)))
    (log ($ "time " expr ": " (- (@getTime (new Date)) n) "ms"))
    result ))

;;;; Options

(define-prototype Option Object ())
(define-prototype Some Option (value))
(define-prototype None Option ())
(define (some value) (new Some value))
(define none (new None))
(define-operative (if-option (option-name option-expr) then else) env
  (let ((option (the Option (eval option-expr env))))
    (if (type? option Some)
        (eval (list (list lambda (list option-name) then) (.value option)) env)
        (eval else env) )))
|#

;;;; Error break routine, called by VM to print stacktrace and throw

(define (print-stacktrace)
  (define (print-frame k)
    ;(log "--" k)
    (if (!= (.next k) #null)
      (print-frame (.next k)) )
    (log "--" k) )
  (take-subcont root-prompt k
    (print-frame k) (push-prompt root-prompt (push-subcont k) )) ;old need 2+ vau args 
    ;(push-prompt root-prompt (push-subcont k (print-frame k) )) ;ok with 3+ vau args
)

(define (userBreak err)
  ;(log "==" err)
  (when (stack) (log "++" err) (print-stacktrace))
  (throw err)
 )
