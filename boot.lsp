;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename %def
(%def def %def)
(%def $define! %def)

;; Rename bindings that will be used as provided by VM

($define! == %==)
($define! != %!=)

($define! + %+)
($define! * %*)
($define! - %-)
($define! / %/)
($define! % %%)
  
($define! ! %!)
($define! < %<)
($define! > %>)
($define! <= %<=)
($define! >= %>=)
  
($define! ~ %~)
($define! & %&)
;($define! | %|) ;; TODO lo vedrebbe come uneascaped symbol!
($define! ^ %^)
($define! << %<<)
($define! >> %>>)
($define! >>> %>>>)

($define! array->list %array->list)
($define! begin %begin)
($define! catchTag %catch)
($define! cons %cons)
($define! cons? %cons?)
($define! ddef %dDef)
($define! dnew %dNew)
($define! dval %dVal)
($define! eq? %eq?)
($define! error %error)
($define! eval %eval)
($define! if %if)
($define! list* %list*)
($define! list->array %list->array)
($define! loop %loop)
($define! makeEnvironment %makeEnv)
($define! nil? %null?)
($define! not !)
($define! reverseList %reverse)
($define! rootPrompt %rootPrompt)
($define! string->symbol %string->symbol)
($define! symbolName %internName)
($define! symbol? %symbol?)
($define! throwTag %throw)
($define! unwrap %unwrap)
($define! wrap %wrap)

;; Important utilities
($define! $vau %vau)
($define! quote ($vau (x) #ignore x))
($define! quote %quote)
($define! list (wrap ($vau elts #ignore elts)))
($define! list %list)
($define! $lambda ($vau (formals . body) env (wrap (eval (list* $vau formals #ignore body) env))))
($define! $lambda %lambda)
($define! \ %lambda)
($define! theEnvironment ($vau () e e))
($define! theEnvironment %theEnvironment)


;;;; Macro

($define! makeMacro
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
        (eval (eval (cons expander operands) (makeEnvironment)) env) ))))

(def evm (dnew #t))

($define! makeMacro
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
      	(def expr (eval (cons expander operands) (makeEnvironment)))
        (if (dval evm) (eval expr env) expr) ))))

($define! macro
  (makeMacro
    ($vau (params . body) #ignore
      (list makeMacro (list* $vau params #ignore body)) )))

($define! defineMacro
  (macro ((name . params) . body)
    (list $define! name (list* macro params body)) ))

(defineMacro (expand macro) (list 'dlet '((evm #f)) macro))

|#
(defineMacro (let2 bindings . body) (list* (list* '\ (mapList car bindings) body) (mapList cadr bindings) ))
(let2 ((a 1) (b 2)) (+ a b))
(dlet ((evm #f)) (let2 ((a 1) (b 2)) (+ a b)))
(expand (let2 ((a 1) (b 2)) (+ a b)))
#|

(defineMacro (defineOperative (name . params) envparam . body)
  (list $define! name (list* $vau params envparam body)) )


;;;; Wrap incomplete VM forms

(defineMacro (catch x . handler)
  (list* catchTag #ignore x handler))

(defineMacro (throw . x)
  (list* throwTag #ignore x))

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catch (throw 1) (%lambda (x) 2)) 2)

(defineOperative (finally protected . cleanup) env
  (eval (list %finally protected (list* begin cleanup)) env) )

(defineMacro (takeSubcont prompt k . body)
  (list %takeSubcont prompt (list* $lambda (list k) body)) )

(defineOperative (pushPrompt prompt . body) env
  (eval (list %pushPrompt (eval prompt env) (list* begin body)) env) )

(defineMacro (pushPromptSubcont p k . body)
  (list %pushPromptSubcont p k (list* $lambda () body)) )

(defineMacro (pushSubcont k . body)
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

($define! mapList
  ($lambda (f lst)
    (if (nil? lst) ()
        (cons (f (car lst)) (mapList f (cdr lst))) )))

(assert (mapList car  '((a 1)(b 2))) '(a b))
(assert (mapList cdr  '((a 1)(b 2))) '((1) (2)))
(assert (mapList cadr '((a 1)(b 2))) '(1 2))
(assert (($vau l e (list* '$define! (mapList car l) (list (list 'quote (mapList cadr l))))) (a 1)(b 2)) '($define! (a b) (quote (1 2))))

($define! listForEach
  ($lambda (f lst)
    (if (nil? lst) ()
        (begin (f (car lst)) (listForEach f (cdr lst))) )))

($define! listKeep
  ($lambda (p lst)
    (if (nil? lst) ()
        (if (p (car lst))
            (cons (car lst) (listKeep p (cdr lst)))
            (listKeep p (cdr lst)) ))))

($define! foldList
  ($lambda (f init lst)
    (if (nil? lst) init
        (foldList f (f init (car lst)) (cdr lst)) )))

(defineMacro (let bindings . body)
  (if (symbol? bindings)
      (list* letLoop bindings body)
      (list* (list* $lambda (mapList car bindings) body)
             (mapList cadr bindings) )))

(defineMacro (let bindings . body)
  (list*
    (list* $lambda (mapList car bindings) body)
    (mapList cadr bindings) ))
    
(assert (let ((a 1)) a) 1)

(defineMacro (let* bindings . body)
  (if (nil? bindings)
      (list* let () body)
      (list let
        (list (car bindings))
        (list* let* (cdr bindings) body) )))

(assert (let* () 1) 1)
(assert (let* ((a 1)(b a)) b) 1)

(defineMacro (letrec bindings . body)
  (list* let ()
         (list $define!
               (mapList car bindings)
               (list* list (mapList cadr bindings)))
         body))

(assert (letrec ( (a 1)) a) 1)
(assert (letrec ( (a ($lambda () 1)) ) (a)) 1)

(defineMacro (letLoop name bindings . body)
  (list letrec
    (list (list name (list* $lambda (mapList car bindings) body)))
    (list* name (mapList cadr bindings) )))

(assert (letLoop l ((a 1)) a) 1)
(assert (letLoop sum ((as (list 1 2)) (bs (list 3 4))) (if (nil? as) () (cons (+ (car as) (car bs)) (sum (cdr as) (cdr bs))))) '(4 6))

(defineMacro (lambda params . body)
  (letrec ((typedParams->namesAndChecks
            ($lambda (ps)
              (if (cons? ps)
                  (let* (((p . restPs) ps)
                         ((names . checks) (typedParams->namesAndChecks restPs)))
                    (if (cons? p)
                        (let* (((name type) p)
                               (check (list the type name)))
                          (cons (cons name names) (cons check checks)) )
                        (cons (cons p names) checks) ))
                  (cons ps ()) ))))
    (let ( ((untypedNames . typeChecks) (typedParams->namesAndChecks params)) )
      (list* $lambda untypedNames (list* begin typeChecks) body) )))

(defineMacro (define lhs . rhs)
  (if (cons? lhs)
    (list $define! (car lhs) (list* lambda (cdr lhs) rhs))
    (list $define! lhs (car rhs))))

(define (apply appv arg . optEnv)
  (if (%instanceof? appv &java.util.function.Function)
      (@apply appv (list->array arg))
      (eval (cons (unwrap appv) arg)
            (if (nil? optEnv) (makeEnvironment) (car optEnv)) )))


;;;; Simple control

(defineOperative (cond . clauses) env
  (if (nil? clauses)
      #inert
      (let ((((test . body) . clauses) clauses))
        (if (eval test env)
            (apply (wrap begin) body env)
            (apply (wrap cond) clauses env) ))))

(define else #t)

(defineOperative (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(defineOperative (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(define (callWithEscape fun)
  (let ((fresh (list #null)))
    (catch (fun ($lambda optArg (throw (list fresh optArg))))
      ($lambda (exc)
        (if (and (cons? exc) (eq? fresh (car exc)))
            (let ((optArg (cadr exc)))
              (if (cons? optArg) (car optArg) #null))
            (throw exc))))))

(defineMacro (label name . body)
  (list callWithEscape (list* $lambda (list name) body)))

(defineOperative (while test . body) env
  (let ((body (list* begin body)))
    (label return
      (loop
        (if (eval test env)
          (eval body env)
          (return))))))

(defineMacro (when test . body)
  (list if test (list* begin body) #null))

(defineMacro (unless test . body)
  (list* when (list not test) body))

(defineMacro (set (getter . args) newVal)
  (list* (list setter getter) newVal args))


;;;; Delimited dynamic binding

(defineOperative (progv dyns vals . forms) env
  (eval
    (list* %dLet 
      (eval (list* list dyns) env)
      (eval (list* list vals) env)
      forms )
    env ))

(assert (begin (ddef (a) 1) (progv (a) (2) (assert (dval a) 2)) (assert (dval a) 1)) #t)

(defineOperative (dlet bindings . forms) env
  (eval
    (list* %dLet 
      (mapList (%lambda ((name #ignore))  (eval name env)) bindings)
      (mapList (%lambda ((#ignore value)) (eval value env)) bindings)
      forms )
    env ))

(assert (begin (ddef (a b) 1 2) (dlet ((a 2)) (assert (dval a) 2)) (dval b)) 2)

(defineMacro (dlet* bindings . body)
  (if (nil? bindings)
    (list* begin body)
    (list dlet
	  (list (car bindings))
      (list* dlet* (cdr bindings) body) )))

(assert (begin (ddef (a) 1) (dlet* ((a (+ 1 (dval a))) (a (+ 1 (dval a)))) (dval a))) 3)

(test defdynamic.1
  (begin
    (ddef (x y) 1 (+ 1 1))
    (assert (dval x) 1)
    (assert (dval y) 2)
    (dlet* ((x 3))
      (assert (dval x) 3)
      (assert (dval y) 2)
      (dlet* ((y 4))
        (assert (dval x) 3)
        (assert (dval y) 4))
      (assert (dval x) 3)
      (assert (dval y) 2))
    (assert (dval x) 1)
    (assert (dval y) 2))
  #t)
  
(test defdynamic.redefine
  (begin  
    (ddef (a) (+ 1 1))
    (def oa a)
    (assert (dval a) 2)
    (assert (dval oa) 2)
    (ddef (a) (+ 2 2))
    (assert (dval a) 4)
    (assert (dval oa) 4)
    (assert (eq? oa a) #t)
    (ddef (a) #null)
    (assert (dval a) #null)
    (assert (eq? oa a) #t) )
  #t )

(test progv.1
  (begin
    (ddef (*x*) 1)
    (ddef (*y*) 2)
    (assert (dval *x*) 1)
    (assert (dval *y*) 2)
    (progv (*x*) (3)
      (assert (dval *x*) 3)
      (assert (dval *y*) 2)
      (progv (*y*) (4)
        (assert (dval *x*) 3)
        (assert (dval *y*) 4) )
      (assert (dval *x*) 3)
      (assert (dval *y*) 2) )
    (assert (dval *x*) 1)
    (assert (dval *y*) 2) )
  #t )
    
|#
  (deftest dynamic.1
    (progn
      (defdynamic *foo*)
      (assert (= (dynamic *foo*) #void))
      (assert (= (slot-value *foo* 'value) #void))
      (assert (typep *foo* #^dynamic))
      (assert (typep *foo* #^standard-object))
      (assert (typep *foo* #^object))
      (assert (subclassp #^dynamic #^standard-object))
      (assert (subclassp #^dynamic #^object)))
    #void)
#|

(test set-dynamic.1
  (begin
    (ddef (*bar*) #null)
    (dlet ((*bar* 1))
      (dval *bar* 2)
      (assert (dval *bar*) 2)
      (dlet ((*bar* 3))
        (assert (dval *bar*) 3) )
      (assert (dval *bar*) 2)
      (dval *bar* 4)
      (assert (dval *bar*) 4) )
    (assert (dval *bar*) #null) )
  #t )

(test dynamic-let*.1
  (dlet* () (+ 1 1))
  2)

(test dynamic-let*.2
  (begin
    (ddef (*x*) 1)
    (dlet* ((*x* 2)) (+ 1 (dval *x*))))
  3)

(test dynamic-let*.2
  (begin
    (ddef (*x*) 1)
    (ddef (*y*) 0)
    (dlet* ((*x* 2) (*y* (+ (dval *x*) 1)))
      (list (dval *x*) (dval *y*))))
  '(2 3))

(test dynamic-let-sanity-check
  (begin
    (ddef (*x*) 1)
    (ddef (*y*) 0)
    (dlet ((*x* 2) (*y* (+ (dval *x*) 1)))
      (list (dval *x*) (dval *y*))))
  '(2 2) )


|#
;;;; Prototypes

(defineOperative (definePrototype name superName propNames) env
  (eval (list $define! name (makePrototype name superName propNames env)) env))

(define (makePrototype name superName propNames env)
  (let ((p (apply %jsMakePrototype (list* (symbolName name) (mapList symbolName propNames))))
        (super (eval superName env)))
    (set (.prototype p) (@create &Object (.prototype super)))
    (set (.constructor (.prototype p)) super)
    p ))

(defineMacro (defineGeneric (name . #ignore))
  (list $define! name (lambda args (apply ((jsGetter name) (car args)) args))))

(defineMacro (defineMethod (name (self ctor) . args) . body)
  (list putMethod ctor (symbolName name) (list* lambda (list* self args) body)))

(define (putMethod ctor name fun)
  (set ((jsGetter name) (.prototype ctor)) fun))
#|


;;;; Modules

(defineOperative (provide symbols . body) env
  (eval
    (list $define! symbols
      (list let ()
        (list* begin body)
        (list* list symbols) ))
    env ))

(assert (begin (provide (x) (define x 10)) x) 10)

(defineOperative (module exports . body) env
  (let ((menv (makeEnvironment env)))
    (eval (list* provide exports body) menv)
    (makeEnvironment menv) ))

(assert (begin (define m (module (x) (define x 10))) (eval 'x m)) 10)

(defineMacro (defineModule name exports . body)
  (list $define! name (list* module exports body)) )

(assert (begin (defineModule m (x) (define x 10)) (eval 'x m)) 10)

(defineOperative (import module imports) env
  (let* ((m (eval module env))
         (values (mapList ($lambda (import) (eval import m)) imports)))
    (eval (list $define! imports (list* list values)) env) ))

(assert (begin (defineModule m (x) (define x 10)) (import m (x)) x) 10)


;;;; JavaScript

(define (relationalOp %binop)
  (let ((binop %binop))
    (letrec ((op (lambda (arg1 arg2 . rest)
                   (if (binop arg1 arg2)
                       (if (nil? rest) #t
                           (apply op (list* arg2 rest)))
                       #f))))
      op)))

(define == (relationalOp ==))
(define < (relationalOp <))
(define > (relationalOp >))
(define <= (relationalOp <=))
(define >= (relationalOp >=))

(define (!= . args) (not (apply == args)))

(define *
  (let ((vm* *))
    (lambda args
      (foldList vm* 1 args) )))

(assert (* 1 2 3) 6)

(define +
  (let ((vm+ +))
	(lambda args
	  (if (nil? args) 0 ;; Can't simply use 0 as unit or it won't work with strings
	      (foldList vm+ (car args) (cdr args)) ))))

(define (negativeOp binop unit)
  (lambda (arg1 . rest)
    (if (nil? rest)
        (binop unit arg1)
        (foldList binop arg1 rest) )))

(define - (negativeOp - 0))
(define / (negativeOp / 1))

|#
(define % (%jsBinop "%"))
(define not (%jsUnop "!"))
(define typeof (%jsUnop "typeof"))
(define in (%jsBinop "in"))
(define instanceof (%jsBinop "instanceof"))

(define bitand (%jsBinop "&"))
(define bitor (%jsBinop "|"))
(define bitxor (%jsBinop "^"))
(define bitnot (%jsUnop "~"))
(define bitshiftl (%jsBinop "<<"))
(define bitshiftr (%jsBinop ">>"))
(define bitshiftr0 (%jsBinop ">>>"))

(defineOperative (object . pairs) env
  (let ((obj (%jsMakeObject)))
    (mapList ($lambda ((name value))
                (set ((jsGetter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(define (elt object key)
  ((jsGetter key) object))

(set (setter elt)
  (lambda (newVal object key)
    (set ((jsGetter key) object) newVal) ))

(define (array . args) (list->array args))

(define (jsCallback fun)
  (%jsFunction ($lambda args (pushPrompt rootPrompt (apply fun args)))) )

(defineMacro (jsLambda params . body)
  (list jsCallback (list* lambda params body)))

(defineMacro (type? obj type)
  (list %type? obj type (symbolName type)))

(defineMacro (the type obj)
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

(definePrototype Cell Object (value))
(define (cell value) (new Cell value))
(define (ref (c Cell)) (.value c))
(set (setter ref) (lambda (newVal (c Cell)) (set (.value c) newVal)))

(defineMacro (++ place)
  (list set place (list + place 1)) )
(defineMacro (-- place)
  (list set place (list - place 1)) )


;;;; Utilities

;; ugh
(define (mapArray fun (arr Array))
  (list->array (mapList fun (array->list arr))) )

(define (arrayKeep pred (arr Array))
  (list->array (listKeep pred (array->list arr))) )

(defineOperative (time expr) env
  (let ((n (@getTime (new Date)))
        (result (eval expr env)))
    (log (+ "time " expr ": " (- (@getTime (new Date)) n) "ms"))
    result ))


;;;; Options

(definePrototype Option Object ())
(definePrototype Some Option (value))
(definePrototype None Option ())
(define (some value) (new Some value))
(define none (new None))
(defineOperative (ifOption (optionName optionExpr) then else) env
  (let ((option (the Option (eval optionExpr env))))
    (if (type? option Some)
        (eval (list (list lambda (list optionName) then) (.value option)) env)
        (eval else env) )))
#|


;;;; Error break routine, called by VM to print stacktrace and throw

(define (printStacktrace)
  (define (printFrame k)
    ;(log "--" k)
    (if (!= (.next k) #null)
      (printFrame (.next k)) )
    (log "--" k) )
  (takeSubcont rootPrompt k
  	(printFrame k) (pushPrompt rootPrompt (pushSubcont k) )) ;old need 2+ vau args
    ;(pushPrompt rootPrompt (pushSubcont k (printFrame k) )) ;ok with 3+ vau args
)

(define (userBreak err)
  ;(log "==" err)
  (when (stack) (log "++" err) (printStacktrace))
  (throw err) )
