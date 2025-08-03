;;;                                                     -*- mode: Scheme -*-
;;; Wat Bootstrap
;;;

;;; Copyright (c) 2021, 2022 Manuel J. Simoni

;;; ``72. An adequate bootstrap is a contradiction in terms.''


#|! Core Built-Ins for Macro and Definition Forms
 |#

(%def def
  #|($nm definiendTree value)
   |($nm definiendTree bindResult value)
   |(type fexpr)
   |
   |(syntax definiendTree (or symbol decomposeTree))
   |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
   |
   |(syntax decomposeTree (parametersTree . decomposeTree))
   |(syntax parametersTree (or #null ignore symbol decomposeTree))
   |(syntax symbol (or Symbol (#: check Symbol)))
   |(syntax ignore (or #ignore (#: check #ignore)))
   |(syntax check Any)
   |(syntax check class)
   |(syntax check checks)
   |(syntax checks (check . checks))
   |(syntax check (min . checks))
   |(syntax check (min max . checks))
   |(syntax check (min oo . checks))
   |(syntax check (or . checks))
   |(syntax check (and . checks))
   |(syntax check (Apv . arguments))
   |(syntax check value)
   |(syntax arguments (value . arguments))
   |
   |Defines into the current environment the resulting bindings from the match of the <b>definiendTree</b> against <b>value</b>, signals an error otherwise.
   |
   |without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
   |with <b>bindResult</b> #inert return #inert
   |with <b>bindResult</b> :rhs return the right side of the last binding
   |with <b>bindResult</b> :prv return the previous value of the last binding
   |with <b>bindResult</b> :cnt return the env
   |#
  %def )

(def set!
  #|($nm definiendTree value)
   |($nm definiendTree bindResult value)
   |(type fexpr)
   |
   |(syntax definiendTree (or symbol decomposeTree))
   |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
   |
   |(syntax decomposeTree (parametersTree . decomposeTree))
   |(syntax parametersTree (or #null ignore symbol decomposeTree))
   |(syntax symbol (or Symbol (#: check Symbol)))
   |(syntax ignore (or #ignore (#: check #ignore)))
   |(syntax check Any)
   |(syntax check class)
   |(syntax check checks)
   |(syntax checks (check . checks))
   |(syntax check (min . checks))
   |(syntax check (min max . checks))
   |(syntax check (min oo . checks))
   |(syntax check (or . checks))
   |(syntax check (and . checks))
   |(syntax check (Apv . arguments))
   |(syntax check value)
   |(syntax arguments (value . arguments))
   |
   |Update into the environment the resulting bindings from the match of the <b>definiendTree</b> against <b>value</b>, signals an error if a binding is not defined.
   |
   |without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
   |with <b>bindResult</b> #inert return #inert
   |with <b>bindResult</b> :rhs return the right side of the last binding
   |with <b>bindResult</b> :prv return the previous value of the last binding
   |with <b>bindResult</b> :cnt return the env
   |#
  %set! )

(def vau
  #|($nm parametersTree environmentParameter . body)
   |($nm parametersTree environmentParameter #: check . body)
   |(type fexpr)
   |
   |(syntax parametersTree (or #null ignore symbol decomposeTree))
   |(syntax environmentParameter (or #ignore Symbol))
   |(syntax body (or forms (#: check . forms)))
   |
   |(syntax decomposeTree (parametersTree . decomposeTree))
   |(syntax symbol (or Symbol (#: check Symbol)))
   |(syntax ignore (or #ignore (#: check #ignore)))
   |(syntax check Any)
   |(syntax check class)
   |(syntax check checks)
   |(syntax checks (check . checks))
   |(syntax check (min . checks))
   |(syntax check (min max . checks))
   |(syntax check (min oo . checks))
   |(syntax check (or . checks))
   |(syntax check (and . checks))
   |(syntax check (Apv . arguments))
   |(syntax check value)
   |(syntax arguments (value . arguments))
   |
   |Return a anonymous fexpr with the given <b>parameterTree</b>, <b>environmentParameter</b> and <b>body</b>.
   |#
  %vau )

(def begin
  #|($nm . forms)
   |(type fexpr)
   |
   |Sequentially evaluate <b>forms</b> returning the value of the last one, #inert if <b>forms</b> is #null.
   |#
  %begin )

(def if
  #|($nm test thenForm . forms)
   |(type fexpr)
   |
   |(syntax forms (or () (elseForm) (test thenForm . forms)))
   |
   |(derivation (vau (test thenForm . forms) env (if (eval test env) (eval thenForm env) (null? forms) #inert (null? (cdr forms)) (eval (car forms) env) (apply if forms env))))
   |
   |Evaluate the <b>test</b> which must yield a boolean.
   |Then evaluate either the <b>thenForm</b> or <b>elseForm</b> expression depending on whether the <b>test</b> yielded #true or #false.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  %if )

(def wrap
  #|($nm operator)
   |(type function)
   |
   |Return a new function that wraps around the underlying <b>operator</b>, and induces argument evaluations around it.
   |Does not wrap the `Apv` and the `java functions`, but wraps the remaining `Combinator`.
   |#
  %wrap )

(def theEnv
  #|($nm)
   |(type fexpr)
   |
   |(derivation (vau () environment environment))
   |
   |Return the current environment.
   |#
  %theEnv )

(def eval
  #|($nm form . environment)
   |(type function)
   |
   |(derivation (eval form (if (null? environment) (theEnv) (car! environment))))
   |
   |Return the result of evaluation of <b>form</b> in the optional <b>environment</b>.
   |#
  %eval )

(def \
  #|($nm parameterTree . forms)
   |($nm parameterTree #: check . forms)
   |(type function)
   |
   |(derivation (vau (parameterTree . forms) env (wrap (eval (list* 'vau parameterTree #ignore forms) env))))
   |
   |Return an anonymous function with the given <b>parameterTree</b> and <b>forms</b> as body,
   |which use the definition environment for evaluate <b>forms</b> as an implicit `begin' and get the values of the free variables.
   |The classic Scheme static lambda.
   |#
  %\ )

(def lambda
  #|(aliasof \. :-)
   |#
  \ )

(def unwrap
  #|($nm function)
   |(type function)
   |
   |Return the underlying operator of a <b>function</b>.
   |Unwraps the `Apv`, wraps the `java functions` in a `JFun` and does not unwrap the remaining `Combinator`.
   |#
  %unwrap )

(def apply
  #|($nm fun args . environment)
   |(type function)
   |
   |(derivation (eval (cons (unwrap fun) args) (if (null? environment) (newEnv) (car! environment)) ))
   |
   |Call the <b>function</b> with a dynamically-supplied list of <b>arguments</b> in the optional <b>environment</b>.
   |#
  %apply )

(def car
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((car . #_)) car)) cons)
   |(derivation (@car cons))
   |
   |Return the contents of the address part of the register.
   |#
  %car )

(def car!
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((car)) car)) cons)
   |
   |Return the contents of the address part of the register
   |if decrement part of the register is #null, signals an error otherwise.
   |#
  (\ ((car)) car) )

(def cadr
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((#_ cadr . #_)) cadr)) cons)
   |(derivation (car (cdr cons)))
   |(derivation (@car cons 1))
   |
   |Return the `car' of the `cdr' of the <b>cons</b>.
   |#
  %cadr )

(def cdr
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((#_ . cdr)) cdr)) cons)
   |(derivation (@cdr cons))
   |
   |Return the contents of the decrement part of the register.
   |#
  %cdr )

(def cons
  #|($nm car cdr)
   |(type function)
   |
   |(derivation (@new vm Cons car cdr))
   |
   |Return a cons with the given <b>car</b> and <b>cdr</b>.
   |#
  %cons )

(def cons?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object Cons))
   |
   |Return #true if the <b>object</b> is a cons, #false otherwise.
   |#
  %cons? )

(def atom?
  #|($nm object)
   |(type function)
   |
   |(derivation (if (cons? object) #f #t))
   |
   |Return #true if the <b>object</b> is not a cons, #false otherwise.
   |#
  (\ (object) (if (cons? object) #f #t)) )

(def list
  #|($nm . arguments)
   |(type function)
   |
   |(derivation (wrap (vau arguments #ignore arguments)))
   |
   |Return the list of evaluated <b>arguments</b>.
   |#
  %list )

(def list*
  #|($nm . arguments)
   |(type function)
   |
   |(derivation (if (null? arguments) () (((\ (loop) (def loop :rhs (\ ((car . cdr)) (if (null? cdr) car (cons car (loop cdr)))))) #inert) arguments))) 
   |(derivation (if (null? arguments) () ((rec\ (loop (car . cdr)) (if (null? cdr) car (cons car (loop cdr)))) arguments)))
   |
   |Return a list of evaluated <b>arguments</b> so that
   |the last argument becomes the `cdr' of the list.
   |#
  %list* )

(def null?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object Null))
   |
   |Return #true if the <b>object</b> is #null, #false otherwise.
   |#
  %null? )

(def quote
  #|($nm operand)
   |(type fexpr)
   |
   |(derivation (vau (operand) #ignore operand))
   |
   |Return the unevaluated <b>operand</b>.
   |#
  %' )

(def assert
  #|($nm expression value)
   |($nm expression)
   |($nm expression class attribute value . attributes)
   |(type fexpr)
   |
   |(syntax attributes (attribute value . attributes))
   |(syntax attribute (or Symbol Keyword String .Field @Method))
   |
   |Signals an error if:
   |- <b>expression</b> does not equal <b>value</b>
   |- <b>value</b> is not present and the <b>expression</b> does not throws
   |- <b>expression</b> throws or returns an object and the object is not of the same <b>class</b> with the <b>attributes</b> of the given <b>value</b>.
   |#
  %assert )


#|! Macro
 |#

(def evalMacro?
  #|A boolean to discriminate when to evaluate or simply expand a macro.
   |Used from expand and makeMacro. 
   |#
  #t )

#|
(def makeMacro
  #|($nm expander)
   |(type function)
   |
   |Return a macro from an <b>expander</b> operator.
   |A macro is an operator that receives an operand and produces a form
   |(by calling the expander with the operand as argument)
   |that is then evaluated in place of the operand.
   |#
  (wrap
    (vau (expander) #ignore
      (vau operands env
        (def evalMacro? (set! evalMacro? :prv #t))
        (def exp (apply expander operands))
        (if evalMacro? (eval exp env) exp) ))))
|#
;TODO da valutare in sostituzione della precedente 
(def makeMacro
  #|($nm expander)
   |(type function)
   |
   |Return a macro from an <b>expander</b> operator.
   |A macro is an operator that receives an operand and produces a form
   |(by calling the expander with the operand as argument)
   |that is then evaluated in place of the operand.
   |#
  (\ (expander)
    (vau operands env
      (def evalMacro? (set! evalMacro? :prv #t))
      (def exp (apply expander operands))
      (if evalMacro? (eval exp env) exp) )))

(def macro
  #|($nm parameterTree . forms)
   |(type macro)
   |
   |Return an anonymous macro with the given <b>parameterTree</b> and <b>forms</b> as body.
   |#
  (makeMacro
    (vau (pt . forms) #ignore
      (list 'makeMacro (list* 'vau pt #ignore forms)) )))

(def expand
  #|($nm form)
   |(type macro)
   |
   |Expands a macro call rather than evaluates it.
   |#
  (macro (form)
    (list 'begin (list 'set! 'evalMacro? #f) form) ))


#|! Definition Forms
 |The forms defMacro defVau def\ defde\ def*\ rec\ let1\ let1rec\ let\ and letrec\ have two equivalent syntax
 |
 |    (_ name parameterTree . forms)
 |    (_ (name . parameterTree) . forms)
 |
 |The forms rec rec\ let1rec let1rec\ letrec and letrec\ pre-initialize all bindings to #inert before evaluating and binding the values or functions.
 |#

(def defMacro
  #|($nm name parameterTree . forms)
   |($nm (name parameterTree) . forms)
   |(type macro)
   |
   |Defines into the current environment the named macro <b>name</b> with the given <b>parameterTree</b> and <b>forms</b> as body.
   |#
  (macro (lhs . rhs)
    (if (cons? lhs)
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs))
      (list 'def lhs (cons 'macro rhs)) )))

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))

(defMacro (defVau lhs . rhs)
  #|($nm name parameterTree . forms)
   |($nm (name parameterTree) . forms)
   |(type macro)
   |
   |Defines into the current environment the named fexpr <b>name</b> with the given <b>parameterTree</b>, <b>environmentParameter</b> and <b>forms</b> as body.
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'vau (cdr lhs) rhs))
    (list 'def lhs (cons 'vau rhs)) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))

(def defConstant
  #|(aliasof def)
   |
   |Defines into the current environment the named constant <b>name</b> with the given <b>value</b>.
   |This is mostly for documentation purposes, as constants are still mutable.
   |#
  def )

(defMacro (def* lhs . rhs)
  #|($nm definiendTree . values)
   |($nm definiendTree bindResult . values)
   |(type macro)
   |
   |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
   |
   |Defines into the current environment the resulting bindings from the match of the <b>definiendTree</b> against <b>values</b>, signals an error otherwise.
   |#
  (list 'def lhs (cons 'list rhs)) )

(defMacro (def\ lhs . rhs)
  #|($nm name parameterTree . forms)
   |($nm (name parameterTree) . forms)
   |(type macro)
   |
   |Defines into the current environment the named function <b>name</b> with the given <b>parameterTree</b> and <b>forms</b> as body.
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'def lhs (cons '\ rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


#|! Other Core Built-Ins
 |#

(def apply*
  #|($nm function . arguments)
   |(type function)
   |
   |(derivation (apply function arguments (newEnv)))
   |
   |Call the <b>function</b> with a dynamically-supplied list of <b>arguments</b>.
   |#
  %apply* )

(def apply**
  #|($nm function . arguments)
   |(type function)
   |
   |(derivation (apply function (apply list* arguments) (newEnv)))
   |
   |Call the <b>function</b> with a dynamically-supplied list of <b>arguments</b>.
   |#
  %apply** )

(defVau set (ep dt value) env
  #|($nm environmentParameter definiendTree value)
   |(type fexpr)
   |
   |Match the <b>definiendTree</b> against the <b>value</b> in the <b>environmentParameter</b>, creating or updating existing bindings.
   |Unlike Common Lisp `setq' (or Scheme `set!') that allows updating arbitrary bindings
   |you always need to know the environment where a binding is in to change it.
   |Therefore, we usually use boxes instead of mutating bindings directly.
   |#
  (eval
    (list 'def dt (list (unwrap eval) value env))
    (eval ep env) ))

(defMacro (wrau pt ep . forms)
  #|($nm parameterTree environmentParameter . forms)
   |(type function)
   |
   |(derivation (wrap (vau parameterTree environmentParameter . forms))
   |
   |Return an anonymous function with the given <b>parameterTree</b>, <b>environmentParameter</b> and <b>forms</b> as body,
   |which may use the execution environment in the <b>environmentParameter</b>, rather than the definition environment, for evaluate <b>forms</b> as an implicit `begin'.
   |Used for defining the dynamic environment and variable lambda de\ and dv\.
   |#
  (list 'wrap (list* 'vau pt ep forms)) )

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))
;(assert (let* ((a 1) (a 2)) ((wrap (vau (b) #_ (list a b))) (+ 1 2))) '(2 3))
;(assert (let* ((a 1) (a 2)) ((wrau (b) #_ (list a b)) (+ 1 2))) '(2 3))

(def de\
  #|($nm parameterTree . forms)
   |(type fexpr)
   |
   |(derivation (wrau args env (apply begin forms (bind (newEnv env) parameterTree args))))
   |
   |Return an anonymous function with the given <b>parameterTree</b> and <b>forms</b> as body,
   |which use the execution environment for evaluate <b>forms</b> as an implicit `begin' and get the values of the free variables.
   |The first type of lambda before Scheme.
   |#
  (vau (pt . forms) #_
    (wrau args env (apply begin forms (bind (newEnv env) pt args)))))

;(assert (let* ((f (de\ () a)) (a 1) (a 2)) (f)) 2) 

(defMacro (defde\ lhs . rhs)
  #|($nm name parameterTree . forms)
   |($nm (name parameterTree) . forms)
   |(type macro)
   |
   |Defines into the current environment the named dynamic environment function <b>name</b> with the given <b>parameterTree</b> and <b>forms</b> as body.
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'de\ (cdr lhs) rhs))
    (list 'def lhs (cons 'de\ rhs)) ))


#|! Env
 |(env)
 |(env attribute)
 |(env attribute value . attributes)
 |(env bindType attribute value . attributes)
 |(env bindResult attribute value . attributes)
 |(env bindType bindResult attribute value . attributes)
 |(type function)
 |
 |(syntax attributes (attribute value . attributes))
 |(syntax attribute (or Symbol Keyword String))
 |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |(syntax bindType (or :def :set!))
 |
 |The Env are functions that encapsulates mutable values.
 |
 |Calling the env:
 |
 |- without arguments returns the env.
 |- with an attribute return the value of the attribute in the env.
 |- with couples of attribute and value defines or update the attribute with the value in the env.
 |
 |- without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
 |- with <b>bindResult</b> #inert return #inert
 |- with <b>bindResult</b> :rhs return the right side of the last binding
 |- with <b>bindResult</b> :prv return the previous value of the last binding
 |- with <b>bindResult</b> :cnt return the env
 |
 |- without <b>bindType</b> use the bindType :def
 |- with <b>bindType</b> :def define or update the bindings in the env
 |- with <b>bindType</b> :set! update the bindings in env, signals an error if the binding is not defined.
 |#

(def newEnv
  #|($nm)
   |($nm parent . bindings)
   |($nm parent obj)
   |(type function)
   |
   |(syntax parent (or () Env))
   |(syntax bindings (attribute value . bindings))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Return a new environment with an optional <b>parent</b> environment in which bindings are looked up if they are not found.
   |The <b>bindings</b> must be of even length, and alternately contain bindings names (symbols, keywords or string) and values.
   |When called with an obj the obj bindings also become env bindings.
   |#
  %newEnv )


#|! Obj
 |(obj)
 |(obj attribute)
 |(obj attribute value . attributes)
 |(obj bindResult attribute value . attributes)
 |(type function)
 |
 |(syntax attributes (attribute value . attributes))
 |(syntax attribute (or Symbol Keyword String))
 |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |
 |The Obj are functions that encapsulates mutable values.
 |
 |Calling the obj:
 |
 |- without arguments returns the obj.
 |- with an attribute return the value of the attribute in the obj.
 |- with couples of attribute and value defines or update the attribute with the value in the obj.
 |
 |- without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
 |- with <b>bindResult</b> #inert return #inert
 |- with <b>bindResult</b> :rhs return the right side of the last binding
 |- with <b>bindResult</b> :prv return the previous value of the last binding
 |- with <b>bindResult</b> :cnt return the obj
 |#

(def\ (newObj . bindings)
  #|($nm . bindings)
   |(type function)
   |
   |(syntax bindings (attribute value . bindings))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Return a new obj with the given <b>binding</b>.
   |#
  (apply new (cons Obj bindings)))

(defMacro (defObj name . bindings)
  #|($nm name . bindings)
   |(type macro)
   |
   |(syntax bindings (attribute value . bindings))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Defines into the current environment the named box <b>name</b> with the given <b>bindings</b>.
   |#
  (list 'def name (cons 'newObj bindings)) )


#|! Env & Obj
 |#

(def bound?
  #|($nm attribute object)
   |(type function)
   |
   |(syntax attribute (or Symbol Keyword String))
   |(syntax object (or Env Obj))
   |
   |(derivation (@isBound object attribute))
   |
   |Return #true if the <b>attribute</b> is bound in the <b>environment</b> or <b>obj</b>, #false otherwise.
   |#
  %bound? )

(def value
  #|($nm attribute object)
   |(type function)
   |
   |(syntax attribute (or Symbol Keyword String))
   |(syntax object (or Env Obj))
   |
   |(derivation (@value object attribute))
   |
   |Return the value of the <b>attribute</b> is bound in the <b>environment</b> or <b>obj</b>, #null otherwise.
   |#
  %value )

(def get
  #|($nm attribute object)
   |(type function)
   |
   |(syntax attribute (or Symbol Keyword String))
   |(syntax object (or Env Obj))
   |
   |(derivation (@get object attribute))
   |
   |Return the value of the <b>attribute</b> is bound in the <b>environment</b> or <b>obj</b>, signals an error otherwise.
   |#
  %get )

(def\ (slotBound? object attribute)
  #|($nm object attribute)
   |(type function)
   |
   |(syntax object (or Env Obj))
   |(syntax attribute (or Symbol Keyword String))
   |
   |(derivation (@isBound object attribute))
   |
   |Return #true if the <b>attribute</b> is bound in the <b>environment</b> or <b>obj</b>, #false otherwise.
   |#
  (%slotBound? object attribute) )

(def\ (getSlot object attribute)
  #|($nm object attribute)
   |(type function)
   |
   |(syntax object (or Env Obj))
   |(syntax attribute (or Symbol Keyword String))
   |
   |(derivation (object attribute))
   |
   |Return the value of <b>attribute</b> is bound in the <b>environment</b> or <b>obj</b>, signals an error otherwise.
   |#
  (%getSlot object attribute) )

(def\ (setSlot object attribute value)
  #|($nm object attribute value)
   |(type function)
   |
   |(syntax object (or Env Obj))
   |(syntax attribute (or Symbol Keyword String))
   |
   |(derivation (object attribute value))
   |
   |Update or define with <b>value</b> the <b>attribute</b> in the <b>environment</b> or <b>obj</b>.
   |#
  (%setSlot object attribute value) )


#|! Box
 |(box)
 |(box value)
 |(box bindResult value)
 |(type function)
 |
 |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |
 |The Box are functions that encapsulates a mutable value.
 |
 |Calling the box:
 |
 |- without arguments returns the value in the box.
 |- with an argument update the value in the box.
 |
 |- without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
 |- with <b>bindResult</b> #inert return #inert
 |- with <b>bindResult</b> :rhs return the update value
 |- with <b>bindResult</b> :prv return the value previous the update
 |- with <b>bindResult</b> :cnt return the box
 |#

(def\ (newBox . value)
  #|($nm . value)
   |(type function)
   |
   |Return a new box with the optional <b>value</b>.
   |The Box are functions that encapsulates a mutable value.
   |Without <b>value</b> use as <b>value</b> `(boxDft)'.
   |#
  (apply new (cons Box value)))

(defMacro (defBox name . value)
  #|($nm name . value)
   |(type macro)
   |
   |Defines into the current environment the named box <b>name</b> with the optional <b>value</b>.
   |Without <b>value</b> use as <b>value</b> `(boxDft)'.
   |#
  (list 'def name (cons 'newBox value)) )


#|! Obj & Box
 |# 

(def new
  #|($nm boxClass)
   |($nm boxClass value)
   |($nm objClass . bindings)
   |($nm objClass string . bindings)
   |($nm objClass throwableObject . bindings)
   |($nm objClass string throwableObject . bindings)
   |(syntax bindings (attribute value . bindings))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Return a new instance of <b>class</b> with the given <b>value</b> or <b>bindings</b>.
   |#
  %new )

(defMacro (defNew name class . args)
  #|($nm name boxClass)
   |($nm name boxClass value)
   |($nm name objClass . bindings)
   |($nm name objClass string . bindings)
   |($nm name objClass throwableObject . bindings)
   |($nm name objClass string throwableObject . bindings)
   |(type macro)
   |
   |(syntax bindings (attribute value . bindings))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Defines into the current environment a named instance <b>name</b> of the given <b>class</b> and <b>value</b> or <b>bindings</b>.
   |#
  (list 'def name (list* 'new class args)) )


#|! Cons
 |#

(def\ (caar x)
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ (((caar . #_) . #_)) caar)) cons)
   |(derivation (car (car cons)))
   |
   |Return the `car' of the `car' of the <b>cons</b>.
   |#
  (car (car x)) )

(def\ (cadr! (#_ cadr))
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((#_ cadr)) cadr)) cons)
   |
   |Return the `car' of the `cdr' of the <b>cons</b> if 'cddr' is #null, signals an error otherwise.
   |#
  cadr)

(def\ (cdar x)
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ (((#_ . cdar) . #_)) cdar)) cons)
   |(derivation (cdr (car cons)))
   |
   |Return the `cdr' of the `car' of the <b>cons</b>.
   |#
  (cdr (car x)) )

(def cddr
  #|($nm cons)
   |(type function)
   |
   |(derivation ((\ ((#_ #_ . cddr)) cddr)) cons)
   |(derivation (cdr (cdr cons)))
   |(derivation (@cdr cons 1))
   |
   |Return the `cdr' of the `cdr' of the <b>cons</b>.
   |#
  %cddr)

(def\ (cons! car)
  #|($nm car)
   |(type function)
   |
   |(derivation ((\ (car) (cons car)) car))
   |
   |Return a cons with the given <b>car</b> and #null.
   |#
  (cons car))

(def nth
  #|($nm n list)
   |(type function)
   |
   |Return element number N of <b>list</b>, where the `car' is element zero.
   |#
  %nth)

(def nthCdr
  #|($nm n list)
   |(type function)
   |
   |Returns the tail of <b>list</b> that would be obtained by calling `cdr' N times in succession.
   |#
  %nthCdr)

(def setCar!
  #|($nm value)
   |($nm bindResult value)
   |(type function)
   |
   |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
   |
   |Set the `car' of the Cons.
   |
   |without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
   |with <b>bindResult</b> #inert return #inert
   |with <b>bindResult</b> :rhs return value
   |with <b>bindResult</b> :prv return the previous value of the `car'
   |with <b>bindResult</b> :cnt return the Cons or List
   |#
  %setCar!)

(def setCdr!
  #|($nm value)
   |($nm bindResult value)
   |(type function)
   |
   |(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
   |
   |Set the `cdr' of the Cons.
   |
   |without <b>bindResult</b> or with <b>bindResult</b> #ignore use as bindResult `(bndRes)'
   |with <b>bindResult</b> #inert return #inert
   |with <b>bindResult</b> :rhs return value
   |with <b>bindResult</b> :prv return the previous value of the `cdr'
   |with <b>bindResult</b> :cnt return the Cons
   |#
  %setCdr!)


#|! List
 |(type Class)
 |(extends Cons)
 |
 |The last `cdr' of lists il alwais #null.
 |#

(def append
  #|($nm list1 list2)
   |(type function)
   |
   |Return the append of the <b>list2</b> to the <b>list1</b>.
   |The first one must be proper and is copied.
   |The second one is not copied (and doesn't even have to be a list).
   |It becomes the `cdr' of the final cons of the first list,
   |or is returned directly if the first list is empty.
   |#
  %append)

(def len
  #|($nm list)
   |(type function)
   |
   |Return the number of elements in the <b>list</b>.
   |#
  %len)

(def list?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object List))
   |
   |Return #true if the <b>object</b> is a list, #false otherwise.
   |#
  %list?)

(def reverse
  #|($nm list)
   |(type function)
   |
   |Reverse the <b>list</b>.
   |#
  %reverse)


#|! Symbol & Keyword
 |(type Class)
 |(extends Intern)
 |#

(def intern
  #|($nm string)
   |(type function)
   |
   |Return the unique symbol or keyword with <b>string</b> as name.
   |Keyword if <b>string</b> start with ':', symbol otherwise
   |#
  %intern)

(def name
  #|($nm intern)
   |(type function)
   |
   |Return the name of the <b>intern</b> as a string.
   |#
  %name)

(def keyword
  #|($nm string)
   |(type function)
   |
   |Return the unique keyword with <b>string</b> as name.
   |#
  %keyword)

(def keyword?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object Keyword))
   |
   |Return #true if the <b>object</b> is a keyword, #false otherwise.
   |#
  %keyword?)

(def\ (keywordName (#: Keyword keyword))
  #|($nm keyword)
   |(type function)
   |
   |Return the name of the <b>keyword</b> as a string.
   |#
  (name keyword) )

(def symbol
  #|($nm string)
   |(type function)
   |
   |Return the unique symbol with <b>string</b> as name.
   |#
  %symbol)

(def symbol?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object Symbol))
   |
   |Return #true if the <b>object</b> is a symbol, #false otherwise.
   |#
  %symbol?)

(def\ (symbolName (#: Symbol symbol))
  #|($nm symbol)
   |(type function)
   |
   |Return the name of the <b>symbol</b> as a string.
   |#
  (name symbol) )


#|! Equals
 |#

(def ==
  #|($nm a b)
   |(type function)
   |
   |Return #true if <b>a</b> istanceof Number && <b>a</b> equals <b>b</b> || <b>a</b> == <b>b</b>, #false otherwise.
   |#
  %== )

(def !=
  #|($nm a b)
   |(type function)
   |
   |Return #false if <b>a</b> istanceof Number && <b>a</b> equals <b>b</b> || <b>a</b> == <b>b</b>, #true otherwise.
   |#
  %!= )

(def eq?
  #|($nm a b)
   |(type function)
   |
   |Return #true if <b>a</b> is equals <b>b</b>, #false otherwise.
   |#
  %eq? )

(def\ (ignore? o) (== o #_))
(def\ (inert? o) (== o #inert))
(def\ (sharpColon? o) (== o #:))


#|! Boolean
 |#

(def !
  #|($nm boolean)
   |(type function)
   |
   |(derivation (if boolean #f #t))
   |
   |Invert the <b>boolean</b>.
   |#
  %! )

(def not
  #|(aliasof !)
   |#
  ! )

(def !!
  #|($nm value)
   |(type function)
   |
   |(derivation (if value #t #f))
   |
   |Convert a <b>value</b> to boolean when typeT != 0.
   |#
  %!! )


#|! Number
 |#

(def number?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object Number))
   |
   |Return #true if the <b>object</b> is a number, #false otherwise.
   |#
  %number? )

(def +
  #|($nm a b)
   |(type function)
   |
   |Java binary + operator, sums the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |#
  %+ )

(def *
  #|($nm a b)
   |(type function)
   |
   |Java binary * operator, multiplies the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |#
  %* )

(def -
  #|($nm a b)
   |(type function)
   |
   |Java binary - operator, subtracts the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |#
  %- )

(def /
  #|($nm a b)
   |(type function)
   |
   |Java binary / operator, divides the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |#
  %/ )

(def %
  #|($nm a b)
   |(type function)
   |
   |Java binary % operator, rest of the division of the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |#
  %% )

(def\ (1+ n) (+ n 1))
(def\ (-1+ n) (- n 1))
(def\ (0? n) (== n 0))
(def\ (1? n) (== n 1))
(def\ (-1? n) (== n -1))
(def\ (even? n) (== (% n 2) 0))
(def\ (odd? n)  (== (% n 2) 1))


#|! String
 |#

(def string?
  #|($nm object)
   |(type function)
   |
   |(derivation (type? object String))
   |
   |Return #true if the <b>object</b> is a string, #false otherwise.
   |#
  %string? )

(def $
  #|($nm a b)
   |(type function)
   |
   |Java binary + operator, join the values after the conversion of the operands to string.
   |#
  %$ )


#|! Comparators
 |#

(def <
  #|($nm a b)
   |(type function)
   |
   |Java binary < operator.
   |#
  %< )

(def >
  #|($nm a b)
   |(type function)
   |
   |Java binary > operator.
   |#
  %> )

(def <=
  #|($nm a b)
   |(type function)
   |
   |Java binary <= operator.
   |#
  %<= )

(def >=
  #|($nm a b)
   |(type function)
   |
   |Java binary >= operator.
   |#
  %>= )

(def\ (min a b) (if (< a b) a b))
(def\ (max a b) (if (> a b) a b))


#|! Bitwise and Shift Operators
 |#

(def ~
  #|($nm a b)
   |(type function)
   |
   |Java binary ~ operator.
   |#
  %~ )

(def &
  #|($nm a b)
   |(type function)
   |
   |Java binary & operator.
   |#
  %& )

(def |
  #|($nm a b)
   |(type function)
   |
   |Java binary | operator.
   |#
  %| )

(def ^
  #|($nm a b)
   |(type function)
   |
   |Java binary ^ operator.
   |#
  %^ )

(def <<
  #|($nm a b)
   |(type function)
   |
   |Java binary << operator.
   |#
  %<< )

(def >>
  #|($nm a b)
   |(type function)
   |
   |Java binary >> operator.
   |#
  %>> )

(def >>>
  #|($nm a b)
   |(type function)
   |
   |Java binary >>> operator.
   |#
  %>>> )


#|! First-Order Controls
 |#

(def loop
  #|($nm . forms)
   |(type fexpr)
   |
   |Evaluate <b>forms</b> as an implicit `begin' in an infinite loop.
   |#
  %loop )

(def atEnd
  #|($nm cleanupForm . protectedForms)
   |(type fexpr)
   |
   |Evaluate the <b>protectedForms</b> as an implicit `begin'.
   |Regardless of whether the protected form returns normally or via a nonlocal exit or panic,
   |the <b>cleanupForm</b> are evaluated after the protected forms.
   |#
  %atEnd )

(defMacro (finally protected . cleanUps)
  #|($nm protectedForm . cleanupForms)
   |(type macro)
   |
   |(derivation (atEnd (begin . cleanupForms) protectedForm))
   |
   |Evaluate the <b>protectedForm</b> and return its result.
   |Regardless of whether the protected form returns normally or via a nonlocal exit or panic,
   |the <b>cleanupForms</b> are evaluated as an implicit `begin' after the <b>protectedForm</b>.
   |#
  (list 'atEnd (cons 'begin cleanUps) protected) )

(def throwTag
  #|($nm tag . forms)
   |(type fexpr)
   |
   |Abort to a nesting catch <b>tag</b> established by `catch', evaluate <b>forms</b> as an implicit `begin'.
   |#
  %throwTag )

(defMacro (throw . forms)
  #|($nm . forms)
   |(type fexpr)
   |
   |(derivation (throwTag #ignore forms))
   |
   |Abort to a nesting generic catch, evaluate <b>forms</b> as an implicit `begin'.
   |#
  (list* 'throwTag #_ forms) )

(def catchTagWth
  #|($nm tag handler . forms)
   |(type fexpr)
   |
   |Establishes a catch <b>tag</b> with an <b>handler</b> (one arg function or any value) and evaluate <b>forms</b> as an implicit `begin'.
   |The <b>forms</b> may use `throw' to nonlocally exit from the <b>tag</b>. if <b>forms</b> throw and <b>handler</b> is a value, value will be the result of `catchTagWth'
   |otherwise the value of the `throw' is passed to the <b>handler</b> and the value returned by the <b>handler</b> will be the result of the `catchTagWth'
   |#
  %catchTagWth )

(defMacro (catchWth hdl . forms)
  #|($nm handler . forms)
   |(type macro)
   |
   |(derivation (catchTagWth #ignore handler . forms))
   |
   |Establishes an generic catch all with <b>handler</b> (one arg function or any value) and evaluate <b>forms</b> as an implicit `begin'.
   |The <b>forms</b> may use `throw' to nonlocally exit. if <b>forms</b> throw and <b>handler</b> is a value, value will be the result of `catchWth'
   |otherwise the value of the `throw' is passed to the <b>handler</b> and the value returned by the <b>handler</b> will be the result of the `catchWth'
   |#
  (list* 'catchTagWth #_ hdl forms) )

(defMacro (catchTag tag . forms)
  #|($nm tag . forms)
   |(type macro)
   |
   |(derivation (catchTagWth tag #ignore . forms))
   |
   |Establishes a catch <b>tag</b> without handler and evaluate <b>forms</b> as an implicit `begin'.
   |The <b>forms</b> may use `throw' to nonlocally exit from the tag. if <b>forms</b> throw the value returned by the `throw' will be the result of the `catchTag'
   |#
  (list* 'catchTagWth tag #_ forms) )

(defMacro (catch . forms)
  #|($nm . forms)
   |(type macro)
   |
   |(derivation (catchTagWth #ignore #ignore . forms))
   |
   |Establishes an generic catch all without handler and evaluate <b>forms</b> as an implicit `begin'.
   |The <b>forms</b> may use `throw' to nonlocally exit. if <b>forms</b> throw the value returned by the `throw' will be the result of the `catch'
   |#
  (list* 'catchTagWth #_ #_ forms) )

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catchWth (\ (x) (+ x 1)) (throw 1) ) 2)
(assert (catchTag 'a (throwTag 'a)) #inert)
(assert (catchTag 'a (throwTag 'a 1)) 1)
(assert (catchTagWth 'a (\ (x) (+ x 1)) (throwTag 'a 1) ) 2)


#|! Delimited-Control Operators
 |These operators follow the <b>api</b> put forth in the delimcc library at <b>url</b> `http://okmij.org/ftp/continuations/implementations.html'.
 |#

(def pushPrompt
  #|($nm prompt . forms)
   |(type fexpr)
   |
   |Push the <b>prompt</b> and evaluate <b>forms</b> as an implicit `begin' inside the <b>prompt</b>.
   |This delimits the continuation.
   |#
  %pushPrompt )

(def takeSubcont
  #|($nm prompt symbol . forms)
   |(type fexpr)
   |
   |Abort outwards to the <b>prompt</b>.
   |When the prompt is reached, evaluate <b>forms</b> as an implicit `begin' with <b>symbol</b> bound to the captured continuation (which does not include the prompt).
   |#
  %takeSubcont )

(def pushSubcont
  #|($nm continuation . forms)
   |(type fexpr)
   |
   |Evaluated <b>forms</b> as an implicit `begin' inside the <b>continuation</b>.
   |#
  %pushSubcont )

(assert (pushPrompt 'p (* 2 (+ 1 (takeSubcont 'p k (pushSubcont k (pushSubcont k 3)))))) 18)

(def pushDelimSubcont
  #|($nm prompt continuation . forms)
   |(type fexpr)
   |
   |Push the <b>prompt</b> and compose the previously captured <b>continuation</b> inside it
   |before evaluated <b>forms</b> as an implicit `begin' inside the new continuation.
   |#
  %pushDelimSubcont )

(def pushSubcontBarrier
  #|($nm . forms)
   |(type fexpr)
   |
   |Evaluate <b>forms</b> as an implicit `begin' after push a continuation barrier that prevent <b>forms</b> from capturing any continuations to the outside 
   |#
  %pushSubcontBarrier )

(def rootPrompt
  #|The prompt used for delimiting all.
   |#
  %rootPrompt )


#|! Errors
 |#

(def error
  #|($nm string . attributes)
   |($nm throwable . attributes)
   |($nm string throwable . attributes)
   |(type function)
   |
   |(syntax attributes (or () (attribute value . attributes)))
   |(syntax attribute (or Symbol Keyword String))
   |
   |Simple function to define and signal an error
   |In LispX it will be replaced by a function adapted to the LispX Condition System 
   |#
  %error )

(def\ makeTypeError (datum expected)
  #|($nm datum expected)
   |(type function)
   |
   |Return a new type error with <b>datum</b> and <b>expected</b>.
   |#
  (new Error "not a {expected}: {datum}" :type 'type :datum datum :expected expected) )

(def\ typeError (datum expected)
  #|($nm datum expected)
   |(type function)
   |
   |Signal a type error with <b>datum</b> and <b>expected</b>.
   |#
  (error (makeTypeError datum expected)) )

(def test
  #|($nm name expression value)
   |($nm name expression)
   |($nm name expression class attribute value . attributes)
   |(type fexpr)
   |
   |(syntax attributes (or () (attribute value . attributes)))
   |(syntax attribute (or Symbol Keyword String .Field @Method))
   |
   |Signals an error if:
   |- <b>expression</b> does not equal <b>value</b>
   |- <b>value</b> is not present and the <b>expression</b> does not throws
   |- <b>expression</b> throws or returns an object and the object is not of the same <b>class</b> with the <b>attributes</b> of the given <b>value</b>.
   |#
  %test )

(def\ (printFrames k) (unless (null? k) (printFrames (.nxt k)) (log "v" k) #inert))

(def\ (printStacktrace throwable)
  #|($nm throwable)
   |(type function)
   |
   |Throw the <b>throwable</b> after print the message of <b>throwable</b> and the stacktrace if `(prStk)'.
   |#
  (when (prStk)
    (log "-" (@getMessage throwable)) 
    (takeSubcont rootPrompt k
      (printFrames k)
      (pushDelimSubcont rootPrompt k) ))
  (throw throwable) )


#|! Classes
 |#

(def className
  #|($nm class)
   |(type function)
   |
   |(derivation (symbol (@getSimpleName class))))
   |
   |Return the name symbol of the <b>class</b>.
   |#
  %className )

(def typeName
  #|(aliasof className)
   |#
  className)

(def classOf
  #|($nm object)
   |(type function)
   |
   |(derivation (if (null? object) #null (@getClass object)))
   |
   |Return the class of the <b>object</b>.
   |#
  %classOf )

(def typeOf
  #|(aliasof classOf)
   |#
  classOf)

(def instanceOf?
  #|($nm object class)
   |(type function)
   |
   |Return #true if <b>object</b> is instaceof <b>class</b>.
   |#
  %instanceOf? )

(def subClass?
  #|($nm class superclass)
   |(type function)
   |
   |(derivation (@isAssignableFrom superClass class)))
   |
   |Return #true if the <b>class</b> is a subclass of the <b>superclass</b>, #false otherwise.
   |A class is considered a subclass of itself.
   |#
  %subClass? )

#|
(def type?
  #|($nm object class)
   |(type function)
   |
   |(derivation (if (null? class) (null? object) (null? object) #f (subClass? (classOf object) class)))
   |(derivation (if (null? class) (null? object) (instanceOf object class)))
   |
   |Return #true if the <b>object</b> is an instance of the <b>class</b>, #false otherwise.
   |#
  %type? )
|#
(def\ (type? object class . classes)
  #|($nm object class . classes)
   |(type function)
   |
   |(derivation (if (null? class) (null? object) (null? object) #f (subClass? (classOf object) class)))
   |(derivation (if (null? class) (null? object) (instanceOf object class)))
   |
   |Return #true if the <b>object</b> is an instance of one the <b>(cons class classes)</b>, #false otherwise.
   |#
  ((rec\ (loop classes) (if (null? classes) #f (%type? object (car classes)) #t (loop (cdr classes)))) (cons class classes)) )


#|! Basic Functions and Macros
 |#

(def\ (idf x)
  #|($nm object)
   |(type function)
   |
   |Identity function.
   |#
  x )

(defMacro _ forms
  #|($nm . forms)
   |(type macro)
   |
   |(derivation (\ (_) . forms))
   |
   |"Implicit" Argument Lambda.
   |#
  (list* '\ '(_) forms) )

(def\ (peval f v)
  #|($nm f v)
   |(type function)
   |
   |(derivation (\ args (apply f (cons v args))))
   |
   |Single arg partial evaluation.
   |#
  (\ args (apply f (cons v args))) )

(def\ (peval* f . v*)
  #|($nm f v*)
   |(type function)
   |
   |(derivation (\ args (apply f (append v* args))))
   |
   |Multiple args partial evaluation.
   |#
  (\ args (apply f (append v* args))) )

(def\ (compose f g)
  #|($nm f g)
   |(type function)
   |
   |(derivation (\ args (f (apply g args))))
   |
   |Return a function equivalent to the composition of the two functions.
   |#
  (\ args (f (apply g args))) )

(def\ (compose* . f*)
  #|($nm . f*)
   |(type function)
   |
   |(derivation (\ args ((rec\ (loop (f . f*)) (if (null? f*) (apply f args) (f (loop f*)))) f*)))
   |
   |Return a function equivalent to the composition of the functions.
   |#
  (\ args ((rec\ (loop (f . f*)) (if (null? f*) (apply f args) (f (loop f*)))) f*)) )

;(defMacro compose* f*
;  (list '\ 'args ((rec\ (loop (f . f*)) (if (null? f*) (list 'apply f 'args) (list f (loop f*)))) f*)) )

(defMacro (rec name value)
  #|($nm name value)
   |(type macro)
   |
   |Return <b>value</b>, after lexically bind <b>name</b> with #inert,
   |and update <b>name</b> with <b>value</b> so that it can reference itself.
   |#
  (list (list '\ (list name) (list 'def name :rhs value)) #inert) )

(def label
  #|(aliasof rec)
   |#
  rec )

(assert ((rec f (\ (l) (if (null? l) "" ($ (car l) (f (cdr l)))))) '(1 2 3)) "123")
(assert ((rec f (\ l (if (null? l) "" ($ (car l) (apply f (cdr l)))))) 1 2 3) "123")

(defMacro (rec\ lhs . rhs)
  #|($nm name parameterTree . forms)
   |($nm (name . parameterTree) . forms)
   |(type macro)
   |
   |Return the function with given <b>parameterTree</b> and <b>forms</b> as body, after lexically bind <b>name</b> with #inert,
   |and updated <b>name</b> with the function so that it can reference to itself.
   |#
  (if (cons? lhs)
    (list 'rec (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'rec lhs (cons '\ rhs)) ))

(def label\
  #|(aliasof rec\)
   |#
  rec\ )

(assert ((rec\ (f l)   (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\  f (l)  (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\ (f . l) (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")
(assert ((rec\  f l    (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")

(def\ (map f lst . lst*)
  #|($nm function list)
   |($nm function list1 list2 ... listN)
   |(type function)
   |
   |Return a new list by applying the <b>function</b> to each element of the <b>list</b> or the <b>lists</b> which must be of the same length
   |#
  (if (null? lst*)
    ((rec\ (map lst) (if (null? lst) #null (cons (f (car lst)) (map (cdr lst))) )) lst)
    ((rec\ (map* lst*) (if (null? (car lst*)) #null (cons (apply f (map car lst*)) (map* (map cdr lst*))) )) (cons lst lst*)) ))

(assert (map car  '((a 1)(b 2))) '(a b))
(assert (map cdr  '((a 1)(b 2))) '((1) (2)))
(assert (map cadr '((a 1)(b 2))) '(1 2))
(assert (map (\ (a b) (+ a b)) '(1 2) '(3 4)) '(4 6))

  #|!
   |Some importanti equivalences with eval apply list and map:
   |
   |apply: (eval (cons opv args) env) <=> (apply opv args env)
   |apply: (eval (cons apv args) env) <=> (apply apv (evlis env args) env)
   |evlis: (eval (cons 'list args) env) <=> (map (\ (arg) (eval arg env)) args)
   |#

(defMacro (def*\ lhs* . rhs*)
  #|($nm definiendTrees . bodies)
   |($nm ((name parameterTree) . definiendTrees) . (forms . bodies))
   |($nm (name . definiendTrees) . ((parameterTree . forms) . bodies))
   |(type macro)
   |
   |(syntax definiendTrees ((name parameterTree) . definiendTrees)
   |(syntax bodies (forms . bodies)
   |(syntax definiendTrees (name . definiendTrees)
   |(syntax bodies ((parameterTree . forms) . bodies)
   |
   |(example (def*\ ((f a) g) ((1+ a)) ((a) (1+ a))) )
   |
   |Defines into the current environment the named functions <b>names</b> with given <b>parameterTrees</b> and <b>forms</b> as body. 
   |#
  (list* 'def*
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map (\ (lhs rhs) (cons '\ (if (cons? lhs) (cons (cdr lhs) rhs) rhs))) lhs* rhs*) ))

(assert (expand (def*\ ((a n) (b n)) ((+ n 1)) ((+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )
(assert (expand (def*\ (a b) ((n) (+ n 1)) ((n) (+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )


#|! Lexical Bindings
 |#

(def\ (->begin binding) (cons 'begin (cdr binding)))
(def\ (->name+#inert (lhs . #_)) (list (if (cons? lhs) (car lhs) lhs) #inert))
(def\ (->name+lambda (lhs . rhs)) (if (cons? lhs) (list (car lhs) (list* '\ (cdr lhs) rhs)) (list lhs (cons '\ rhs)) ))


(defMacro (wth1 dt value . forms)
  #|($nm binding . forms)
   |(type macro)
   |
   |(syntax binding . (definiendTree initForm))
   |
   |Establishes the <b>binding</b>, before the evaluation of <b>forms</b> as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  (list (list* '\ (cons dt) forms) value))

(defMacro (wth* bindings . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings (definiendTree intiForm . bindings))
   |
   |Establishes <b>bindings</b> serially, so that every binding can refer to previous one,
   |before the evaluation of <b>forms</b> as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  ( (rec\ (loop bindings)
      (if (null? bindings)
        (cons (list* '\ () forms))
        (wth1 (dt value . bindings) bindings
          (list* 'wth1 dt value (cons (loop bindings))) )))
    bindings ))

(assert (expand (wth* (a 1 b (1+ a)) 1 2 (+ a b))) '(wth1 a 1 (wth1 b (1+ a) ((\ () 1 2 (+ a b))))))
(assert (wth* (a 1 b (1+ a)) 1 2 (+ a b)) 3)

(defMacro (wth b* . forms)
  #|($nm bindings . forms)
   |($nm name bindings . forms)
   |(type macro)
   |
   |(syntax bindings (definiendTree initForm . bindings))
   |
   |Establishes <b>bindings</b> parallelly, so that no binding can refer to itself or the other ones,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  (def dt* ((rec\ (loop b*) (if (null? b*) #null (wth1 (dt #_ . b*) b* (cons dt (loop b*))))) b*))
  (def vl* ((rec\ (loop b*) (if (null? b*) #null (wth1 (#_ vl . b*) b* (cons vl (loop b*))))) b*))
  (cons (list* '\ dt* forms) vl*) )


(defMacro (let1Loop lhs . rhs)
  #|($nm name binding . forms)
   |($nm (name . binding) . forms)
   |(type macro)
   |
   |(syntax binding (parameterTree . initForms))
   |
   |Labelled recursive loop.
   |Lexically bind <b>name</b> with the function with the specified <b>parameterTree</b> and <b>forms</b> as body,
   |so that it can recursively refer to itself.
   |The function is immediately applied to the list containing the value of the <b>initForms</b> evaluate as an implicit `begin',
   |after evaluate <b>forms</b> as an implicit `begin'.
   |#
  (if (cons? lhs)
    (def* ((name . binding) forms) lhs rhs)
    (def* (name (binding . forms)) lhs rhs) )
  (list
    (list* 'rec\ name (cons (car binding)) forms)
    (->begin binding) ))

(assert (let1Loop add1 (a '(1 2)) (if (null? a) () (cons (1+ (car a)) (add1 (cdr a))))) '(2 3))
(assert (let1Loop (add1 a '(1 2)) (if (null? a) () (cons (1+ (car a)) (add1 (cdr a))))) '(2 3))


(defMacro (let1 lhs . rhs)
  #|($nm binding . forms)
   |($nm name binding . forms)
   |(type macro)
   |
   |(syntax binding (definiendTree . initForms))
   |
   |Establishes the <b>binding</b>, after evaluate <b>forms</b> as an implicit `begin'.
   |The <b>initForms</b> is evaluate as an implicit `begin'.
   |If the first argument is a symbol is like 'let1Loop'.
   |#
  (if (symbol? lhs)
    (list* 'let1Loop lhs rhs)
    (list (list* '\ (list (car lhs)) rhs)
      (->begin lhs) )))

(assert (let1 (a 1) a) 1)
(assert (let1 (a 1 2) a) 2)
(assert (let1 f (a 2) (if (0? a) 'end (f (- a 1)))) 'end)

(defMacro (let1\ binding . forms)
  #|($nm functionBinding . forms)
   |(type macro)
   |
   |(syntax functionBinding (name parameterTree . bodyForms))
   |(syntax functionBinding ((name . parameterTree) . bodyForms))
   |
   |Establishes the <b>functionBindings</b>, after evaluate of <b>forms</b> as an implicit `begin'.
   |#
  (list* 'let1 (->name+lambda binding) forms) )

(assert (let1\ (f (a) a) (f 5)) 5)
(assert (let1\ ((f a) a) (f 5)) 5)

(defMacro (let1rec binding . forms)
  #|($nm binding . forms)
   |(type macro)
   |
   |(syntax binding (name . intForms))
   |
   |Establishes the <b>binding</b> recursively so that the binding can recursively refer to itself,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |The binding is initializated to #inert before evaluating <b>initForms</b> as an implicit `begin'.
   |#
  (def name (car binding))
  (list* 'let1 (list name #inert)
    (list 'def (car name (->begin binding))
      forms )))

(defMacro (let1rec\ binding . forms)
  #|($nm functionBinding . forms)
   |(type macro)
   |
   |(syntax functionBinding (name parameterTree . bodyForms))
   |(syntax functionBinding ((name . parameterTree) . bodyForms))
   |
   |Establishes the <b>functionBinding</b> recursively, so that the function can refer to itself,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |The binding is initializated to #inert before the bind of the function.
   |#
  (list* 'let1 (->name+#inert binding)
    (cons 'def\ binding)
    forms ))


(defMacro (let* bindings . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((definiendTree . intiForms) . bindings))
   |
   |Establishes <b>bindings</b> serially, so that every binding can refer to previous one,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |The <b>initForms</b> are evaluate as an implicit `begin'.
   |#
  ( (rec\ (loop bindings)
      (if (null? bindings)
        (cons (list* '\ () forms))
        (list* 'let1 (car bindings) (cons (loop (cdr bindings)))) ))
    bindings ))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)


(defMacro (letLoop lhs . rhs)
  #|($nm name bindings . forms)
   |($nm (name . bindings) . forms)
   |(type macro)
   |
   |(syntax bindings ((parameterTree . initForms) . bindings))
   |
   |Labelled recursive loop.
   |Lexically bind <b>name</b> with the function with the specified multiple <b>parameterTrees</b> and <b>forms</b> as body,
   |so that it can recursively refer to itself.
   |The function is immediately applied to the list containing the values of <b>initForm</b> evaluate as an implicit `begin',
   |after evaluates <b>forms</b> as an implicit `begin'.
   |#
  (if (cons? lhs)
    (def* ((name . bindings) forms) lhs rhs)
    (def* (name (bindings . forms)) lhs rhs) )
  (cons
    (list* 'rec\ name (map car bindings) forms)
    (map ->begin bindings) ))

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))
(assert (letLoop (sum (a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))


(defMacro (let lhs . rhs)
  #|($nm bindings . forms)
   |($nm name bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((definiendTree . initForms) . bindings))
   |
   |Establishes the <b>bindings</b> parallelly, so that no binding can refer to itself or the other ones,
   |after evaluates <b>forms</b> as an implicit `begin'.
   |The <b>initForms</b> are evaluate as an implicit `begin'.
   |If the first argument is a symbol is like 'letLoop'.
   |#
  (if (symbol? lhs)
    (list* 'letLoop lhs rhs)
    (cons (list* '\ (map car lhs) rhs)
      (map ->begin lhs) )))

(assert (let ((a 1)) a) 1)

(defMacro (let\ bindings . forms)
  #|($nm functionBindings . forms)
   |(type macro)
   |
   |(syntax functionBindings (functionBinding . functionBindings))
   |(syntax functionBinding (name parameterTree . bodyForms))
   |(syntax functionBinding ((name . parameterTree) . bodyForms))
   |
   |Establishes the <b>functionBindings</b> parallelly, so that no function can refer to itself or the other ones,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |#
  (list* 'let (map ->name+lambda bindings) forms) )

(assert (expand (let\ ((f1 (a) a) ((f2 b) b)) 1)) '(let ((f1 (\ (a) a)) (f2 (\ (b) b))) 1))

(defMacro (letrec bindings . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((name . intForms) . bindings))
   |
   |Establishes the <b>bindings</b> recursively, so that the bindings can refer to itself and the other ones,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |The bindings are initializated to #inert before evaluating the <b>initForms</b> as an implicit `begin'.
   |#
  (def names (map car bindings))
  (list* 'let (map (\ (name) (list name #inert)) names)
    (list* 'def* names (map ->begin bindings))
    forms ))

(assert (letrec ( (even? (\ (n) (if (0? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (0? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . forms)
  #|($nm functionBindings . forms)
   |(type macro)
   |
   |(syntax functionBindings (functionBinding . functionBindings))
   |(syntax functionBinding ((name parameterTree . bodyForms) . functionBindings))
   |(syntax functionBinding (((name . parameterTree) . bodyForms) . functionBindings))
   |
   |Establishes the <b>functionBindings</b> recursively, so that the functions can refer to itself and the other ones,
   |after evaluate <b>forms</b> as an implicit `begin'.
   |The bindings are initializated to #inert before the bind of the functions.
   |#
  (list* 'let (map ->name+#inert bindings)
    (list* 'def*\ (map car bindings) (map cdr bindings))
    forms ))

(def labels
  #|(aliasof letrec\)
   |#
  letrec\ )

(assert (labels ( ((even? n) (if (0? n) #t (odd? (- n 1)))) (odd? (n) (if (0? n) #f (even? (- n 1)))) ) (even? 88) ) #t)

#|! Simple Controls
 |#

(def* (then else)
  #|(aliasof begin)
   |#
  begin begin )

(defVau prog1 (form . forms) env
  #|($nm form . forms)
   |(type fexpr)
   |
   |Evaluate <b>form</b> and return the result after evaluate <b>forms</b> as an implicit `begin'.
   |#
  (let1 (result (eval form env))
    (apply begin forms env)
    result ))

(defMacro (if! test thenForm . forms)
  #|($nm test thenForm . forms)
   |(type fexpr)
   |
   |(syntax forms (or () (elseForm)))
   |
   |(derivation (list 'if (list '! test) thenForm (if (null? forms) #inert (car! forms))))
   |
   |Evaluate the <b>test</b> which must yield a boolean.
   |Then evaluate either the <b>thenForm</b> or <b>elseForm</b> expression depending on whether the <b>test</b> yielded #false or #true.
   |#
  ; tre alternative
  ;(list* 'if (list '! test) thenForm forms) ;; more test then forms
  ;(list 'if (list '! test) thenForm (list* 'begin forms)) ;; more else form 
  (list 'if (list '! test) thenForm (if (null? forms) #inert (car! forms))) ) ;; one else form

(assert (if! #t 1) #inert)
(assert (if! #t 1 2) 2)
(assert (if! #t 1 2 3))
(assert (if! #f 1 2) 1)

(defMacro (when test . forms)
  #|($nm test . forms)
   |(type macro)
   |
   |If <b>test</b> yields #true evaluate <b>forms</b> as an implicit `begin', #inert otherwise.
   |#
  (list 'if test (cons 'then forms)) )

(defMacro (unless test . forms)
  #|($nm test . forms)
   |(type macro)
   |
   |If <b>test</b> yields #false evaluate <b>forms</b> as an implicit `begin', #inert otherwise.
   |#
  (list 'if test #inert (cons 'else forms)) )

(defVau && ops env
  #|($nm . operands)
   |(type fexpr)
   |
   |Return #true if all <b>operands</b> evaluate to #true, #false otherwise.
   |If an operand evaluates to #false, later operands are not evaluated.
   |If there are no operands, return #true.
   |#
  (if
    (null? ops) #t
    (eval (car ops) env) (apply && (cdr ops) env)
    #f ))

(def and
  #|(aliasof &&)
   |#
  && )

(defVau || ops env
  #|($nm . operands)
   |(type fexpr)
   |
   |Return #true if one of the <b>operands</b> evaluates to #true, #false otherwise.
   |If an operand evaluates to #true, later operands are not evaluated.
   |If there are no operands, return #false.
   |#
  (if
    (null? ops) #f
    (eval (car ops) env) #t
    (apply || (cdr ops) env) ))

(def or
  #|(aliasof ||)
   |#
  || )

(def\ (&&b f key val . lst)
  #|($nm function key . list)
   |(type function)
   |
   |Return #true if <b>function</b> evaluate to #true when applied to <b>key</b> and all element of <b>list</b>, #false otherwise.
   |#
  ((rec\ (loop lst) (if (null? lst) #t (f key (car lst)) (loop (cdr lst)) #f) ) (cons val lst)) )

(def\ (||b f key val . lst)
  #|($nm function key . list)
   |(type function)
   |
   |Return #true if <b>function</b> evaluate to #true when applied to <b>key</b> and any element of <b>list</b>, #false otherwise.
   |#
  ((rec\ (loop lst) (if (null? lst) #f (f key (car lst)) #t (loop (cdr lst))) ) (cons val lst)) )

(def\ &&f f*
  #|($nm . functions)
   |(type function)
   |
   |Returns a function that returns #true if all <b>functions</b> return #true when applied to the arguments, #false otherwise.
   |If an function evaluates to #false, later <b>functions</b> are not evaluated.
   |If there are no <b>functions</b>, when applied return #false.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  (\ args
    ( (rec\ (&&f f*)
        (if
          (null? f*) #t
          (apply (car f*) args) (&&f (cdr f*))
          #f ))
      f* )))

(def\ ||f f*
  #|($nm . functions)
   |(type function)
   |
   |Return a function that return #true if one <b>functions</b> return #true when applied to the arguments, #false otherwise.
   |If an function evaluates to #true, later <b>functions</b> are not evaluated.
   |If there are no <b>functions</b>, when applied return #true.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |#
  (\ args
    ( (rec\ (||f f*)
        (if
          (null? f*) #f
          (apply (car f*) args) #t
          (||f (cdr f*)) ))
      f* )))


#|! Bind Bind? IfBind? CaseVau DefCaseVau Case\ DefCase\ Match Cond
 |#

(def bind
  #|($nm environment definiendTree value)
   |(type function)
   |
   |(derivation (eval (list 'def :cnt definiendTree value) environment))
   |
   |Return the <b>environment</b> if <b>value</b> match the <b>definiendTree</b> updated with new bindings, signals an error otherwise.
   |#
  %bind )

(def bind?
  #|($nm environment definiendTree value)
   |(type function)
   |
   |(derivation (catch #f (bind environment definiendTree value) #t))
   |
   |Return #true if <b>value</b> match the <b>definiendTree</b> and update the <b>environment</b> with the new bindings, #false otherwise.
   |#
  %bind? )

(defVau (ifBind? (dt exp) then . else) env
  #|($nm (definiendTree value) then . else)
   |(type fexpr)
   |
   |Return the evaluation of <b>then</b> into resulting <b>environment</b> if <b>value</b> match the <b>definiendTree</b>,
   |the evaluation of <b>else</b> if present, #inert otherwise.
   |#
  (let1 (env+ (newEnv env))
    (if (bind? env+ dt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (car! else) env) ))))

(defVau (caseVau . clauses) env
  #|($nm . clauses)
   |($nm #: check . clauses)
   |(type fexpr)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (else => apv1))
   |(syntax clause (definiendTree . forms))
   |
   |Return a multi-armed vau operator, when applied go through the <b>clauses</b> in order.
   |If <b>clauses</b> is #null return #inert.
   |If `car' of <b>clause</b> is else
   |  if the `cadr' of <b>clauses</b> is =>, evaluate `caddr' of <b>clause</b> and apply it to the operands,
   |  otherwise evaluate <b>forms</b> as an implicit `begin'.
   |If the operands match the <b>definiendTree</b> evaluate <b>forms</b> as an implicit `begin' into resulting <b>environment</b>.
   |Otherwise go to the next <b>clause</b>.
   |#
  (vau values #ignore :caseVau
    (let1 loop (clauses clauses)
      (unless (null? clauses)
        (let1 (((dt . forms) . clauses) clauses)
          (if (== dt 'else)
            (if (== (car forms) '=>)
              ((eval (cadr! forms) env) values)
              (apply begin forms env) )
            (let1 (env+ (newEnv env))
              (if (bind? env+ dt values)
                (apply begin forms env+)
                (loop clauses) ))))))))

(def arity
  #|($nm obj)
   |(type function)
   |
   |(derivation
   |  (def\ (arity obj)
   |    (if (type? obj Apv) (def obj (.cmb obj)))
   |    (if (type? obj JFUN) (def obj (.jfun obj)))
   |    (if (type? obj Opv)
   |          (let1 (xs (.xs obj))
   |            (if (|| (null? xs) (!= (car xs) :caseVau))
   |              (%arity (.pt obj))
   |              (map (_ (%arity (car _))) ((.e obj) :clauses)) ))
   |      (&& (type? obj JFun) (!(null? (.arity obj)))) (.arity obj)
   |      (type? obj Supplier) 0
   |      (type? obj ArgsList) '(>= 0)
   |      (type? obj Consumer Function) 1
   |      (type? obj BiConsumer BiFunction) 2
   |      (type? obj Field) (1 2)    
   |      (type? obj Method)
   |        (let1 (pc (@getParameterCount obj))
   |          (if (.isVarArgs obj) '(>= pc) (1+ pc)) )
   |      (type? obj Constructor)
   |        (let1 (pc (@getParameterCount obj))
   |          (if (.isVarArgs obj) '(>= (-1+ pc)) pc) )
   |      (type? obj Combinable) 
   |        (.arity obj) ))
   |
   |Return the number of parameter if <b>obj</b> is an operator, #inert otherwise.
   |  - an integer for fixed parameters operator
   |  - a list (>= interger) for variables parameters operator
   |  - a list of integer and (>= integer) for caseVau e case\ operator
   |#
  %arity )
  
(assert (arity 1) #inert)
(assert (arity (_)) 1)
(assert (arity (\ (a b))) 2)
(assert (arity (\ (a b . c))) '(>= 2))
(assert (arity (caseVau ((a)) ((a b)) (a))) (1 2 (>= 0)))
(assert (arity (caseVau ((a)) ((a b)) ((a b . c)))) (1 2 (>= 2)))
(assert (arity %wrap) 1)
(assert (arity %+) 2)
(assert (arity %==) 2)
(assert (arity begin '(>= 0)))

(defMacro (defCaseVau name . clauses)
  #|($nm name . clauses)
   |($nm #: check . clauses)
   |(type macro)
   |
   |(derivation (def name (caseVau . clauses)))
   |
   |Defines into the current environment the named caseVau <b>name</b> with the given <b>clauses</b>.
   |#
  (list 'def name (cons 'caseVau clauses)) )

(defMacro (case\ . clauses)
  #|($nm . clauses)
   |($nm #: check . clauses)
   |(type macro)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (else => apv1))
   |(syntax clause (definiendTree . forms))
   |
   |(derivation (wrap (caseVau . clauses)))
   |
   |Return a multi-armed \ function, when applied go through the <b>clauses</b> in order.
   |If <b>clauses</b> is #null return #inert.
   |If `car' of <b>clause</b> is else
   |  if the `cadr' of <b>clauses</b> is =>, evaluate `caddr' of <b>clause</b> and apply it to the arguments,
   |  otherwise evaluate <b>forms</b> as an implicit `begin'.
   |If the arguments match the <b>definiendTree</b> evaluate <b>forms</b> as an implicit `begin' into resulting <b>environment</b>.
   |Otherwise go to the next <b>clause</b>.
   |#
  (list 'wrap (cons 'caseVau clauses)) )

(defMacro (defCase\ name . clauses)
  #|($nm name . clauses)
   |($nm #: check . clauses)
   |(type macro)
   |
   |(derivation (def name (case\ . clauses)))
   |
   |Defines into the current environment the named case\ <b>name</b> with the given <b>clauses</b>.
   |#
  (list 'def name (cons 'case\ clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))
(assert ((case\ #: Integer ((a) (+ a 1)) (else 0)) 1) 2)
(assert ((case\ #: String ((a) (+ a 1)) (else 0)) 1))
(assert ((case\ #: String ((a) (+ a 1)) (else 0)) 1) Error :type 'type :datum 2 :expected 'String)

(defMacro (match exp . clauses)
  #|($nm value . clauses)
   |(type macro)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (else => apv1))
   |(syntax clause (definiendTree . forms))
   |
   |(derivation (wrap (caseVau . clauses)))
   |
   |Evaluates <b>value</b> and go through the <b>clauses</b> in order.
   |If <b>clauses</b> is #null return #inert.
   |If `car' of <b>clause</b> is else
   |  if the `cadr' of <b>clauses</b> is =>, evaluate `caddr' of <b>clause</b> and apply it to the arguments,
   |  otherwise evaluate <b>forms</b> as an implicit `begin'.
   |If <b>value</b> match the <b>definiendTree</b> evaluate <b>forms</b> as an implicit `begin' into resulting <b>environment</b>.
   |Otherwise go to the next <b>clause</b>.
   |#
  (list (cons 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match   '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match     '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match        1 ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match        4 ((a) 1) ((a b) 2)    (a a)) 4)
(assert (match '(1 2 3) ((a) 1) ((a b) 2)    (a a)) '(1 2 3))

(defVau (cond . clauses) env
  #|($nm . clauses)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (test->bool . forms))
   |(syntax clause (test))
   |(syntax clause (test => apv1))
   |(syntax clause (test guard => apv1))
   |
   |Multi-armed conditional.
   |Go through the <b>clauses</b> in order.
   |If <b>clauses</b> is #null return #inert.
   |If `car' of <b>clause</b> is else evaluate <b>forms</b> as an implicit `begin'.
   |Otherwise evaluate the <b>test</b>.
   |If <b>test</b> is a boolean
   |  if is #true, evaluate <b>forms</b> as an implicit `begin',
   |  otherwise go to the next <b>clause</b>.
   |If <b>forms</b> is #null return <b>test</b>.
   |If `car' di <b>forms</b> is => evaluate the `cadr' of <b>forms</b> and apply it to <b>test</b>.
   |If `cadr' di <b>forms</b> is => evaluate `car' of <b>forms</b>
   |  if is #true evaluate the `caddr' of <b>forms</b> and apply it to <b>test</b>
   |  otherwise go to the next <b>clause</b>.
   |Otherwise go to the next <b>clause</b>.
   |#
  (unless (null? clauses)
    (let1 (((test . forms) . clauses) clauses)
      (if (== test 'else)
        (apply begin forms env)
        (let1 (test (eval test env))
          (if (instanceOf? test Boolean)
            (if test
              (apply begin forms env)
              (apply cond clauses env) )
            (match forms
              (() test)
              (('=> apv1) ((eval apv1 env) test))
              ((guard '=> apv1)
                 (if ((eval guard env) test)
                   ((eval apv1 env) test)
                   (apply cond clauses env) ))
              (else (apply cond clauses env)) )))))))

(assert (let1 (a 1) (cond ((== a 1) 1 2 (+ a 2)) (else 3 4))) 3)
(assert (let1 (a 2) (cond ((== a 1) 1 2 (+ a 2)) (else 3 4))) 4)
(assert (let1 (a 1) (cond (a (\ (a) (== a 1)) => (\ (a) (+ a 1))))) 2)


#|! Quasiquote
 |Idea from Alf Petrofsky http://scheme-reports.org/mail/scheme-reports/msg00800.html
 |#

(defVau %` (x) env
  #|(let ((a 1) (b 2) (c '(3 4))) `(,@c ,a (,a) (,@c) b ,@c)) -> (3 4 1 (1) (3 4) b 3 4)
   |(let1 (x '(a b c)) ``(,,x ,@,x ,,@x ,@,@x)) -> `(,(a b c) ,@(a b c) ,a ,b ,c ,@a ,@b ,@c)
   |``(,,@'() ,@,@(list)) -> `()
   |`````(a ,(b c ,@,,@,@'(a b c))) -> ````(a ,(b c ,@,,@a ,@,,@b ,@,,@c))
   |#
  (defCase\ qq
    ( ((('%,@ x) . y) #f . d) (append (map (\ (x) (list '%,@ x)) (apply** qq (list x) d)) (apply** qq y #f d)) )
    ( ((('%,@ x) . y) . d)    (append (eval x env) (apply** qq y d)) )
    ( ((('%, x) . y) #f . d)  (append (map (\ (x) (list '%, x))  (apply** qq (list x) d)) (apply** qq y #f d)) )
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


#|! Options
 |An option is either #null ("none"), or a one-element list ("some").
 |Idea from Taylor R. Campbell's blag. https://mumble.net/~campbell/blag.txt
 |#

(def some
  #|(aliasof cons!)
   |
   |Create a one-element list from the <b>value</b>.
   |#
  cons!)

(defVau (ifOpt (pt opt) then . else) env
  #|($nm (name option) then . else)
   |(type fexpr)
   |
   |Single-value <b>option</b> destructuring.
   |Evaluate the <b>then</b> form with the <b>name</b> bound to the contents of the <b>option</b> if the evaluation of <b>option</b> is !#null,
   |evaluate the <b>else</b> forms as an implicit `begin' otherwise.
   |#
  (let1 (opt (eval opt env))
    (if (null? opt)
      (if (null? else) #null
        (eval (cons 'begin else) env))
      (if (list? opt)
        (eval (list* (list 'vau (list pt) #ignore then) opt) env)
        (typeError opt '(or () List)) ))))

(assert (ifOpt (a ()) (+ a 1)) #null)
(assert (ifOpt (a '(2)) (+ a 1)) 3)
(assert (ifOpt (a '(2 3)) (+ a 1)) Error :type 'match :operands# -1)
(assert (ifOpt ((a b) '((2 3))) (+ a b)) 5)

(defVau (ifOpt* (pt opt) then . else) env
  #|($nm (definiendTree option) then . else)
   |
   |Multi-valued <b>option</b> destructuring.
   |Evaluate <b>then</b> form with the <b>definiendTree</b> bound to <b>option</b> value if the evaluation of <b>option</b> is !#null,
   |evaluate the <b>else</b> forms as an implicit `begin' otherwise.
   |#
  (let1 (opt (eval opt env))
    (if (null? opt)
      (if (null? else) #null
        (eval (cons 'begin else) env))
      (if (list? opt)
        (eval (list* (list 'vau pt #ignore then) opt) env)
        (typeError opt '(or () List)) ))))

(assert (ifOpt* ((a) ()) (+ 1 a)) #null)
(assert (ifOpt* ((a) ()) (+ 1 a) 0) 0)
(assert (ifOpt* ((a) ()) (+ 1 a) 0 1) 1)
(assert (ifOpt* ((a) '(2)) (+ a 1)) 3)
(assert (ifOpt* ((a) '(2 3)) (+ a 1)) Error :type 'match :operands# -1)
(assert (ifOpt* ((a b) '(2 3)) (+ a b)) 5)
(assert (ifOpt* (a '(2 3)) (apply + a)) 5)
(assert (ifOpt* (a ()) (apply + a)) #null)

(defMacro whenOpt ((pt opt) . forms)
  #|($nm (name option) . forms)
   |(type fexpr)
   |
   |Destructure the <b>option</b>.
   |Return #null if the evaluation of <b>option</b> it's #null,
   |evaluate <b>forms</b> as an implicit `begin' with the <b>name</b> bound to the contents of the <b>option</b>, otherwise.
   |#
  (list 'ifOpt (list pt opt) (if (null? forms) #null (cons 'begin forms))) )

(defMacro unlessOpt (opt . forms)
  #|($nm option . forms)
   |(type fexpr)
   |
   |Destructure the <b>option</b>.
   |Return the evaluation of <b>forms</b> as an implicit `begin' if the evaluation of <b>option</b> is #null, #null otherwise.
   |#
  (list* 'ifOpt (list #ignore opt) #null (if (null? forms) #null (cons 'begin forms))) )

(defVau (caseOpt opt . clauses) env
  #|($nm option . clauses)
   |(type fexpr)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (definiendTree . forms))
   |
   |Multi-armed ifOpt.
   |Evaluate <b>option</b> and go through the <b>clauses</b> in order.
   |If <b>option</b> is #null return #null.
   |If <b>clauses</b> is #null return #null.
   |If `car' of <b>clause</b> is else evaluate <b>forms</b> as an implicit `begin'.
   |If the <b>option</b> match the <b>definiendTree</b> evaluate <b>forms</b> as an implicit `begin' into resulting <b>environment</b>.
   |Otherwise go to the next <b>clause</b>.
   |#
  (let1 (opt (eval opt env))
    (if (null? opt) #null
      (let1 loop (clauses clauses)
        (if (null? clauses) #null
          (let ( (env+ (newEnv env))
                 (((dt . forms) . clauses) clauses) )
            (if (|| (== dt 'else) (bind? env+ dt opt))
              (apply begin forms env+)
              (loop clauses) )))))))

(assert (caseOpt () ((a) 1) ((a b) (+ a b))) ())
(assert (caseOpt '(2) ((a) 1) ((a b) (+ a b))) 1)
(assert (caseOpt '(1 2) ((a) 1) ((a b) (+ a b))) 3)
(assert (caseOpt '(1 2 3) ((a) 1) ((a b) (+ a b))) #null)

(defVau (optDft opt . dft) env
  #|($nm option . default)
   |(type fexpr)
   |
   |Return the contents of the <b>option</b> if the option is !#null,
   |the evaluation if <b>default</b> if present, #inert otherwise.
   |The <b>default</b> is evaluated lazily, only when the <b>option</b> is #null.
   |#
  (ifOpt (opt (eval opt env)) opt
    (ifOpt (dft (eval (cons 'list dft) env)) dft) ))

(assert (optDft () 10) 10)
(assert (optDft '(2) 10) 2)
(assert (optDft '(2 3) 10))

(defVau optDft* (lst . dfts) env
  #|($nm list . defaults)
   |(type fexpr)
   |
   |(syntax defaults (default . defaults))
   |
   |Similar to `optdft', but provides <b>default</b> for any elements of <b>list</b>.
   |This is useful for implementing functions that take multiple optional arguments.
   |Each <b>default</b> is evaluated lazily, only when needed.
   |#
  (let loop ((lst (eval lst env)) (dfts dfts))
    (if (null? lst)
      (if (null? dfts) #null
         (cons (eval (car dfts) env) (loop #null (cdr dfts))) )
      (if (null? (car lst))
        (if (null? dfts)
          (cons #null (loop (cdr lst) #null))
          (cons (eval (car dfts) env) (loop (cdr lst) (cdr dfts))) )
        (cons (car lst)
          (loop (cdr lst) (if (null? dfts) #null (cdr dfts)))) ))))

(assert (optDft* '(1 () 3) 1 2 3 4) '(1 2 3 4))

(def\ optDft! (opt)
  #|($nm option)
   |(type fexpr)
   |
   |Returns the contents of the <b>option</b> or signals an error if it is #null.
   |#
  (optDft opt (simpleError "Option is #null")))


#|! OptValue Member Member? !Member? OptKey Assoc Member?*
 |#

(def\ (optValue key lst)
  #|($nm keyword list)
   |(type function)
   |
   |Search for the <b>keyword</b> in the property <b>list</b> (a list of alternating keywords and values)
   |and return the next value as an option if found it, #null otherwise.
   |#
  (let1 loop (lst lst)
    (if (cons? lst)
      (let1 ((k v . lst) lst)
        (if (== k key) (cons v) (loop lst)) )
      #null )))

(assert (optValue :b '(:a 1 :b 2 :c 3)) '(2))
(assert (optValue 'b '(a 1 b 2 c 3)) '(2))

(def\ (member k lst . keywords)
  #|($nm item list . keywords))
   |(type function)
   |
   |(syntax keywords (:cmp function . keywords))
   |(syntax keywords (:key function . keywords))
   |(syntax keywords (:get function . keywords))
   |
   |Search for <b>item</b> in the <b>list</b> according to the <b>cmp</b> predicate (defaults to `==').
   |Return, if found it, the tail of the list starting with <b>item</b> after apply the <b>get</b> function (defaults to 'idf'), #null otherwise.
   |The <b>key</b> function is applied to each list element before comparison (defaults to `idf').
   |#
  (let ( (cmp (optDft (optValue :cmp keywords) ==))
         (key (optDft (optValue :key keywords) idf))
         (ret (optDft (optValue :get keywords) idf)) )
    (let1 loop (lst lst)
      (if (cons? lst)
        (if (cmp (key (car lst)) k) (ret lst)
          (loop (cdr lst)) )
        #null ))))

(def\ (member? key lst . keywords)
  #|($nm item list . keywords)
   |(type function)
   |
   |(syntax keywords (:cmp function . keywords))
   |(syntax keywords (:key function . keywords))
   |(syntax keywords (:get function . keywords))
   |
   |Return #true if the <b>item</b> is in the <b>list</b>, #false otherwise.
   |#
  (cons? (apply** member key lst keywords)) )

(def\ (!member? key lst . keywords)
  #|($nm item list . keywords)
   |(type function)
   |
   |(syntax keywords (:cmp function . keywords))
   |(syntax keywords (:key function . keywords))
   |(syntax keywords (:get function . keywords))
   |
   |Return #false if the <b>item</b> is in the <b>list</b>, #true otherwise.
   |#
  (null? (apply** member key lst keywords)) )

(assert (member 'b '(a b c d)) '(b c d))
(if (intStr) (assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")))

(def\ (optKey key lst)
  #|($nm item list)
   |(type function)
   |
   |Return <b>item</b> if the <b>item</b> is in the <b>list</b>, #null otherwise.
   |<b>item</b> can be an sigle element or a list of element
   |#
  (if (cons? key) ((rec\ (loop lst) (if (cons? lst) (let1 (k (car lst)) (if (member? k key) k (loop (cdr lst)))) #null)) lst)
    (member? key lst) key
    #null ))

(assert (optKey :b '(:a :c)) #null)
(assert (optKey :b '(:a :b :c)) :b)
(assert (optKey (:b :d) '(:a :b :c)) :b)
(assert (optKey (:b :c) '(:a :b :c)) :b)

(def\ (assoc k lst)
  #|($nm item list)
   |(type function)
   |
   |Return the `car' of a assoc <b>list</b> (a list of lists of keywords and values) is <b>item</b> is the `caar' if == to <b>item</b>, #null otherwise.
   |#
  (member k lst :key car :get car) )

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (member?* key . lst)
  #|($nm item . list)
   |(type function)
   |
   |Return #true if the <b>item</b> is in the <b>list</b>, #false otherwise.
   |#
  (member? key lst) )


#|! Case MatchType? MatchType?* CaseType CaseType\
 |#

(defVau (case exp . clauses) env
  #|($nm key . clauses)
   |(type fexpr)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (else . forms))
   |(syntax clause (else => apv1))
   |(syntax clause (value . forms))
   |(syntax clause (value => apv1))
   |(syntax clause (values . forms))
   |(syntax clause (values => apv1))
   |(syntax values (value . values))
   |
   |Multi-armed value test.
   |Evaluate <b>key</b> and go through the <b>clauses</b>.
   |If <b>clauses</b> is #null return #inert.
   |If `car' of <b>clause</b> is else or `eq?' <b>key</b> <b>value</b> or `member? :cmp eq?' <b>key</b> <b>values</b>
   |  if `car' <b>forms</b> is => apply evaluate `cadr' <b>forms</b> to <b>value</b>
   |  otherwise evaluate <b>forms</b> as an implicit `begin'.
   |Otherwise go to the next <b>clause</b>.
   |#
  (let1 (value (eval exp env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((values . forms) . clauses) clauses)
          (if (|| (== values 'else) (eq? value values) (member? value values :cmp eq?))
            (if (null? forms) #inert
              (if (== (car forms) '=>)
                ((eval (cadr! forms) env) value)
                (apply begin forms env) ))
            (next clauses) ))))))

(assert (case 3 ((2 4 6 8) 'pair) ((1 3 5 7 9) 'odd)) 'odd)

(defVau (matchType? object typeCheck . typeChecks) env
  #|($nm object typeCheck . typeChecks)
   |(type function)
   |
   |(syntax typeCheck class)
   |(syntax typeCheck (class . attributes))
   |(syntax typeChecks (typeCheck . typeChecks))
   |(syntax attributes (attribute check . attributes))
   |(syntax attribute (or Symbol Keyword String .Field @Method))
   |
   |Return #true if <b>object</b> matches one the <b>typeCheck</b>, #false otherwise.
   |Matchs one <b>typeCheck</b> if <b>object</b> is an instance of <b>class</b> and all the optional specified <b>attribute</b> matches the corresponding <b>check</b>.
   |#
  (apply %matchType? (cons (eval object env) (map (\ (x) (eval (if (atom? x) x (cons 'list x)) env)) (cons typeCheck typeChecks))) env) )

(assert (matchType? 1 Integer) #t)
(assert (matchType? 1 String Integer) #t)
(assert (matchType? 1 String (Integer)) #t)
(assert (matchType? 1 (String) (Integer)) #t)
(assert (matchType? 1 (Integer)) #t)
(assert (matchType? 1 (Integer @intValue 1)) #t)
(assert (matchType? 1 (Integer @intValue 2)) #f)
(assert (matchType? (newBox 1) Box) #t)
(assert (matchType? (newBox 1) String Box) #t)
(assert (matchType? (newBox 1) String (Box)) #t)
(assert (matchType? (newBox 1) (String) (Box)) #t)
(assert (matchType? (newBox 1) (Box)) #t)
(assert (matchType? (newBox 1) (Box 1)) #t)
(assert (matchType? (newBox 1) (Box 2)) #f)
(assert (matchType? (newObj :type 'a) (Obj :type 'a)) #t)

(defMacro (matchType?* obj class . attributes)
  #|($nm object class . attributes)
   |(type function)
   |
   |(syntax attributes (attribute check . attributes))
   |(syntax attribute (or Symbol Keyword String .Field @Method))
   |
   |Return #true if <b>object</b> is an instance of <b>class</b> and all the optional specified <b>attribute</b> matches the corresponding <b>check</b>, #false otherwise.
   |#
  (list 'matchType? obj (cons class attributes)) )


; vedi signalsError? in vm.lispx (o testUtil.lispx) per codice simile
(defVau (caseType key . clauses) env
  #|($nm object . clauses)
   |(type fexpr)
   |
   |(syntax clauses (clause . clauses))
   |(syntax clause (class . forms))
   |(syntax clause ((class . attributes) . forms))
   |(syntax attributes (attribute check . attributes))
   |(syntax attribute (or Symbol Keyword String .Field @Method))
   |
   |Multi-armed type test.
   |Evaluate the <b>object</b> and go through the <b>clauses</b>.
   |If <b>clauses</b> is #null return #inert.
   |If <b>object</b> is an instance of <b>class</b> and the optional specified <b>attribute</b> matchs the corresponding <b>check</b>, evaluate <b>forms</b> as an implicit `begin'.
   |Otherwise go to the next <b>clause</b>.
   |#
  (let1 (key (eval key env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((test . forms) . clauses) clauses)
          (if (|| (== test 'else)
                  (apply matchType? (list (list 'quote key) test) env) )
            (if (== (car forms) '=>)
              ((eval (cadr! forms) env) key)
              (apply begin forms env) )
            (next clauses) ))))))

(assert (caseType 2.0 (else 3)) 3)
(assert (caseType (+ 2 2) (else => (\ (v) v))) 4)
(assert (caseType 2.0 (String "string") (Double "double")) "double")
(assert (caseType (newObj :a 1) (Double "double") ((Obj :a 1) "Obj :a 1")) "Obj :a 1")
(assert (caseType 1 ((Integer @intValue 1) 'ok) (else 'ko)) 'ok)
(assert (caseType 1 (Integer 'ok) (else 'ko)) 'ok)

(defMacro (caseType\ (#: (1 Symbol) key) . clauses)
  #|($nm (symbol) . clauses))
   |(type macro)
   |
   |(derivation (\ (symbol) (caseType symbol . clauses)))
   |(use (catchWth (caseType\ (e) ...) ...)
   |
   |Return a sigle argument function for multi-armed type test, useful as a catch handler.
   |#
  (list '\ key (list* 'caseType (car key) clauses) ))

(assert (catchWth (caseType\ (e) ((Error :type 'xx) 1) (else 2)) (error "errore {type}!" :type 'zz)) 2)


#|! Sort
 |#

(def\ (sort lst . opts)
  #|($nm list . (#: 0 3 (or (2 :key function) (1 (or :up :dn)) )))
   |(type function)
   |
   |Return the sorted <b>list</b>, getting the key with the :key function (defaults to 'idf') and up or down with key :up or :dn (defaults to :up).
   |#
  (def cmp (case (optKey (:up :dn) opts) ((#null :up) <) (:dn >=)))
  (def key (optDft (optValue :key opts) idf))
  (def\ (sort lst)
    (if (<= (len lst) 1) lst
      (let loop ( (left ()) (right ()) (pivot (car lst)) (rest (cdr lst)) )
        (if (null? rest)
            (append (append (sort left) (list pivot)) (sort right))
          (cmp (key (car rest)) (key pivot))
            (loop (append left (list (car rest))) right pivot (cdr rest))
            (loop left (append right (list (car rest))) pivot (cdr rest)) ))))
  (sort lst) )

(assert (sort (1 4 2 5 7 3)) (1 2 3 4 5 7))
(assert (sort ((1) (4) (2) (5) (7) (3)) :key car) ((1) (2) (3) (4) (5) (7)))
(assert (sort ((1) (4) (3 1) (5) (7) (3 2)) :key car) ((1) (3 1) (3 2) (4) (5) (7)))
(assert (sort ((1) (4) (3 1) (5) (7) (3 2)) :key car :dn) ((7) (5) (4) (3 2) (3 1) (1)))


#|! Type and Value Checks
 |Type and value checks are possible with (#: check symbol) wherever there is a Symbol in a definined or parameters tree including all lexical bindings.
 |
 |(syntax definiendTree (or symbol decomposeTree))
 |(syntax parametersTree (or #null ignore symbol decomposeTree))
 |(syntax decomposeTree (parametersTree . decomposeTree))
 |(syntax symbol (or Symbol (#: check Symbol)))
 |(syntax ignore (or #ignore (#: check #ignore)))
 |(syntax check Any)
 |(syntax check class)
 |(syntax check checks)
 |(syntax checks (check . checks))
 |(syntax check (min . checks))
 |(syntax check (min max . checks))
 |(syntax check (min oo . checks))
 |(syntax check (or . checks))
 |(syntax check (and . checks))
 |(syntax check (Apv . arguments))
 |(syntax check value)
 |(syntax arguments (value . arguments))
 |
 |A `check' can be:
 |- a `value',
 |- the `Any' class,
 |- a `class',
 |- a `List' with zero, one or two `Integers' followed by zero or more `checks',
 |- a `List' with an `Integer' and the symbol `oo' followed by zero or more `checks',
 |- a `List' with `car' equals `or' followed by two or more `check',
 |- a `List' with `car' equals `and' followed by two or more `check',
 |- a `List' with `car' `Apv' followed by zero o more arguments.
 |
 |When the `check' is:
 |- a `value': the parameter value must be equal to that `value'
 |- the `Any' class : the parameter can be any value
 |- a `class': the parameter value can be an instance of that `class' or a class that extends that `class'
 |- a `List': the parameter value must be a `List' where, in the `check' `List',
 |  - the first `Integer' indicates the minimum number of elements, default is `0'
 |  - the second `Integer' indicates the maximum number of elements
 |    default is the value of the first `Integer' if present, otherwise `oo' for `Integer.maxValue'
 |    for the second `Integer' can be also specified the symbol `oo' for `Integer.maxValue'
 |  - if the number of `check' arguments in the list is
 |    - less than the minimum: parameters values that exceed the minimum can have any value
 |    - greater than the minimum: the parameters values exceeding the minimum will be checked cyclically using `check' arguments exceeding the minimum
 |  - if the first element of the list is:
 |    - `or': the parameter value must match one of the `check' arguments of the `or'
 |    - `and': the parameter value must match all the `check' arguments of the `and'
 |    - `Apv': applying `Apv' to the cons of the parameter value and the remaining arguments must return #true
 |#

(def\ assert#t (boolean)
  #|($nm boolean)
   |(type function)
   |
   |Return #inert if <b>boolean</b> is #true, signals an error otherwise.
   |#
  (unless boolean (error (new Error "invalid assetion" :type 'assert :datum boolean :expected #t))) )

(def check
  #|($nm expr check)
   |(type fexpr)
   |
   |(derivation ((\ ((#: check value)) (len value)) expr))
   |(derivation (let ( (value (eval exor env)) (chk (%evalChk check env)) ) (@check vm value chk))) 
   |
   |Returns the length of <b>expr</b> if it matches <b>check</b>, signals an error otherwise.
   |#
  %check )

(defMacro (check* o . cks)
  #|($nm list . checks)
   |(type macro)
   |
   |(derivation ((\ (#: checks list) (len list)) . list) 
   |(derivation (check list checks)) 
   |
   |Returns the length of <b>list</b> if the elements of <b>list</b> match the corresponding <b>check</b>, signals an error otherwise.
   |#
  (list 'check o cks) )

(assert (check* '(1 (:a 1 :b 2) c 3) 1 oo Integer (Keyword Integer) Symbol (or 3 4)) 4)
(assert (check* '(a 1 2) 'a 1 2) 3)
(assert (check* '(a) (or '(b) '(a))) 1)
(assert (check* '(a 1 2) (or '(b 3) '(a 1 2))) 3)
(assert (check* '(a #null 1) 2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 3)
(assert (check* '(a :prv 1)  2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 3)
(assert (check* '(a 1)       2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 2)

(defVau check? args env
  #|($nm object check)
   |(type fexpr)
   |
   |(derivation wat (catchWth #f (apply check args env) #t)) 
   |(derivation lispx (catch (handlerBind ( (Error (_ (throw #f))) ) (apply check args env) #t))) 
   |
   |Return #true if <b>object</b> match <b>check</b>, #false otherwise.
   |#
  (catchWth #f
    (apply check args env)
    #t ))

(def :
  #|($nm check form . forms)
   |(type fexpr)
   |
   |(derivation ((\ ((#: check value)) value) (begin form . forms)))
   |(derivation (let ( (value (eval (list* 'begin form . forms) env)) (chk (%evalChk check env)) ) (@check vm value chk) value)) 
   |
   |Returns the value of the last form of (begin <b>form</b> . <b>forms</b>) if it matches <b>check</b>, signals an error otherwise.
   |#
  %: )

(def the
  #|(aliasof :)
   |#
  : )

(assert (the Integer 1) 1)
(assert (the Integer "1") Error :type 'type :datum "1" :expected 'Integer)
(assert (the (or 1 2) 1) 1)
(assert (the (or 1 2) 2) 2)
(assert (the (or 1 2) 3) Error :type 'type :datum 3 :expected '(or 1 2))


#|! Block ReturnFrom Loop For While Until Repeat DoTimes DoList
 |#

(defVau block (blockName . forms) env
  #|($nm blockName . forms)
   |(type fexpr)
   |
   |Establishes a block named <b>blockName</b> after evaluate <b>forms</b> as an implicit `begin'.
   |The forms may use `returnFrom' to nonlocally exit from the block.
   |Note that unlike in Common Lisp, there is no separate namespace for block names;
   |a block is named in the normal variable namespace.
   |#
  (let* ( (tag (cons #inert)) ; cons up a fresh object as tag
          (escape (\ value (throwTag tag (if (cons? value) (car! value))))) )
    (catchTag tag
      (eval (list (list* '\ (list blockName) forms) escape) env) )))

(assert (block exit (exit)) #inert)
(assert (block exit (exit 7)) 7)
(assert (block exit (def x 1) (loop (if (== x 4) (exit 7)) (def x (+ x 1)))) 7)

(def\ returnFrom (blockName . value)
  #|($nm blockName . value)
   |(type function)
   |
   |Abort evaluation and return the optional <b>value</b> (which defaults to #inert) from the block named <b>blockName</b>.
   |It is an error to return from a block whose dynamic extent has ended.
   |#
  (blockName (optDft value #inert)) )

(assert (block ciclo (def x 1) (loop (if (== x 4) (returnFrom ciclo 7)) (def x (+ x 1)))) 7)

(defVau while (testForm . forms) env
  #|($nm testForm . forms)
   |(type fexpr)
   |
   |Evaluate <b>forms</b> as an implicit `begin' while <b>testForm</b> evaluates to #true.
   |Defined using block and returnFrom.
   |#
  (let ((forms (cons 'begin forms)))
    (block exit
      (loop
        (if (eval testForm env)
          (eval forms env)
          (returnFrom exit #inert)) ))))

(defde\ (mkTag tag (#: (and Integer (>= 0) (<= %deep)) n))
  #|($nm tag n)
   |(type dynamic function)
   |
   |Return a tag for the break/continue throws forms of into the loop form by joining <b>tag</b> and N.
   |Get the %deep of the enhanced loop form in the dynamic environment.
   |Used from break, continue, until and while forms for the enhanced loops.
   |#
  (symbol ($ tag (- %deep n))) )

(defMacro (break . forms) (list* 'throwTag (list 'mkTag "break" 0) forms))
(defMacro (break- n . forms) (list* 'throwTag (list 'mkTag "break" n) forms))
(defMacro (break? b . forms) (list 'if b (list* 'throwTag (list 'mkTag "break" 0) forms)))
(defMacro (break?- n b . forms) (list 'if b (list* 'throwTag (list 'mkTag "break" n) forms)))

(defMacro (continue . forms) (list* 'throwTag (list 'mkTag "continue" 0) forms))
(defMacro (continue- n . forms) (list* 'throwTag (list 'mkTag "continue" n) forms))
(defMacro (continue? b . forms) (list 'if b (list* 'throwTag (list 'mkTag "continue" 0) forms)))
(defMacro (continue?- n b . forms) (list 'if b (list* 'throwTag (list 'mkTag "continue" n) forms)))

(defMacro (until? b . forms) (list 'if b        (list* 'throwTag (list 'mkTag "break" 0) forms)))
(defMacro (while? b . forms) (list 'if b #inert (list* 'throwTag (list 'mkTag "break" 0) forms)))

(def %loop
  #|($nm . forms)
   |($nm for1 binding . forms)
   |($nm for bindings . forms)
   |(type fexpr)
   |
   |(syntax bindings (binding . bindings))
   |(syntax binding (symbol initForm . incrFrom))
   |(syntax forms (form . forms) )
   |(syntax form (break . forms))
   |(syntax form (break- n . forms))
   |(syntax form (break? testForm . forms))
   |(syntax form (break?- n testForm . forms))
   |(syntax form (continue . forms))
   |(syntax form (continue- n . forms))
   |(syntax form (continue? testForm . forms))
   |(syntax form (continue?- n testForm . forms))
   |(syntax form (while? testForm . forms))
   |(syntax form (until? testForm . forms))
   |(syntax form ...)
   |
   |Redefine the primitive %loop to add the clauses <b>for</b> and <b>for1</b> and the forms <b>break</b>, <b>continue</b>, <b>while</b> and <b>until</b>.
   |#
  (let1 (%loop ((.parent (theEnv)) '%loop))
    (vau forms env
      (let1 (%deep (let1 (%deep (value :%deep env)) (if (null? %deep) 0 (1+ %deep))))
        (let ( (break (symbol ($ 'break %deep)))
               (continue (symbol ($ 'continue %deep))) )
          (let1 (env (newEnv (newEnv env :%deep %deep)))
            (if 
              (check? forms (2 oo 'for ((2 3 Symbol)) )) ;loop for
                (let ( (for (cadr forms))
                       (forms (cddr forms)) )
                  (def increments (list* 'def* (map car for) (map (\((#_ init . incr)) (optDft incr init)) for)))
                  (catchTag break
                    (eval (list* 'def* (map car for) (map cadr for)) env)
                    (%loop
                      (catchTag continue
                        (apply begin forms env) )
                      (eval increments env) )))
              (check? forms (2 oo 'for1 (2 3 Symbol))) ;loop for1
                (let ( ((pt init . incr) (cadr forms))
                       (forms (cddr forms)) )
                  (def increment (list 'def pt (optDft incr init)))
                  (catchTag break
                    (eval (list 'def pt init) env)
                    (%loop
                      (catchTag continue
                        (apply begin forms env) )
                      (eval increment env) )))
              (catchTag break ;simple loop
                (%loop
                  (catchTag continue
                    (apply begin forms env) ))) ))))) ))

(def loop
  #|(aliasof %loop)
   |#
  %loop)

(defMacro (for1 binding whileCond . forms)
  #|($nm binding whileCond . forms)
   |(type macro)
   |
   |(syntax binding (symbol initForm . incrFrom))
   |
   |Single variabile for loop.
   |#
  (list* 'loop 'for1 binding
    (if (%ignore? whileCond) forms
      (cons (list 'while? whileCond) forms) )))

(defMacro (for bindings whileCond . forms)
  #|($nm bindings whileCond . forms)
   |(type macro)
   |
   |(syntax bindings (binding . bindings))
   |(syntax binding (symbol initForm . incrForm))
   |
   |Multile variabiles for loop.
   |#
  (list* 'loop 'for bindings
    (if (%ignore? whileCond) forms
      (cons (list 'while? whileCond) forms) )))

(let ()
  (assert (loop (break 3)) 3)
  (assert (loop (loop (break- 1 3))) 3)
  (defMacro (+= n v) (list 'set! n :rhs (list '+ n v)) )
  (defMacro (-= n v) (list 'set! n :rhs (list '- n v)) )
  (assert (let1 (a 0) (loop for ((x 0 (1+ x)) (y 0 (-1+ y))) (while? (< x 3) a) (+= a (+ x y)))) 0)
  (assert (let1 (a 0) (loop for ((x 0 (1+ x))) (while? (< x 3) a) (+= a (* x 10)) (loop for ((y 0 (1+ y))) (break? (> y 2)) (+= a y)) )) 39)
  (assert (loop for1 (x 0 (1+ x)) (break x)) 0)
  (assert (loop for1 (x 0 (1+ x)) (while? (< x 3) x)) 3)
  (assert (let1 (a 0) (loop for1 (x 0 (1+ x)) (while? (< x 3) a) (+= a (* x 10)) (loop for1 (y 0 (1+ y)) (break? (> y 2)) (+= a y) ))) 39)
  (assert (let1 (a 0) (loop for1 (x 0 (1+ x)) (while? (< x 3) a) (+= a (* x 10)) (loop for1 (y 0 (1+ y)) (break?- 1 (> y 3) a) (+= a y)))) 6)
  (assert (let1 (a 0) (for1 (x 0 (1+ x)) (< x 3) (+= a (* x 10)) (for1 (y 0 (1+ y)) #ignore (break?- 1 (> y 3) a) (+= a y)))) 6)
  (assert (let1 (a 0) (for ((x 0 (1+ x)) (y 10 (-1+ y)) ) (< x 10) (+= a (+ x y))) a) 100)
)

(defMacro (while whileCond . forms)
  #|($nm whileCond . forms)
   |(type macro)
   |
   |Evaluate <b>forms</b> as an implicit `begin' while <b>whileCond</b> evaluates to #true.
   |#
  (list* 'loop (list 'while? whileCond) forms) )

(defMacro until (untilCond . forms)
  #|($nm untilCond . forms)
   |(type macro)
   |
   |Evaluate <b>forms</b> as an implicit `begin' until <b>untilCond</b> evaluates to #false.
   |#
  (list* 'loop (list 'until? untilCond) forms) )

(let ()
  (defMacro (++ n) (list 'set! n :rhs (list '1+ n)) )
  (defMacro (-- n) (list 'set! n :rhs (list '-1+ n)) )
  (assert (let1 (i 0) (while (< i 3) (++ i)) i) 3)
  (assert (let1 (i 0) (until (> i 2) (++ i)) i) 3)
  (assert (let1 (c 2) (while (> c 0) (-- c)) c) 0)
  (assert (let1 (c 2) (while #t (if (0? c) (break (+ c 5)) (-- c)))) 5)
  (assert (let1 (c 2) (loop (until? (0? c) (+ c 5)) (-- c))) 5)
  (assert (let ((c 10) (r #null)) (while #t (if (0? c) (break r)) (if (0? (% (-- c) 2)) (continue)) (set! r (cons c r)) )) '(1 3 5 7 9))
  (assert (let ((c 10) (r #null)) (while #t (break? (0? c) r) (continue? (0? (% (-- c) 2))) (set! r (cons c r)) )) '(1 3 5 7 9))
  (assert (let ((c 10) (r #null)) (loop (until? (0? c) r) (if (0? (% (-- c) 2)) (continue)) (set! r (cons c r)) )) '(1 3 5 7 9))
  (assert (let ((c 10) (r #null)) (loop (until? (0? c) r) (continue? (0? (% (-- c) 2))) (set! r (cons c r)) )) '(1 3 5 7 9))
)

#|TODO sostituito dal seguente, eliminare
(defMacro doTimes ((var times . endForms) . forms)
  #|($nm (symbol times . endForms) . forms)
   |(type macro)
   |
   |Cf. Common Lisp's <b>dotimes</b>.
   |#
  (let1\
    (doTimes (times forms endForms)
      (let1 (i (newBox 0))
        (while (< (i) times) (forms (i)) (++ i) )
        (result (i)) ))
    (list doTimes
      times
      (list* '\ (list var) forms)
      (list* '\ (list var) endForms) )))
|#

(defVau (repeat times . forms) env
  #|($nm times . forms)
   |($nm (symbol times . endForms) . forms)
   |(type macro)
   |
   |Evaluate <b>forms</b> as an implicit `begin' <b>times</b> times.
   |#
  (if (cons? times)
    (let* ( (((#: Symbol var) times . endForms) times)
            ((#: (and Integer (> 0)) times) (eval times env))
            (env (newEnv env var 0)) )
      (loop (def result (apply begin forms env))
        (break? (>= (eval (list 'set! var :rhs (list '1+ var)) env) times)
          (if (null? endForms) result (apply begin endForms env)) )))
    (let1 ((#: (and Integer (> 0)) times) (eval times env))
      (loop (def result (apply begin forms env))
        (break? (0? (set! times :rhs (-1+ times))) result) ))))

(let ()
  ;(defMacro (++ n) (list 'set! n :rhs (list '1+ n)) )
  ;(defMacro (-- n) (list 'set! n :rhs (list '-1+ n)) )
  (defMacro (+= n v) (list 'set! n :rhs (list '+ n v)) )
  ;(defMacro (-= n v) (list 'set! n :rhs (list '- n v)) )
  (assert (let1 (a 0) (repeat 4 (+= a 1)) a) 4)
  (assert (let1 (a 0) (repeat (i 4) (+= a i)) a) 6)
  (assert (let1 (a 0) (repeat (i 4 a) (+= a i))) 6)
)

(defMacro doTimes ((var times . endForms) . forms)
  #|($nm (symbol times . endForms) . forms)
   |(type macro)
   |
   |Cf. Common Lisp's <b>dotimes</b>.
   |#
  (list* 'repeat (list* var times endForms) forms) )

#|TODO in alternativa del precedente, da verificare
(defVau (repeat times . forms) env
  #|($nm times . forms)
   |($nm (symbol times . endForms) . forms)
   |(type macro)
   |
   |Evaluate <b>forms</b> as an implicit `begin' <b>times</b> times.
   |#
  (if (cons? times)
    (let* ( (((#: Symbol var) times . endForms) times)
            ((#: (and Integer (>= 0)) times) (eval times env))
            (env (newEnv env var 0)) )
      (loop (break? (>= (env var) times) (if (cons? endForms) (apply begin endForms env) (0? times) #inert result) )
            (def result (apply begin forms env))
            (eval (list '++ var) env) ))
    (let1 ((#: (and Integer (> 0)) times) (eval times env))
      (loop (def result (apply begin forms env))
        (if (0? (-- times)) (break result)) ))))

(defMacro doTimes ((var times . endForms) . forms)
  #|($nm (symbol times . endForms) . forms)
   |(type macro)
   |
   |Cf. Common Lisp's <b>dotimes</b>.
   |#
  (list* 'repeat (list* var times (if (null? endForms) (cons #inert) endForms)) forms) )
|#

(defMacro doList ((var lst . endForms) . forms)
  #|($nm (symbol list . endForms) . forms)
   |(type macro)
   |
   |Cf. Common Lisp's <b>dolist</b>.
   |#
  (let1rec\
    (doList (lst body end)
      (if (null? lst) (end lst)
        (else
          (body (car lst))
          (doList (cdr lst) body end) )))
    (list doList
      lst
      (list* '\ (list var) forms)
      (list* '\ (list var) endForms) )))


#|! Lists Functions
 |#

(def\ (any? f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Return #true if the apply of the <b>function</b> to every first elements of the <b>lists</b> return #true, #false otherwise.
   |#
  (if (null? lst*)
    ((rec\ (any? lst) (if (null? lst) #f (f (car lst)) #t (any? (cdr lst))) ) lst)
    ((rec\ (any?* lst*) (if (null? (car lst*)) #f (apply f (map car lst*)) #t (any?* (map cdr lst*))) ) (cons lst lst*)) ))

(assert (any? null? (1 2 3 4)) #f)
(assert (any? null? (1 2 () 4)) #t)
(assert (any? > '(1 2) '(3 4)) #f)
(assert (any? < '(1 2) '(3 4)) #t)

(defMacro (any?* f . lst)
  #|($nm function . values).
   |(type function).
   |
   |Return #true if the apply of the <b>function</b> to every element of <b>values</b> return #true, #false otherwise.
   |#
  (list 'any? f lst))

(def\ (all? f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Return #true if the apply of the <b>function</b> to one first elements of the <b>lists</b> return #true, #false otherwise.
   |#
  (if (null? lst*)
    ((rec\ (all? lst) (if (null? lst) #t (f (car lst)) (all? (cdr lst)) #f) ) lst)
    ((rec\ (all?* lst*) (if (null? (car lst*)) #t (apply f (map car lst*)) (all?* (map cdr lst*)) #f) ) (cons lst lst*)) ))

(assert (all? number? (1 2 3 4)) #t)
(assert (all? number? (1 2 () 4)) #f)
(assert (all? > '(1 2) '(3 4)) #f)
(assert (all? < '(1 2) '(3 4)) #t)

(defMacro (all?* f . lst)
  #|($nm function . values).
   |(type function).
   |
   |Return #true if the apply of the <b>function</b> to one element of <b>values</b> return #true, #false otherwise.
   |#
  (list 'all? f lst))

(def\ (forEach# f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Return #inert after applies the <b>function</b> to every first elements of the <b>lists</b>.
   |#
  (if (null? lst*)
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) lst)
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) (cons lst lst*)) ))

(assert (forEach# (\ (#ignore)) '(1 2)) #inert)
(assert (forEach# (\ (#ignore #ignore)) '(1 2) '(3 4)) #inert)
(assert (let1 (n 1) (forEach# (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach# (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ (forEach f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Return <b>lists</b> after applies the <b>function</b> to every first elements of the <b>lists</b>.
   |#
  (if (null? lst*)
    (let1 (res lst) ((rec\ (forEach lst) (if (null? lst) res (else (f (car lst)) (forEach (cdr lst)) ))) res))
    (let1 (res* (cons lst lst*)) ((rec\ (forEach* lst*) (if (null? (car lst*)) res* (else (apply f (map car lst*)) (forEach* (map cdr lst*)) ))) res*) )) )

(assert (forEach (\ (#ignore)) '(1 2)) '(1 2))
(assert (forEach (\ (#ignore #ignore)) '(1 2) '(3 4)) '((1 2) (3 4)))
(assert (let1 (n 1) (forEach (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ maplist (f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Return the list of the results of apply the <b>function</b>, which must return a list, to every first elements of the <b>lists</b>.
   |(Note: this currently uses `append', but might be changed to use `nconc' in the future, like Common Lisp.)
   |#
  (if (null? lst*)
    ((rec\ (maplist lst) (if (null? lst) #null (append (f (car lst)) (maplist (cdr lst))))) lst)
    ((rec\ (maplist* lst*) (if (null? (car lst*)) #null (append (apply f (map car lst*)) (maplist* (map cdr lst*))))) (cons lst lst*)) ))

(assert (maplist (\ (x) (list x)) '(1 2 3 4)) '(1 2 3 4))

(def\ (filter f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Returns the list of the first elements of the <b>lists</b> for which the application of the <b>function</b> returns #true.
   |(Note: this currently uses `append', but might be changed to use `nconc' in the future, like Common Lisp.)
   |#
  (if (null? lst*)
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (cons (car lst) (filter (cdr lst))) (filter (cdr lst))))) lst)
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (cons cars (filter* (map cdr lst*))) (filter* (map cdr lst*)) )))) (cons lst lst*)) ))

(assert (filter even? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (filter != '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(defMacro (remove f lst . lst*)
  #|($nm function . lists).
   |(type function).
   |
   |Returns the list of the first elements of the <b>list</b> without those for which the application of the <b>function</b> returns #true.
   |#
  (list* 'filter (list 'compose '! f) lst lst*) )

(assert (remove odd? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (remove == '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(def\ (reduceL f init lst . lst*)
  #|($nm function  init . lists).
   |(type function).
   |
   |Use the <b>function</b> to combine the elements of the <b>list</b> from first element.
   |The <b>initialValue</b> is logically placed before the first element of the <b>list</b>.
   |#
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (reduce* (apply* f acc (map car lst*)) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def reduce
  #|(aliasof reduceL)
   |#
  reduceL)

(def\ (reduceR f init lst . lst*)
  #|($nm function init . list).
   |(type function).
   |
   |Use the <b>function</b> to combine the elements of the <b>list</b> from last element.
   |The <b>initialValue</b> is logically placed before the last element of the <b>list</b>.
   |#
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init (cons lst lst*)) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init lst . lst*)
  #|($nm function init . list).
   |(type function).
   |
   |Use the <b>function</b> to combine the elements of the <b>list</b> from first element.
   |The <b>initialValue</b> is logically placed after the last element of the <b>list</b>.
   |#
  (if (null? lst*)
    ((rec\ (foldl acc lst) (if (null? lst) acc (f (car lst) (foldl acc (cdr lst)) ) )) init lst)
    ((rec\ (foldl* acc lst*) (if (null? (car lst*)) acc (apply* f (map car lst*) (foldl* acc (map cdr lst*)) ) )) init (cons lst lst*)) ))

(assert (foldL cons () '(1 2 3 4)) '(1 . (2 . (3 . (4 . ())))))

(def\ (foldR f init lst . lst*)
  #|($nm function  init . list).
   |(type function).
   |
   |Use the <b>function</b> to combine the elements of the <b>list</b> from last element.
   |The <b>initialValue</b> is logically placed after the last element of the <b>list</b>.
   |#
  (if (null? lst*)
    ((rec\ (foldr acc lst) (if (null? lst) acc (foldr (f (car lst) acc) (cdr lst)) )) init lst)
    ((rec\ (foldr* acc lst*) (if (null? (car lst*)) acc (foldr* (apply* f (map car lst*) acc) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (foldR cons () '(1 2 3 4)) '(4 . (3 . (2 . (1 . ())))))


#|! Arrays
 |#

(def\ (array->list arr)
  #|($nm array)
   |(type function)
   |
   |Array to proper list.
   |#
  (%array->list #t arr) )

(def\ (array->cons arr)
  #|($nm array)
   |(type function)
   |
   |Array to improper list.
   |The last element of the array becomes the last `cdr' of the list.
   |#
  (%array->list #f arr) )

(def list->array
  #|($nm list)
   |(type function)
   |
   |List to array.
   |#
  %list->array)

(def\ (array . args)
  #|($nm . list)
   |(type function)
   |
   |List to array.
   |#
  (list->array args))

(def\ (arrayMap fun (#: Object[] arr))
  #|($nm function array)
   |(type function)
   |
   |Return a new array by applying the <b>function</b> to each element of the <b>array</b>.
   |#
  (list->array (map fun (array->list arr))) )

(assert (arrayMap 1+ (array 1 2 3)) (array 2 3 4))

(def\ (arrayFilter pred (#: Object[] arr))
  #|($nm function array)
   |(type function)
   |
   |Returns a new array with the elements of the <b>array</b> for which the application of the <b>function</b> returns #true.
   |#
  (list->array (filter pred (array->list arr))) )

(assert (arrayFilter odd? (array 1 2 3)) (array 1 3))

(def\ (newInstance class dim . dims)
  #|($nm class . dimensions)
   |(type function)
   |
   |(syntax dimension (integer . dimensions)
   |
   |Returns a new array with the elements of <b>class</b> tipe and the specified <b>dimensions</b>.
   |#
  (apply** @newInstance Array class dim dims))

(def\ (arrayGet array index)
  #|($nm array index)
   |(type function)
   |
   |Returns the element at <b>index</b> position int the <b>array</b>.
   |#
  (if (cons? index)
    (apply** arrayGet* array index)
    (@get Array array index) ))

(def\ (arrayGet* array . indexes)
  #|($nm array . indexes)
   |(type function)
   |
   |(syntax indexes (integer . indexes)
   |
   |Returns the element at <b>indexes</b> position int the multi-dimensions <b>array</b>.
   |#
  (let loop ((array array) (indexes indexes))
    (if (null? indexes) array
      (loop (arrayGet array (car indexes)) (cdr indexes)) )))

(def\ (arraySet array index value)
  #|($nm array index value)
   |(type function)
   |
   |Returns the <b>array</b> after update the element at <b>index</b> position with <b>value</b>.
   |#
  (if (cons? index)
    (apply** arraySet* array value index)
    (else (@set Array array index value) array) ))

(def\ (arraySet* array0 value . indexes)
  #|($nm array value . indexes)
   |(type function)
   |
   |(syntax indexes (integer . indexes)
   |
   |Returns the multi-dimensions <b>array</b> after update the element at <b>indexes</b> position with <b>value</b>.
   |#
  (if (null? indexes) array
    (let loop ((array array0) (indexes indexes))
       (if (null? (cdr indexes))
         (then (arraySet array (car indexes) value) array0)
         (loop (arrayGet array (car indexes)) (cdr indexes)) ))))

(assert (arrayGet (arraySet (newInstance &int 2 2) (1 1) 3) (1 1)) 3)
(assert (arrayGet* (arraySet* (newInstance &int 2 2) 3 1 1) 1 1) 3)


#|! Simple Sets
 |#

;(def set? /=) ; TODO solo se (/=) -> #t

(def\ (set? lst)
  #|($nm list)
   |(type function)
   |
   |Return #true if all element of <b>list</b> are distinct, #false otherwise.
   |#
  (if (null? lst) #t (apply /= lst)))

(def\ (set+ lst v)
  #|($nm set value)
   |(type function)
   |
   |#
  (if (member? v lst) lst (cons v lst)))

(defVau (defSet+ (#: Symbol plc) v) env
  #|($nm symbol value)
   |(type function)
   |
   |Return the set after update the set of <b>symbol</b> with an <b>value</b> more if <b>value</b> not is in the set.
   |#
  (let ( (v (eval v env)) (lst (env plc)) )
    (if (member? v lst) lst (env :def :rhs plc (cons v lst))) ))

(def\ ->set (lst)
  #|($nm list)
   |(type function)
   |
   |Return the <b>list</b> without duplicate elements.
   |#
  (let loop ( (res ()) (lst lst) )
    (if (null? lst) (reverse res)
      (let1 ((v . lst) lst)
        (loop (if (member? v res) res (cons v res)) lst) ))))

#|TODO da valutare
(def\ ->set (lst)
  #|($nm list)
   |(type function)
   |
   |Return the <b>list</b> without duplicate elements.
   |#
  (let1 loop (res ())
    (if (null? lst) (reverse res)
      (let1 (v (car lst))
        (set! lst (cdr lst))
        (loop (if (member? v res) res (cons v res))) ))))
|#


#|! Syntetic Expressions
 |Idea stolen from Anarki https://github.com/arclanguage/anarki
 |#

(defMacro %´ ((#: (or Symbol List) x))
  #|´a;b -> (compose a b)
   |´a;b;c -> (compose* a b c)
   |´!a -> (compose ! a)
   |´a…b -> (a b)
   |´a…'b -> (a 'b)
   |´a…'b…c -> (a 'b c)
   |´a,b -> (peval a b)
   |´a,'b,c -> (peval* a 'b c)
   |´a,_,'b,c -> (\ (_) (a _ 'b c))
   |´dd,_c,1,_b,"a",_a,_a,'c,_,:ff,_* -> (\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a 'c _ :ff _*))
   |´(dd _c 1 _b "a" _a _a 'c _ :ff _*) -> (\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a 'c _ :ff _*))
   |#
  (def (comma semicolon apostrophe ellipsis bang) (map symbol (array->list (@split ",;'…!" ""))))
  (def\ (mkc t)
    (def\ (pt t)
      (if
        (&& (symbol? t) (@startsWith (.name t) "_")) (cons t)
        (cons? t) (append (pt (car t)) (pt (cdr t)))
        #null ))
    (let1 (pt (sort (->set (pt t))))
      (if (null? pt)
        (cons (if (null? (cddr t)) 'peval 'peval*) t)
        (list* '\ (if (member? '_* pt) (append (remove [_ (== _ '_*)] pt) '_*) pt) (cons t)) )))
  (def\ (end? r . s*) (|| (null? r) (member? (car r) s*)) )
  (def\ (expd1 t r)
    (if (null? r)
      (if (null? (cdr t)) (car t) (cons (if (null? (cddr t)) 'compose 'compose*) (reverse t)))
      (let1 ((f . r) r)
        (if
          (== f bang) ;negate
            (if (end? r semicolon comma ellipsis) (%error "! without function")
              (expd1 (cons (list 'compose bang (car r)) t) (cdr r)) )
          (== f comma) ;peval
            (let1 ((f . r) (expd2 0 comma mkc (list (car r) (car t)) (cdr r)))
              (expd1 (cons f (cdr t)) r) )
          (== f ellipsis) ;eval
            (let1 ((f . r) (expd2 0 ellipsis idf (list (car r) (car t)) (cdr r)))
              (expd1 (cons f (cdr t)) r) )
          (expd1 (if (== f semicolon) t (cons f t)) r) ))))
  (def\ (switch sep a b) (if (== sep a) b a))
  (def\ (expd2 lev sep mk t r)
    (if (end? r semicolon)
        (cons (if (null? (cdr t)) (car t) (mk (reverse t))) (if (null? r) r (cdr r)))
      (end? r (switch sep comma ellipsis))
        (if (0? lev)
          (let1 ((f . r) (expd2 (1+ lev) (switch sep comma ellipsis) (switch mk mkc idf) (car t) r))
            (expd2 lev sep mk (cons f (cdr t)) r) )
          (cons (mk (reverse t)) r) )
      (let1 ((f . r) r)
        (if
          (== f apostrophe) ;quote
            (if (end? r semicolon sep) (%error "' without value")
              (expd2 lev sep mk (cons (list 'quote (car r)) t) (cdr r)) )
          (== f bang) ;not
            (if (end? r semicolon sep) (%error "! without value")
              (expd2 lev sep mk (cons (list bang (car r)) t) (cdr r)) )
          (== f sep)
            (if (member?* (car r) apostrophe bang)
              (expd2 lev sep mk t r)
              (expd2 lev sep mk (cons (car r) (if (cons? t) t (cons t))) (cdr r)) )
          (%error ("invalid syntax " f)) ))))
  (def\ (expd0 x)
     (map [_ (if (>= (@indexOf ";!,'…_" (%subString _ 0 1)) 0) (symbol _) (car (@str2lst vm _)))]
       (filter [_ (!= _ "")] (array->list (@splitWithDelimiters (name x) ";|!|,|'|…|_[1-9a-z*]?" -1)) )) )
  (if (symbol? x)
    (expd1 () (expd0 x))
    (mkc x)) )

(assert (expand ´a) 'a)
(assert (expand ´!a) '(compose ! a))
(assert (expand ´a;b) '(compose a b))
(assert (expand ´a;!b;c) '(compose* a (compose ! b) c))

(assert (expand ´cc) 'cc)
(assert (expand ´cc,1) '(peval cc 1))
(assert (expand ´cc,1,2) '(peval* cc 1 2))
(assert (expand ´cc,1,'a,2) '(peval* cc 1 (quote a) 2))
(assert (expand ´cc,1,_,2) '(\ (_) (cc 1 _ 2)))

(assert (expand ´cc) 'cc)
(assert (expand ´cc…1) '(cc 1))
(assert (expand ´cc…1…2) '(cc 1 2))
(assert (expand ´cc…1…'a…2) '(cc 1 (quote a) 2))

(assert (expand ´a;!b;cc,1,'d;e) '(compose* a (compose ! b) (peval* cc 1 (quote d)) e))
(assert (expand ´a;!b;cc,1,'d;ee…2…'f;g) '(compose* a (compose ! b) (peval* cc 1 (quote d)) (ee 2 (quote f)) g))
(assert (expand ´a;!b;cc,1,'d;ee…2…'f…:gg;h) '(compose* a (compose ! b) (peval* cc 1 (quote d)) (ee 2 (quote f) :gg) h))

(assert (expand ´cc,1,bb…2) '(peval* cc 1 (bb 2)))
(assert (expand ´cc…1…bb,2) '(cc 1 (peval bb 2)))
(assert (expand ´aa,bb…2,cc…3,dd) '(peval* aa (bb 2) (cc 3) dd))
(assert (expand ´aa,bb…2,3;dd) '(compose (peval* aa (bb 2) 3) dd))
(assert (expand ´aa,bb…2…'b…!x,3;dd) '(compose (peval* aa (bb 2 (quote b) (! x)) 3) dd))

(assert (expand ´dd,_c,1,_b,"a",_a,_a,'c,_,:ff,_*) '(\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a (quote c) _ :ff _*)) )
(assert (expand ´(dd _c 1 _b "a" _a _a 'c _ :ff _*) '(\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a 'c _ :ff _*)) ))


#|! Dynamic Variables
 |(type Class)
 |(extends Box)
 |
 |The form ddef ddef* dlet progv and dlet* are defined as macro using the primitive operator <b>%dv\</b> that return a function.   
 |
 |(<b>%dv\</b> symbols . forms)
 |(type fexpr)
 |
 |(syntax symbols (symbol . symbols))
 |
 |(derivation
 |  (vau (var* . forms) #ignore
 |    (wrau val* env
 |        (def\ (ckdvar var)
 |          (def lkp (@get env var))
 |          (def ndv (.value lkp))
 |          ;(if (or (and (null? forms) (null? ndv)) (type? ndv DVar)) ndv
 |          ;  (error ($ "not " (if (null? forms) "null or " "") "a dynamic value: " var)) )
 |          (if (or (and (null? forms) (! (.isBound lkp))) (type? ndv DVar)) ndv
 |            (error ($ "not " (if (null? forms) "unbound or " "") "a dynamic value: " var)) ))
 |        (def ndv* (map ckdvar var*))
 |        (unless (null? forms) (def old* (map (\ (ndv) (if (null? ndv) ndv (ndv))) ndv*)))
 |        (forEach (\ (ndv var val) (if (type? ndv DVar) (ndv val) (env var (newDVar val)) )) ndv* var* (if (null? val*) (map (\ (var) (boxDft)) var*) val*))
 |        (unless (null? forms)
 |          (atEnd
 |            (forEach (\ (ndv old) (ndv old)) ndv* old*)  
 |            (eval (list* 'begin forms) env) )))))
 |#

(def newDVar
  #|($nm . value)
   |(type function)
   |
   |Define a new dynamic variable with optional <b>value</b>.
   |#
  %newDVar)

(def dval
  #|($nm dynamicVariable)
   |(type function)
   |
   |Return the current value of the <b>dynamicVariable</b>.
   |#
  %dVal)

(defMacro (ddef var . val)
  #|($nm name . value)
   |(type macro)
   |
   |Define a new or update an existing dynamic variable with the given <b>name</b> and optional <b>value</b>.
   |#
  (list* (list '%dv\ (list var)) val) )

(defMacro (ddef* var* . val*)
  #|($nm names . values)
   |(type macro)
   |(syntax names (name . names))
   |
   |Define a new or update an existing dynamic variable with the given <b>names</b> and optional <b>values</b>.
   |#
  (list* (list '%dv\ var*) val*) )

(def\ (dget dvar)
  #|($nm dynamicVariable)
   |(type macro)
   |
   |Return the current value of the <b>dynamicVariable</b>.
   |#
  (dvar))

(def\ (dset dvar value)
  #|($nm dynamicVariable value)
   |(type macro)
   |
   |Set the current value of the <b>dynamicVariable</b>.
   |#
  (dvar value))

(defMacro (dlet bindings form . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((name value) . bindings))
   |
   |With the dynamic variables specified by <b>names</b> temporarily bound to new <b>values</b>, evaluate <b>forms</b> as an implicit `begin'.
   |Bindings are established parallely as per `let'.
   |#
  (cons (list* '%dv\ (map car bindings) form forms) (map cadr bindings)) )

(defMacro (dlet1 binding form . forms)
  #|($nm binding . forms)
   |(type macro)
   |
   |(syntax binding (name value))
   |
   |With the dynamic variables specified by <b>name</b> temporarily bound to new <b>value</b>, evaluate <b>forms</b> as an implicit `begin'.
   |Bindings are established parallely as per `let'.
   |#
  (list* 'dlet (cons binding) form forms) )

(defMacro (progv var* val* exp . exps)
  #|($nm names values . forms)
   |(type fexpr)
   |
   |(syntax names (name . names))
   |(syntax values (value . values))
   |(type macro)
   |
   |With the dynamic variables specified by <b>names</b> temporarily bound to new <b>values</b>, evaluate <b>forms</b> as an implicit `begin'.
   |The <b>names</b> and <b>values</b> lists must have the same length.
   |#
  (cons (list* '%dv\ var* exp exps) val*) )

(defMacro (dlet* bindings . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((name value) . bindings))
   |
   |With the dynamic variables specified by <b>names</b> temporarily bound to new <b>values</b>, evaluate <b>forms</b> as an implicit `begin'.
   |Bindings are established serially as per `let*'.
   |#
  (if (null? bindings)
    (cons 'begin forms)
    (list 'dlet
      (list (car bindings))
      (list* 'dlet* (cdr bindings) forms) )))

(let ()
	(def a (newDVar 1))
	(assert (expand (ddef a 1)) '((%dv\ (a)) 1) )
	(assert (expand (ddef* (a b) 1 2)) '((%dv\ (a b)) 1 2) )
	(assert (expand (progv (a b) (3 4)  (+ (a) (b)))) '((%dv\ (a b) (+ (a) (b))) 3 4) )
	(assert (expand (dlet ((a 3) (b 4)) (+ (a) (b)))) '((%dv\ (a b) (+ (a) (b))) 3 4) )
	(ddef* (a b) 1 2)
	(assert (progv (a b) (3 4)  (+ (a) (b))) 7)
	(assert (dlet ((a 3) (b 4)) (+ (a) (b))) 7)
	(assert (begin (ddef a 1) (progv (a) (2) (assert (dval a) 2)) (assert (dval a) 1)) #t)
	(assert (begin (ddef* (a b) 1 2) (dlet ((a 2)) (assert (dval a) 2)) (dval b)) 2)
	(assert (begin (ddef* (a) 1) (dlet* ((a (+ 1 (dval a))) (a (+ 1 (dval a)))) (dval a))) 3)
)


#|! Classes
 |#

(def\ findClass ((#: Symbol name) env)
  #|($nm name environment)
   |(type function)
   |
   |Look up a class based on its <b>name</b> symbol (evaluated) in the given <b>environment</b>.
   |#
  (eval name env))

(defVau defClass (name (#: (0 1 Symbol) superClass) (#: (Symbol) attributes) . properties) env
  #|($nm superclass attributes . properties)
   |(type fexpr)
   |
   |(syntax superclass (or () (symbol)))
   |(syntax attributes (symbol . attributes))
   |
   |Define a new `ObjClass' with the given <b>name</b>, optional <b>superclass</b>, and <b>attributes</b>.
   |The <b>superclass</b> defaults to `Obj'.
   |The <b>attributes</b> and <b>properties</b> are currently ignored.
   |#
  ;; Attribute-specs are ignored for now, but check that they are symbols nevertheless.
  (def superClass (findClass (optDft superClass 'Obj) env))
  (eval (list 'def name (%newClass name superClass)) env) )


#|! Generic Functions
 |#

;; receiverName e parameters dei defMethod dovrebbero corrispondere a quelli del proprio defGeneric

(defVau (defGeneric . args) env
  #|($nm name receiverName . parameters)
   |($nm (name receiverName) . parameters)
   |(type fexpr)
   |
   |Define a new generic function with the given <b>name</b>.
   |<b>receiverName</b>, <b>parameters</b>, and <b>properties</b> are currently ignored.
   |#
  (if (cons? (car args))
    (def ((name receiverName . parameters) . properties) args)
    (def (name (receiverName . parameters) . properties) args) )
  (let1\ (generic args ((%getMethod (classOf (car args)) name) args))
    (eval (list 'def name generic) env) ))

(defVau (defMethod . args) env
  #|($nm ((name (receiverName class) . parameters) . forms))
   |($nm (name ((receiverName class) . parameters) . forms))
   |(type fexpr)
   |
   |Define a new method for the generic function <b>name</b> specialized for <b>class</b>.
   |#
  (if (cons? (car args))
    (def ((name (receiverName class) . parameters) . forms) args)
    (def (name ((receiverName class) . parameters) . forms) args) )
  (def\ (method args)
    (apply
      (eval
        (list* '\ (cons receiverName parameters) forms)
        (let1 (receiver (car args)) (if (type? receiver Obj) (newEnv env receiver) env)) )
      args ))
  (def prv (%addMethod (eval class env) name method))
  (case (bndRes) (:rhs method) (:prv prv) (else #inert)) )

(let ()
  (defClass Foo () ())
  (defClass Bar (Foo) (a b))
  (defGeneric g1 (obj p))
  (defMethod g1 ((foo Foo) p) (+ p 100))

  (defNew foo Foo)
  (defNew bar Bar :a 1 :b (+ 2 3))

  (assert (g1 foo (+ 1 1)) 102)
  (assert (g1 bar (+ 2 3)) 105)

  (defMethod (g1 (bar Bar) p) (+ p (bar :b)))

  (assert (bar :b) 5)
  (assert (g1 bar 2) 7)
  (assert (g1 bar (* 2 (bar :b))) 15)
  (assert (bar :prv :b 6) 5)
  (assert (bar :b) 6)

  (defMethod (g1 (bar Bar) p) (* p (+ a b)))
  (assert (g1 bar 3) 21)
)


#|! Modules
 |#

(defVau (provide symbols . forms) env
  #|($nm symbols . forms)
   |(type fexpr)
   |
   |(syntax symbols (symbol . symbols))
   |
   |Defines <b>symbols</b> in the current environment with values generated evaluating <b>forms</b> as an implicit `begin'.
   |#
  (eval
    (list 'def symbols
      (list 'let ()
        (cons 'begin forms)
        (cons 'list symbols) ))
    env ))

(assert (begin (provide (x) (def x 10)) x) 10)

(defVau (module symbols . forms) env
  #|($nm symbols . forms)
   |(type fexpr)
   |
   |(syntax symbols (symbol . symbols))
   |
   |Return an environment where <b>symbols</b> are defined with values generated evaluating <b>forms</b> as an implicit `begin'.
   |#
  (let1 (env (newEnv env))
    (eval (list* 'provide symbols forms) env)
    (newEnv env) ))

(assert (begin (def m (module (x) (def x 10))) (eval 'x m)) 10)

(defMacro (defModule name symbols . forms)
  #|($nm name symbols . forms)
   |(type macro)
   |
   |(syntax symbols (symbol . symbols))
   |
   |Define a `environment' with the given <b>name</b>, where <b>symbols</b> are defined with values generated evaluating <b>forms</b> as an implicit `begin'.
   |#
  (list 'def name (list* 'module symbols forms)) )

(assert (begin (defModule m (x) (def x 10)) (eval 'x m)) 10)

(defVau (import module symbols) env
  #|($nm module symbols)
   |(type fexpr)
   |
   |(syntax symbols (symbol . symbols))
   |
   |Define in the current environment the <b>symbols</b> defined in <b>module</b>.
   |#
  (let* ((module (eval module env))
         (values (eval (cons 'list symbols) module)) )
    (eval (list* 'def* symbols values) env) ))

(assert (begin (defModule m (x) (def x 10)) (import m (x)) x) 10)


#|! Relational Operators
 |Note that unlike in Common Lisp, these operators currently require at least two arguments.
 |This will be improved in the future.
 |#

(def\ (relationalOp binop)
  #|($nm binop)
   |(type function)
   |
   |Utility to create an n-ary relational operator from a <b>binaryOperator</b>.
   |#
  (rec\ (op arg1 arg2 . args)
    (if (binop arg1 arg2)
      (if (null? args) #t
        (apply op (cons arg2 args)))
      #f )))

(def <
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp <))
   |
   |Return #true if the <b>arguments</b> are in monotonically increasing order, #false otherwise.
   |#
  (relationalOp <) )

(def >
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp >))
   |
   |Return #true if the <b>arguments</b> are in monotonically decreasing order, #false otherwise.
   |#
  (relationalOp >) )

(def <=
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp <=))
   |
   |Return #true if the <b>arguments</b> are in monotonically nondecreasing order, #false otherwise.
   |#
  (relationalOp <=) )

(def >=
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp >=))
   |
   |Return #true if the <b>arguments</b> are in monotonically nonincreasing order, #false otherwise.
   |#
  (relationalOp >=) )

(def eq?
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp eq?))
   |
   |Return #true if all <b>arguments</b> are equal, #false otherwise.
   |#
  (relationalOp eq?) )

(def ==
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp eq?))
   |
   |Return #true if all <b>arguments</b> are ==, #false otherwise.
   |#
  (relationalOp ==))

(assert (< 1 2 3) #t)
(assert (> 3 2 1) #t)
(assert (<= 1 2 2 3) #t)
(assert (>= 3 2 2 1) #t)
(assert (eq? (1) (1) (1)) #t)
(assert (== 1 1 1) #t)

(def !=
  #|($nm arguments)
   |(type function)
   |
   |(derivation (relationalOp !=))
   |
   |Return #false if all <b>arguments</b> are ==, #true otherwise.
   |#
  (relationalOp !=))

(def\ /= (arg . args)
  #|($nm argument . arguments)
   |(type function)
   |
   |Return #true if all <b>arguments</b> are distinct, #false otherwise.
   |#
  (if (null? args) #t
    (if (member? arg args :cmp eq?) #f
      (apply /= args) )))

#|TODO in sostituzione del prededente, utile?
(def\ /= args
  #|($nm . arguments)
   |(type function)
   |
   |Return #true if all <b>arguments</b> are distinct, #false otherwise.
   |#
  (if (null? args) #t
    (let1 ((arg . args) args)
      (if (member? arg args :cmp eq?) #f
        (apply /= args) ))))
|#

(assert (!= 1 1 1) #f)
(assert (!= 1 2 1) #t)
(assert (/= 1 2 3) #t)
(assert (/= 1 2 1) #f)


#|! Thetics & Lytics
 |The terms thetic (for + and *) and lytic (for - and /) are due to Hankel.
 |#

(def\ (theticOp binOp unit)
  #|($nm binOp unit)
   |(type function)
   |
   |Return an n-ary thetic operator from a <b>binaryOperator</b> and <b>initialValue</b>.
   |#
  (\ args (reduce binOp unit args)) )

(def +
  #|($nm arguments)
   |(type function)
   |
   |(derivation (tethicOp + 0))
   |
   |Return the sum of the <b>arguments</b>, or 0 if no arguments are supplied.
   |#
  (theticOp + 0) )

(def *
  #|($nm arguments)
   |(type function)
   |
   |(derivation (tethicOp * 1))
   |
   |Return the product of the <b>arguments</b>, or 1 if no arguments are supplied.
   |#
  (theticOp * 1) )

(def $
  #|($nm arguments)
   |(type function)
   |
   |(derivation (tethicOp $ ""))
   |
   |Return the join of the <b>arguments</b>, or "" if no arguments are supplied.
   |#
  (theticOp $ "") )

(assert (+ 1 2 3) 6)
(assert (* 1 2 3) 6)
(assert ($ 1 2 3) "123")

(def\ (lyticOp binOp unit)
  #|($nm binOp unit)
   |(type function)
   |
   |Return an n-ary lytic operator from a <b>binaryOperator</b> and <b>initialValue</b>.
   |#
  (\ (arg1 . args)
    (if (null? args)
      (binOp unit arg1)
      (reduce binOp arg1 args) )))

(def -
  #|($nm arguments)
   |(type function)
   |
   |(derivation (lithicOp - 0))
   |
   |If only one number is supplied in the <b>arguments</b>, return the negation of that number.
   |If more than one number is supplied, subtract all of the later ones from the first one and return the result.
   |#
  (lyticOp - 0) )

(def /
  #|($nm arguments)
   |(type function)
   |
   |(derivation (lithicOp / 1))
   |
   |If only one number is supplied in the <b>arguments</b>, return the reciprocal of that number.
   |If more than one number is supplied, divide the first one by all of the later ones and return the result.
   |#
  (lyticOp / 1) )


#|! Greatest Common Divisor e Lowest Common Multiple
 |#

(def\ (gcd a b . args)
  #|($nm integer integer . integers)
   |(type function)
   |
   |(syntax integers (integer . integers))
   |
   |Return the greatest common divisor of two or more integer arguments.
   |#
  (if (null? args)
    (if (0? b) a (gcd b (% a b)))
    (gcd a (apply gcd (cons b args))) ))

(def abs
  #|($nm integer)
   |(type function)
   |
   |Return the abs value of one integer argument.
   |#
  (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))

(assert (gcd 8 108) 4)
(assert (gcd 108 216 432) 108)

(def\ (lcm a b . args)
  #|($nm integer integer . integers)
   |(type function)
   |
   |(syntax integers (integer . integers))
   |
   |Return the lowest common multiple of two or more integer arguments
   |#
  (if (null? args)
    (if (|| (0? a) (0? b)) 0
      (abs (* b (/ a (gcd a b)))) )
    (lcm a (apply lcm (cons b args))) ))

(assert (lcm 8 108) 216)
(assert (lcm 3 4 5 6) 60)


#|! Sequences
 |#

(defGeneric length (sequence)
  #|($nm sequence)
   |(type generic)
   |
   |Return the number of elements in a <b>sequence</b>.
   |#
)
(defMethod length ((seq Null))
  0)
(defMethod length ((seq List))
  (%len seq))
(defMethod length ((seq String))
  (@length seq))
(defMethod length ((seq Object[]))
  (.length seq))


(defGeneric elt (sequence index)
  #|($nm sequence index)
   |(type generic)
   |
   |Return the <b>sequence</b> element at the specified <b>index</b>.
   |#
)
(defMethod elt ((seq List) index)
  (nth index seq))
(defMethod elt ((seq String) index)
  (%subString seq index (+ index 1)))
(defMethod elt ((seq Object[]) index)
  (@get Array seq index))


(defGeneric subSeq (sequence start . end)
  #|($nm sequence start . end)
   |(type generic)
   |
   |Create a sequence that is a copy of the subsequence of the <b>sequence</b> bounded by <b>start</b> and optional <b>end</b>.
   |If <b>end</b> is not supplied, the subsequence stretches until the end of the sequence.
   |#
)
(defMethod subSeq ((seq List) start . end)
  (apply** %subList seq start end))
(defMethod subSeq ((seq String) start . end)
  (apply** %subString seq start end))
(defMethod subSeq ((seq Object[]) start . end)
  (apply** %subArray seq start end))


#|! Coroutines
 |#

(defConstant coroutinePrompt
  #|This prompt is used for general coroutine-like use of continuations.
   |#
  'coroutine-prompt)

(defMacro coroutine forms
  #|Evaluate <b>forms</b> as an implicit `begin' in a context in which `yield' can be used to pause execution.
   |#
  (list* 'pushPrompt 'coroutinePrompt forms))

(defMacro yield (name . forms)
  #|($nm name . forms)
   |(type macro)
   |
   |Pause the current coroutine.
   |In the place where the enclosing `coroutine' (or `resume') was called,
   |evaluate <b>forms</b> as an implicit `begin' with <b>name</b> bound to the paused coroutine.
   |`resume' can later be used to restart execution inside the coroutine.
   |#
  (list* 'takeSubcont 'coroutinePrompt name forms))

(defMacro resume (k . forms)
  #|($nm continuation . forms)
   |(type macro)
   |
   |Resume the paused coroutine <b>continuation</b> and evaluate <b>forms</b> as an implicit `begin' in the place where `yield' was called in the coroutine, and return the result.
   |#
  (list* 'pushDelimSubcont 'coroutinePrompt k forms))


#|! Fibers
 |The following implementation of fibers follows the one at <b>url</b> `http://okmij.org/ftp/continuations/implementations.html#dget-wind'
 |We're calling them fibers instead of coroutines so as to not conflict with the built-in coroutine operators.
 |We use it for testing that built-in operators properly suspend and resume.
 |#

(defConstant fiberPrompt
  #|The prompt used for delimiting fibers.
   |#
  'fiber-prompt)

(defClass YieldRecord ()
  #|(type Class)
   |(extends Obj)
   |(attributes value continuation)
   |
   |Instances of this class are yielded.
   |#
  (value continuation) )

(def\ makeYieldRecord (v k)
  #|($nm value continuation)
   |(type function)
   |
   |Create a new yield record with the given yielded <b>value</b> and resume <b>continuation</b>.
   |#
  (new YieldRecord :value v :continuation k))

(def\ fiberYield v
  #|($nm . value)
   |(type function)
   |
   |Yield a optional <b>value</b> (which defaults to #inert).
   |#
  (takeSubcont fiberPrompt k
    (makeYieldRecord (optDft v #inert) k)))

(def\ fiberResume (yieldRecord . v)
  #|($nm yieldRecord . value)
   |(type function)
   |
   |Resume a suspended fiber <b>yieldRecord</b> with an optional <b>value</b> (which defaults to #inert).
   |#
  (pushDelimSubcont fiberPrompt (yieldRecord 'continuation)
    (optDft v #inert)))

(defMacro fiber forms
  #|($nm . forms)
   |(type macro)
   |
   |Evaluate the forms expressions as a fiber.
   |#
  (list* pushPrompt 'fiberPrompt forms))

(def\ runFiber (thunk . values)
  #|($nm thunk . forms)
   |(type macro)
   |
   |Get all values yielded by a fiber, and its final result, and collect them in a list.
   |Uses the optional list of values to sent to the fiber with `fiberResume'.
   |#
  (let run ((result (fiber (thunk))) (values values))
    (if (type? result YieldRecord)
      (cons (result 'value)
        (if (null? values)
          (run (fiberResume result) #null)
          (run (fiberResume result (car values)) (cdr values)) ))
      (list result) )))

(assert (runFiber (\ () (fiberYield 1) (fiberYield 2) 3)) '(1 2 3))

(assert (runFiber (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t) '(1 2 #inert))
(assert (runFiber (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t #_) '(1 2 #ignore))
(assert (runFiber (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t 4) '(1 2 4))
(assert (runFiber (\ () (if (fiberYield 1) (fiberYield 2) 3)) #f) '(1 3))

(assert (runFiber (\ () ((\ (a b) (+ a b)) (fiberYield 1) (fiberYield 2)) ) 3 4) '(1 2 7))

(defMacro (runFiberWithValues f args) (list 'apply** 'runFiber f args))

(assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) '(#inert 3)) (1 2 3))
(assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) (#inert 3)) (1 2 3))


#|! Auto Increment/Decrement and Assignement Operators
 |#

(defVau (++ plc . args) env
  #|($nm number)
   |($nm box)
   |($nm obj field)
   |(type fexpr)
   |
   |Return the incremented value of Number, Box and Obj.
   |#
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (1+ (val)))))
    (Obj    (let1 ((fld) args) (val :rhs fld (1+ (val fld)))))
    (Number (let1 (() args) (env :set! :rhs plc (1+ val))))
    (else   (error ($ "not valid type: " val))) ))

(defVau (-- plc . args) env
  #|($nm number)
   |($nm box)
   |($nm obj field)
   |(type fexpr)
   |
   |Return the decrenented value of Number, Box and Obj.
   |#
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (-1+ (val)))))
    (Obj    (let1 ((fld) args) (val :rhs fld (-1+ (val fld)))))
    (Number (let1 (() args) (env :set! :rhs plc (-1+ val))))
    (else   (error ($ "not valid type: " val))) ))

(assert (let () (def obj (newObj :a 1)) (++ obj :a) (++ obj :a) (-- obj :a)) 2)
(assert (let () (def box (newBox 1)) (++ box) (++ box) (-- box)) 2)
(assert (let () (def n 1) (++ n) (++ n) (-- n)) 2)

(def\ (assignOp op)
  #|($nm operator)
   |(type function)
   |
   |Return a fexpr which returns the value assigned to Box, Obj and Object after applying the <b>operator</b>.
   |#
  (vau (plc . args) env
    (def lval (env plc))
    (caseType lval
      (Box (match args
        ((rval) (lval (op (lval) (eval rval env))))
        ((key rval) (lval key (op (lval) (eval rval env)))) ))
      (Obj (match args
        ((fld rval) (lval fld (op (lval fld) (eval rval env))))
        ((key fld rval) (lval key fld (op (lval fld) (eval rval env)))) ))
      (Object (match args
        ((rval) (env :set! plc (op lval (eval rval env))))
        ((key rval) (env :set! key plc (op lval (eval rval env)))) )))))

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (newBox 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (newObj :fld 1)) (+= a :rhs :fld 3)) 4)


#|! Java
 |Classes
 |  &canonicalClassName
 |
 |  &int
 |  &java.lang.Integer
 |  &Wat.Vm$Cons
 |  &Object[]
 |
 |Costructors
 |  (@new class . arguments)
 |  (@new class containerInstance . arguments)
 |
 |  (@new &java.lang.String "abc")
 |  (@new &Wat.Vm$Cons vm 1 2)
 |
 |Fields
 |  (.fieldName object)
 |
 |  (.MAX_VALUE &java.lang.Integer)
 |  (.car cons)
 |
 |Methods
 |  (@methodName object . arguments)
 |
 |  (@toString cons)
 |  (@doubleValue 10)
 |
 |  (@getField &java.lang.Integer "MAX_VALUE")
 |  (@getDeclaredField &Wat.Vm$Cons "car")
 |  (@getConstructor &java.lang.Integer &java.lang.String)
 |  (@getMethod &java.lang.Integer "compare" &int &int)
 |  (@getConstructor &java.lang.Character &char)
 |
 |Utility
 |  (@getField &Wat.Utility &java.lang.Integer "MAX_VALUE")
 |  (@getField &Wat.Utility &Wat.Vm$Cons "car")
 |  (@getExecutable &Wat.Utility &java.lang.Integer "new" &java.lang.String)
 |  (@getExecutable &Wat.Utility &java.lang.Integer "compare")
 |  (@getExecutable &Wat.Utility &java.lang.Character "new")
 |#

(def getMethod (@getMethod Class "getMethod" String Class[]))
(def getDeclaredMethod (getMethod Class "getDeclaredMethod" String Class[]))
(def getMethods (getMethod Class "getMethods"))
(def getDeclaredMethods (getMethod Class "getDeclaredMethods"))
(def getConstructor (getMethod Class "getConstructor" Class[]))
(def getDeclaredConstructor (getMethod Class "getDeclaredConstructor" Class[]))
(def getConstructors (getMethod Class "getConstructors"))
(def getDeclaredConstructors (getMethod Class "getDeclaredConstructors"))
(def getField (getMethod Class "getField" String))
(def getDeclaredField (getMethod Class "getDeclaredField" String))
(def getFields (getMethod Class "getFields"))
(def getDeclaredFields (getMethod Class "getDeclaredFields"))

(def getExecutable
  #|($nm Class name . classes)
   |(type function)
   |
   |Return a constructor or a method with given name also without specify parameter classes.
   |#
  (let1 (getExecutable (getMethod Utility "getExecutable" Object String Class[]))
    (\ (class name . classes)
      (apply** getExecutable #null class name classes) )))

(def supplier
  #|($nm () . forms)
   |
   |Return a java Supplier.
   |#
  %supplier)

(def consumer
  #|($nm (symbol) . forms)
   |
   |Return a java Consumer.
   |#
  %consumer)

(def function
  #|($nm (symbol) . forms)
   |
   |Return a java Function.
   |#
  %function)

(def biConsumer
  #|($nm (symbol symbol) . forms)
   |
   |Return a java BiConsumer.
   |#
  %biConsumer)

(def biFunction
  #|($nm (symbol symbol) . forms)
   |
   |Return a java BiFunction.
   |#
  %biFunction)

(defMacro (close1 binding . forms)
  #|($nm binding . forms)
   |(type macro)
   |
   |(syntax binding (name . initForms))
   |
   |Single try/resource
   |#
  (list 'let1 binding
    (list* 'atEnd
      (list '@close (car binding))
      forms )))

(defMacro (close bindings . forms)
  #|($nm bindings . forms)
   |(type macro)
   |
   |(syntax bindings ((name . initForms) . bindings))
   |
   |Multiple try/resource
   |#
  (list 'let bindings
    (list* 'atEnd
      (list 'forEach '@close (cons 'list (map car bindings)))
      forms )))


#|! Apl/J
 |#

(def\ (iota n) (reverse ((rec\ (iota n) (if (0? n) () (cons n (iota (-1+ n))))) n)))
(def\ (fork f l r) [_ (f (l _) (r _))])
(def\ (hook l r) [_ (l _ (r _))])


#|! Utility
 |#

(def vm
  #|The virtual machine.
   |#
 vm)
 
(def toString
  #|($nm object)
   |(type function)
   |
   |internal to string function
   |#
  toString)

(def log
  #|($nm . arguments)
   |(type function)
   |
   |Log all <b>arguments</b> to console and return the value of the first if present, #inert otherwise.
   |#
  log)

(def print
  #|($nm . arguments)
   |(type function)
   |
   |Print all <b>arguments</b> to console and return the value of the last argument if present, #inert otherwise.
   |#
  print)

(def write
  #|($nm . arguments)
   |(type function)
   |
   |Write all <b>arguments</b> to console and return the value of the last argument if present, #inert otherwise.
   |#
write)

(def load
  #|($nm fileName . environment)
   |(type function)
   |
   |load <b>fileName</b> in the optional <b>environment</b>.
   |#
load)

(def read
  #|($nm)
   |(type function)
   |
   |Read expression from console.
   |#
  read)

(def readString
  #|($nm string)
   |(type function)
   |
   |Read expression from string
   |#
  readString)

(def system
  #|($nm string)
   |(type function)
   |
   |Exec system commands
   |#
  system)

(defVau (time times . forms) env
  #|($nm n . forms)
   |(type fexpr)
   |
   |Return the elapsed time in milliseconds after evaluate n times <b>forms</b> as implicit `begin'
   |#
  (let* ( (currentTime (@getMethod System "currentTimeMillis"))
          ((#: (and Integer (> 0)) times) (eval times env))
          (milli (currentTime #null))
          (result (apply repeat (cons times forms) env))
          (milli (- (currentTime #null) milli)) )
    (print "time " times " " (if (null? (cdr forms)) (car forms) (cons 'begin forms)) ": " milli "ms" (if (== times 1) "" ($ ", on average: " (@format String "%.2f" (/ milli (@doubleValue times))) "ms" )))
    result ))

(def\ (make\* n f)
  #|($nm n function)
   |(type function)
   |
   |Returns a function that applies <b>function</b> with the arguments from n placed in a list.
   |#
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))) )

(assert ((make\* 2 (\ (a b) b)) 1 2 3 4 5) '(2 3 4 5))

(def\ (minMax a . b)
  #|($nm . values)
   |(type function)
   |
   |Returns a cons with the min and the max of the <b>values</b>.
   |#
  (let loop [ (x ((caseType a (Double .POSITIVE_INFINITY) (Long .MAX_VALUE) (Integer .MAX_VALUE)) a))
              (y ((caseType a (Double .NEGATIVE_INFINITY) (Long .MIN_VALUE) (Integer .MIN_VALUE)) a))
              (a (cons a b)) ]
    (if (null? a) (cons x y)
      (loop (min x (car a)) (max y (car a)) (cdr a)) )))


#|! Configuration
 |#

(def intStr
  #|($nm)
   |(type function)
   |
   |Return #true if the string are interned, #false otherwise.
   |#
  intStr)

(def doTco
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the use of the tail call optimization.
   |#
  doTco)

(def doAsrt
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the execution of the assert.
   |#
  doAsrt)

(def prStk
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the print of the stack for uncatched errors.
   |#
  prStk)

(def prAttr
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the print of the attributes for the uncatched errors.
   |#
  prAttr)

(def prWrn
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the print of warnings.
   |#
 prWrn)

(def aQuote
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the auto quote property for the list without combinable car. 
   |#
aQuote)

(def hdlAny
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the use of an arbitraty value for the catch handler.
   |#
  hdlAny)

(def prInert
  #|($nm . boolean)
   |(type function)
   |
   |Return or update the print of #inert value in the repl
   |#
  prInert)

(def typeT
  #|($nm . integer)
   |(type function)
   |
   |Return or update the type of #true.
   |
   |- 0: #true
   |- 1: !#false
   |- 2: !(or #false #null)
   |- 3: !(or #false #null #inert)
   |- 4: !(or #false #null #inert 0)
   |#
  typeT)

(def bndRes
  #|($nm . integer)
   |(type function)
   |
   |Return or update the type of the bind result.
   |
   |- 0: #inert
   |- 1: the value used to set the last binding
   |- 2: the previous value of the last binding
   |- 3: the container obj or environment
   |#
  bndRes)

(def prTrc
  #|($nm . integer)
   |(type function)
   |
   |Return or update the type of the trace.
   |
   |- 0: none
   |- 1: files loaded
   |- 2: root evaluations
   |- 3: all deep evaluations
   |- 4: values returned
   |- 5: applications executed
   |- 6: bind/lookup symbols
   |#
  prTrc)

(def prEnv
  #|($nm . integer)
   |(type function)
   |
   |Return or update the max number of attributes for printable environments.
   |#
  prEnv)

(def boxDft
  #|($nm . value)
   |(type function)
   |
   |Return or update the <b>value</b> to use as default value for the Box.
   |#
  boxDft)
