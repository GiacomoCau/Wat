;;;                                                     -*- mode: Scheme -*-
;;; Wat Bootstrap
;;;

;;; Copyright (c) 2021, 2022 Manuel J. Simoni

;;; ``72. An adequate bootstrap is a contradiction in terms.''

#|! Core Built-Ins for Macro and Definition Forms
 |#

(%def def
  #|Defines into the current environment the resulting bindings from the Match of the DEFINIEND-TREE against VALUE, signals an error otherwise.
   |
   |without bindResult or with bindResult #ignore use as bindResult (bndRes)
   |with bindResult #inert return #inert
   |with bindResult :rhs return the right side of the last binding
   |with bindResult :prv return the previous value of the last binding
   |with bindResult :cnt return the env
   |
   |$(fn definiendTree value)
   |$(fn definiendTree bindResult value)
   |$(syntax bindResult (or #ignore #inert :rhs :prv))
   |$(type fexpr)
   |#
  %def )

(def vau
  #|Return a anonymous fexpr with the given PARAMETER-TREE, ENVIRONMENT-PARAMETER and FORMS as body.
   |
   |$(fn parameterTree environmentParameter . forms)
   |$(type fexpr)
   |$(derivation (vau (parameterTree environmentParameter . forms) env (eval (vau parameterTree environmentParameter (begin . forms)) env)))
   |#
  %vau )

(def \
  #|Return an anonymous function with the given PARAMETER-TREE and FORMS as body,
   |which accesses to the definition environment for the values of free variables.
   |The classic Scheme static lambda.
   |
   |$(fn parameterTree . forms)
   |$(type function)
   |$(derivation (vau (parameterTree . forms) env (wrap (eval (list* 'vau parameterTree #ignore forms) env))))
   |#
  %\ )

(def lambda
  #|alias for \ :-).
   |#
  \ )

(def wrap
  #|Return a new function that wraps around an underlying OPERATOR, and induces argument evaluation around it.
   |
   |$(fn operator)
   |$(type function)
   |#
  %wrap )

(def assert
  #|Signals an error if:
   |- EXPRESSION does not equal VALUE
   |- VALUE is not present and the EXPRESSION does not throws
   |- EXPRESSION throws or returns an object and the object is not of the same CLASS with the ATTRIBUTES of the given VALUE.
   |
   |$(fn expression value)
   |$(fn expression)
   |$(fn expression class attribute value . attributes)
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String .Field @Method))
   |$(type fexpr)
   |#
  %assert )

(def apply
  #|Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.
   |
   |$(fn fun args . env)
   |$(type function)
   |$(derivation (eval (cons (unwrap fun) args) (if (null? env) (newEnv) (car! env)) ))
   |#
  %apply )

(def begin
  #|Sequentially evaluate FORMS returning the value of the last one or #inert if FORMS is #null.
   |
   |$(fn . forms)
   |$(type fexpr)
   |#
  %begin )

(def car
  #|Return the contents of the address part of the register.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((car . #_)) car)) cons)
   |$(derivation (@car cons))
   |#
  %car )

(def car!
  #|Return the contents of the address part of the register
   |if decrement part of the register is #null, signals an error otherwise.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((car)) car)) cons)
   |#
  (\ ((car)) car) )

(def cadr
  #|Return the `car' of the `cdr' of the CONS.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((#_ cadr . #_)) cadr)) cons)
   |$(derivation (car (cdr cons)))
   |$(derivation (@car cons 1))
   |#
  %cadr )

(def cdr
  #|Return the contents of the decrement part of the register.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((#_ . cdr)) cdr)) cons)
   |$(derivation (@cdr cons))
   |#
  %cdr )

(def cons
  #|Return a cons with the given CAR and CDR.
   |
   |$(fn car cdr)
   |$(type function)
   |$(derivation (@new vm Cons car cdr))
   |#
  %cons )

(def cons?
  #|Return true if the OBJECT is a cons, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Cons))
   |#
  %cons? )

(def eval
  #|Evaluate the FORM in the ENVIRONMENT, returning its result.
   |
   |$(fn form . environment)
   |$(type function)
   |$(derivation (eval form (if (null? environment) ((vau () env env)) (car! environment))))
   |#
  %eval )

(def if
  #|Evaluate the TEST which must yield a boolean.
   |Then evaluate either the THEN or ELSE expression depending on whether the TEST yielded true or false.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn test then . forms)
   |$(syntax forms (or () (else) (test then . forms)))
   |$(type fexpr)
   |$(derivation (vau (test then . forms) env (if (eval test env) (eval then env) (null? forms) #inert (null? (cdr forms)) (eval (car forms) env) (apply if forms env))))
   |#
  %if )

(def list
  #|Return the list of evaluated ARGUMENTS.
   |
   |$(fn . arguments)
   |$(type function)
   |$(derivation (wrap (vau arguments #ignore arguments)))
   |#
  %list )

(def list*
  #|Return a list of evaluated ARGUMENTS so that
   |the last argument becomes the `cdr' of the list.
   |
   |$(fn . arguments)
   |$(type function)
   |#
  %list* )

(def newBox
  #|Return a new box with the optional VALUE.
   |The Box are functions that encapsulates a mutable value.
   |Without VALUE use as VALUE (boxDft).
   |
   |Calling the box without arguments returns the value in the box.
   |Calling the box with an argument update the value in the box.
   |
   |$(fn . value)
   |$(type function)
   |#
  %newBox )

(def null?
  #|Return true if the OBJECT is #null, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Null))
   |#
  %null? )

(def quote
  #|Return the unevaluated OPERAND.
   |
   |$(fn operand)
   |$(type fexpr)
   |$(derivation (vau (operand) #ignore operand))
   |#
  %' )


#|! Macro
 |#

(def evalMacro
  #|A box to discriminate when to evaluate or simply expand a macro.
   |Used from expand and makeMacro. 
   |#
  (newBox #t))

(def makeMacro
  #|Return a macro from an EXPANDER operator.
   |A macro is an operator that receives an operand and produces a form
   |(by calling the expander with the operand as argument)
   |that is then evaluated in place of the operand.
   |
   |$(fn expander)
   |$(type function)
   |#
  (wrap
    (vau (expander) #ignore
      (vau operands env
        (def evalMacro (evalMacro :prv #t))
        (def exp (apply expander operands))
        (if evalMacro (eval exp env) exp) ))))

(def macro
  #|Return an anonymous macro with the given PARAMETER-TREE and FORMS as body.
   |
   |$(fn parameterTree . forms)
   |$(type macro)
   |#
  (makeMacro
    (vau (pt . forms) #ignore
      (list 'makeMacro (list* 'vau pt #ignore forms)) )))

(def expand
  #|Expands a macro call rather than evaluates it.
   |
   |$(fn form)
   |$(type macro)
   |#
  (macro (form)
    (list 'begin (list 'evalMacro #f) form) ))


#|! Definition Forms
 |The forms defMacro defVau def\ defde\ def*\ rec\ let1\ let1rec\ let\ and letrec\ have two equivalent syntax
 |
 |    (_ name parameterTree . forms)
 |    (_ (name . parameterTree) . forms)
 |
 |The forms rec rec\ let1rec let1rec\ letrec and letrec\ pre-initialize all bindings to #inert before evaluating and binding the values ot functions.
 |#

(def defMacro
  #|Defines into the current environment the named macro NAME with the given PARAMETER-TREE and FORMS as body.
   |
   |$(fn name parameterTree . forms)
   |$(fn (name parameterTree) . forms)
   |$(type macro)
   |#
  (macro (lhs . rhs)
    (if (cons? lhs)
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs))
      (list 'def lhs (cons 'macro rhs)) )))

(assert (expand (defMacro (succ n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))
(assert (expand (defMacro succ (n) (list '+ n 1))) '(def succ (macro (n) (list (%' +) n 1))))

(defMacro (defVau lhs . rhs)
  #|Defines into the current environment the named fexpr NAME with the given PARAMETER-TREE, ENVIRONMENT-PARAMETER and FORMS as body.
   |
   |$(fn name parameterTree . forms)
   |$(fn (name parameterTree) . forms)
   |$(type macro)
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'vau (cdr lhs) rhs))
    (list 'def lhs (cons 'vau rhs)) ))

(assert (expand (defVau (succ n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))
(assert (expand (defVau succ (n) env (eval (list '+ n 1) env))) '(def succ (vau (n) env (eval (list (%' +) n 1) env))))

(def defConstant
  #|Defines into the current environment the named constant NAME with the given VALUE.
   |This is mostly for documentation purposes, as constants are still mutable.
   |Alias for def.
   |#
  def )

(defMacro (def* lhs . rhs)
  #|Defines into the current environment the resulting bindings from the match of the DEFINIEND-TREE against VALUES, signals an error otherwise.
   |
   |$(fn definiendTree . values)
   |$(fn definiendTree bindResult . values)
   |$(syntax bindResult (or #ignore #inert :rhs :prv))
   |$(type macro)
   |#
  (list 'def lhs (cons 'list rhs)) )

(defMacro (def\ lhs . rhs)
  #|Defines into the current environment the named function NAME with the given PARAMETER-TREE and FORMS as body.
   |
   |$(fn name parameterTree . forms)
   |$(fn (name parameterTree) . forms)
   |$(type macro)
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'def lhs (cons '\ rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


#|! Other Core Built-Ins
 |#

(def apply*
  #|Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.
   |
   |$(fn fun . args)
   |$(type function)
   |$(derivation (apply fun args (newEnv)))
   |#
  %apply* )

(def apply**
  #|Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.
   |
   |$(fn fun . args)
   |$(type function)
   |$(derivation (apply fun (apply list* args) (newEnv)))
   |#
  %apply** )

(defVau set (ep dt value) env
  #|Match the DEFINIEND-TREE against the VALUE in the ENVIRONMENT-PARAMETER, creating or updating existing bindings.
   |Unlike Common Lisp `setq' (or Scheme `set!') that allows updating arbitrary bindings
   |you always need to know the environment where a binding is in to change it.
   |Therefore, we usually use boxes instead of mutating bindings directly.
   |
   |$(fn environmentParameter definiendTree value)
   |$(type fexpr)
   |#
  (eval
    (list 'def dt (list (unwrap eval) value env))
    (eval ep env) ))

(def set!
  #|Update the bindings defined of the environment, signals an error if the binding is not defined.
   |So 'def' match the DEFINIEND-TREE against the VALUE and update the resulting bindings into environment if exist.
   |without bindResult or with bindResult #ignore use as bindResult (bndRes)
   |with bindResult #inert return #inert
   |with bindResult :rhs return the right side of the last binding
   |with bindResult :prv return the previous value of the last binding
   |with bindResult :cnt return the env
   |
   |$(fn definiendTree value)
   |$(fn definiendTree bindResult value)
   |$(syntax bindResult (or #ignore #inert :rhs :prv))
   |$(type fexpr)
   |#
  %set! )

(def unwrap
  #|Return the underlying operator of a FUNCTION.
   |
   |$(fn function)
   |$(type function)
   |#
  %unwrap )

(defMacro (wrau pt ep . forms)
  #|Return an anonymous function with the given PARAMETER-TREE, ENVIRONMENT-PARAMETER and FORMS as body,
   |which may use the execution environment in the ENVIRONMENT-PARAMETER rather than the definition environment for evaluate FORMS.
   |Used for defining the dynamic environment and variable lambda de\ and dv\.
   |
   |$(fn parameterTree environmentParameter . forms)
   |$(type function)
   |$(derivation (wrap (vau parameterTree environmentParameter . forms))
   |#
  (list 'wrap (list* 'vau pt ep forms)) )

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))
;(assert (let* ((a 1) (a 2)) ((wrap (vau (b) #_ (list a b))) (+ 1 2))) '(2 3))
;(assert (let* ((a 1) (a 2)) ((wrau (b) #_ (list a b)) (+ 1 2))) '(2 3))

(def de\
  #|Return an anonymous function with the given PARAMETER-TREE and FORMS as body,
   |which accesses to the execution environment, rather than the definition environment, for the values of free variables.
   |The classic lambda of the first type, before the Scheme static lambda.
   |
   |$(fn parameterTree . forms)
   |$(type fexpr)
   |$(derivation (wrau args env (apply begin forms (bind (newEnv env) parameterTree args))))
   |#
  (vau (pt . forms) #_
    (wrau args env (apply begin forms (bind (newEnv env) pt args)))))

;(assert (let* ((f (de\ () a)) (a 1) (a 2)) (f)) 2) 

(defMacro (defde\ lhs . rhs)
  #|Defines into the current environment the named dynamic environment function NAME with the given PARAMETER-TREE and FORMS as body.
   |
   |$(fn name parameterTree . forms)
   |$(fn (name parameterTree) . forms)
   |$(type macro)
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* 'de\ (cdr lhs) rhs))
    (list 'def lhs (cons 'de\ rhs)) ))


#|! Env
 |The Env are functions that encapsulates mutable values.
 |
 |Calling the env:
 |- without arguments returns the env.
 |- with an attribute return the value of the attribute in the env.
 |- with couples of attribute and value defines or update the attribute with the value in the env.
 |- without bindResult or with bindResult #ignore use as bindResult (bndRes)
 |- with bindResult #inert return #inert
 |- with bindResult :rhs return the right side of the last binding
 |- with bindResult :prv return the previous value of the last binding
 |- with bindResult :cnt return the env
 |- without bindType use the bindType :def
 |- with bindType :def define or update the bindings in the env
 |- with bindType :set! update the bindings in env, signals an error if the binding is not defined.
 |
 |$(env)
 |$(env attribute)
 |$(env attribute value . attributes)
 |$(env bindType attribute value . attributes)
 |$(env bindResult attribute value . attributes)
 |$(env bindType bindResult attribute value . attributes)
 |$(syntax attributes (or () (attribute value . attributes)))
 |$(syntax attribute (or Symbol Keyword String))
 |$(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |$(syntax bindType (or :def :set!))
 |#

(def newEnv
  #|Return a new environment with an optional PARENT environment in which bindings are looked up if they are not found.
   |The BINDINGS must be of even length, and alternately contain bindings names (symbols, keywords or string) and values.
   |When called with an obj the obj bindings also become env bindings.
   |
   |$(fn)
   |$(fn parent . bindings)
   |$(fn parent obj)
   |$(syntax parent (or () Env))
   |$(syntax bindings (or () (attribute value . bindings)))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |#
  %newEnv )

(def theEnv
  #|Return the current environment.
   |
   |$(fn)
   |$(type fexpr)
   |$(derivation (vau () environment environment))
   |#
  %theEnv )


#|! Obj
 |The Obj are functions that encapsulates mutable values.
 |
 |Calling the obj:
 |- without arguments returns the env.
 |- with an attribute return the value of the attribute in the obj.
 |- with couples of attribute and value defines or update the attribute with the value in the obj.
 |- without bindResult or with bindResult #ignore use as bindResult (bndRes)
 |- with bindResult #inert return #inert
 |- with bindResult :rhs return the right side of the last binding
 |- with bindResult :prv return the previous value of the last binding
 |- with bindResult :cnt return the obj
 |
 |$(obj)
 |$(obj attribute)
 |$(obj attribute value . attributes)
 |$(obj bindResult attribute value . attributes)
 |$(syntax attributes (or () (attribute value . attributes)))
 |$(syntax attribute (or Symbol Keyword String))
 |$(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |#

(def new
  #|Return a new instance of CLASS (that must be a `standard-class').
   |The BINDINGS must be of even length, and alternately contain bindings names (symbols, keywords or string) and values.
   |
   |$(fn boxClass)
   |$(fn boxClass value)
   |$(fn objClass . bindings)
   |$(fn objClass string . bindings)
   |$(fn objClass string throwableObject . bindings)
   |$(syntax bindings (or () (attribute value . bindings)))
   |$(syntax attribute (or Symbol Keyword String))
   |#
  %new )

(defMacro (defObj name class . attr)
  #|Defines into the current environment a named instance NAME of the given CLASS.
   |
   |$(fn name boxClass)
   |$(fn name boxClass value)
   |$(fn name objClass . bindings)
   |$(fn name objClass string . bindings)
   |$(fn name objClass string throwableObject . bindings)
   |$(syntax bindings (or () (attribute value . bindings)))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type macro)
   |#
  (list 'def name (list* 'new class attr)) )


#|! Env & Obj
 |#

(def bound?
  #|Return true if the SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, false otherwise.
   |
   |$(fn attribute object)
   |$(syntax attribute (or Symbol Keyword String))
   |$(syntax object (or Env Obj))
   |$(type function)
   |$(derivation (@isBound object attribute))
   |#
  %bound? )

(def value
  #|Return the value of SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, #null otherwise.
   |
   |$(fn attribute object)
   |$(syntax attribute (or Symbol Keyword String))
   |$(syntax object (or Env Obj))
   |$(type function)
   |$(derivation (@value object attribute))
   |#
  %value )

(def\ (slotBound? object attribute)
  #|Return true if the SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, false otherwise.
   |
   |$(fn object attribute)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (@isBound object attribute))
   |#
  (%slotBound? object attribute) )

(def\ (getSlot object attribute)
  #|Return the value of SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, signals an error otherwise.
   |
   |$(fn object attribute)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (object attribute))
   |#
  (%getSlot object attribute) )

(def\ (setSlot object attribute value)
  #|Update or define with VALUE the SYMBOL or KEYWORD or STRING in the ENVIRONMENT or OBJ.
   |
   |$(fn object attribute value)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (object attribute value))
   |#
  (%setSlot object attribute value) )


#|! Cons
 |#

(def\ (caar x)
  #|Return the `car' of the `car' of the CONS.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ (((caar . #_) . #_)) caar)) cons)
   |$(derivation (car (car cons)))
   |#
  (car (car x)) )

(def\ (cadr! (#_ cadr))
  #|Return the `car' of the `cdr' of the CONS if 'cddr' is #null, signals an error otherwise.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((#_ cadr)) cadr)) cons)
   |#
  cadr)

(def\ (cdar x)
  #|Return the `cdr' of the `car' of the CONS.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ (((#_ . cdar) . #_)) cdar)) cons)
   |$(derivation (cdr (car cons)))
   |#
  (cdr (car x)) )

(def cddr
  #|Return the `cdr' of the `cdr' of the CONS.
   |
   |$(fn cons)
   |$(type function)
   |$(derivation ((\ ((#_ #_ . cddr)) cddr)) cons)
   |$(derivation (cdr (cdr cons)))
   |$(derivation (@cdr cons 1))
   |#
  %cddr)

(def\ (cons! car)
  #|Return a cons with the given CAR and #null.
   |
   |$(fn car)
   |$(type function)
   |$(derivarion ((\ (car) (cons car)) car))
   |#
  (cons car))

(def nth
  #|Return element number N of LIST, where the `car' is element zero.
   |
   |$(fn n list)
   |$(type function)
   |#
  %nth)

(def nthCdr
  #|Returns the tail of LIST that would be obtained by calling `cdr' N times in succession.
   |
   |$(fn n list)
   |$(type function)
   |#
  %nthCdr)


#|! List
 |#

(def append
  #|Append two lists. The first one must be proper and is copied.
   |The second one is not copied (and doesn't even have to be a list).
   |It becomes the `cdr' of the final cons of the first list,
   |or is returned directly if the first list is empty.
   |
   |$(fn list1 list2)
   |$(type function)
   |#
  %append)

(def len
  #|Return the number of elements in the LIST.
   |
   |$(fn list)
   |$(type function)
   |#
  %len)

(def list?
  #|Return true if the OBJECT is a list, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Keyword))
   |#
  %list?)

(def reverse
  #|Reverse the LIST.
   |
   |$(fn list)
   |$(type function)
   |#
  %reverse)


#|! Symbol & Keyword
 |Symbol and Keyword extends Intern
 |#

(def intern
  #|Return the unique symbol or keyword with STRING as name.
   |Keyword if STRING start with ':', symbol otherwise
   |
   |$(fn string)
   |$(type function)
   |#
  %intern)

(def name
  #|Return the name of the SYMBOL or KEYWORD as a string.
   |
   |$(fn intern)
   |$(type function)
   |#
  %name)

(def keyword
  #|Return the unique keyword with STRING as name.
   |
   |$(fn string)
   |$(type function)
   |#
  %keyword)

(def keyword?
  #|Return true if the OBJECT is a keyword, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Keyword))
   |#
  %keyword?)

(def\ (keywordName (#! Keyword keyword))
  #|Return the name of the KEYWORD as a string.
   |
   |$(fn keyword)
   |$(type function)
   |#
  (name keyword) )

(def symbol
  #|Return the unique symbol with STRING as name.
   |
   |$(fn string)
   |$(type function)
   |#
  %symbol)

(def symbol?
  #|Return true if the OBJECT is a symbol, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Symbol))
   |#
  %symbol?)

(def\ (symbolName (#! Symbol symbol))
  #|Return the name of the SYMBOL as a string.
   |
   |$(fn symbol)
   |$(type function)
   |#
  (name symbol) )


#|! Equals
 |#

(def ==
  #|Return true if A istanceof Number && A equals B || A == B, false otherwise.
   |
   |$(fn a b)
   |$(type function)
   |#
  %==)

(def !=
  #|Return false if A istanceof Number && A equals B || A == B, thrue otherwise.
   |
   |$(fn a b)
   |$(type function)
   |#
  %!=)

(def eq?
  #|Return true if A is equals B, false otherwise.
   |
   |$(fn a b)
   |$(type function)
   |#
  %eq?)

(def\ (ignore? o) (== o #_))
(def\ (sheBang? o) (== o #!))
(def\ (inert? o) (== o #inert))


#|! Boolean
 |#

(def !
  #|Invert the BOOLEAN.
   |
   |$(fn boolean)
   |$(type function)
   |$(derivation (if boolean #f #t))
   |#
  %!)

(def not
  #|Alias of !.
   |#
  !)

(def !!
  #|Convert a VALUE to boolean when tTrue != 0.
   |
   |$(fn value)
   |$(type function)
   |$(derivation (if value #t #f))
   |#
  %!!)


#|! Number
 |#

(def number?
  #|Return true if the OBJECT is a number, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object Number))
   |#
  %number?)

(def +
  #|Java binary + operator, sums the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |
   |$(fn a b)
   |$(type function)
   |#
  %+)

(def *
  #|Java binary - operator, multiplies the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |
   |$(fn a b)
   |$(type function)
   |#
  %*)

(def -
  #|Java binary - operator, subtracts the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |
   |$(fn a b)
   |$(type function)
   |#
  %-)

(def /
  #|Java binary / operator, divides the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |
   |$(fn a b)
   |$(type function)
   |#
  %/)

(def %
  #|Java binary % operator, rest of the division of the values after the conversion of the operands to the same type.
   |Converts to BigDecimal, Double, BigInteger, Long or Integer if one operands is BigDecimal, Double, BigInteger, Long or Integer
   |
   |$(fn a b)
   |$(type function)
   |#
  %%)

(def\ (1+ n) (+ n 1))
(def\ (1- n) (- n 1))
(def\ (0? n) (== n 0))
(def\ (1? n) (== n 1))
(def\ (-1? n) (== n -1))
(def\ (even? n) (== (% n 2) 0))
(def\ (odd? n)  (== (% n 2) 1))


#|! String
 |#

(def string?
  #|Return true if the OBJECT is a string, false otherwise.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (type? object String))
   |#
  %string?)

(def $
  #|Java binary + operator, join the values after the conversion of the operands to string.
   |
   |$(fn a b)
   |$(type function)
   |#
  %$)


#|! Comparators
 |#

(def <
  #|Java binary < operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %<)

(def >
  #|Java binary > operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %>)

(def <=
  #|Java binary <= operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %<=)

(def >=
  #|Java binary >= operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %>=)

(def\ (min a b) (if (< a b) a b))
(def\ (max a b) (if (> a b) a b))


#|! Bitwise and Shift Operators
 |#

(def ~
  #|Java binary ~ operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %~)

(def &
  #|Java binary & operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %&)

(def \|
  #|Java binary | operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %|)

(def ^
  #|Java binary ^ operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %^)

(def <<
  #|Java binary << operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %<<)

(def >>
  #|Java binary >> operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %>>)

(def >>>
  #|Java binary >>> operator.
   |
   |$(fn a b)
   |$(type function)
   |#
  %>>>)


#|! First-Order Controls
 |#

(def* (then else)
  #|Alias of begin.
   |#
  begin begin )

(def loop
  #|Evaluate the FORMS as an implicit `begin' in an infinite loop.
   |
   |$(fn . forms)
   |$(type fexpr)
   |#
  %loop)

(def atEnd
  #|Evaluate the PROTECTED-FORMS as an implicit `begin' and return its result.
   |Regardless of whether the protected form returns normally, or via a nonlocal exit or panic,
   |the CLEANUP-FORM are evaluated after the protected forms.
   |
   |$(fn cleanupForm . protectedForms)
   |$(type fexpr)
   |#
  %atEnd)

(defMacro (finally protected . cleanUp)
  #|Evaluate the PROTECTED-FORM and return its result.
   |Regardless of whether the protected form returns normally, or via a nonlocal exit, or panic,
   |the CLEANUP-FORMS are evaluated as an implicit `begin' after the protected forms.
   |
   |$(fn protectedForm . cleanupForms)
   |$(type macro)
   |$(derivation (atEnd (begin . cleanupForms) protectedForm))
   |#
  (list 'atEnd (cons 'begin cleanUp) protected) )

(def throwTag
  #|Abort to a nesting catch TAG established by `catch', evaluate FORMS as an implicit `begin' and return its result.
   |
   |$(fn tag . forms)
   |$(type fexpr)
   |#
  %throwTag)

(defMacro (throw . forms)
  #|Abort to a nesting generic catch, evaluate FORMS as an implicit `begin' and return its result.
   |
   |$(fn . forms)
   |$(type fexpr)
   |$(derivation (throwTag #ignore forms))
   |#
  (list* 'throwTag #_ forms) )

(def catchTagWth
  #|Establish a catch TAG with an HANDLER (one arg function or any value) and evaluate FORMS as an implicit `begin' and return its result.
   |The FORMS may use `throw' to nonlocally exit from the TAG. if FORMS throw and HANDLER is a value, value will be the result of `catchTagWth'
   |otherwise the value of the `throw' is passed to the HANDLER and the value returned by the HANDLER will be the result of the `catchTagWth'
   |
   |$(fn tag handler . forms)
   |$(type (if (ctApv) macro fexpr))
   |#
  (if (ctApv) ;; ctApv non andrebbe cambiato dopo il boot, anche la riassegnazione di catchTagWth potrebbe non bastare!
    (macro (tag hdl . forms)
      (list '%catchTagWth tag hdl (list* '\ () forms)) )
    %catchTagWth ))

(defMacro (catchWth hdl . forms)
  #|Establish an generic catch all with HANDLER (one arg function or any value) and evaluate FORMS as an implicit `begin' and return its result.
   |The FORMS may use `throw' to nonlocally exit. if FORMS throw and HANDLER is a value, value will be the result of `catchWth'
   |otherwise the value of the `throw' is passed to the HANDLER and the value returned by the HANDLER will be the result of the `catchWth'
   |
   |$(fn handler . forms)
   |$(type macro)
   |$(derivation (catchTagWth #ignore handler . forms))
   |#
  (list* 'catchTagWth #_ hdl forms))

(defMacro (catchTag tag . forms)
  #|Establish a catch TAG without handler and evaluate FORMS as an implicit `begin' and return its result.
   |The FORMS may use `throw' to nonlocally exit from the tag. if FORMS throw the value returned by the `throw' will be the result of the `catchTag'
   |
   |$(fn tag . forms)
   |$(type macro)
   |$(derivation (catchTagWth tag #ignore . forms))
   |#
  (list* 'catchTagWth tag #_ forms) )

(defMacro (catch . forms)
  #|Establish an generic catch all without handler and evaluate FORMS as an implicit `begin' and return its result.
   |The FORMS may use `throw' to nonlocally exit. if FORMS throw the value returned by the `throw' will be the result of the `catch'
   |
   |$(fn . forms)
   |$(type macro)
   |$(derivation (catchTagWth #ignore #ignore . forms))
   |#
  (list* 'catchTagWth #_ #_ forms))

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catchWth (\ (x) (+ x 1)) (throw 1) ) 2)
(assert (catchTag 'a (throwTag 'a)) #inert)
(assert (catchTag 'a (throwTag 'a 1)) 1)
(assert (catchTagWth 'a (\ (x) (+ x 1)) (throwTag 'a 1) ) 2)


#|! Delimited-Control Operators
 |These operators follow the API put forth in the delimcc library at URL `http://okmij.org/ftp/continuations/implementations.html'.
 |#

(def takeSubcont
  #|Abort outwards to the PROMPT.
   |When the prompt is reached, evaluate the FORMS as an implicit `begin' with SYMBOL bound to the captured continuation (which does not include the prompt) and return its result.
   |
   |$(fn prompt symbol . forms)
   |$(type fexpr)
   |#
  %takeSubcont)

(def pushPrompt
  #|Push the PROMPT and evaluate the FORMS as an implicit `begin' inside the prompt and return its result.
   |This delimits the continuation.
   |
   |$(fn prompt . forms)
   |$(type fexpr)
   |#
  %pushPrompt)

(def pushDelimSubcont
  #|Push the PROMPT and compose the previously captured CONTINUATION inside it
   |before evaluated the FORMS as an implicit `begin' inside the new continuation and return its result.
   |
   |$(fn prompt continuation . forms)
   |$(type fexpr)
   |#
  %pushDelimSubcont)

(defMacro (pushSubcont continuation . forms)
  #|We don't have `pushSubcont' but we can emulate it with a `pushDelimSubcont' that pushes an #ignore prompt.
   |
   |$(fn continuation . forms)
   |$(type macro)
   |$(derivation (pushDelimSubcont #ignore continuation . forms))
   |#
  (list* 'pushDelimSubcont #_ continuation forms) )

(def pushSubcontBarrier
  #|Push a continuation barrier that prevents the FORMS from capturing any continuations to the outside,
   |before evaluated the FORMS as an implicit `begin'
   |
   |$(fn . forms)
   |$(type fexpr)
   |#
  %pushSubcontBarrier)


#|! Errors
 |#

(def test
  #|signals an error if:
   |- EXPRESSION does not equal VALUE
   |- VALUE is not present and the EXPRESSION does not throws
   |- EXPRESSION throws or returns an object and the object is not of the same CLASS with the ATTRIBUTES of the given VALUE.
   |$(fn name expression value)
   |$(fn name expression)
   |$(fn name expression class attribute value . attributes)
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String .Field @Method))
   |$(type fexpr)
   |#
  %test)

(def error
  #|Signal an error.
   |$(fn error)
   |$(fn string . attributes)
   |$(fn throwable . attributes)
   |$(fn string throwable . attributes)
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String))
   |#
  %error)

(def rootPrompt
  #|The prompt used for delimiting all.
   |#
  %rootPrompt)

(def\ makeTypeError (datum expected)
  #|Return a new type error with DATUM and EXPECTED.
   |
   |$(fn datum expected)
   |$(type function)
   |#
  (new Error "not a {expected}: {datum}" :type 'type :datum datum :expected expected) )

(def\ typeError (datum expected)
  #|Signal a type error with DATUM and EXPECTED.
   |
   |$(fn datum expected)
   |$(type function)
   |#
  (error (makeTypeError datum expected)) )


#|! Classes
 |#

(def className
  #|Return the name symbol of the CLASS.
   |
   |$(fn class)
   |$(type function)
   |$(derivation (symbol (@getSimpleName class))))
   |#
  %className)

(def classOf
  #|Return the class of the OBJECT.
   |
   |$(fn object)
   |$(type function)
   |$(derivation (if (null? object) #null (@getClass object)))
   |#
  %classOf)

(def instanceOf?
  #|Return true if OBJECT is instaceof CLASS.
   |
   |$(fn object class)
   |$(type function)
   |#
  %instanceOf?)

(def subClass?
  #|Return true if the CLASS is a subclass of the SUPERCLASS, false otherwise.
   |A class is considered a subclass of itself.
   |
   |$(fn class superclass)
   |$(type function)
   |$(derivation (@isAssignableFrom superClass class)))
   |#
  %subClass?)

(def type?
  #|Return true if the OBJECT is an instance of the CLASS, false otherwise.
   |
   |$(fn object class)
   |$(type function)
   |$(derivation (if (null? class) (null? object) (null? object) #f (subClass? (classOf object) class)))
   |$(derivation (if (null? class) (null? object) (instanceOf object class)))
   |#
  %type?)


#|! Basic Functions and Macros
 |#

(def\ (idf x)
  #|Identity function.
   |
   |$(fn object)
   |$(type function)
   |#
  x)

(defMacro _ forms
  #|"Implicit" Argument Lambda.
   |
   |$(fn . forms)
   |$(type macro)
   |$(derivation (\ (_) . forms))
   |#
  (list* '\ '(_) forms) )

(def\ (peval f v)
  #|Single arg partial evaluation.
   |
   |$(fn f v)
   |$(type function)
   |$(derivation (\ args (apply f (cons v args))))
   |#
  (\ args (apply f (cons v args))) )

(def\ (peval* f . v*)
  #|Multiple args partial evaluation.
   |
   |$(fn f v*)
   |$(type function)
   |$(derivation (\ args (apply f (append v* args))))
   |#
  (\ args (apply f (append v* args))) )

(def\ (compose f g)
  #|Compose two functions, creating a new function equivalent to (f (g . args)).
   |
   |$(fn f g)
   |$(type function)
   |$(derivation (\ args (f (apply g args))))
   |#
  (\ args (f (apply g args))) )

(def\ (compose* . f*)
  #|Compose multiple functions, creating a new function equivalent to (f1 (f2 (... (fn . args)))).
   |
   |$(fn . f*)
   |$(type function)
   |#
  (\ args ((rec\ (loop (f . f*)) (if (null? f*) (apply f args) (f (loop f*)))) f*)) )

;(defMacro compose* f*
;  (list '\ 'args ((rec\ (loop (f . f*)) (if (null? f*) (list 'apply f 'args) (list f (loop f*)))) f*)) )

(defMacro (rec name value)
  #|Return VALUE, after lexically bind NAME with #inert,
   |and update NAME with VALUE so that it can reference itself.
   |
   |$(fn name value)
   |$(type macro)
   |#
  (list (list '\ (list name) (list 'def name :rhs value)) #inert) )

(def label
  #|Alias of rec.
   |#
  rec )

(assert ((rec f (\ (l) (if (null? l) "" ($ (car l) (f (cdr l)))))) '(1 2 3)) "123")
(assert ((rec f (\ l (if (null? l) "" ($ (car l) (apply f (cdr l)))))) 1 2 3) "123")

(defMacro (rec\ lhs . rhs)
  #|Return the function with given PARAMETER-TREE and FORMS as body, after lexically bind NAME with #inert,
   |and updated NAME with the function so that it can reference to itself.
   |
   |$(fn name parameterTree . forms)
   |$(fn (name . parameterTree) . forms)
   |$(type macro)
   |#
  (if (cons? lhs)
    (list 'rec (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'rec lhs (cons '\ rhs)) ))

(def label\
  #|Alias of rec\.
   |#
  rec\ )

(assert ((rec\ (f l)   (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\  f (l)  (if (null? l) "" ($ (car l) (f (cdr l))))) '(1 2 3)) "123")
(assert ((rec\ (f . l) (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")
(assert ((rec\  f l    (if (null? l) "" ($ (car l) (apply f (cdr l))))) 1 2 3) "123")

(def\ (map f lst . lst*)
  #|Return a new list by applying the FUNCTION to each element of the LIST or the LISTS which must be of the same length
   |
   |$(fn function list)
   |$(fn function list1 list2 ... listN)
   |$(type function)
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
  #|Defines into the current environment the named functions NAMES with given PARAMETER-TREES and FORMS as body. 
   |
   |$(fn definiendTrees . bodies)
   |$(fn ((name parameterTree) . definiendTrees) . (forms . bodies))
   |$(fn (name . definiendTrees) . ((parameterTree . forms) . bodies))
   |$(syntax definiendTrees ((name parameterTree) . definiendTrees)
   |$(syntax bodies (forms . bodies)
   |$(syntax definiendTrees (name . definiendTrees)
   |$(syntax bodies ((parameterTree . forms) . bodies)
   |$(type macro)
   |$(example (def*\ ((f a) g) ((1+ a)) ((a) (1+ a))) )
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
  #|Establish the BINDING, before the evaluation of FORMS as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn binding . forms)
   |$(type macro)
   |$(syntax binding . (definiendTree initForm))
   |#
  (list (list* '\ (cons dt) forms) value))

(defMacro (wth* bindings . forms)
  #|Establish BINDINGS serially, so that every binding can refer to previous one,
   |before the evaluation of FORMS as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn bindings . forms)
   |$(type macro)
   |$(syntax bindings (or () (definiendTree intiForm . bindings)))
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
  #|Establish BINDINGS parallelly, so that no binding can refer to itself or the other ones,
   |before the evaluation of FORMS as an implicit `begin'.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn bindings . forms)
   |$(fn name bindings . forms)
   |$(type macro)
   |$(syntax bindings (or () (definiendTree initForm . bindings))
   |#
  (def dt* ((rec\ (loop b*) (if (null? b*) #null (wth1 (dt #_ . b*) b* (cons dt (loop b*))))) b*))
  (def vl* ((rec\ (loop b*) (if (null? b*) #null (wth1 (#_ vl . b*) b* (cons vl (loop b*))))) b*))
  (cons (list* '\ dt* forms) vl*) )

(assert (expand (wth (a 1 b 2) 1 2 (+ a b))) '((\ (a b) 1 2 (+ a b)) 1 2))
(assert (wth (a 1 b 2) 1 2 (+ a b)) 3)


(defMacro (let1Loop lhs . rhs)
  #|Labelled recursive loop.
   |Lexically bind NAME with the function with the specified PARAMETER-TREE and FORMS as body,
   |so that it can recursively refer to itself.
   |The function is immediately applied to the list containing the value of the INIT_FORMS evaluate as an implicit `begin',
   |and then FORMS is evaluate as an implicit `begin'.
   |
   |$(fn name binding . forms)
   |$(fn (name . binding) . forms)
   |$(type macro)
   |$(syntax binding (parameterTree . initForms))
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
  #|Establishes the BINDING, before the evaluation of FORMS as an implicit `begin'.
   |The INIT_FORMS is evaluate as an implicit `begin'.
   |If the first argument is a symbol is like 'let1Loop'.
   |
   |$(fn binding . forms)
   |$(fn name binding . forms)
   |$(type macro)
   |$(syntax binding (definiendTree . initForms))
   |#
  (if (symbol? lhs)
    (list* 'let1Loop lhs rhs)
    (list (list* '\ (list (car lhs)) rhs)
      (->begin lhs) )))

(assert (let1 (a 1) a) 1)
(assert (let1 (a 1 2) a) 2)
(assert (let1 f (a 2) (if (0? a) 'end (f (- a 1)))) 'end)

(defMacro (let1\ binding . forms)
  #|Establishes the FUNCTION-BINDINGS, before the evaluation of FORMS as an implicit `begin'.
   |
   |$(fn functionBinding . forms)
   |$(type macro)
   |$(syntax functionBinding (name parameterTree . bodyForms))
   |$(syntax functionBinding ((name . parameterTree) . bodyForms))
   |#
  (list* 'let1 (->name+lambda binding) forms))

(assert (let1\ (f (a) a) (f 5)) 5)
(assert (let1\ ((f a) a) (f 5)) 5)

(defMacro (let1rec binding . forms)
  #|Establishes the BINDING recursively so that the binding can recursively refer to itself,
   |before the evaluation of FORMS as an implicit `begin'.
   |The binding is initializated to #inert before evaluating INIT_FORMS as an implicit `begin'.
   |
   |$(fn binding . forms)
   |$(type macro)
   |$(syntax binding (name . intForms))
   |#
  (def name (car binding))
  (list* 'let1 (list name #inert)
    (list 'def (car name (->begin binding))
      forms )))

(defMacro (let1rec\ binding . forms)
  #|Establishes the FUNCTION-BINDING recursively, so that the function can refer to itself,
   |before the evaluation of FORMS as an implicit `begin'.
   |The binding is initializated to #inert before the bind of the function.
   |
   |$(fn functionBinding . forms)
   |$(type macro)
   |$(syntax functionBinding (name parameterTree . bodyForms))
   |$(syntax functionBinding ((name . parameterTree) . bodyForms))
   |#
  (list* 'let1 (->name+#inert binding)
    (cons 'def\ binding)
    forms ))


(defMacro (let* bindings . forms)
  #|Establishes BINDINGS serially, so that every binding can refer to previous one,
   |before the evaluation of FORMS as an implicit `begin'.
   |The INIT_FORMS are evaluate as an implicit `begin'.
   |
   |$(fn bindings . forms)
   |$(type macro)
   |$(syntax bindings (or () ((definiendTree . intiForms) . bindings)))
   |#
  ( (rec\ loop (bindings)
      (if (null? bindings)
        (cons (list* '\ () forms))
        (list* 'let1 (car bindings) (cons (loop (cdr bindings)))) ))
    bindings ))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)


(defMacro (letLoop lhs . rhs)
  #|Labelled recursive loop.
   |Lexically bind NAME with the function with the specified multiple PARAMETER-TREES and FORMS as body,
   |so that it can recursively refer to itself.
   |The function is immediately applied to the list containing the values of INIT_FORM evaluate as an implicit `begin',
   |and then FORMS is evaluate as an implicit `begin'.
   |
   |$(fn name bindings . forms)
   |$(fn (name . bindings) . forms)
   |$(type macro)
   |$(syntax bindings (or () ((parameterTree . initForms) . bindings)))
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
  #|Establishes the BINDINGS parallelly, so that no binding can refer to itself or the other ones,
   |before the evaluation of FORMS as an implicit `begin'.
   |The INIT_FORMS are evaluate as an implicit `begin'.
   |If the first argument is a symbol is like 'letLoop'.
   |
   |$(fn bindings . forms)
   |$(fn name bindings . forms)
   |$(type macro)
   |$(syntax bindings (or () ((definiendTree . initForms) . bindings))
   |#
  (if (symbol? lhs)
    (list* 'letLoop lhs rhs)
    (cons (list* '\ (map car lhs) rhs)
      (map ->begin lhs) )))

(assert (let ((a 1)) a) 1)

(defMacro (let\ bindings . forms)
  #|Establishes the FUNCTION-BINDINGS parallelly, so that no function can refer to itself or the other ones,
   |before the evaluation evaluation of FORMS as an implicit `begin'.
   |
   |$(fn functionBindings . forms)
   |$(type macro)
   |$(syntax functionBindings (or () (functionBinding . functionBindings)))
   |$(syntax functionBinding (name parameterTree . bodyForms))
   |$(syntax functionBinding ((name . parameterTree) . bodyForms))
   |#
  (list* 'let (map ->name+lambda bindings) forms))

(assert (expand (let\ ((f1 (a) a) ((f2 b) b)) 1)) '(let ((f1 (\ (a) a)) (f2 (\ (b) b))) 1))

(defMacro (letrec bindings . forms)
  #|Establishes the BINDINGS recursively, so that the bindings can refer to itself and the other ones,
   |before the evaluation of FORMS as an implicit `begin'.
   |The bindings are initializated to #inert before evaluating the INIT_FORMS as an implicit `begin'.
   |
   |$(fn bindings . forms)
   |$(type macro)
   |$(syntax bindings (or () ((name . intForms) . bindings)))
   |#
  (def names (map car bindings))
  (list* 'let (map (\ (name) (list name #inert)) names)
    (list* 'def* names (map ->begin bindings))
    forms ))

(assert (letrec ( (even? (\ (n) (if (0? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (0? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . forms)
  #|Establishes the FUNCTION-BINDINGS recursively, so that the functions can refer to itself and the other ones,
   |before the evaluation of FORMS as an implicit `begin'.
   |The bindings are initializated to #inert before the bind of the functions.
   |
   |$(fn functionBindings . forms)
   |$(type macro)
   |$(syntax functionBindings (or () (functionBinding . functionBindings)))
   |$(syntax functionBinding ((name parameterTree . bodyForms) . functionBindings))
   |$(syntax functionBinding (((name . parameterTree) . bodyForms) . functionBindings))
   |#
  (list* 'let (map ->name+#inert bindings)
    (list* 'def*\ (map car bindings) (map cdr bindings))
    forms ))

(def labels
  #|Alias of letrec\.
   |#
  letrec\)

(assert (labels ( ((even? n) (if (0? n) #t (odd? (- n 1)))) (odd? (n) (if (0? n) #f (even? (- n 1)))) ) (even? 88) ) #t)

#|! Simple Controls
 |#

(defVau prog1 (form . forms) env
  #|Evaluate FORM and return the result after evaluate any additional FORMS as an implicit `begin'.
   |
   |$(fn form . forms)
   |$(type fexpr)
   |#
  (let1 (result (eval form env))
    (apply begin forms env)
    result))

(defMacro (when test . forms)
  #|If TEST yields true, evaluate the FORMS as an implicit `begin', inert otherwise.
   |
   |$(fn test . forms)
   |$(type macro)
   |#
  (list 'if test (cons 'then forms)))

(defMacro (unless test . forms)
  #|If TEST yields false, evaluate the FORMS as an implicit `begin', inert otherwise.
   |
   |$(fn test . forms)
   |$(type macro)
   |#
  (list 'if test #inert (cons 'else forms)))

(defVau && ops env
  #|Return true if all OPERANDS evaluate to true, false otherwise.
   |If an operand evaluates to false, later operands are not evaluated.
   |If there are no operands, return false.
   |
   |$(fn . operands)
   |$(type fexpr)
   |#
  (if
    (null? ops) #t
    (eval (car ops) env) (apply && (cdr ops) env)
    #f ))

(def and
  #|Alias of &&.
   |#
  &&)

(defVau || ops env
  #|Return true if one of the OPERANDS evaluates to true, false otherwise.
   |If an operand evaluates to true, later operands are not evaluated.
   |If there are no operands, return true.
   |
   |$(fn . operands)
   |$(type fexpr)
   |#
  (if
    (null? ops) #f
    (eval (car ops) env) #t
    (apply || (cdr ops) env) ))

(def or
  #|Alias of ||.
   |#
  ||)

(def\ &&f f*
  #|Return a function the return true if all FUNCTIONS evaluate to true when applied to the arguments, false otherwise.
   |If an function evaluates to false, later FUNCTIONS are not evaluated.
   |If there are no FUNCTIONS, when applied return false.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn . functions)
   |$(type function)
   |#
  (\ args
    ( (rec\ (&&f f*)
        (if
          (null? f*) #t
          (apply (car f*) args) (&&f (cdr f*))
          #f ))
      f* )))

(def\ ||f f*
  #|Return a function the return true if one FUNCTIONS evaluate to true when applied to the arguments, false otherwise.
   |If an function evaluates to true, later FUNCTIONS are not evaluated.
   |If there are no FUNCTIONS, when applied return true.
   |Idea stolen from Anarki https://github.com/arclanguage/anarki
   |
   |$(fn . functions)
   |$(type function)
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
  #|return #inert if VALUE match the DEFINIEND-TREE and define resulting bindings into ENVIRONMENT, signals an error otherwise.
   |
   |$(fn environment definiendTree value)
   |$(type function)
   |$(derivation (eval (list 'def :cnt definiendTree value) environment))
   |#
  %bind)

(def bind?
  #|return true if VALUE match the DEFINIEND-TREE and define resulting bindings into ENVIRONMENT, false otherwise.
   |
   |$(fn environment definiendTree value)
   |$(type function)
   |$(derivation (catch #f (bind environment definiendTree value) #t))
   |#
  %bind?)

(defVau (ifBind? (pt exp) then . else) env
  #|return the evaluation of THEN into resulting ENVIRONMENT if VALUE match the DEFINIEND-TREE, inert otherwise.
   |
   |$(fn (definiendTree value) then . forms)
   |$(type fexpr)
   |#
  (let1 (env+ (newEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (car! else) env) ))))

(defVau (caseVau . clauses) env
  #|Multi-armed vau.
   |When applied go through the CLAUSES in order.
   |If CLAUSES is #null return #inert.
   |If car of CLAUSE is else
   |  if the cadr of CLAUSES is =>, evaluate caddr of CLAUSE and apply it to the operands,
   |  otherwise evaluate FORMS as an implicit `begin'.
   |If the operands match the DEFINIEND-TREE evaluate FORMS as an implicit `begin' into resulting ENVIRONMENT.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn . clauses)
   |$(type fexpr)
   |$(syntax clauses (or () (clause . clauses)))
   |$(syntax clause (else . forms))
   |$(syntax clause (else => apv1))
   |$(syntax clause (definiendTree . forms))
   |#
  (vau values #ignore
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

(defMacro (defCaseVau name . clauses)
  #|Defines into the current environment the named caseVau NAME with the given CLAUSES.
   |
   |$(fn name . clauses)
   |$(type fexpr)
   |$(derivation (def name (caseVau . clauses)))
   |#
  (list 'def name (cons 'caseVau clauses)) )

(defMacro (case\ . clauses)
  #|Multi-armed \.
   |When applied go through the CLAUSES in order.
   |If CLAUSES is #null return #inert.
   |If car of CLAUSE is else
   |  if the cadr of CLAUSES is =>, evaluate caddr of CLAUSE and apply it to the arguments,
   |  otherwise evaluate FORMS as an implicit `begin'.
   |If the arguments match the DEFINIEND-TREE evaluate FORMS as an implicit `begin' into resulting ENVIRONMENT.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn . clauses)
   |$(type function)
   |$(syntax clauses (or () (clause . clauses)))
   |$(syntax clause (else . forms))
   |$(syntax clause (else => apv1))
   |$(syntax clause (definiendTree . forms))
   |$(derivation (wrap (caseVau . clauses)))
   |#
  (list 'wrap (cons 'caseVau clauses)) )

(defMacro (defCase\ name . clauses)
  #|Defines into the current environment the named case\ NAME with the given CLAUSES.
   |
   |$(fn name . clauses)
   |$(type fexpr)
   |$(derivation (def name (case\ . clauses)))
   |#
  (list 'def name (cons 'case\ clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))

(defMacro (match exp . clauses)
  #|Evaluates VALUE and go through the CLAUSES in order.
   |If CLAUSES is #null return #inert.
   |If car of CLAUSE is else
   |  if the cadr of CLAUSES is =>, evaluate caddr of CLAUSE and apply it to the arguments,
   |  otherwise evaluate FORMS as an implicit `begin'.
   |If VALUE match the DEFINIEND-TREE evaluate FORMS as an implicit `begin' into resulting ENVIRONMENT.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn value . clauses)
   |$(type fexpr)
   |$(syntax clauses (or () (clause . clauses)))
   |$(syntax clause (else . forms))
   |$(syntax clause (else => apv1))
   |$(syntax clause (definiendTree . forms))
   |$(derivation (wrap (caseVau . clauses)))
   |#
  (list (cons 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match   '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match     '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match        1 ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match        4 ((a) 1) ((a b) 2)    (a a)) 4)
(assert (match '(1 2 3) ((a) 1) ((a b) 2)    (a a)) '(1 2 3))

(defVau (cond . clauses) env
  #|Multi-armed conditional.
   |Go through the CLAUSES in order.
   |If CLAUSES is #null return #inert.
   |If car of CLAUSE is else evaluate FORMS as an implicit `begin'.
   |Otherwise evaluate the TEST.
   |If TEST is a boolean
   |  if is true, evaluate the FORMS as an implicit `begin',
   |  otherwise go to the next CLAUSE.
   |If FORMS is #null return TEST.
   |If car di FORMS is => evaluate the cadr of FORMS and apply it to TEST.
   |If cadr di FORMS is => evaluate car of FORMS
   |  if is true evaluate the caddr of FORMS and apply it to TEST
   |  otherwise go to the next CLAUSE.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn . clauses)
   |$(syntax clauses (clause . clauses))
   |$(syntax clause (else . forms))
   |$(syntax clause (test->bool . forms))
   |$(syntax clause (test))
   |$(syntax clause (test => apv1))
   |$(syntax clause (test guard => apv1))
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
  #|Create a one-element list from the VALUE.
   |Alias of cons!.
   |#
  cons!)

(defVau (ifOpt (pt opt) then . else) env
  #|Single-value OPTION destructuring.
   |If the evaluation of OPTION it's not #null, evaluate the THEN form with the NAME bound to the contents of the OPTION.
   |Otherwise, evaluate the ELSE forms as an implicit `begin'.
   |
   |$(fn (name option) then . else)
   |$(type fexpr)
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
  #|Multi-valued OPTION destructuring.
   |If the evaluation of OPTION it's not #null, evaluate THEN form with the DEFINIEND-TREE bound to OPTION value.
   |Otherwise, evaluate the ELSE forms as an implicit `begin'.
   |
   |$(fn (definiendTree option) then . else)
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
  #|Destructure the OPTION.
   |If the evaluation of OPTION it's #null, return #null.
   |Otherwise, evaluate the FORMS as an implicit `begin' with the NAME bound to the contents of the OPTION.
   |
   |$(fn (name option) . forms)
   |$(type fexpr)
   |#
  (list 'ifOpt (list pt opt) (if (null? forms) #null (cons 'begin forms))) )

(defMacro unlessOpt (opt . forms)
  #|Destructure the OPTION.
   |If the evaluation of OPTION it's #null, evaluate the FORMS as an implicit `begin'.
   |Otherwise, return #null.
   |
   |$(fn option . forms)
   |$(type fexpr)
   |#
  (list* 'ifOpt (list #ignore opt) #null (if (null? forms) #null (cons 'begin forms))) )

(defVau (caseOpt opt . clauses) env
  #|Multi-armed ifOpt.
   |Evaluate OPTION and go through the CLAUSES in order.
   |If OPTION is #null return #null.
   |If CLAUSES is #null return #null.
   |If car of CLAUSE is else evaluate FORMS as an implicit `begin'.
   |If the OPTION match the DEFINIEND-TREE evaluate FORMS as an implicit `begin' into resulting ENVIRONMENT.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn option . clauses)
   |$(type fexpr)
   |$(syntax clauses (or () (clause . clauses)))
   |$(syntax clause (else . forms))
   |$(syntax clause (definiendTree . forms))
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
  #|Return the contents of the OPTION, or the DEFAULT if the option is #null.
   |The default itself defaults to #inert.
   |The DEFAULT is evaluated lazily, only when the OPTION is #null.
   |
   |$(fn option . default)
   |$(type fexpr)
   |#
  (ifOpt (opt (eval opt env)) opt
    (ifOpt (dft (eval (cons 'list dft) env)) dft) ))

(assert (optDft () 10) 10)
(assert (optDft '(2) 10) 2)
(assert (optDft '(2 3) 10))

(defVau optDft* (lst . dft) env
  #|Similar to `opt', but provides DEFAULTS for any number of elements of LIST.
   |This is useful for implementing functions that take multiple optDft arguments.
   |Each default is evaluated lazily, only when needed.
   |
   |$(fn list . defaults)
   |$(type fexpr)
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

(assert (optDft* '(1 () 3) 1 2 3 4) '(1 2 3 4))

(def\ optDft! (opt)
  #|Returns the contents of the OPTION or signals an error if it is #null.
   |
   |$(fn option)
   |$(type fexpr)
   |#
  (optDft opt (simpleError "Option is #null")))


#|! OptValue Member Member? !Member? OptKey Assoc Member*?
 |#

(def\ (optValue key lst)
  #|Search for the KEYWORD in the property LIST (a list of alternating keywords and values)
   |and return the next value as an option if found it, #null otherwise.
   |
   |$(fn keyword list)
   |$(type function)
   |#
  (let1 loop (lst lst)
    (if (cons? lst)
      (let1 ((k v . lst) lst)
        (if (== k key) (cons v) (loop lst)) )
      #null )))

(assert (optValue :b '(:a 1 :b 2 :c 3)) '(2))
(assert (optValue 'b '(a 1 b 2 c 3)) '(2))

(def\ (member k lst . keywords)
  #|Search for ITEM in the LIST according to the CMP predicate (defaults to `==').
   |Return, if found it, the tail of the list starting with ITEM after apply the GET function (defaults to 'idf'), #null otherwise.
   |The KEY function is applied to each list element before comparison (defaults to `idf').
   |
   |$(fn item list &Value cmp key ret))
   |$(type function)
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
  #|Return true if the ITEM is in the LIST, false otherwise.
   |
   |$(fn (item list &key cmp key ret))
   |$(type function)
   |#
  (cons? (apply** member key lst keywords)) )

(def\ (!member? key lst . keywords)
  #|Return false if the ITEM is in the LIST, true otherwise.
   |
   |$(fn (item list &key cmp key ret))
   |$(type function)
   |#
  (null? (apply** member key lst keywords)) )

(assert (member 'b '(a b c d)) '(b c d))
(if (intStr) (assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")))

(def\ (optKey key lst)
  #|Return ITEM if the ITEM is in the LIST, true otherwise.
   |ITEM can be an sigle element or a list of element
   |
   |$(fn (item list))
   |$(type function)
   |#
  (if (cons? key) ((rec\ (loop lst) (if (cons? lst) (let1 (k (car lst)) (if (member? k key) k (loop (cdr lst)))) #null)) lst)
    (member? key lst) key
    #null ))

(assert (optKey :b '(:a :c)) #null)
(assert (optKey :b '(:a :b :c)) :b)
(assert (optKey (:b :d) '(:a :b :c)) :b)
(assert (optKey (:b :c) '(:a :b :c)) :b)

(def\ (assoc k lst)
  #|Return the car of a assoc LIST (a list of lists of keywords and values) is ITEM is the caar if == to ITEM, #null otherwise.
   |
   |$(fn (item list))
   |$(type function)
   |#
  (member k lst :key car :get car) )

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (member*? key . lst)
  #|Return true if the ITEM is in the LIST, false otherwise.
   |
   |$(fn (item . list))
   |$(type function)
   |#
  (member? key lst) )


#|! Case MatchObj? MatchObj*? CaseType CaseType\
 |#

(defVau (case exp . clauses) env
  #|Multi-armed value test.
   |Evaluate KEY and Go through the CLAUSES.
   |If CLAUSES is #null return #inert.
   |If car of CLAUSE is else or 'eq?' KEY VALUE or 'member? :cmp eq?' KEY VALUES
   |  if car FORMS is => apply evaluate cadr FORMS to VALUE
   |  otherwise evaluate FORMS as an implicit `begin'.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn (key . clauses))
   |$(type fexpr)
   |$(syntax clauses (clause . clauses))
   |$(syntax clause (else . forms))
   |$(syntax clause (else => apv1))
   |$(syntax clause (value . forms))
   |$(syntax clause (value => apv1))
   |$(syntax clause (values . forms))
   |$(syntax clause (values => apv1))
   |$(syntax values (value . values))
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

(def matchObj?
  #|Return true if OBJECT is an instance of CLASE and the optional specified ATTRIBUTES are 'eq?' to relative VALUE, false otherwise.
   |
   |$(fn object (class . attributes))
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String .Field @Method))
   |#
  %matchObj?)

(def\ (matchObj*? obj class . attributes)
  #|Return true if OBJECT is an instance of CLASE and the optional specified ATTRIBUTES are 'eq?' to relative VALUE, false otherwise.
   |
   |$(fn object (class . attributes))
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String .Field @Method))
   |#
  (matchObj? obj (cons class attributes)) )

; vedi signalsError? in vm.lispx (o test-util.lispx) per codice simile
(defVau (caseType key . clauses) env
  #|Multi-armed type test.
   |Evaluate the OBJECT and Go through the CLAUSES.
   |If CLAUSES is #null return #inert.
   |If OBJECT is an instance of CLASS and the optional specified ATTRIBUTES are 'eq?' to relative VALUE, evaluate the FORMS as an implicit `begin'.
   |Otherwise go to the next CLAUSE.
   |
   |$(fn (object . clauses))
   |$(type fexpr)
   |$(syntax clauses (clause . clauses))
   |$(syntax clause (class . forms))
   |$(syntax clause ((class . attributes) . forms))
   |$(syntax attributes (or () (attribute value . attributes)))
   |$(syntax attribute (or Symbol Keyword String .Field @Method))
   |#
  (let1 (key (eval key env))
    (let1 next (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((test . forms) . clauses) clauses)
          (if (|| (== test 'else)
                  (let* ( (symbol? (symbol? test))
                          (class (eval (if symbol? test (car test)) env)) )
                    (if symbol? (type? key class) (matchObj? key (eval (cons 'list test) env))) ))
            (if (== (car forms) '=>)
              ((eval (cadr! forms) env) key)
              (apply begin forms env) )
            (next clauses) ))))))

(assert (caseType 2.0 (else 3)) 3)
(assert (caseType (+ 2 2) (else => (\(v) v))) 4)
(assert (caseType 2.0 (String "string") (Double "double")) "double")
(assert (caseType (new Obj :a 1) (Double "double") ((Obj :a 1) "Obj :a 1")) "Obj :a 1")

(defMacro (caseType\ (#! (1 Symbol) key) . clauses)
  #|Return a sigle argument function for multi-armed type test, useful as a catch handler.
   |
   |$(fn (symbol) . clauses))
   |$(type macro)
   |$(derivation (\ (symbol) (caseType symbol . clauses)))
   |$(use (catchWth (caseType\ (e) ...) ...)
   |#
  (list '\ key (list* 'caseType (car key) clauses) ))

(assert (catchWth (caseType\ (e) ((Error :type 'xx) 1) (else 2)) (error :type 'zz)) 2)


#|! Sort
 |#

(def\ (sort lst . opt)
  #|Return the sorted LIST, getting the key with the :key function (defaults to 'idf') and up or down with key :up or :dn (defaults to :up).
   |
   |$(fn list . (#! 0 3 (or (2 :key function) (1 (or :up :dn)) )))
   |$(type function)
   |#
  (def cmp (case (optKey (:up :dn) opt) ((#null :up) <) (:dn >=)))
  (def key (optDft (optValue :key opt) idf))
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
 |Type and value checks are possible with (#! check symbol) wherever there is a Symbol in a definined or parameters tree including all lexical bindings.
 |
 |A `check` can be:
 |- a `value`,
 |- the `Any` class,
 |- a `Class`,
 |- a `List` with zero, one or two `Integers` followed by zero or more `checks`,
 |- a `List` with an `Integer` and the symbol `oo` followed by zero or more `checks`,
 |- a `List` with car equals `or` followed by two or more `check`,
 |- a `List` with car equals `and` followed by two or more `check`,
 |- a `List` with car `Apv` followed by zero o more arguments.
 |
 |When the `check` is:
 |- a `value`: the parameter value must be equal to that `value`
 |- the `Any` class : the parameter can be any value
 |- a `Class`: the parameter value can be an instance of that `Class` or a class that extends that `Class`
 |- a `List`: the parameter value must be a `List` where, in the `check` `List`,
 |  - the first `Integer` indicates the minimum number of elements, default is `0`
 |  - the second `Integer` indicates the maximum number of elements
 |    default is the value of the first `Integer` if present, otherwise `oo` for `Integer.maxValue`
 |    for the second `Integer` can be also specified the symbol `oo` for `Integer.maxValue`
 |  - if the number of `check` arguments in the list is
 |    - less than the minimum: parameters values that exceed the minimum can have any value
 |    - greater than the minimum: the parameters values exceeding the minimum will be checked cyclically using `check` arguments exceeding the minimum
 |  - if the first element of the list is:
 |    - `or`: the parameter value must match one of the `check` arguments of the `or`
 |    - `and`: the parameter value must match all the `check` arguments of the `and`
 |    - `Apv`: applying `Apv` to the cons of the parameter value and the remaining arguments must return `#true`
 |
 |$(syntax defieniendTree (or symbol decomposeTree))
 |$(syntax parametersTree (or () #ignore symbol decomposeTree))
 |$(syntax decomposeTree ((or () #ignore symbol decomposeTree) . decomposeTree))
 |$(syntax symbol (or Symbol (#! check Symbol)))
 |$(syntax check Any)
 |$(syntax check Class)
 |$(syntax check checks)
 |$(syntax checks (check . checks))
 |$(syntax check (min . checks))
 |$(syntax check (min max . checks))
 |$(syntax check (min oo . checks))
 |$(syntax check (or . checks))
 |$(syntax check (and . checks))
 |$(syntax check (Apv . moreArguments))
 |$(syntax check value)
 |#

(def\ assert#t (boolean)
  #|Return #inert if BOOLEAN is true, signal an error otherwise.
   |
   |$(fn boolean)
   |$(type function)
   |#
  (unless boolean (error (new Error "invalid assetion" :type 'assert :datum boolean :expected #t))))

(def the
  #|Return OBJECT if OBJECT is an instance of the CLASS, signal a `type-error' otherwise.
   |
   |$(fn class object)
   |$(type function)
   |#
  %the)

(defVau (check o ck) env
  #|Return the lenght of OBJECT if match CHECK, signals an error otherwise.
   |
   |$(fn object check)
   |$(type fexpr)
   |#
  (apply %check (list (eval o env) ck) env) )

(defMacro (check* o . cks)
  #|Return the lenght of OBJECT if match CHECKS, signals an error otherwise.
   |
   |$(fn object . checks)
   |$(type macro)
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
  #|Return true if OBJECT match CHECK, false otherwise.
   |
   |$(fn object check)
   |$(type fexpr)
   |#
  (catchWth #f
    (apply check args env)
    #t ))

(defMacro (the+ ck obj)
  #|Return OBJECT if OBJECT match CHECK, signals a error otherwise.
   |
   |$(fn check object)
   |$(type macro)
   |#
  (list 'let1 (list 'obj obj) (list 'check 'obj ck) 'obj))

(assert (the+ Integer 1) 1)
(assert (the+ Integer "1") Error :type 'type :datum "1" :expected 'Integer)
(assert (the+ (or 1 2) 1) 1)
(assert (the+ (or 1 2) 2) 2)
(assert (the+ (or 1 2) 3) Error :type 'type :datum 3 :expected '(or 1 2))

; TODO non è più così costosa la conversione, si può fare
; (def the the+)


#|! Block ReturnFrom Loop For While Until Repeat DoTimes
 |#

(defVau block (blockName . forms) env
  #|Establish a block named BLOCK-NAME and evaluate the FORMS as an implicit `begin', and return the result.
   |The forms may use `returnFrom' to nonlocally exit from the block.
   |Note that unlike in Common Lisp, there is no separate namespace for block names;
   |a block is named in the normal variable namespace.
   |
   |$(fn blockName . forms)
   |$(type fexpr)
   |#
  (let* ( (tag (cons #inert)) ; cons up a fresh object as tag
          (escape (\ value (throwTag tag (if (cons? value) (car! value))))) )
    (catchTag tag
      (eval (list (list* '\ (list blockName) forms) escape) env) )))

(assert (block exit (exit)) #inert)
(assert (block exit (exit 7)) 7)
(assert (block exit (def x 1) (loop (if (== x 4) (exit 7)) (def x (+ x 1)))) 7)

(def\ returnFrom (blockName . value)
  #|Abort evaluation and return the optional VALUE (which defaults to #inert) from the block named BLOCK-NAME.
   |It is an error to return from a block whose dynamic extent has ended.
   |
   |$(fn (blockName . value))
   |$(type function)
   |#
  (blockName (optDft value #inert)) )

(assert (block ciclo (def x 1) (loop (if (== x 4) (returnFrom ciclo 7)) (def x (+ x 1)))) 7)

(defVau while (testForm . forms) env
  #|Evaluate FORMS as an implicit `begin' while TEST-FORM evaluates to true.
   |Defined using block and returnFrom.
   |
   |$(fn testForm . forms)
   |$(type fexpr)
   |#
  (let ((forms (cons 'begin forms)))
    (block exit
      (loop
        (if (eval testForm env)
          (eval forms env)
          (returnFrom exit #inert)) ))))

(defde\ (mkTag tag (#! (and Integer (>= 0) (<= %deep)) n))
  #|Return a tag for the throws forms by joining TAG and N.
   |Get the %deep of the enhanced loop form in the dynamic environment.
   |Used from break, continue, until and while forms for the enhanced loops.
   |
   |$(fn tag n)
   |$(type dynamic function)
   |#
  (symbol ($ tag (- %deep n))) )

(defMacro (break . forms) (list* 'throwTag (list 'mkTag "break" 0) forms))
(defMacro (break- n . forms) (list* 'throwTag (list 'mkTag "break" n) forms))
(defMacro (break? b . forms) (list 'if b (list* 'throwTag (list 'mkTag "break" 0) forms)))
(defMacro (break-? n b . forms) (list 'if b (list* 'throwTag (list 'mkTag "break" n) forms)))

(defMacro (continue . forms) (list* 'throwTag (list 'mkTag "continue" 0) forms))
(defMacro (continue- n . forms) (list* 'throwTag (list 'mkTag "continue" n) forms))
(defMacro (continue? b . forms) (list 'if b (list* 'throwTag (list 'mkTag "continue" 0) forms)))
(defMacro (continue-? n b . forms) (list 'if b (list* 'throwTag (list 'mkTag "continue" n) forms)))

(defMacro (until? b . forms) (list 'if b        (list* 'throwTag (list 'mkTag "break" 0) forms)))
(defMacro (while? b . forms) (list 'if b #inert (list* 'throwTag (list 'mkTag "break" 0) forms)))

(def %loop
  #|Redefine the primitive %loop to add the clauses for and for1 and the forms break, continue, while and until.
   |
   |$(fn . forms)
   |$(fn for1 binding . forms)
   |$(fn for bindings . forms)
   |$(type fexpr)
   |$(syntax bindings (binding . bindings))
   |$(syntax binding (symbol initForm . incrFrom))
   |$(syntax forms (form . forms) )
   |$(syntax form (break . forms))
   |$(syntax form (break- n . forms))
   |$(syntax form (break? testForm . forms))
   |$(syntax form (break-? n testForm . forms))
   |$(syntax form (continue . forms))
   |$(syntax form (continue- n . forms))
   |$(syntax form (continue? testForm . forms))
   |$(syntax form (continue-? n testForm . forms))
   |$(syntax form (while? testForm . forms))
   |$(syntax form (until? testForm . forms))
   |$(syntax form ...)
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
  #|Alias of %loop.
   |#
  %loop)

(defMacro (for1 binding cond . forms)
  #|Single variabile for loop.
   |
   |$(fn binding whileForm . forms)
   |$(type macro)
   |$(syntax binding (symbol initForm . incrFrom))
   |#
  (list* 'loop 'for1 binding
    (if (%ignore? cond) forms
      (cons (list 'while? cond) forms) )))

(defMacro (for bindings cond . forms)
  #|Multile variabiles for loop.
   |
   |$(fn bindings whileForm . forms)
   |$(type macro)
   |$(syntax bindings (binding . bindings))
   |$(syntax binding (symbol initForm . incrFrom))
   |#
  (list* 'loop 'for bindings
    (if (%ignore? cond) forms
      (cons (list 'while? cond) forms) )))

(let ()
  (assert (loop (break 3)) 3)
  (assert (loop (loop (break- 1 3))) 3)
  (defMacro (+= n v) (list 'set! n :rhs (list '+ n v)) )
  (defMacro (-= n v) (list 'set! n :rhs (list '- n v)) )
  (assert (let1 (a 0) (loop for ((x 0 (1+ x)) (y 0 (1- y))) (while? (< x 3) a) (+= a (+ x y)))) 0)
  (assert (let1 (a 0) (loop for ((x 0 (1+ x))) (while? (< x 3) a) (+= a (* x 10)) (loop for ((y 0 (1+ y))) (break? (> y 2)) (+= a y)) )) 39)
  (assert (loop for1 (x 0 (1+ x)) (break x)) 0)
  (assert (loop for1 (x 0 (1+ x)) (while? (< x 3) x)) 3)
  (assert (let1 (a 0) (loop for1 (x 0 (1+ x)) (while? (< x 3) a) (+= a (* x 10)) (loop for1 (y 0 (1+ y)) (break? (> y 2)) (+= a y) ))) 39)
  (assert (let1 (a 0) (loop for1 (x 0 (1+ x)) (while? (< x 3) a) (+= a (* x 10)) (loop for1 (y 0 (1+ y)) (break-? 1 (> y 3) a) (+= a y)))) 6)
  (assert (let1 (a 0) (for1 (x 0 (1+ x)) (< x 3) (+= a (* x 10)) (for1 (y 0 (1+ y)) #ignore (break-? 1 (> y 3) a) (+= a y)))) 6)
  (assert (let1 (a 0) (for ((x 0 (1+ x)) (y 10 (1- y)) ) (< x 10) (+= a (+ x y))) a) 100)
)

(defMacro (while cond . forms)
  #|Evaluate FORMS as an implicit `begin' while WHILE-FORM evaluates to true.
   |
   |$(fn whileForm . forms)
   |$(type macro)
   |#
  (list* 'loop (list 'while? cond) forms) )

(defMacro until (cond . forms)
  #|Evaluate FORMS as an implicit `begin' until UNTIL-FORM evaluates to false.
   |
   |$(fn whileForm . forms)
   |$(type macro)
   |#
  (list* 'loop (list 'until? cond) forms) )

(let ()
  (defMacro (++ n) (list 'set! n :rhs (list '1+ n)) )
  (defMacro (-- n) (list 'set! n :rhs (list '1- n)) )
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
(defMacro doTimes ((var times . result) . forms)
  #|Cf. Common Lisp's DOTIMES.
   |
   |$(fn (symbol times . endForm) . forms)
   |$(type macro)
   |#
  (let1\
    (doTimes (times forms result)
      (let1 (i (newBox 0))
        (while (< (i) times) (forms (i)) (++ i) )
        (result (i)) ))
    (list doTimes
      times
      (list* '\ (list var) forms)
      (list* '\ (list var) result) )))
|#

(defVau (repeat times . forms) env
  #|Evaluate FORMS as an implicit `begin' TIMES times.
   |
   |$(fn times . forms)
   |$(fn (symbol times . endForms) . forms)
   |$(type macro)
   |#
  (if (cons? times)
    (let* ( (((#! Symbol var) times . ending) times)
            ((#! (and Integer (> 0)) times) (eval times env))
            (env (newEnv env var 0)) )
      (loop (def result (apply begin forms env))
        (break? (>= (eval (list 'set! var :rhs (list '1+ var)) env) times)
          (if (null? ending) result (apply begin ending env)) )))
    (let1 ((#! (and Integer (> 0)) times) (eval times env))
      (loop (def result (apply begin forms env))
        (break? (0? (set! times :rhs (1- times))) result) ))))

(let ()
  ;(defMacro (++ n) (list 'set! n :rhs (list '1+ n)) )
  ;(defMacro (-- n) (list 'set! n :rhs (list '1- n)) )
  (defMacro (+= n v) (list 'set! n :rhs (list '+ n v)) )
  ;(defMacro (-= n v) (list 'set! n :rhs (list '- n v)) )
  (assert (let1 (a 0) (repeat 4 (+= a 1)) a) 4)
  (assert (let1 (a 0) (repeat (i 4) (+= a i)) a) 6)
  (assert (let1 (a 0) (repeat (i 4 a) (+= a i))) 6)
)

(defMacro doTimes ((var times . result) . forms)
  #|Cf. Common Lisp's DOTIMES.
   |
   |$(fn (symbol times . endForm) . forms)
   |$(type macro)
   |#
  (list* 'repeat (list* var times result) forms) )

#| TODO in alternativa del precedente, da verificare
(defVau (repeat times . forms) env
  #|Evaluate FORMS as an implicit `begin' TIMES times.
   |
   |$(fn times . forms)
   |$(fn (symbol times . endForms) . forms)
   |$(type macro)
   |#
  (if (cons? times)
    (let* ( (((#! Symbol var) times . ending) times)
            ((#! (and Integer (>= 0)) times) (eval times env))
            (env (newEnv env var 0)) )
      (loop (break? (>= (env var) times) (if (cons? ending) (apply begin ending env) (0? times) #inert result) )
            (def result (apply begin forms env))
            (eval (list '++ var) env) ))
    (let1 ((#! (and Integer (> 0)) times) (eval times env))
      (loop (def result (apply begin forms env))
        (if (0? (-- times)) (break result)) ))))

(defMacro doTimes ((var times . result) . forms)
  #|Cf. Common Lisp's DOTIMES.
   |
   |$(fn (symbol times . endForm) . forms)
   |$(type macro)
   |#
  (list* 'repeat (list* var times (if (null? result) (cons #inert) result)) forms) )
|#

(defMacro doList ((var lst . resultForms) . bodyForms)
  #|Cf. Common Lisp's DOLIST.
   |
   |$(fn symbol list . forms)
   |$(type macro)
   |#
  (let1rec\
    (doList (lst body\ result\)
      (if (null? lst) (result\ lst)
        (else
          (body\ (car lst))
          (doList (cdr lst) body\ result\) )))
    (list doList
      lst
      (list* '\ (list var) bodyForms)
      (list* '\ (list var) resultForms) )))


#|! Lists
 |#

(def\ (any? f lst . lst*)
  #|Return true if the apply of the FUNCTION to every first elements of the LISTS return true, false otherwise.
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (any? lst) (if (null? lst) #f (f (car lst)) #t (any? (cdr lst))) ) lst)
    ((rec\ (any*? lst*) (if (null? (car lst*)) #f (apply f (map car lst*)) #t (any*? (map cdr lst*))) ) (cons lst lst*)) ))

(assert (any? null? (1 2 3 4)) #f)
(assert (any? null? (1 2 () 4)) #t)
(assert (any? > '(1 2) '(3 4)) #f)
(assert (any? < '(1 2) '(3 4)) #t)

(defMacro (any*? f . lst)
  #|Return true if the apply of the FUNCTION to every element of VALUES return true, false otherwise.
   |
   |$(fn function . values).
   |$(type function).
   |#
  (list 'any? f lst))

(def\ (all? f lst . lst*)
  #|Return true if the apply of the FUNCTION to one first elements of the LISTS return true, false otherwise.
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (all? lst) (if (null? lst) #t (f (car lst)) (all? (cdr lst)) #f) ) lst)
    ((rec\ (all*? lst*) (if (null? (car lst*)) #t (apply f (map car lst*)) (all*? (map cdr lst*)) #f) ) (cons lst lst*)) ))

(assert (all? number? (1 2 3 4)) #t)
(assert (all? number? (1 2 () 4)) #f)
(assert (all? > '(1 2) '(3 4)) #f)
(assert (all? < '(1 2) '(3 4)) #t)

(defMacro (all*? f . lst)
  #|Return true if the apply of the FUNCTION to one element of VALUES return true, false otherwise.
   |
   |$(fn function . values).
   |$(type function).
   |#
  (list 'all? f lst))

(def\ (forEach# f lst . lst*)
  #|Return #inert after applies the FUNCTION to every first elements of the LISTS.
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) lst)
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) (cons lst lst*)) ))

(assert (forEach# (\ (#ignore)) '(1 2)) #inert)
(assert (forEach# (\ (#ignore #ignore)) '(1 2) '(3 4)) #inert)
(assert (let1 (n 1) (forEach# (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach# (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ (forEach f lst . lst*)
  #|Return LISTS after applies the FUNCTION to every first elements of the LISTS.
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    (let1 (res lst) ((rec\ (forEach lst) (if (null? lst) res (else (f (car lst)) (forEach (cdr lst)) ))) res))
    (let1 (res* (cons lst lst*)) ((rec\ (forEach* lst*) (if (null? (car lst*)) res* (else (apply f (map car lst*)) (forEach* (map cdr lst*)) ))) res*) )) )

(assert (forEach (\ (#ignore)) '(1 2)) '(1 2))
(assert (forEach (\ (#ignore #ignore)) '(1 2) '(3 4)) '((1 2) (3 4)))
(assert (let1 (n 1) (forEach (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ maplist (f lst . lst*)
  #|Return the list of the results of apply the FUNCTION, which must return a list, to every first elements of the LISTS.
   |(Note: this currently uses `append', but might be changed to use `nconc' in the future, like Common Lisp.)
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (maplist lst) (if (null? lst) #null (append (f (car lst)) (maplist (cdr lst))))) lst)
    ((rec\ (maplist* lst*) (if (null? (car lst*)) #null (append (apply f (map car lst*)) (maplist* (map cdr lst*))))) (cons lst lst*)) ))

(assert (maplist (\ (x) (list x)) '(1 2 3 4)) '(1 2 3 4))

(def\ (filter f lst . lst*)
  #|Returns the list of the first elements of the LISTS for which the application of the FUNCTION returns true.
   |(Note: this currently uses `append', but might be changed to use `nconc' in the future, like Common Lisp.)
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (filter lst) (if (null? lst) #null (if (f (car lst)) (cons (car lst) (filter (cdr lst))) (filter (cdr lst))))) lst)
    ((rec\ (filter* lst*) (if (null? (car lst*)) #null (let1 (cars (map car lst*)) (if (apply f cars) (cons cars (filter* (map cdr lst*))) (filter* (map cdr lst*)) )))) (cons lst lst*)) ))

(assert (filter even? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (filter != '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(defMacro (remove f lst . lst*)
  #|Returns the list of the first elements of the LIST without those for which the application of the FUNCTION returns true.
   |
   |$(fn function . lists).
   |$(type function).
   |#
  (list* 'filter (list 'compose '! f) lst lst*) )

(assert (remove odd? '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8))
(assert (remove == '(1 2 3) '(3 2 1)) '((1 3) (3 1)))

(def\ (reduceL f init lst . lst*)
  #|Use the FUNCTION to combine the elements of the LIST from first element.
   |The INITIAL-VALUE is logically placed before the first element of the LIST.
   |
   |$(fn function  init . lists).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (reduce (f acc (car lst)) (cdr lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (reduce* (apply* f acc (map car lst*)) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (reduceL + 0 '(1 2 3 4)) 10)
(assert (reduceL (\ (init lst) (+ init (reduceL * 1 lst))) 0 '(1 2 3 4) '(1 2 3 4)) 30)
(assert (reduceL cons () '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))

(def reduce
  #|Alias of reduceL.
   |#
  reduceL)

(def\ (reduceR f init lst . lst*)
  #|Use the FUNCTION to combine the elements of the LIST from last element.
   |The INITIAL-VALUE is logically placed before the last element of the LIST.
   |
   |$(fn function  init . list).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (reduce acc lst) (if (null? lst) acc (f (reduce acc (cdr lst)) (car lst)) )) init lst)
    ((rec\ (reduce* acc lst*) (if (null? (car lst*)) acc (apply* f (reduce* acc (map cdr lst*)) (map cadr lst*)) )) init (cons lst lst*)) ))

(assert (reduceR cons () '(1 2 3 4)) '((((() . 4) . 3) . 2) . 1))

(def\ (foldL f init lst . lst*)
  #|Use the FUNCTION to combine the elements of the LIST from first element.
   |The INITIAL-VALUE is logically placed after the last element of the LIST.
   |
   |$(fn function  init . list).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (foldl acc lst) (if (null? lst) acc (f (car lst) (foldl acc (cdr lst)) ) )) init lst)
    ((rec\ (foldl* acc lst*) (if (null? (car lst*)) acc (apply* f (map car lst*) (foldl* acc (map cdr lst*)) ) )) init (cons lst lst*)) ))

(assert (foldL cons () '(1 2 3 4)) '(1 2 3 4))

(def\ (foldR f init lst . lst*)
  #|Use the FUNCTION to combine the elements of the LIST from last element.
   |The INITIAL-VALUE is logically placed after the last element of the LIST.
   |
   |$(fn function  init . list).
   |$(type function).
   |#
  (if (null? lst*)
    ((rec\ (foldr acc lst) (if (null? lst) acc (foldr (f (car lst) acc) (cdr lst)) )) init lst)
    ((rec\ (foldr* acc lst*) (if (null? (car lst*)) acc (foldr* (apply* f (map car lst*) acc) (map cdr lst*)) )) init (cons lst lst*)) ))

(assert (foldR cons () '(1 2 3 4)) '(4 3 2 1))

(defMacro doList ((var lst . resultForms) . bodyForms)
  #|Cf. Common Lisp's DOLIST.
   |
   |$(fn symbol list . forms)
   |$(type macro)
   |#
  (let1rec\
    (doList (lst body\ result\)
      (if (null? lst) (result\ lst)
        (else
          (body\ (car lst))
          (doList (cdr lst) body\ result\) )))
    (list doList
      lst
      (list* '\ (list var) bodyForms)
      (list* '\ (list var) resultForms) )))


#|! Arrays
 |#

(def\ (array->list arr)
  #|Array to proper list.
   |
   |$(fn array)
   |$(type function)
   |#
  (%array->list #t arr) )

(def\ (array->cons arr)
  #|Array to improper list.
   |
   |$(fn array)
   |$(type function)
   |#
  (%array->list #f arr) )

(def list->array
  #|List to array.
   |
   |$(fn list)
   |$(type function)
   |#
  %list->array)

(def\ (array . args)
  #|List to array.
   |
   |$(fn . list)
   |$(type function)
   |#
  (list->array args))

(def\ (arrayMap fun (#! Object[] arr))
  #|Return a new array by applying the FUNCTION to each element of the ARRAY.
   |
   |$(fn function array)
   |$(type function)
   |#
  (list->array (map fun (array->list arr))) )

(assert (arrayMap 1+ (array 1 2 3)) (array 2 3 4))

(def\ (arrayFilter pred (#! Object[] arr))
  #|Returns a new array with the elements of the ARRAY for which the application of the FUNCTION returns true.
   |
   |$(fn function array)
   |$(type function)
   |#
  (list->array (filter pred (array->list arr))) )

(assert (arrayFilter odd? (array 1 2 3)) (array 1 3))

(def\ (newInstance class dim . dims)
  #|Returns a new array with the elements of CLASS tipe and the specified DIMENSIONS.
   |
   |$(fn class . dimensions)
   |$(type function)
   |#
  (apply** @newInstance Array class dim dims))

(def\ (arrayGet array index)
  #|Returns the element at INDEX position int the ARRAY.
   |
   |$(fn array index)
   |$(type function)
   |#
  (if (cons? index)
    (apply** arrayGet* array index)
    (@get Array array index) ))

(def\ (arrayGet* array . indexes)
  #|Returns the element at INDEXES position int the multi-dimensions ARRAY.
   |
   |$(fn array . indexes)
   |$(type function)
   |#
  (let loop ((array array) (indexes indexes))
    (if (null? indexes) array
      (loop (arrayGet array (car indexes)) (cdr indexes)) )))

(def\ (arraySet array index value)
  #|Returns the ARRAY after update the element at INDEX position with VALUE.
   |
   |$(fn array index value)
   |$(type function)
   |#
  (if (cons? index)
    (apply** arraySet* array value index)
    (else (@set Array array index value) array) ))

(def\ (arraySet* array0 value . indexes)
  #|Returns the multi-dimensions ARRAY after update the element at INDEXES position with VALUE.
   |
   |$(fn array value . indexes)
   |$(type function)
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
  #|Return true if all element of LIST are distinct, false otherwise.
   |
   |$(fn list)
   |$(type function)
   |#
  (if (null? lst) #t (apply /= lst)))

(def\ (set+ lst v)
  #|Return a set, with an VALUE more if VALUE not is in the SET.
   |$(fn set value)
   |$(type function)
   |#
  (if (member? v lst) lst (cons v lst)))

(defVau (defSet+ (#! Symbol plc) v) env
  #|Return the set after update the set of SYMBOL with an VALUE more if VALUE not is in the set.
   |
   |$(fn symbol value)
   |$(type function)
   |#
  (let ( (v (eval v env)) (lst (env plc)) )
    (if (member? v lst) lst (env :def :rhs plc (cons v lst))) ))

(def\ ->set (lst)
  #|Return the LIST without duplicate elements.
   |
   |$(fn list)
   |$(type function)
   |#
  (let loop ( (res ()) (lst lst) )
    (if (null? lst) (reverse res)
      (let1 ((v . lst) lst)
        (loop (if (member? v res) res (cons v res)) lst) ))))

#| TODO da valutare
(def\ ->set (lst)
  #|Return the LIST without duplicate elements.
   |
   |$(fn list)
   |$(type function)
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

(defMacro %´ ((#! (or Symbol List) x))
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
            (if (member*? (car r) apostrophe bang)
              (expd2 lev sep mk t r)
              (expd2 lev sep mk (cons (car r) (if (cons? t) t (cons t))) (cdr r)) )
          (%error ("invalid syntax " f)) ))))
  (def\ (expd0 x)
     (map [_ (if (>= (@indexOf ";!,'…_" (%subString _ 0 1)) 0) (symbol _) (car (@toLispList vm _)))]
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


#|! Boxes
 |The Box are functions that encapsulates a mutable value.
 |
 |Calling the box:
 |- without arguments returns the value in the box.
 |- with an attribute update the value in the box.
 |- without bindResult or with bindResult #ignore use as bindResult (bndRes)
 |- with bindResult #inert return #inert
 |- with bindResult :rhs return the right side of the last binding
 |- with bindResult :prv return the previous value of the last binding
 |- with bindResult :cnt return the box
 |
 |$(box)
 |$(box value)
 |$(box bindResult value)
 |$(type function)
 |$(syntax bindResult (or #ignore #inert :rhs :prv :cnt))
 |#

(def newBox
  #|Return a new box with the optional VALUE.
   |The Box are functions that encapsulates a mutable value.
   |Without VALUE use as VALUE (boxDft).
   |
   |$(fn . value)
   |$(type function)
   |#
  %newBox)

(defMacro (defBox name . value)
  #|Defines into the current environment the named box NAME with the optional VALUE.
   |Without VALUE use as VALUE (boxDft).
   |
   |$(fn name . value)
   |$(type macro)
   |#
  (list 'def name (cons 'newBox value)) )


#|! Dynamic Bindings
 |DVar estends Box
 |#

(def newDVar
  #|Define a new dynamic variable with optional default VALUE.
   |
   |$(fn . value)
   |$(type function)
   |#
  %newDVar)

(def dval
  #|Return the current value of the DYNAMIC-VARIABLE.
   |
   |$(fn dvar)
   |$(type function)
   |#
  %dVal)

(defMacro (ddef var . val)
  #|Define a new or update an existing dynamic variable with the given NAME and optional default VALUE.
   |
   |$(fn name . value)
   |$(type macro)
   |#
  (list* (list '%dv\ (list var)) val) )

(defMacro (ddef* var* . val*)
  #|Define a new or update an existing dynamic variable with the given NAMES and optional default VALUES.
   |
   |$(fn (names) . values)
   |$(type macro)
   |#
  (list* (list '%dv\ var*) val*) )

(def\ (dget dvar)
  #|Return the current value of the DYNAMIC-VARIABLE.
   |
   |$(fn dvar)
   |$(type macro)
   |#
  (dvar))

(def\ (dset dvar value)
  #|Set the current value of the DYNAMIC-VARIABLE.
   |
   |$(fn dvar value)
   |$(type macro)
   |#
  (dvar value))

(defMacro (dlet bindings form . forms)
  #|With the dynamic variables specified by NAMES temporarily bound to new VALUES, evaluate the FORMS as an implicit `begin'.
   |Bindings are established parallely as per `let'.
   |
   |$(fn bindings . forms)
   |$(type macro)
   |$(syntax bindings ((name value) . bindings))
   |#
  (cons (list* '%dv\ (map car bindings) form forms) (map cadr bindings)) )

(defMacro (progv var* val* exp . exps)
  #|With the dynamic variables specified by NAMES temporarily bound to new VALUES, evaluate the FORMS as an implicit `begin'.
   |The NAMES and VALUES lists must have the same length.
   |
   |$(fn names values . forms)
   |$(syntax names (name . names))
   |$(syntax values (value . values))
   |$(type macro)
   |#
  (cons (list* '%dv\ var* exp exps) val*) )

(defMacro (dlet* bindings . forms)
  #|With the dynamic variables specified by NAMES temporarily bound to new VALUES, evaluate the FORMS as an implicit `begin'.
   |Bindings are established serially as per `let*'.
   |
   |$(fn bindings . forms)
   |$(type macro)
   |$(syntax bindings ((name value) . bindings))
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

(def\ findClass ((#! Symbol name) env)
  #|Look up a class based on its NAME symbol (evaluated) in the given ENVIRONMENT.
   |
   |$(fn name environment)
   |$(type function)
   |#
  (eval name env))

(defVau defClass (name (#! (0 1 Symbol) superClass) (#! (Symbol) attributes) . properties) env
  #|Define a new `ObjClass' with the given NAME, optional SUPERCLASS, and ATTRIBUTES.
   |The SUPERCLASS defaults to `Obj'.
   |The ATTRIBUTES and PROPERTIES are currently ignored.
   |
   |$(fn (or () (superclass)) environment)
   |$(type fexpr)
   |$(syntax superclass (or () (symbol)))
   |$(syntax attributes (symbol . attributes))
   |#
  ;; Slot-specs are ignored for now, but check that they are symbols nevertheless.
  (def superClass (findClass (optDft superClass 'Obj) env))
  (eval (list 'def name (%newClass name superClass)) env) )


#|! Generic Functions
 |#

;; receiverName e parameters dei defMethod dovrebbero corrispondere a quelli del proprio defGeneric

(defVau (defGeneric . args) env
  #|Define a new generic function with the given NAME.
   |RECEIVER-NAME, PARAMETERS, and PROPERTIES are currently ignored.
   |
   |$(fn name receiverName . parameters)
   |$(fn (name receiverName) . parameters)
   |$(type fexpr)
   |#
  (if (cons? (car args))
    (def ((name receiverName . parameters) . properties) args)
    (def (name (receiverName . parameters) . properties) args) )
  (let1\ (generic args ((%getMethod (classOf (car args)) name) args))
    (eval (list 'def name generic) env) ))

(defVau (defMethod . args) env
  #|Define a new method for the generic function NAME specialized for CLASS.
   |
   |$(fn ((name (receiverName class) . parameters) . forms))
   |$(fn (name ((receiverName class) . parameters) . forms))
   |$(type fexpr)
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

  (defObj foo Foo)
  (defObj bar Bar :a 1 :b (+ 2 3))

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
  #|Defines SYMBOLS in the current environment with values generated evaluating the FORMS as an implicit `begin'.
   |
   |$(fn symbols . forms)
   |$(type fexpr)
   |$(syntax symbols (symbol . symbols))
   |#
  (eval
    (list 'def symbols
      (list 'let ()
        (cons 'begin forms)
        (cons 'list symbols) ))
    env ))

(assert (begin (provide (x) (def x 10)) x) 10)

(defVau (module symbols . forms) env
  #|Return an environment where SYMBOLS are defined with values generated evaluating the FORMS as an implicit `begin'.
   |
   |$(fn symbols . forms)
   |$(type fexpr)
   |$(syntax symbols (symbol . symbols))
   |#
  (let1 (env (newEnv env))
    (eval (list* 'provide symbols forms) env)
    (newEnv env) ))

(assert (begin (def m (module (x) (def x 10))) (eval 'x m)) 10)

(defMacro (defModule name symbols . forms)
  #|Define a `environment' with the given NAME, where SYMBOLS are defined with values generated evaluating the FORMS as an implicit `begin'.
   |
   |$(fn name symbols . forms)
   |$(type macro)
   |$(syntax symbols (symbol . symbols))
   |#
  (list 'def name (list* 'module symbols forms)) )

(assert (begin (defModule m (x) (def x 10)) (eval 'x m)) 10)

(defVau (import module symbols) env
  #|Define in the current environment the SYMBOLS defined in MODULE.
   |
   |$(fn module symbols)
   |$(type fexpr)
   |$(syntax symbols (symbol . symbols))
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
  #|Utility to create an n-ary relational operator from a BINARY-OPERATOR.
   |
   |$(fn binop)
   |$(type function)
   |#
  (rec\ (op arg1 arg2 . rest)
    (if (binop arg1 arg2)
      (if (null? rest) #t
        (apply op (cons arg2 rest)))
      #f )))

(def <
  #|Return true if the ARGUMENTS are in monotonically increasing order, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp <))
   |#
  (relationalOp <) )

(def >
  #|Return true if the ARGUMENTS are in monotonically decreasing order, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp >))
   |#
  (relationalOp >) )

(def <=
  #|Return true if the ARGUMENTS are in monotonically nondecreasing order, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp <=))
   |#
  (relationalOp <=) )

(def >=
  #|Return true if the ARGUMENTS are in monotonically nonincreasing order, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp >=))
   |#
  (relationalOp >=) )

(def eq?
  #|Return true if all ARGUMENTS are equal, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp eq?))
   |#
  (relationalOp eq?) )

(def ==
  #|Return true if all ARGUMENTS are ==, false otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (relationalOp eq?))
   |#
  (relationalOp ==))

(assert (< 1 2 3) #t)
(assert (> 3 2 1) #t)
(assert (<= 1 2 2 3) #t)
(assert (>= 3 2 2 1) #t)
(assert (eq? (1) (1) (1)) #t)
(assert (== 1 1 1) #t)

(def !=
  #|Return false if all ARGUMENTS are ==, true otherwise.
   |
   |$(fn arguments)
   |$(type function)
   |#
  (relationalOp !=))

(def\ /= (arg . args)
  #|Return true if all ARGUMENTS are distinct, false otherwise.
   |
   |$(fn argument . arguments)
   |$(type function)
   |#
  (if (null? args) #t
    (if (member? arg args :cmp eq?) #f
      (apply /= args) )))

#| TODO in sostituzione del prededente, utile?
(def\ /= args
  #|Return true if all ARGUMENTS are distinct, false otherwise.
   |
   |$(fn . arguments)
   |$(type function)
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
  #|Utility to create an n-ary thetic operator from a BINARY-OPERATOR and INITIAL-VALUE.
   |
   |$(fn binOp unit)
   |$(type function)
   |#
  (\ args (reduce binOp unit args)) )

(def +
  #|Return the sum of the ARGUMENTS, or 0 if no arguments are supplied.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (tethicOp + 0))
   |#
  (theticOp + 0) )

(def *
  #|Return the product of the ARGUMENTS, or 1 if no arguments are supplied.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (tethicOp * 1))
   |#
  (theticOp * 1) )

(def $
  #|Return the join of the ARGUMENTS, or "" if no arguments are supplied.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (tethicOp $ ""))
   |#
  (theticOp $ "") )

(assert (+ 1 2 3) 6)
(assert (* 1 2 3) 6)
(assert ($ 1 2 3) "123")

(def\ (lyticOp binOp unit)
  #|Utility to create an n-ary lytic operator from a BINARY-OPERATOR and INITIAL-VALUE.
   |
   |$(fn binOp unit)
   |$(type function)
   |#
  (\ (arg1 . rest)
    (if (null? rest)
      (binOp unit arg1)
      (reduce binOp arg1 rest) )))

(def -
  #|If only one number is supplied in the ARGUMENTS, return the negation of that number.
   |If more than one number is supplied, subtract all of the later ones from the first one and return the result.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (lithicOp - 0))
   |#
  (lyticOp - 0) )

(def /
  #|If only one number is supplied in the ARGUMENTS, return the reciprocal of that number.
   |If more than one number is supplied, divide the first one by all of the later ones and return the result.
   |
   |$(fn arguments)
   |$(type function)
   |$(derivation (lithicOp / 1))
   |#
  (lyticOp / 1) )


#|! Greatest Common Divisor e Lowest Common Multiple
 |#

(def\ (gcd a b . more)
  #|Return the greatest common divisor of two or more integer arguments.
   |
   |$(fn integer integer . integers)
   |$(type function)
   |#
  (if (null? more)
    (if (0? b) a (gcd b (% a b)))
    (gcd a (apply gcd (cons b more))) ))

(def abs
  #|Return the abs value of one integer argument.
   |
   |$(fn integer)
   |$(type function)
   |#
  (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))

(assert (gcd 8 108) 4)
(assert (gcd 108 216 432) 108)

(def\ (lcm a b . more)
  #|Return the lowest common multiple of two or more integer arguments
   |
   |$(fn integer integer . integers)
   |$(type function)
   |#
  (if (null? more)
    (if (|| (0? a) (0? b)) 0
      (abs (* b (/ a (gcd a b)))) )
    (lcm a (apply lcm (cons b more))) ))

(assert (lcm 8 108) 216)
(assert (lcm 3 4 5 6) 60)


#|! Sequences
 |#

(defGeneric length (sequence)
  #|Return the number of elements in a SEQUENCE.
   |
   |$(fn sequence)
   |$(type generic)
   |#
)
(defMethod length ((seq List))
  (%len seq))
(defMethod length ((seq Null))
  (%len seq))
(defMethod length ((seq String))
  (@length seq))


(defGeneric elt (sequence index)
  #|Return the SEQUENCE element at the specified INDEX.
   |
   |$(fn sequence index)
   |$(type generic)
   |#
)
(defMethod elt ((seq List) index)
  (nth index seq))
(defMethod elt ((seq String) index)
  (%subString seq index (+ index 1)))


(defGeneric subSeq (sequence start . end)
  #|Create a sequence that is a copy of the subsequence of the SEQUENCE bounded by START and optional END.
   |If END is not supplied, the subsequence stretches until the end of the list
   |
   |$(fn sequence start . end)
   |$(type generic)
   |#
)
(defMethod subSeq ((seq List) start . end)
  (apply** %subList seq start end))
(defMethod subSeq ((seq Null) start . end)
  (apply** %subList seq start end))
(defMethod subSeq ((seq String) start . end)
  (apply** %subString seq start end))


#|! Coroutines
 |#

(defConstant coroutinePrompt
  #|This prompt is used for general coroutine-like use of continuations.
   |#
  'coroutine-prompt)

(defMacro coroutine forms
  #|Evaluate the FORMS as an implicit `begin' in a context in which `yield' can be used to pause execution.
   |#
  (list* 'pushPrompt 'coroutinePrompt forms))

(defMacro yield (name . forms)
  #|Pause the current coroutine.
   |In the place where the enclosing `coroutine' (or `resume') was called,
   |evaluate the FORMS as an implicit `begin' with NAME bound to the paused coroutine, and return the result.
   |`resume' can later be used to restart execution inside the coroutine.
   |
   |$(fn name . forms)
   |$(type macro)
   |#
  (list* 'takeSubcont 'coroutinePrompt name forms))

(defMacro resume (k . forms)
  #|Resume the paused coroutine CONTINUATION and evaluate FORMS as an implicit `begin' in the place where `yield' was called in the coroutine, and return the result.
   |
   |$(fn continuation . forms)
   |$(type macro)
   |#
  (list* 'pushDelimSubcont 'coroutinePrompt k forms))


#|! Fibers
 |The following implementation of fibers follows the one at URL `http://okmij.org/ftp/continuations/implementations.html#dget-wind'
 |We're calling them fibers instead of coroutines so as to not conflict with the built-in coroutine operators.
 |We use it for testing that built-in operators properly suspend and resume.
 |#

(defConstant fiberPrompt
  #|The prompt used for delimiting fibers.
   |#
  'fiber-prompt)

(defClass YieldRecord ()
  #|Instances of this class are yielded.
   |#
  (value continuation) )

(def\ makeYieldRecord (v k)
  #|Create a new yield record with the given yielded VALUE and resume CONTINUATION.
   |
   |$(fn value continuation)
   |$(type function)
   |#
  (new YieldRecord :value v :continuation k))

(def\ fiberYield v
  #|Yield a VALUE (which defaults to #inert).
   |
   |$(fn . value)
   |$(type function)
   |#
  (takeSubcont fiberPrompt k
    (makeYieldRecord (optDft v #inert) k)))

(def\ fiberResume (yieldRecord . v)
  #|Resume a suspended fiber YIELD-RECORD with a VALUE (which defaults to #inert).
   |
   |$(fn yieldRecord . value)
   |$(type function)
   |#
  (pushDelimSubcont fiberPrompt (yieldRecord 'continuation)
    (optDft v #inert)))

(defMacro fiber forms
  #|Evaluate the forms expressions as a fiber.
   |
   |$(fn . forms)
   |$(type macro)
   |#
  (list* pushPrompt 'fiberPrompt forms))

(def\ runFiber (thunk . values)
  #|Get all values yielded by a fiber, and its final result, and collect them in a list.
   |Uses the optional list of values to sent to the fiber with `fiberResume'.
   |
   |$(fn thunk . forms)
   |$(type macro)
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
  #|Return the incremented value of Number, Box and Obj.
   |
   |$(fn number)
   |$(fn box)
   |$(fn obj field)
   |$(type fexpr)
   |#
  (def val (eval plc env))
  (caseType val
    (Box    (let1 (() args) (val :rhs (+ (val) 1))))
    (Obj    (let1 ((fld) args) (val :rhs fld (+ (val fld) 1))))
    (Number (let1 (() args) (eval (list 'set! plc :rhs (+ val 1)) env)))
    (else   (error ($ "not valid type: " val))) ))

(defVau (-- plc . args) env
  #|Return the decrenented value of Number, Box and Obj.
   |
   |$(fn number)
   |$(fn box)
   |$(fn obj field)
   |$(type fexpr)
   |#
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
  #|Return a fexpr which returns the value assigned to Box, Obj and Object after applying the OPERATOR.
   |
   |$(fn operator)
   |$(type function)
   |#
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
        ((rval) (eval (list 'set! plc (op lval (eval rval env))) env))
        ((key rval) (eval (list 'set! plc key (op lval (eval rval env))) env)) )))))

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (newBox 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (new Obj :fld 1)) (+= a :rhs :fld 3)) 4)


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

(defMacro (close1 binding . forms)
  #|Single try/resource
   |
   |$(fn binding . forms)
   |$(syntax binding (name . initForms))
   |$(type macro)
   |#
  (list 'let1 binding
    (list* 'atEnd
      (list '@close (car binding))
      forms )))

(defMacro (close bindings . forms)
  #|Multiple try/resource
   |
   |$(fn bindings . forms)
   |$(syntax bindings ((name . initForms) . bindings))
   |$(type macro)
   |#
  (list 'let bindings
    (list* 'atEnd
      (list 'forEach '@close (cons 'list (map car bindings)))
      forms )))


#|! Apl/J
 |#

(def\ (iota n) (reverse ((rec\ (iota n) (if (0? n) () (cons n (iota (1- n))))) n)))
(def\ (fork f l r) [_ (f (l _) (r _))])
(def\ (hook l r) [_ (l _ (r _))])


#|! Utility
 |#

(defVau (time times . forms) env
  #|Evaluate n times FORMS as implicit `begin` and return the elapsed time in milliseconds
   |
   |$(fn n . forms)
   |$(type fexpr)
   |#
  (let* ( (currentTime (@getMethod System "currentTimeMillis"))
          ((#! (and Integer (> 0)) times) (eval times env))
          (milli (currentTime #null))
          (result (apply repeat (cons times forms) env))
          (milli (- (currentTime #null) milli)) )
    (print "time " times " " forms ": " milli "ms" (if (== times 1) "" ($ ", on average: " (@format String "%.2f" (/ milli (@doubleValue times))) "ms" )))
    result ))

(def\ (make*\ n f)
  #|Returns a function that applies FUNCTION with the arguments from n placed in a list.
   |
   |$(fn n function)
   |$(type function)
   |#
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))) )

(assert ((make*\ 2 (\(a b) b)) 1 2 3 4 5) '(2 3 4 5))

(def\ (minMax a . b)
  #|Returns a cons with the min and the max of the VALUES.
   |
   |$(fn . values)
   |$(type function)
   |#
  (let loop [ (x ((caseType a (Double .POSITIVE_INFINITY) (Long .MAX_VALUE) (Integer .MAX_VALUE)) a))
              (y ((caseType a (Double .NEGATIVE_INFINITY) (Long .MIN_VALUE) (Integer .MIN_VALUE)) a))
              (a (cons a b)) ]
    (if (null? a) (cons x y)
      (loop (min x (car a)) (max y (car a)) (cdr a)) )))
