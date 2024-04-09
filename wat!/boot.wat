;;;                                                     -*- mode: Scheme -*-
;;; Wat Bootstrap
;;;

;; Copyright (c) 2021, 2022 Manuel J. Simoni

;; ``72. An adequate bootstrap is a contradiction in terms.''

;;; Core Built-Ins for Macro and Definition Forms

(%def def
  #|Match the DEFINIEND-TREE against the VALUE and place resulting bindings into the current environment.
   |without bindResult or with bindResult #ignore use as bindResult (bndRes)
   |with bindResult #inert return #inert
   |with bindResult :rhs return the right side of the last binding
   |with bindResult :prv return the previous value of the last binding
   |with bindResult :obj return the env
   |$(fn definiendTree value)
   |$(fn definiendTree bindResult value)
   |$(syntax bindResult (or #ignore #inert :rhs :prv))  
   |$(type fexpr)
   |#
  %def )

(def vau
  #|Construct a fexpr with the given PARAMETER-TREE, ENVIRONMENT-PARAMETER, and FORMS.
   |$(fn parameterTree environmentParameter . forms)
   |$(type fexpr)
   |$(derivation (vau (pt ep . forms) env (eval (list 'vau pt ep (cons 'begin forms)) env)))
   |#
  %vau )

(def \
  #|Create an anonymous function with the given PARAMETER-TREE and FORMS.
   |$(fn parameterTree . forms)
   |$(type function)
   |$(derivation (vau (parameterTree . forms) env (wrap (eval (list* 'vau parameterTree #ignore forms) env))))
   |#
  %\ )
(def lambda
  #|alias for \ :-)
   |#
  \ )

(def wrap
  #|Create a new function that wraps around an underlying OPERATOR, and induces argument evaluation around it.
   |$(fn operator)
   |$(type function)
   |#
  %wrap )

(def assert
  #|Reports an error if:
   |- EXPRESSION does not equal VALUE
   |- VALUE is not present and the EXPRESSION does not throws
   |- EXPRESSION throws or returns an object and the object is not of the same CLASS with the ATTRIBUTES of the specified VALUE.
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
   |$(fn fun args . env)
   |$(type function)
   |$(derivation (eval (cons (unwrap fun) args) (if (null? env) (newEnv) (car! env)) ))
   |#
  %apply )

(def begin
  #|Sequentially evaluate FORMS, returning the value of the last one, or void if there are no forms.
   |$(fn . forms)
   |$(type fexpr)
   |#
  %begin )

(def car
  #|Return the contents of the address part of the register.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((car . #_)) car))
   |$(derivation (@car cons))
   |#
  %car )

(def car!
  #|Return the contents of the address part of the register
   |if decrement part of the register is null, else error.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((car)) car))
   |#
  (\ ((car)) car) )

(def cadr
  #|Return the `car' of the `cdr' of the CONS.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((#_ cadr . #_)) cadr))
   |$(derivation (car (cdr cons)))
   |$(derivation (@car cons 1))
   |#
  %cadr )

(def cdr
  #|Return the contents of the decrement part of the register.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((#_ . cdr)) cdr))
   |$(derivation (@cdr cons))
   |#
  %cdr )

(def cons
  #|Create a cons with the given CAR and CDR.
   |$(fn car cdr)
   |$(type function)
   |$(derivation (@new vm Cons car cdr))
   |#
  %cons )

(def cons?
  #|Return true if the OBJECT is a cons, false otherwise.
   |$(fn object)
   |$(type function)
   |$(derivation (\ (object) (type? object Cons)))
   |#
  %cons? )

(def eval
  #|Evaluate the FORM in the ENVIRONMENT, returning its result.
   |$(fn form . environment)
   |$(type function)
   |$(derivation (eval (if (null? environment) ((vau () env env)) (car! environment))))
   |#
  %eval )

(def if
  #|Evaluate the TEST which must yield a boolean.
   |Then evaluate either the THEN or ELSE expression
   |depending on whether the TEST yielded true or false.
   |$(fn test then . forms)
   |$(syntax forms (or () (else) (test then . forms)))
   |$(type fexpr)
   |$(derivation (vau (test then . forms) env (if (eval test env) (eval then env) (null? forms) #inert (null? (cdr forms)) (eval (car forms) env) (apply if forms env))))
   |#
  %if )

(def list
  #|Return the list of evaluated ARGUMENTS.
   |$(fn . arguments)
   |$(type function)
   |$(derivation (wrap (vau arguments #ignore arguments)))
   |#
  %list )

(def list*
  #|Create a list of evaluated ARGUMENTS so that
   |the last argument becomes the `cdr' of the list.
   |$(fn . arguments)
   |$(type function)
   |#
  %list* )

(def newBox
  #|Create a new box with the optional VALUE.
   |The Box are functions that encapsulates a mutable value.
   |Calling the box without arguments returns the value in the box.
   |Calling the box with an attribute update the value in the box.
   |$(fn)
   |$(fn value)
   |$(type function)
   |#
  %newBox )

(def null?
  #|Return true if the OBJECT is nil, false otherwise.
   |$(fn object)
   |$(type function)
   |$(derivation (\ (object) (type? object Null)))
   |#
  %null? )

(def quote
  #|Return the unevaluated OPERAND.
   |$(fn operand)
   |$(type fexpr)
   |$(derivation (vau (operand) #ignore operand))
   |#
  %' )


;;; Macro

(def evalMacro (newBox #t))

(def makeMacro
  #|Create a macro from an EXPANDER operator.
   |A macro is an operator that receives an operand and produces a form
   |(by calling the expander with the operand as argument)
   |that is then evaluated in place of the operand.
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
  #|Create an anonymous macro with the given PARAMETER-TREE and FORMS.
   |$(fn parameterTree . forms)
   |$(type macro)
   |#
  (makeMacro
    (vau (pt . forms) #ignore
      (list 'makeMacro (list* 'vau pt #ignore forms)) )))

(def expand
  #|Expands a macro rather than evaluates it.
   |$(fn form)
   |$(type macro)
   |#
  (macro (form)
    (list 'begin (list 'evalMacro #f) form) ))


;;; Definition Forms

#|!
defMacro defVau def\ def*\ rec\ let1\ let1rec\ let\ and letrec\ have two equivalent forms

    (_ name parameterTree . forms)
    (_ (name . parameterTree) . forms)

rec rec\ let1rec let1rec\ letrec and letrec\ pre-<initialize all bindings to #inert before evaluating values.
|#

(def defMacro
  #|Define a macro with the given NAME, PARAMETER-TREE, and FORMS.
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
  #|Define a fexpr with the given NAME, PARAMETER-TREE, ENVIRONMENT-PARAMETER, and FORMS.
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
  #|Define a constant with the given NAME and VALUE.
   |This is mostly for documentation purposes, as constants are still mutable.
   |alias for def.
   |#
  def )

(defMacro (def* lhs . rhs)
  #|Match the DEFINIEND-TREE against the VALUES and place resulting bindings into the current environment.
   |$(fn definiendTree . values)
   |$(fn definiendTree resultType . values)
   |$(syntax resultType (or #ignore #inert :rhs :prv))  
   |$(type macro)
   |#
  (list 'def lhs (cons 'list rhs)) )

(defMacro (def\ lhs . rhs)
  #|Define a function with the given NAME, PARAMETER-TREE, and FORMS.
   |$(fn name parameterTree . forms)
   |$(fn (name parameterTree) . forms)
   |$(type macro)
   |#
  (if (cons? lhs)
    (list 'def (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'def lhs (cons '\ rhs)) ))

(assert (expand (def\ succ (n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )
(assert (expand (def\ (succ n) (+ n 1))) '(def succ (\ (n) (+ n 1))) )


;;; Other Core Built-Ins

(def apply*
  #|Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.
   |$(fn fun . args) 
   |$(type function)
   |$(derivation (apply fun args (newEnv)))
   |#
  %apply* )

(def apply**
  #|Call the FUNCTION with a dynamically-supplied list of ARGUMENTS.
   |$(fn fun . args)
   |$(type function)
   |$(derivation (apply fun (apply list* args) (newEnv)))
   |#
  %apply** )

(defVau set (ep dt value) env
  #|Match the DEFINIEND-TREE against the VALUE in the ENVIRONMENT, creating or updating existing bindings.
   |Unlike Common Lisp `setq' (or Scheme `set!') that allows updating arbitrary bindings
   |you always need to know the environment where a binding is in to change it.
   |Therefore, we usually use boxes instead of mutating bindings directly.
   |$(fn ep dt value)
   |$(type fexpr)
   |#
  (eval
    (list 'def dt (list (unwrap eval) value env))
    (eval ep env) ))

(def set!
  #|Update the bindings defined of the environment, throws an error if the binding is not defined.
   |So 'def' match the DEFINIEND-TREE against the VALUE and update the resulting bindings into environment if exist.
   |without bindResult or with bindResult #ignore use as bindResult (bndRes)
   |with bindResult #inert return #inert
   |with bindResult :rhs return the right side of the last binding
   |with bindResult :prv return the previous value of the last binding
   |with bindResult :obj return the env
   |$(fn definiendTree value)
   |$(fn definiendTree bindResult value)
   |$(syntax bindResult (or #ignore #inert :rhs :prv))  
   |$(type fexpr)
   |#
  %set! )

(def unwrap
  #|Return the underlying operator of a FUNCTION.
   |$(fn function)
   |$(type function)
   |#
  %unwrap )

(defMacro (wrau pt ep . forms)
  #|Create an anonymous function with the given PARAMETER-TREE and FORMS
   |which accesses the execution environment rather than the definition environment.
   |Useful for defining the primitive for dynamically-scoped variables.
   |$(fn pt ep . forms)
   |$(type function)
   |#
  (list 'wrap (list* 'vau pt ep forms)) )

(assert (expand (wrau pt env a b c)) '(wrap (vau pt env a b c)))


;;; Env

#|! 
The Env are functions that encapsulates mutable values.
Calling the env:
- without arguments returns the env.
- with an attribute return the value of the attribute in the env.
- with couples of attribute and value defines or update the attribute with the value in the env.
- without bindResult or with bindResult #ignore use as bindResult (bndRes)
- with bindResult #inert return #inert
- with bindResult :rhs return the right side of the last binding
- with bindResult :prv return the previous value of the last binding
- with bindResult :obj return the env
- without bindType use the bindType :def
- with bindType :def define or update the bindings in the env
- with bindType :set! update the bindings in env, throws an error if the binding is not defined.

$(env)
$(env attribute)
$(env attribute value . attributes)
$(env bindType attribute value . attributes)
$(env bindResult attribute value . attributes)
$(env bindType bindResult attribute value . attributes)
$(syntax attributes (attribute value . attributes))
$(syntax attribute (or Symbol Keyword String))
$(syntax bindResult (or #ignore #inert :rhs :prv :obj))  
$(syntax bindType (or :def :set!))  
|#

(def newEnv
  #|Create a new environment with an optional PARENT environment in which bindings are looked up if they are not found.
   |The BINDINGS must be of even length, and alternately contain bindings names (symbols, keywords or string) and values.
   |When called with an obj the obj bindings also become env bindings.
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
   |$(fn)
   |$(type fexpr)
   |$(derivation (vau () environment environment))
   |#
  %theEnv )


;;; Obj

#|! 
The Obj are functions that encapsulates mutable values.
Calling the obj:
- without arguments returns the env.
- with an attribute return the value of the attribute in the obj.
- with couples of attribute and value defines or update the attribute with the value in the obj.
- without bindResult or with bindResult #ignore use as bindResult (bndRes)
- with bindResult #inert return #inert
- with bindResult :rhs return the right side of the last binding
- with bindResult :prv return the previous value of the last binding
- with bindResult :obj return the obj

$(obj)
$(obj attribute)
$(obj attribute value . attributes)
$(obj bindResult attribute value . attributes)
$(syntax attributes (attribute value . attributes))
$(syntax attribute (or Symbol Keyword String))
$(syntax bindResult (or #ignore #inert :rhs :prv :obj))  
|#

(def new
  #|Create a new instance of CLASS (that must be a `standard-class').
   |The BINDINGS must be of even length, and alternately contain bindings names (symbols, keywords or string) and values.
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
  #|Define a new instance of CLASS with the given NAME.
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


;;; Env & Obj

(def bound?
  #|Return true if the SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, false otherwise.
   |$(fn attribute object)
   |$(syntax attribute (or Symbol Keyword String))
   |$(syntax object (or Env Obj))
   |$(type function)
   |$(derivation (@isBound object attribute))
   |#
  %bound? )

(def value
  #|Return the value of SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, #null otherwise.
   |$(fn attribute object)
   |$(syntax attribute (or Symbol Keyword String))
   |$(syntax object (or Env Obj))
   |$(type function)
   |$(derivation (@value object attribute))
   |#
  %value )

(def\ slotBound? (object attribute)
  #|Return true if the SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, false otherwise.
   |$(fn object attribute)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (@isBound object attribute))
   |#
  (%slotBound? object attribute) )

(def\ getSlot (object attribute)
  #|Return the value of SYMBOL or KEYWORD or STRING is bound in the ENVIRONMENT or OBJ, error otherwise.
   |$(fn object attribute)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (object attribute))
   |#
  (%getSlot object attribute) )

(def\ setSlot (object attribute value)
  #|Set the value of SYMBOL or KEYWORD or STRING in the ENVIRONMENT or OBJ to VALUE.
   |$(fn object attribute value)
   |$(syntax object (or Env Obj))
   |$(syntax attribute (or Symbol Keyword String))
   |$(type function)
   |$(derivation (object attribute value))
   |#
  (%setSlot object attribute value) )


;;; Cons

(def\ (caar x)
  #|Return the `car' of the `car' of the CONS.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ (((caar . #_) . #_)) caar))
   |$(derivation (car (car cons)))
   |#
  (car (car x)) )

(def\ (cadr! (#_ cadr))
  #|Return the `car' of the `cdr' of the CONS if 'cddr' is null, else error.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((#_ cadr)) cadr))
   |#
  cadr)

(def\ (cdar x)
  #|Return the `cdr' of the `car' of the CONS.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ (((#_ . cdar) . #_)) cdar))
   |$(derivation (cdr (car cons)))
   |#
  (cdr (car x)) )

(def cddr
  #|Return the `cdr' of the `cdr' of the CONS.
   |$(fn cons)
   |$(type function)
   |$(derivation (\ ((#_ #_ . cddr)) cddr))
   |$(derivation (cdr (cdr cons)))
   |$(derivation (@cdr cons 1))
   |#
  %cddr)

(def\ (cons! car)
  #|Create a cons with the given CAR and #null.
   |$(fn car)
   |$(type function)
   |$(derivarion (cons car))
   |#
  (cons car))

(def nth
  #|Return element number N of LIST, where the `car' is element zero.
   |$(fn n list)
   |$(type function)
   |#
  %nth)

(def nthCdr
  #|Returns the tail of LIST that would be obtained by calling `cdr' N times in succession.
   |$(fn n list)
   |$(type function)
   |#
  %nthCdr)


;;; List

(def append
  #|Append two lists. The first one must be proper and is copied.
   |The second one is not copied (and doesn't even have to be a list).
   |It becomes the `cdr' of the final cons of the first list,
   |or is returned directly if the first list is empty.
   |$(fn list1 list2)
   |$(type function)
   |#
  %append)

(def len
  #|Return the number of elements in the LIST.
   |$(fn list)
   |$(type function)
   |#
  %len)

(def list?
  #|Return true if the OBJECT is a list, false otherwise.
   |$(fn object)
   |$(type function)
   |$(derivation (\ (object) (type? object Keyword)))
   |#
  %list?)

(def reverse
  #|Reverse the LIST.
   |$(fn list)
   |$(type function)
   |#
  %reverse)


;;; Symbol & Keyword

;;!Symbol and Keyword extends Intern

(def intern
  #|Get or create the unique symbol or keyword with STRING as name.
   |Keyword if STRING start with ':', symbol otherwise
   |$(fn string)
   |$(type function)
   |#
  %intern)

(def name
  #|Return the name of the SYMBOL or KEYWORD as a string.
   |$(fn intern)
   |$(type function)
   |#
  %name)
  
(def keyword
  #|Get or create the unique keyword with STRING as name.
   |$(fn string)
   |$(type function)
   |#
  %keyword)

(def keyword?
  #|Return true if the OBJECT is a keyword, false otherwise.
   |$(fn object)
   |$(type function)
   |$(derivation (\ (object) (type? object Keyword)))
   |#
  %keyword?)

(def\ (keywordName (#! Keyword keyword))
  #|Return the name of the KEYWORD as a string.
   |$(fn keyword)
   |$(type function)
   |#
  (name keyword) )

(def symbol
  #|Get or create the unique symbol with STRING as name.
   |$(fn string)
   |$(type function)
   |#
  %symbol)

(def symbol?
  #|Return true if the OBJECT is a symbol, false otherwise.
   |$(fn object)
   |$(type function)
   |$(derivation (\ (object) (type? object Symbol)))
   |#
  %symbol?)

(def\ (symbolName (#! Symbol symbol))
  #|Return the name of the SYMBOL as a string.
   |$(fn symbol)
   |$(type function)
   |#
  (name symbol) )


;;; Equals

(def ==
  %==)
(def != %!=)
(def eq? %eq?)

(def\ (ignore? o) (== o #_))
(def\ (sheBang? o) (== o #!))
(def\ (inert? o) (== o #inert))


;;; Boolean

(def !
  %!)

(def not
  !)

(def !!
  %!!)


;;; Number

(def number? %number?)
(def + %+)
(def * %*)
(def - %-)
(def / %/)
(def % %%)
(def\ (1+ n) (+ n 1))
(def\ (1- n) (- n 1))
(def\ (0? n) (== n 0))
(def\ (1? n) (== n 1))
(def\ (-1? n) (== n -1))
(def\ (even? n) (== (% n 2) 0))
(def\ (odd? n)  (== (% n 2) 1))


;;; String

(def string? %string?)
(def $ %$)


;;; Comparator

(def < %<)
(def > %>)
(def <= %<=)
(def >= %>=)
(def\ (min a b) (if (< a b) a b))
(def\ (max a b) (if (> a b) a b))


;;; Bit

(def ~ %~)
(def & %&)
(def \| %|)
(def ^ %^)
(def << %<<)
(def >> %>>)
(def >>> %>>>)


;;; First-Order Control

(def* (then else) begin begin)

(def loop
  %loop)

(def atEnd
  %atEnd)

(defMacro (finally x . cnl)
  (list 'atEnd (cons 'begin cnl) x) )  

(def throwTag
  %throwTag)

(defMacro (throw . forms) (list* 'throwTag #_ forms) )

;; ctApv non andrebbe mai cambiato dopo il boot, anche la riesecuzione di quanto segue potrebbe non bastare!
(if (ctApv)
  (then
    (defMacro (catchTagWth tag hdl . forms)
      (list '%catchTagWth tag hdl (list* '\ () forms)) )
    (defMacro (catch . forms)
      (list* 'catchTagWth #_ #_ forms) )
    (defMacro (catchWth hdl . forms)
      (list* 'catchTagWth #_ hdl forms) )
    (defMacro (catchTag tag . forms)
      (list* 'catchTagWth tag #_ forms) )
  )
  (else
    (def catchTagWth
      %catchTagWth)
    (defMacro (catch . forms)
      (list* 'catchTagWth #_ #_ forms))
    (defMacro (catchWth hdl . forms)
      (list* 'catchTagWth #_ hdl forms))
    (defMacro (catchTag tag . forms)
      (list* 'catchTagWth tag #_ forms) )
  )
)

(assert (catch (throw)) #inert)
(assert (catch (throw 1)) 1)
(assert (catchWth (\ (x) (+ x 1)) (throw 1) ) 2)
(assert (catchTag 'a (throwTag 'a)) #inert)
(assert (catchTag 'a (throwTag 'a 1)) 1)
(assert (catchTagWth 'a (\ (x) (+ x 1)) (throwTag 'a 1) ) 2)


;;; Delimited-Control Operators

;;!These operators follow the API put forth in the delimcc library at URL `http://okmij.org/ftp/continuations/implementations.html'.

(def takeSubcont
  %takeSubcont)

(def pushPrompt
  %pushPrompt)

(def pushDelimSubcont
  %pushDelimSubcont)

(defMacro pushSubcont (continuation . forms)
  (list* 'pushDelimSubcont #ignore continuation forms) )

(def pushSubcontBarrier
  %pushSubcontBarrier)


;;; Error

(def test %test)
(def error %error)
(def rootPrompt %rootPrompt)

(def\ makeTypeError (datum expected)
  (new Error "not a {expected}: {datum}" :type 'type :datum datum :expected expected) )

(def\ typeError (datum expected)
  (error (makeTypeError datum expected)) )


;;; Classes

(def className
  %className)

(def classOf
  %classOf)

(def instanceOf? %instanceOf?)

(def subClass?
  %subClass?)

(def type?
  %type?)


;;; Basic Functions and Macro

(def\ (idf x)
  x)

(defMacro _ forms
  (list* '\ '(_) forms) )

(def\ (curry f v)
  (\ args (apply f (cons v args))) )

(def\ (curry* f . v*)
  (\ args (apply f (append v* args))) )

(def\ (compose f g)
  (\ args (f (apply g args))) )

(def\ (compose* . f*)
  (\ args ((rec\ (loop (f . f*)) (if (null? f*) (apply f args) (f (loop f*)))) f*)) )

;(defMacro compose* f* 
;  (list '\ 'args ((rec\ (loop (f . f*)) (if (null? f*) (list 'apply f 'args) (list f (loop f*)))) f*)) )

(defMacro (rec lhs . rhs)
  (list (list '\ (list lhs) (list* 'def lhs :rhs rhs)) #inert) )

(def label rec)

(assert ((rec f (\ (l) (if (null? l) "" ($ (car l) (f (cdr l)))))) '(1 2 3)) "123")
(assert ((rec f (\ l (if (null? l) "" ($ (car l) (apply f (cdr l)))))) 1 2 3) "123")

(defMacro (rec\ lhs . rhs)
  (if (cons? lhs)
    (list 'rec (car lhs) (list* '\ (cdr lhs) rhs))
    (list 'rec lhs (cons '\ rhs)) ))

(def label\ rec\)

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

; apply: (eval (cons opv args) env) <=> (apply opv args env)
; apply: (eval (cons apv args) env) <=> (apply apv (evlis env args) env)
; evlis: (map (\ (x) (eval x env)) x*) <=> (eval (cons 'list x*) env)

(defMacro (def*\ lhs* . rhs*)
  (list* 'def*
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map (\ (lhs rhs) (cons '\ (if (cons? lhs) (cons (cdr lhs) rhs) rhs))) lhs* rhs*) ))

(assert (expand (def*\ ((a n) (b n)) ((+ n 1)) ((+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )
(assert (expand (def*\ (a b) ((n) (+ n 1)) ((n) (+ n 1)))) '(def* (a b) (\ (n) (+ n 1)) (\ (n) (+ n 1))) )


;;; Lexical Bindings

(def\ (->begin binding) (cons 'begin (cdr binding)))
(def\ (->name+#inert (lhs . #_)) (list (if (cons? lhs) (car lhs) lhs) #inert))
(def\ (->name+lambda (lhs . rhs)) (if (cons? lhs) (list (car lhs) (list* '\ (cdr lhs) rhs)) (list lhs (cons '\ rhs)) ))


(defMacro (wth1 dt value . forms)
  (list (list* '\ (cons dt) forms) value))

(defMacro (wth* bindings . forms)
  ( (rec\ (loop bindings)
      (if (null? bindings)
        (cons (list* '\ () forms))
        (wth1 (dt value . bindings) bindings
          (list* 'wth1 dt value (cons (loop bindings))) ))) 
    bindings ))

(assert (expand (wth* (a 1 b (1+ a)) 1 2 (+ a b))) '(wth1 a 1 (wth1 b (1+ a) ((\ () 1 2 (+ a b))))))
(assert (wth* (a 1 b (1+ a)) 1 2 (+ a b)) 3)

(defMacro (wth b* . forms)
  (def dt* ((rec\ (loop b*) (if (null? b*) #null (wth1 (dt #_ . b*) b* (cons dt (loop b*))))) b*))
  (def vl* ((rec\ (loop b*) (if (null? b*) #null (wth1 (#_ vl . b*) b* (cons vl (loop b*))))) b*))
  (cons (list* '\ dt* forms) vl*) )

(assert (expand (wth (a 1 b 2) 1 2 (+ a b))) '((\ (a b) 1 2 (+ a b)) 1 2))
(assert (wth (a 1 b 2) 1 2 (+ a b)) 3)


(defMacro (let1Loop lhs . rhs)
  (if (cons? lhs)
    (def* ((name . binding) body) lhs rhs)
    (def* (name (binding . body)) lhs rhs) )
  (list
    (list* 'rec\ name (cons (car binding)) body)
    (->begin binding) ))

(assert (let1Loop add1 (a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))
(assert (let1Loop (add1 a '(1 2)) (if (null? a) () (cons (+ (car a) 1) (add1 (cdr a))))) '(2 3))


(defMacro (let1 lhs . rhs)
  (if (symbol? lhs)
    (list* 'let1Loop lhs rhs)
    (list (list* '\ (list (car lhs)) rhs)
      (->begin lhs) )))

(assert (let1 (a 1) a) 1)
(assert (let1 (a 1 2) a) 2)
(assert (let1 f (a 2) (if (0? a) 'end (f (- a 1)))) 'end)

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
    (cons 'def\ binding)
    body ))


(defMacro (let* bindings . body)
  ( (rec\ loop (bindings)
      (if (null? bindings)
        (cons (list* '\ () body))
        (list* 'let1 (car bindings) (cons (loop (cdr bindings)))) ))
    bindings ))

(assert (let* ((a 1)) a) 1)
(assert (let* ((a 1)(b a)) b) 1)


(defMacro (letLoop lhs . rhs)
  (if (cons? lhs)
    (def* ((name . bindings) body) lhs rhs)
    (def* (name (bindings . body)) lhs rhs) )
  (cons
    (list* 'rec\ name (map car bindings) body)
    (map ->begin bindings) ))

(assert (letLoop sum ((a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))
(assert (letLoop (sum (a '(1 2)) (b '(3 4))) (if (null? a) () (cons (+ (car a) (car b)) (sum (cdr a) (cdr b))))) '(4 6))


(defMacro (let lhs . rhs)
  (if (symbol? lhs)
    (list* 'letLoop lhs rhs)
    (cons (list* '\ (map car lhs) rhs)
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

(assert (letrec ( (even? (\ (n) (if (0? n) #t (odd? (- n 1))))) (odd? (\ (n) (if (0? n) #f (even? (- n 1))))) ) (even? 88)) #t)

(defMacro (letrec\ bindings . body)
  (list* 'let (map ->name+#inert bindings)
    (list* 'def*\ (map car bindings) (map cdr bindings))
    body ))

(def labels
  letrec\)

(assert (labels ( ((even? n) (if (0? n) #t (odd? (- n 1)))) (odd? (n) (if (0? n) #f (even? (- n 1)))) ) (even? 88) ) #t)


;;; Simple Control

(defVau prog1 (form . forms) env
  (let1 (result (eval form env))
    (apply begin forms env)
    result))

(defMacro (when test . forms)
  (list 'if test (cons 'then forms)))

(defMacro (unless test . forms)
  (list 'if test #inert (cons 'else forms)))

(defVau && ops env
  (if
    (null? ops) #t
    (eval (car ops) env) (apply && (cdr ops) env)
    #f ))

(def and &&)

(defVau || ops env
  (if
    (null? ops) #f
    (eval (car ops) env) #t
    (apply || (cdr ops) env) ))

(def or ||)

(def\ &&f fs
  (\ args
    ( (rec\ (&&f fs)
        (if
          (null? fs) #t
          (apply (car fs) args) (&&f (cdr fs))
          #f ))
      fs )))  

(def\ ||f fs
  (\ args
    ( (rec\ (||f fs)
        (if
          (null? fs) #f
          (apply (car fs) args) #t
          (||f (cdr fs)) ))
      fs )))    


;;; Bind Bind? IfBind? CaseVau DefCaseVau Case\ DefCase\ Match Cond

(def bind? %bind?)

(defVau (ifBind? (pt exp) then . else) env
  (let1 (env+ (newEnv env))
    (if (bind? env+ pt (eval exp env))
      (eval then env+)
      (unless (null? else)
        (eval (car! else) env) ))))

(defVau (caseVau . clauses) env
  (vau values #ignore
    (let1 loop (clauses clauses)
      (if (null? clauses) #inert
        (let1 (((bindings . forms) . clauses) clauses)
          (if (== bindings 'else)
            (if (== (car forms) '=>)
              ((eval (cadr! forms) env) values)
              (apply begin forms env) )
            (let1 (env+ (newEnv env)) 
              (if (bind? env+ bindings values)
                (apply begin forms env+)
                (loop clauses) ))))))))

(defMacro (defCaseVau sym . clauses)
  (list 'def sym (cons 'caseVau clauses)) )

(defMacro (case\ . clauses)
  (list 'wrap (cons 'caseVau clauses)) )

(defMacro (defCase\ sym . clauses)
  (list 'def sym (cons 'case\ clauses)) )

(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1) 2)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2) 3)
(assert ((case\ ((a) (+ a 1)) ((a b) (+ b 1)) (a (map (\ (a) (+ a 1)) a))) 1 2 3) '(2 3 4))

(defMacro (match exp . clauses)
  (list (cons 'case\ (map (\ ((a . b)) (list* (if (== a 'else) a (list a)) b)) clauses)) exp) )

(assert (match '(1 2 3) ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match   '(1 2) ((a) 1) ((a b) 2) (else 3)) 2)
(assert (match     '(1) ((a) 1) ((a b) 2) (else 3)) 1)
(assert (match        1 ((a) 1) ((a b) 2) (else 3)) 3)
(assert (match        4 ((a) 1) ((a b) 2)    (a a)) 4)
(assert (match '(1 2 3) ((a) 1) ((a b) 2)    (a a)) '(1 2 3))

(defVau (cond . clauses) env
  (unless (null? clauses)
    (let1 (((test . body) . clauses) clauses)
      (if (== test 'else)
        (apply begin body env)
        (let1 (test (eval test env))
          (if (instanceOf? test Boolean)
            (if test
              (apply begin body env)
              (apply cond clauses env) )
            (match body
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


;;; Quasiquote

;;!(Idea from Alf Petrofsky http://scheme-reports.org/mail/scheme-reports/msg00800.html)

(defVau %` (x) env
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


;;; Options

;;!An option is either nil ("none"), or a one-element list ("some").

(def some
  #|Create a one-element list from the VALUE.
   |#
  cons!)

;; (Idea from Taylor R. Campbell's blag. https://mumble.net/~campbell/blag.txt)
(defVau (ifOpt (pt opt) then . else) env
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
  (list 'ifOpt (list pt opt) (if (null? forms) #null (cons 'begin forms))) )

(defMacro unlessOpt (opt . forms)
  (list* 'ifOpt (list #ignore opt) #null (if (null? forms) #null (cons 'begin forms))) )

(defVau (caseOpt opt . clauses) env
  (let1 (opt (eval opt env))
    (if (null? opt) #null
      (let1 loop (clauses clauses)
        (if (null? clauses) #null
          (let ( (env+ (newEnv env))
                 (((bindings . forms) . clauses) clauses) )
            (if (|| (== bindings 'else) (bind? env+ bindings opt))
              (apply begin forms env+)
              (loop clauses) )))))))

(assert (caseOpt () ((a) 1) ((a b) (+ a b))) ())
(assert (caseOpt '(2) ((a) 1) ((a b) (+ a b))) 1)
(assert (caseOpt '(1 2) ((a) 1) ((a b) (+ a b))) 3)
(assert (caseOpt '(1 2 3) ((a) 1) ((a b) (+ a b))) #null)

(defVau (optDft opt . dft) env
  (ifOpt (opt (eval opt env)) opt
    (ifOpt (dft (eval (cons 'list dft) env)) dft) ))

(assert (optDft () 10) 10)
(assert (optDft '(2) 10) 2)
(assert (optDft '(2 3) 10))

(defVau optDft* (lst . dft) env
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
  (optDft opt (simpleError "Option is #null")))


;;; OptValue Member Member? !Member? OptKey Assoc Member*?

(def\ (optValue key lst)
  (let1 loop (lst lst)
    (if (cons? lst)
      (let1 ((k v . lst) lst)
        (if (== k key) (cons v) (loop lst)) )
      #null )))

(assert (optValue :b '(:a 1 :b 2 :c 3)) '(2))
(assert (optValue 'b '(a 1 b 2 c 3)) '(2))

(def\ (member k lst . keywords)
  (let ( (cmp (optDft (optValue :cmp keywords) ==))
         (key (optDft (optValue :key keywords) idf))
         (ret (optDft (optValue :ret keywords) idf)) )
    (let1 loop (lst lst)
      (if (cons? lst)
        (if (cmp (key (car lst)) k) (ret lst)
          (loop (cdr lst)) )
        #null ))))

(def\ (member? key lst . keywords)
  (cons? (apply** member key lst keywords)) )
  
(def\ (!member? key lst . keywords)
  (null? (apply** member key lst keywords)) )

(assert (member 'b '(a b c d)) '(b c d))
;(assert (member "b" '("a" "b" "c" "d")) '("b" "c" "d")) ; solo se String interned!

(def\ (optKey key lst)
  (if (cons? key) ((rec\ (loop lst) (if (cons? lst) (let1 (k (car lst)) (if (member? k key) k (loop (cdr lst)))) #null)) lst)
    (member? key lst) key
    #null ))

(assert (optKey :b '(:a :c)) #null)
(assert (optKey :b '(:a :b :c)) :b)
(assert (optKey (:b :d) '(:a :b :c)) :b)
(assert (optKey (:b :c) '(:a :b :c)) :b)

(def\ (assoc k lst) 
  (member k lst :key car :ret car) )

(assert (assoc 'b '((a 1) (b 2) (c 3) (d 4))) '(b 2))

(def\ (member*? key . lst)
  (member? key lst) )


;;; Case MatchObj? CaseType CaseType\

(defVau (case exp . clauses) env
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

(def matchObj? %matchObj?)

(def\ (matchObj*? obj class . slots) 
  (matchObj? obj (cons class slots)) )

; vedi signalsError? in vm.lispx (o test-util.lispx) per codice simile
(defVau (caseType key . clauses) env
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

(defMacro (caseType\ (#! Symbol key) . clauses)
  (list '\ (cons key) (list* 'caseType key clauses) ))   

(assert (catchWth (caseType\ e ((Error :type 'xx) 1) (else 2)) (error :type 'zz)) 2)

(defMacro (caseType\ (#! (1 Symbol) key) . clauses)
  (list '\ key (list* 'caseType (car key) clauses) ))   

(assert (catchWth (caseType\ (e) ((Error :type 'xx) 1) (else 2)) (error :type 'zz)) 2)


;;; Sort

(def\ (sort lst . opt)
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


;;; Checks

(def\ assert#t (boolean)
  (unless boolean (error (new Error "invalid assetion" :type 'assert :datum boolean :expected #t))))

(def the
  %the)

(defVau (check o ck) env
  ((wrap %check) (eval o env) ck) )

(defVau (check o ck) env
  (apply %check (list (eval o env) ck) env) )

(defMacro (check* o . cks)
    (list 'check o cks) )

(assert (check* '(1 (:a 1 :b 2) c 3) 1 oo Integer (Keyword Integer) Symbol (or 3 4)) 4)
(assert (check* '(a 1 2) 'a 1 2) 3)
(assert (check* '(a) (or '(b) '(a))) 1)
(assert (check* '(a 1 2) (or '(b 3) '(a 1 2))) 3)
(assert (check* '(a #null 1) 2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 3)
(assert (check* '(a :prv 1)  2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 3)
(assert (check* '(a 1)       2 3 Symbol (or (1) (2 (or () Inert :prv :rhs)))) 2)

(defVau check? args env
  (catchWth #f
    (apply check args env)
    #t ))

(defMacro (the+ ck obj) (list 'let1 (list 'obj obj) (list 'check 'obj ck) 'obj))

(assert (the+ Integer 1) 1)
(assert (the+ Integer "1") Error :type 'type :datum "1" :expected 'Integer)
(assert (the+ (or 1 2) 1) 1)
(assert (the+ (or 1 2) 2) 2)
(assert (the+ (or 1 2) 3) Error :type 'type :datum 3 :expected '(or 1 2))

; TODO non è più così costosa la conversione, si può fare
; (def the the+)


;;; Block Loop For While Until Repeat DoTimes

(defVau block (blockName . forms) env
  (let* ( (tag (list #inert)) ; cons up a fresh object as tag
          (escape (\ value (throwTag tag (if (cons? value) (car! value))))) )
    (catchTag tag
      (eval (list (list* '\ (list blockName) forms) escape) env) )))

(assert (block exit (exit)) #inert)
(assert (block exit (exit 7)) 7)
(assert (block exit (def x 1) (loop (if (== x 4) (exit 7)) (def x (+ x 1)))) 7)

(def\ returnFrom (blockName . value?)
  (blockName (optDft value? #inert)) )

(assert (block ciclo (def x 1) (loop (if (== x 4) (returnFrom ciclo 7)) (def x (+ x 1)))) 7)

(defVau while (testForm . forms) env
  (let ((forms (cons 'begin forms)))
    (block exit
      (loop
        (if (eval testForm env)
            (eval forms env)
            (returnFrom exit #inert))))))

(def %loop
  (let1 (%loop ((.parent (theEnv)) '%loop))
    (vau forms env
      (let1 (%deep (let1 (%deep (value :%deep env)) (if (null? %deep) 0 (1+ %deep))))
        (let ( (break (symbol ($ 'break %deep)))
               (continue (symbol ($ 'continue %deep))) )
          (def\ (mkThrow tag (#! (and Integer (>= 0) (<= %deep)) n) forms)
            (list* 'throwTag (list 'quote (symbol ($ tag (- %deep n)))) forms) )
          (let1 (env (newEnv (newEnv env
                  :%deep %deep
                  :break (macro forms (mkThrow 'break 0 forms))
                  :break- (macro (n . forms) (mkThrow 'break n forms))
                  :break? (macro (b . forms) (list 'if b (mkThrow 'break 0 forms)))
                  :break-? (macro (n b . forms) (list 'if b (mkThrow 'break n forms)))
                  :continue (macro forms (mkThrow 'continue 0 forms)) 
                  :continue- (macro (n . forms) (mkThrow 'continue n forms))
                  :continue? (macro (b . forms) (list 'if b (mkThrow 'continue 0 forms))) 
                  :continue-? (macro (n b . forms) (list 'if b (mkThrow 'continue n forms)))
                  :until? (macro (b . forms) (list 'if b (mkThrow 'break 0 forms))) ; same of break?
                  :while? (macro (b . forms) (list 'if b #inert (mkThrow 'break 0 forms))) ))) 
            (if (check? forms (2 oo 'for ((2 3)) )) ;loop for
              (let ( (for (cadr forms))
                     (forms (cons 'begin (cddr forms))) )
                (def increments (list* 'def* (map car for) (map (\((#_ init . incr)) (optDft incr init)) for)))
                (catchTag break
                  (eval (list* 'def* (map car for) (map cadr for)) env)   
                  (%loop
                    (catchTag continue (eval forms env) )
                    (eval increments env) )))
              (if (check? forms (2 oo 'for1 (2 3))) ;loop for1
                (let ( ((pt init . incr) (cadr forms))
                       (forms (cons 'begin (cddr forms))) )
                  (def increment (list 'def pt (optDft incr init)))
                  (catchTag break
                    (eval (list 'def pt init) env)
                    (%loop
                      (catchTag continue (eval forms env) )
                      (eval increment env) )))
                (let1 (forms (cons 'begin forms)) ;loop
                  (catchTag break
                    (%loop
                      (catchTag continue
                        (eval forms env) )))) )))))) ))

#| in alternativa al precedente
; ambiguo per (loop (loop (break -1))) deve teminare il primo loop o tornarci con -1 
(def %loop
  (let1 (%loop ((.parent (theEnv)) '%loop))
    (vau forms env
      (let1 (%deep (let1 (%deep (value :%deep env)) (if (null? %deep) 0 (1+ %deep))))
        (let ( (break (symbol ($ 'break %deep)))
               (continue (symbol ($ 'continue %deep))) )
          (def\ (mkThrow tag forms)
            (if (|| (null? forms) (null? (cdr forms)) (! (type? (car forms) Integer)))
              (list* 'throwTag (list 'quote (symbol ($ tag %deep))) forms)
              (list* 'throwTag (list 'quote (symbol ($ tag (+ %deep (the+ (and (>= (- %deep)) (<= 0)) (car forms)))))) forms) ))
          (let1 (env (newEnv (newEnv env
                  :%deep %deep
                  :break (macro forms (mkThrow 'break forms))
                  :continue (macro forms (mkThrow 'continue forms)) 
                  :until? (macro (b . forms) (list 'if b (mkThrow 'break forms))) 
                  :while? (macro (b . forms) (list 'if b #inert (mkThrow 'break forms)))  ))) 
            (if (check? forms (2 oo 'for ((2 3 Symbol)) )) ;loop for
              (let ( (for (cadr forms))
                     (forms (cons 'begin (cddr forms))) )
                (def increments (list* 'def* (map car for) (map (\((#_ init . incr)) (optDft incr init)) for)))
                (catchTag break
                  (eval (list* 'def* (map car for) (map cadr for)) env)   
                  (%loop
                    (catchTag continue (eval forms env) )
                    (eval increments env) )))
              (if (check? forms (2 oo 'for1 (2 3 Symbol))) ;loop for1
                (let ( ((pt init . incr) (cadr forms))
                       (forms (cons 'begin (cddr forms))) )
                  (def increment (list 'def pt (optDft incr init)))
                  (catchTag break
                    (eval (list 'def pt init) env)
                    (%loop
                      (catchTag continue (eval forms env) )
                      (eval increment env) )))
                (let1 (forms (cons 'begin forms)) ;loop
                  (catchTag break
                    (%loop
                      (catchTag continue
                        (eval forms env) )))) )))))) ))
|#

(def loop %loop)

(defMacro (for1 binding cond . body)
  (list* 'loop 'for1 binding
    (if (%ignore? cond) body
      (cons (list 'while? cond) body) )))

(defMacro (for bindings cond . body)
  (list* 'loop 'for bindings
    (if (%ignore? cond) body
      (cons (list 'while? cond) body) )))

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
  (list* 'loop (list 'while? cond) forms) )

(defMacro until (cond . forms)
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
 
(defMacro doTimes ((var times . result) . body)
  (let1\
    (doTimes (times body result)
      (let1 (i (newBox 0))
        (while (< (i) times) (body (i)) (++ i) )
        (result (i)) ))
    (list doTimes
      times
      (list* '\ (list var) body)
      (list* '\ (list var) result) )))

(defVau (repeat times . forms) env
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

(defMacro doTimes ((var times . result) . body)
  (list* 'repeat (list* var times result) body) )

#| TODO in alternativa del precedente, da verificare
(defVau (repeat times . forms) env
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

(defMacro doTimes ((var times . result) . body)
  (list* 'repeat (list* var times (if (null? result) (cons #inert) result)) body) )
|#


;;; Lists

(def\ (any? f lst . lst*)
  (if (null? lst*)
    ((rec\ (any? lst) (if (null? lst) #f (f (car lst)) #t (any? (cdr lst))) ) lst)
    ((rec\ (any*? lst*) (if (null? (car lst*)) #f (apply f (map car lst*)) #t (any*? (map cdr lst*))) ) (cons lst lst*)) ))

(assert (any? null? (1 2 3 4)) #f)
(assert (any? null? (1 2 () 4)) #t)
(assert (any? > '(1 2) '(3 4)) #f)
(assert (any? < '(1 2) '(3 4)) #t)

(defMacro (any?* f . lst) (list 'any? f lst))

(def\ (all? f lst . lst*)
  (if (null? lst*)
    ((rec\ (all? lst) (if (null? lst) #t (f (car lst)) (all? (cdr lst)) #f) ) lst)
    ((rec\ (all*? lst*) (if (null? (car lst*)) #t (apply f (map car lst*)) (all*? (map cdr lst*)) #f) ) (cons lst lst*)) ))

(assert (all? number? (1 2 3 4)) #t)
(assert (all? number? (1 2 () 4)) #f)
(assert (all? > '(1 2) '(3 4)) #f)
(assert (all? < '(1 2) '(3 4)) #t)

(defMacro (all?* f . lst) (list 'all? f lst))

(def\ (forEach# f lst . lst*)
  (if (null? lst*)
    ((rec\ (forEach lst) (unless (null? lst) (f (car lst)) (forEach (cdr lst)))) lst)
    ((rec\ (forEach* lst*) (unless (null? (car lst*)) (apply f (map car lst*)) (forEach* (map cdr lst*)) )) (cons lst lst*)) ))

(assert (forEach# (\ (#ignore)) '(1 2)) #inert)
(assert (forEach# (\ (#ignore #ignore)) '(1 2) '(3 4)) #inert)
(assert (let1 (n 1) (forEach# (\ (a) (set! n (+ n a))) '(1 2)) n) 4)
(assert (let1 (n 1) (forEach# (\ (a b) (set! n (+ n (+ a b)))) '(1 2) '(3 4)) n) 11)

(def\ (forEach f lst . lst*)
  (if (null? lst*)
    (let1 (res lst) ((rec\ (forEach lst) (if (null? lst) res (else (f (car lst)) (forEach (cdr lst)) ))) res))
    (let1 (res* (cons lst lst*)) ((rec\ (forEach* lst*) (if (null? (car lst*)) res* (else (apply f (map car lst*)) (forEach* (map cdr lst*)) ))) res*) )) )

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

(defMacro dolist ((var listForm . resultForm) . bodyForms)
  (let1rec\
    (dolist (list body result)
      (if (null? list) (result list)
        (else
          (body (car list))
          (dolist (cdr list) body result) )))
    (list dolist
          listForm
          (list* '\ (list var) bodyForms)
          (list* '\ (list var) resultForm))))

(def\ (make\* n f)
  (def\ (resize n lst)
    (let loop ((n n) (h ()) (t lst))
      (if (null? t) (reverse h)
        (if (<= n 1)
          (reverse (cons (if (null? (cdr t)) (car t) t) h))
          (loop (- n 1) (cons (car t) h) (cdr t)) ))))
  (\ lst (apply f (resize n lst))))

(assert ((make\* 2 (\(a b) b)) 1 2 3 4 5) '(2 3 4 5))

(def\ (minMax a . b)
  (let loop [ (x ((caseType a (Double .POSITIVE_INFINITY) (Long .MAX_VALUE) (Integer .MAX_VALUE)) a))
              (y ((caseType a (Double .NEGATIVE_INFINITY) (Long .MIN_VALUE) (Integer .MIN_VALUE)) a))
              (a (cons a b)) ]
    (if (null? a) (list x y)
      (loop (min x (car a)) (max y (car a)) (cdr a)) )))


;;; Arrays

(def\ (array->list arr)
  (%array->list #t arr) )

(def\ (array->cons arr)
  (%array->list #f arr) )

(def list->array %list->array)

(def\ (array . args) (list->array args))

(def Object[] &java.lang.Object[])

(def\ (arrayMap fun (#! Object[] arr))
  (list->array (map fun (array->list arr))) )

(assert (arrayMap 1+ (array 1 2 3)) (array 2 3 4))

(def\ (arrayFilter pred (#! Object[] arr))
  (list->array (filter pred (array->list arr))) )

(assert (arrayFilter odd? (array 1 2 3)) (array 1 3))

(def\ (newInstance class dim . dims)
  (apply** @newInstance Array class dim dims))

(def\ (arrayGet array index)
  (if (cons? index)
    (apply** arrayGet* array index)
    (@get Array array index) ))

(def\ (arrayGet* array . indexes)
  (let loop ((array array) (indexes indexes))
    (if (null? indexes) array
      (loop (arrayGet array (car indexes)) (cdr indexes)) )))  

(def\ (arraySet array index value)
  (if (cons? index)
    (apply** arraySet* array value index)
    (else
      (@set Array array index value)
      array )))

(def\ (arraySet* array0 value . indexes)
  (if (null? indexes) array
    (let loop ((array array0) (indexes indexes))
       (if (null? (cdr indexes))
         (then (arraySet array (car indexes) value) array0)
         (loop (arrayGet array (car indexes)) (cdr indexes)) ))))  

(assert (arrayGet (arraySet (newInstance &int 2 2) (1 1) 3) (1 1)) 3)
(assert (arrayGet* (arraySet* (newInstance &int 2 2) 3 1 1) 1 1) 3)


;;; Simple Set

;(def set? /=) ; TODO solo se (/=) -> #t

(def\ (set? lst) (if (null? lst) #t (apply /= lst)))

(def\ (set+ v lst)
  (if (member? v lst) lst (cons v lst)))
  
(defVau (defSet+ (#! Symbol plc) v) env
  (let ( (v (eval v env)) (lst (env plc)) )
    (if (member? v lst) lst (env :def :rhs plc (cons v lst))) ))

(def\ ->set (lst)
  (let loop ( (res ()) (lst lst) )
    (if (null? lst) (reverse res)
      (let1 ((v . lst) lst)
        (loop (if (member? v res) res (cons v res)) lst) ))))

#| TODO da valutare
(def\ ->set (lst)
  (let1 loop (res ())
    (if (null? lst) (reverse res)
      (let1 (v (car lst))
        (set! lst (cdr lst)) 
        (loop (if (member? v res) res (cons v res))) ))))
|#


;;; Syntetic Expression

;;! stolen from Anarki :-)

(defMacro %´ ((#! (or Symbol List) x))
  #|!
   |´a;b -> (compose a b c)
   |´a;b;c -> (compose* a b c)
   |´!a -> (compose ! a)
   |´a…b -> (a b)
   |´a…'b -> (a 'b)
   |´a…'b…c -> (a 'b c)
   |´a,b -> (curry a b)
   |´a,'b,c -> (curry* a 'b c) 
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
        (cons (if (null? (cddr t)) 'curry 'curry*) t)
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
          (== f comma) ;curry
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
(assert (expand ´cc,1) '(curry cc 1))
(assert (expand ´cc,1,2) '(curry* cc 1 2))
(assert (expand ´cc,1,'a,2) '(curry* cc 1 (quote a) 2))
(assert (expand ´cc,1,_,2) '(\ (_) (cc 1 _ 2)))

(assert (expand ´cc) 'cc)
(assert (expand ´cc…1) '(cc 1))
(assert (expand ´cc…1…2) '(cc 1 2))
(assert (expand ´cc…1…'a…2) '(cc 1 (quote a) 2))

(assert (expand ´a;!b;cc,1,'d;e) '(compose* a (compose ! b) (curry* cc 1 (quote d)) e))
(assert (expand ´a;!b;cc,1,'d;ee…2…'f;g) '(compose* a (compose ! b) (curry* cc 1 (quote d)) (ee 2 (quote f)) g)) 
(assert (expand ´a;!b;cc,1,'d;ee…2…'f…:gg;h) '(compose* a (compose ! b) (curry* cc 1 (quote d)) (ee 2 (quote f) :gg) h)) 

(assert (expand ´cc,1,bb…2) '(curry* cc 1 (bb 2)))
(assert (expand ´cc…1…bb,2) '(cc 1 (curry bb 2)))
(assert (expand ´aa,bb…2,cc…3,dd) '(curry* aa (bb 2) (cc 3) dd))
(assert (expand ´aa,bb…2,3;dd) '(compose (curry* aa (bb 2) 3) dd))
(assert (expand ´aa,bb…2…'b…!x,3;dd) '(compose (curry* aa (bb 2 (quote b) (! x)) 3) dd))

(assert (expand ´dd,_c,1,_b,"a",_a,_a,'c,_,:ff,_*) '(\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a (quote c) _ :ff _*)) )
(assert (expand ´(dd _c 1 _b "a" _a _a 'c _ :ff _*) '(\ (_ _a _b _c . _*) (dd _c 1 _b "a" _a _a 'c _ :ff _*)) ))


;;; Box

#|! 
The Box are functions that encapsulates a mutable value.
Calling the box:
- without arguments returns the value in the box.
- with an attribute update the value in the box.
- without bindResult or with bindResult #ignore use as bindResult (bndRes)
- with bindResult #inert return #inert
- with bindResult :rhs return the right side of the last binding
- with bindResult :prv return the previous value of the last binding
- with bindResult :obj return the box

$(box)
$(box value)
$(box bindResult value)
$(syntax bindResult (or #ignore #inert :rhs :prv :obj))  
|#

(def newBox
  %newBox)

(defMacro (defBox name . value?)
  (list 'def name (cons 'newBox value?)) )


;;; Dynamic Binding

(def newDVar %newDVar)
(def dval %dVal)

(defMacro (ddef var . val?)
  (list* (list '%d\ (list var)) val?) )

(defMacro (ddef* var* . val*)
  (list* (list '%d\ var*) val*) )

(def\ (dget dvar)
  (dvar))

(def\ (dset dvar value)
  (dvar value))

(defMacro (dlet bindings exp . exps)
  (cons (list* '%d\ (map car bindings) exp exps) (map cadr bindings)) )

(defMacro (progv var* val* exp . exps)
  (cons (list* '%d\ var* exp exps) val*) )

(defMacro (dlet* bindings . forms)
  (if (null? bindings)
    (cons 'begin forms)
    (list 'dlet
      (list (car bindings))
      (list* 'dlet* (cdr bindings) forms) )))

(let ()
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
)


;;; Classes

(def\ findClass ((#! Symbol name) env)
  (eval name env))

(defVau defClass (name (#! (0 1 Symbol) superClass?) (#! (Symbol) slotSpecs) . properties) env
  ;; Slot-specs are ignored for now, but check that they are symbols nevertheless.
  (def superClass (findClass (optDft superClass? 'Obj) env))
  (eval (list 'def name (%newClass name superClass)) env) )


;;; Generic Functions

;; receiver e parameters dei defMethod dovrebbero corrispondere a quelli del corrispondente defGeneric con quel nome

(defVau (defGeneric . args) env
  (if (cons? (car args))
    (def ((name receiver . parameters) . properties) args)
    (def (name (receiver . parameters) . properties) args) )
  (let1\ (generic args ((%getMethod (classOf (car args)) name) args))
    (eval (list 'def name generic) env) ))

(defVau (defMethod . args) env
  (if (cons? (car args))
    (def ((name (receiver class) . parameters) . forms) args)
    (def (name ((receiver class) . parameters) . forms) args) )
  (def method (\ (args) (apply (eval (list* '\ (cons receiver parameters) forms) (let1 (receiver (car args)) (if (type? receiver Obj) (newEnv env receiver) env)) ) args)))  
  (def prv (%addMethod (eval class env) name method))
  (case (bndRes) (#inert #inert) (:rhs method) (:prv prv)) )

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


;;; Modules

(defVau (provide symbols . body) env
  (eval
    (list 'def symbols
      (list 'let ()
        (cons 'begin body)
        (cons 'list symbols) ))
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
         (values (eval (cons 'list imports) module)) )
    (eval (list* 'def* imports values) env) ))

(assert (begin (defModule m (x) (def x 10)) (import m (x)) x) 10)


;;; Relational Operators

;; Note that unlike in Common Lisp, these operators currently require
;; at least two arguments.  This will be improved in the future.

(def\ (relationalOp binop)
  (rec\ (op arg1 arg2 . rest)
    (if (binop arg1 arg2)
      (if (null? rest) #t
        (apply op (cons arg2 rest)))
      #f )))

(def <
  (relationalOp <) )

(def >
  (relationalOp >) )

(def <=
  (relationalOp <=) )

(def >=
  (relationalOp >=) )

(def eq?
  (relationalOp eq?) )

(def ==
  (relationalOp ==))

(assert (< 1 2 3) #t)
(assert (> 3 2 1) #t)
(assert (<= 1 2 2 3) #t)
(assert (>= 3 2 2 1) #t)
(assert (eq? (1) (1) (1)) #t)
(assert (== 1 1 1) #t)

(def !=
  (relationalOp !=))

(def\ /= (arg . args)
  (if (null? args) #t
    (if (member? arg args :cmp eq?) #f
      (apply /= args) )))

#| TODO in sostituzione del prededente, utile?
(def\ /= args
  (if (null? args) #t
    (let1 ((arg . args) args)
      (if (member? arg args :cmp eq?) #f
        (apply /= args) ))))
|#

(assert (!= 1 1 1) #f)
(assert (!= 1 2 1) #t)
(assert (/= 1 2 3) #t)
(assert (/= 1 2 1) #f)


;;; Thetic & Lytic

;;!The terms thetic (for + and *) and lytic (for - and /) are due to Hankel.

(def\ (theticOp binOp unit)
  (\ args (reduce binOp unit args)) )

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
      (reduce binOp arg1 rest) )))

(def -
  (lyticOp - 0) )

(def /
  (lyticOp / 1) )


;;; Greatest Common Divisor e Lowest Common Multiple

(def\ (gcd a b . more)
  (if (null? more)
    (if (0? b) a (gcd b (% a b)))
    (gcd a (apply gcd (cons b more))) ))

(def abs (let1 (abs (@getMethod Math "abs" &int)) (\ (n) (abs #null n))))

(assert (gcd 8 108) 4)
(assert (gcd 108 216 432) 108)

(def\ (lcm a b . more)
  (if (null? more)
    (if (|| (0? a) (0? b)) 0
      (abs (* b (/ a (gcd a b)))) )
    (lcm a (apply lcm (cons b more))) ))

(assert (lcm 8 108) 216)
(assert (lcm 3 4 5 6) 60)


;;; Sequences

(defGeneric length (sequence)
)
(defMethod length ((seq List))
  (%len seq))
(defMethod length ((seq Null))
  (%len seq))
(defMethod length ((seq String))
  (@length seq))


(defGeneric elt (sequence index)
)
(defMethod elt ((seq List) index)
  (nth index seq))
(defMethod elt ((seq String) index)
  (%subString seq index (+ index 1)))


(defGeneric subSeq (sequence start . end)
)
(defMethod subSeq ((seq List) start . end)
  (apply** %subList seq start end))
(defMethod subSeq ((seq Null) start . end)
  (apply** %subList seq start end))
(defMethod subSeq ((seq String) start . end)
  (apply** %subString seq start end))


;;; Coroutines

(defConstant coroutinePrompt
  'coroutine-prompt)

(defMacro coroutine forms
  (list* 'pushPrompt 'coroutinePrompt forms))

(defMacro yield (name . forms)
  (list* 'takeSubcont 'coroutinePrompt name forms))

(defMacro resume (k . forms)
  (list* 'pushDelimSubcont 'coroutinePrompt k forms))


;;; Fibers

#|!
The following implementation of fibers follows the one at URL `http://okmij.org/ftp/continuations/implementations.html#dget-wind'
We're calling them fibers instead of coroutines so as to not conflict with the built-in coroutine operators.
We use it for testing that built-in operators properly suspend and resume.
|#

(defConstant fiberPrompt
  'fiber-prompt)

(defClass YieldRecord ()
  (value continuation) )

(def\ makeYieldRecord (v k)
  (new YieldRecord :value v :continuation k))

(def\ fiberYield v?
  (takeSubcont fiberPrompt k
    (makeYieldRecord (optDft v? #inert) k)))

(def\ fiberResume (yieldRecord . v?)
  (pushDelimSubcont fiberPrompt (yieldRecord 'continuation)
    (optDft v? #inert)))

(defMacro fiber body
  (list* pushPrompt 'fiberPrompt body))

(def\ runFiber* (thunk . values)
  (let run ((result (fiber (thunk))) (values values))
    (if (type? result YieldRecord)
      (cons (result 'value)
        (if (null? values)
          (run (fiberResume result) #null)
          (run (fiberResume result (car values)) (cdr values)) ))
      (list result) )))

(assert (runFiber* (\ () (fiberYield 1) (fiberYield 2) 3)) '(1 2 3))

(assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t) '(1 2 #inert))
(assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t #_) '(1 2 #ignore))
(assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #t 4) '(1 2 4))
(assert (runFiber* (\ () (if (fiberYield 1) (fiberYield 2) 3)) #f) '(1 3))

(assert (runFiber* (\ () ((\ (a b) (+ a b)) (fiberYield 1) (fiberYield 2)) ) 3 4) '(1 2 7))

(defMacro (runFiberWithValues f args) (list 'eval (list 'list* 'runFiber* f args))) 
(defMacro (runFiberWithValues f args) (list 'apply 'runFiber* (list 'cons f args))) 
(defMacro (runFiberWithValues f args) (list 'apply** 'runFiber* f args)) 

(assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) '(#inert 3)) (1 2 3))
(assert (runFiberWithValues (\ () (fiberYield 1) (fiberYield 2)) (#inert 3)) (1 2 3))

(def runFiber runFiber*)


;;; Auto Increment/Decrement and Assignement Operators

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
        ((rval) (eval (list 'set! plc (op lval (eval rval env))) env))
        ((key rval) (eval (list 'set! plc key (op lval (eval rval env))) env)) )))))

(def $= (assignOp %$))
(def += (assignOp %+))
(def -= (assignOp %-))

(assert (begin (def a 1) (+= a :rhs 3)) 4)
(assert (begin (def a (newBox 1)) (+= a :rhs 3)) 4)
(assert (begin (def a (new Obj :fld 1)) (+= a :rhs :fld 3)) 4)


;;; Java Try/Resource

(defMacro (close1 binding . body)
  (list 'let1 binding
    (list* 'atEnd
      (list '@close (car binding))
      body )))

(defMacro (close bindings . body)
  (list 'let bindings
    (list* 'atEnd
      (list 'forEach '@close (cons 'list (map car bindings)))
      body )))


;;; Apl/J

(def\ (iota n) (reverse ((rec\ (iota n) (if (0? n) () (cons n (iota (1- n))))) n)))
(def\ (fork f l r) [_ (f (l _) (r _))])
(def\ (hook l r) [_ (l _ (r _))])


;;; Utility

(defVau (time times . forms) env
  (let* ( (currentTime (@getMethod System "currentTimeMillis"))
          ((#! (and Integer (> 0)) times) (eval times env))
          (milli (currentTime #null))
          (result (apply repeat (cons times forms) env))
          (milli (- (currentTime #null) milli)) )
    (print "time " times " " forms ": " milli "ms" (if (== times 1) "" ($ ", on average: " (@format String "%.2f" (/ milli (@doubleValue times))) "ms" )))
    result ))
