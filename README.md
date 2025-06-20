# Wat

### Java Kernel Wat LispX

Java port of [wat-js](https://github.com/GiacomoCau/wat-js)
with more then a shadow of [lispx](https://github.com/lispx/lispx)
and the tco suggestion of [jscheme](https://github.com/chidiwilliams/jscheme)

Now Wat and LispX have essentially the same boot except for the different error handling,
simple throw and catch for Wat and similar to Common Lisp for LispX.

Here the [language reference](https://htmlpreview.github.io?https://github.com/GiacomoCau/Wat/blob/main/reference/reference.html)

These are the differences compared to the original Wat/LispX
* LispX was converted from lisp-3 (value,function,class) to lisp-1
* LispX's `#nil` has been converted to `#null`
* `#null` and `()` are the same value and implemented with java `null`
* Lispx `#void` has been converted to `#inert`
* `#ignore` and `#_` are two forms of the same valore
* the nomenclature has been changed by eliminating the hyphens and camelizing
* Proper and improper lists are distinct (`List` <: `Cons`)
* Proper lists, ending with the last cdr equal to `#null`, are evaluable expressions
* Improper lists, ending with the last cdr other than `#null`, are literal
* Proper lists where the car evaluation is not a `Combinable` are literal `(aQuote (or #t #f))`
* Lists can be delimited by either `()` or `[]`
* The __if__ operator can have multiple `test` and `then` forms, idea stolen from [Anarki](https://github.com/arclanguage/anarki)
* The expressions can be `true` if is equals to `#t`, `!#f`, `!(or #f #null)`, `!(or #f #null #inert)` or `!(or #f #null #inert 0)` by setting `(typeT (or 0 1 2 3 4))`
* The __def__ and __set!__ operators can return:
	`#inert`, the assigned value (`:rhs`) or the previous value (`:prv`) of the last bind made, or the current environment (`:cnt`)
	specifying it directly in the expression `(def/set! symbol (or #inert :rhs :prv :cnt) value)`
	or indirectly by setting `(bndRes (or #inert :rhs :prv :cnt))`
* The `Obj` extends `Throwable` and can be defined with
	* key value pairs which will become attribute value pairs of the new obj `(new Obj key value ...)`
	<br>these pairs can be preceded by a
		* String `(new Obj string key value ...)`
		* Throwable `(new Obj throwable key value ...)`
		* String and a Trowable `(new Obj string throwable key value ...)`
	* an `Env` whose symbol value pairs from the first frame will become attribute value pairs of the new obj `(new Obj env)`
* The `Obj` are `Combinable` and combined with
	* a key return the value associated with that key `(obj key)`
	* key value pairs
		* these pairs will become obj attribute value pairs `(obj key value ...)`
		* can return `#inert`, the assigned value (`:rhs`) or the previous value (`:prv`) of the last assigned attribute, or the object itself (`:cnt`)
		specifying it directly in the expression `(obj (or #inert :rhs :prv :cnt) key value ...)`
		or indirectly by setting `(bndRes (or #inert :rhs :prv :cnt))`
* The `Env` can also be defined without `(newEnv)` or with an env parent  `(newEnv env)` and
	* key value pairs which will become symbol value pairs of the new env `(newEnv env key value ...)`
	* an `Obj` whose attribute value pairs will become symbol value pairs of the new env `(newEnv env obj)`
* The `Env` are `Combinable` and combined with
	* a key return the value associated with that key `(env key)`
	* key value pairs
		* these pairs will become value symbol pairs of the env `(env key value ...)`
		* can return `#inert`, the assigned value (`:rhs`) or previous value (`:prv`) of the last bound symbol, or the environment itself (`:cnt`)
		specifying it directly in the expression `(env (or #inert :rhs :prv :cnt) key value ...)`
		or indirectly by setting `(bndRes (or #inert :rhs :prv :cnt))`
		* preceded by `:def` to bind in the last frame or `:set!` to bind in the frame where the single key is present `(env (or :def :set!) (or #inert :rhs :prv :cnt) key value ...)`
* The keys used for `Obj` assignments and `Env` bindings can be `(or Keyword Symbol String)`
* The __eval__ function can be called with only the expression to evaluate, the `Env` will be the current one `(eval exp)`
* The __def__, __set!__ and __vau__ (i.e. __\\__ or __lambda__) operators allow the decomposition of `Object[]` `Obj` and `Object` in addition to lists.

	the decomposition of the `Object[]` is for the position
	* `(def/set! (a b) (array 1 2 3))`
	* `((\ ((a b)) (+ a b)) (array 1 2 3))`

	the `Obj` decomposition is for the attribute name
	* `(def/set! (a b) (newObj :a 1 :b 2))`
	* `((\ ((a b)) (+ a b)) (newObj :a 1 :b 2))`

	in the decomposition lists you can
	* insert `.field` or `@method` or `(@method ...)` to have the resulting value with the name of the specified field or method
	* specify a symbol in the last cdr, for `Object[]` it is the residual array while for all other objects it is the object itself

* The __def__ and __set!__ operators allows control over the type and value of bindings.
	
	for checking the type and value of the
	* bindings: instead of the single `symbol` in the definiend tree, must be specified the expression `(#: check symbol)`

* The __vau__ operator (i.e. __\\__ or __lambda__) allows control over the type and value of parameters and value returned.

	for checking the type and value of the
	* return value: the body of the vau must start with a `#:` followed by the `check` followed by the `forms` of the body `(vau pt ep #: check . forms)`
	* parameters: instead of the single `symbol` in the parameter tree, must be specified the expression `(#: check symbol)`,
	
* The `check` can be:
	* a `value`,
	* the `Any` class,
	* a `Class`,
	* a `List` with zero, one or two `Integers` followed by zero or more `checks`,
	* a `List` with an `Integer` and the symbol `oo` followed by zero or more `checks`,
	* a `List` with car equals `or` followed by two or more `check`,
	* a `List` with car equals `and` followed by two or more `check`,
	* a `List` with car an `Apv` followed by zero o more arguments.

	When the `check` is:
	* a `value`: the parameter value or value returned must be equal to that `value`
	* the `Any` class: the parameter value or value returned can be any value
	* a `Class`: the parameter value or value returned can be an instance of that `Class` or a class that extends that `Class`
	* a `List`: the parameter value or value returned must be a `List` where, in the `List` `check`
		* the first `Integer` indicates the minimum number of elements, the default is `0`
		* the second `Integer` indicates the maximum number of elements,
		  the default is the value of the minimum number of elements if present, otherwise `Integer.maxValue`.
		  For the maximum number of elements can be also specified the symbol `oo` for `Integer.maxValue`.
		* if the number of `check` arguments in `List` is:
			* less than the minimum: parameters values or values returned that exceed the minimum can have any value
			* greater than the minimum: the parameters values or values returned exceeding the minimum will be checked cyclically using `check` arguments exceeding the minimum 
		* if the first element of `List` is:
			* `or`: the parameter value or value returned must match one of the `check` arguments of the `or`
			* `and`: the parameter value or value returned must match all the `check` arguments of the `and`
			* an `Apv`: the applying of `Apv` to the cons of the parameter value or value returned and the remaining arguments must return `#true`
* The `Box` are different objects from `Obj`
* The `Box` are `Combinable` and combined with
	* `()` return the associated value `(box)`
	* a value
		* this will become the value of the box `(box value)` 
		* can return `#inert`, the assigned value (`:rhs`), the previous value (`:prv`), or the box itself (`:cnt`)
		  specifying it directly in the expression `(box (or #inert :rhs :prv :cnt) value)`
		  or indirectly by setting `(bndRes (or #inert :rhs :prv :cnt))`
* The `DVar` or dynamic variables extend the `Box`
* The `DVar` are managed by a single operator `(dv\ (#: (Symbol) symbols) . forms)`
	which allows you to implement the various operators for dynamic variables as macros __ddef__ __ddef*__ __progv__ __dlet__ __dlet1__ __dlet*__
	* if `forms` is
		* `()` will assign the values ​​it is combined with, converted to `DVar`, to the corresponding symbols in the current `Env` `((dv\ symbols) . values)`
		* a `List` will assign the values ​​with which it is combined to the `DVar` associated with the symbols, execute the forms in the current `Env`
		  and finally restore the values ​​of the `DVar` to those preceding the assignment `((dv\ symbols form . forms) . values)`
	* if it is combined with `()` it will use the value obtainable from `(boxDft)` as the value to be assigned
* The __catchTagWth__ handler, can be not only a function of an argument (`Apv1`), but also any value that will be returned if an `Error` is caught
* The __finally__ operator is derived, as a macro, from the __atEnd__ operator `(atEnd (#: Apv0 cleaner) form . forms)`
* __pushPrompt__, __takeSubcont__, __pushSubcont__, __pushDelimSubcont__ and __pushSubcontDelim__ are operators and do not require completion
* `Supplier` `Consumer` `Function` `BiConsumer` `BiFunction` `Executable` and `Field`, generically `java functions`, are implicitly considered `Apv` functions
* __wrap__ does not wrap the `Apv` and the `java functions`, but wraps the remaining `Combinator`
* __unwrap__ unwraps the `Apv`, wraps the `java functions` in a `JFun` and does not unwrap the remaining `Combinator`
* `String` can be interned depending on `intStr = (or true false)`, if interned they can be compared with `==`
* __defMacro__, __defVau__, __def\\__, __defde\\__, __def*\\__, __rec\\__, __let1\\__, __let1rec\\__, __let\\__ and __letrec\\__ allow the definition of `Combinable` with two equivalent forms
	* `(___ name parameters . body)`
	* `(___ (name . parameters) . body)`
* __rec__, __rec\\__, __let1rec__, __let1rec\\__, __letrec__ and __letrec\\__ initialize definitions to `#inert` before evaluation
* Unified the various types of comments within LispX definitions with only the nestable multiline comment `#| ... |#`
* The special symbol semicolon #; makes the following expression a comment
* Ability to enable or disable Tail Call Optimization `(doTco (or #t #f))`