# Wat

### Java Wat Kernel

Java port of [wat-js](https://github.com/GiacomoCau/wat-js)
with more then a shadow of [lispx](https://github.com/lispx/lispx)
and the tco suggestion of [jscheme](https://github.com/chidiwilliams/jscheme)

Queste le differenze rispetto a Wat/lispX
* lispx è stato convertito da lisp-2 a lisp-1
* il #nil di lixpx è stato convertito in `#null`
* `#null` e `()` sono uguali ed implementati con il null di java
* il #void di lispx è stato convertito in `#inert`
* `#ignore` e `#_` sono due forme per la stessa cosa
* la nomenclatura è stata camellizata eliminando i trattini
* le liste proprie ed improprie sono distinte (`List` <: `Cons`)
* le liste improprie, terminano con un cdr diverso da `#null`, sono literal
* le liste proprie, terminano con cdr uguale a `#null`, sono espressioni valutabili
* le liste proprie dove la valutazione del car non è un `Combinable` sono literal `(autoquote (or #t #f))`
* le liste possono essere delimitate sia da `()` che da `[]`
* l'operatore __if__ può avere più forme test then
* true può essere #t, !#f, !(or #f #null), !(or #f #null #inert) o !(or #f #null #inert 0) `(typeT (or 0 1 2 3 4))`
* l'operatote __def__ può restituire:  
	`#inert`, il valore assegnato (`:rhs`) o il valore precedente (`:prv`) dell'ultima bind effettuata  
	specificandolo direttamente nell'espressione `(def symbol (or #inert :rhs :prv) value)`  
	o indirettamente per l'impostazione di `(bndRes (or #inert :rhs :prv))`
* gli `Obj` possono essere definiti con coppie key valore che diventeranno coppie slot valore dell'obj `(new Obj key value ...)`
* gli `Obj` sono `Combinable` e combinati con
	* una key ritornano il valore associato a quella key `(obj key)`
	* coppie key valore
		* queste coppie diventeranno coppie slot valore dell'obj `(obj key value ...)`
		* possono restituire `#inert`, il valore assegnato (`:rhs`) o il valore precedente (`:prv`) dell'ultimo slot assegnato, o l'oggetto stesso (`:obj`)  
		specificandolo direttamente nell'espressione `(obj (or #inert :rhs :prv :obj) key value ...)`  
		o indirettamente per l'impostazione di `(bndRes (or #inert :rhs :prv))`
* gli `Env` possono essere definiti anche con un env parent e
	* coppie key valore che diventeranno coppie simbolo valore del nuovo env `(newEnv env key value ...)`
	* un `Obj` le cui coppie slot valore diventeranno coppie simbolo valore del nuovo env `(newEnv env obj)`  
* gli `Env` sono `Combinable` e combinati con
	* una key ritornano il valore associato a quella key `(env key)`
	* coppie key valore
		* queste coppie diventeranno coppie simbolo valore dell'env `(env key value ...)`
		* possono restituire `#inert`, il valore assegnato (`:rhs`) o il valore precedente (`:prv`) dell'ultimo simbolo binded, o l'oggetto stesso (`:obj`)  
		specificandolo direttamente nell'espressione `(env (or #inert :rhs :prv :obj) key value ...)`  
		o indirettamente per l'impostazione di `(bndRes (or #inert :rhs :prv))`
		* precedute da `:def` per bindare nell'ultimo frame o `:set!` per bindare nel frame dove la singola key è presente `(env (or :def :set!) (or #inert :rhs :prv :obj) key value ...)` 
* le key utilizzate per le assegnazioni degli `Obj` e le bind degli `Env` possono essere `(or Keyword Symbol String)`
* la funzione __eval__ può essere chiamata con la sola espressione da valutare, l'`Env` sarà quello corrente `(eval exp)`
* l'operatore __vau__ (ovvero __\\__ o __lambda__) permette il controllo su tipo e valore dei parametri.  
	Al posto del singolo parametro `symbol` potrà essere specificata l'espressione `(#! check symbol)`  
	dove `check` può essere: un `value`, la classe `Any`, una `Class`, una `List` con uno o due `Integer` o un `Integer` e `oo` seguiti da 0 o più `check`, 	una `List` con car `or` o `and` seguiti da più `check`, un `List` con car `<` `<=` `>=` o `>` seguita da un `Comparable`.  
	Se il `check` è
	* un `value` il parametro deve essere uguale a quel `value`
	* la classe `Any` il parametro può essere qualunque valore
	* una `Class` il parametro può essere una istanza di quella `Class` o una classe che estende quella `Class`
	* una `List` il parametro deve essere una `List` dove	
		* il primo `Integer` indica il numero minimo di elementi, se assente vale `0`
		* il secondo `Integer` indica il numero massimo di elementi, se assente vale `oo` ovvero `Integer.maxValue`
		* se il numero degli argomenti `check` della lista è
			* minore del minimo, gli elementi del parametro eccedenti il minimo potranno essere `Any` ovvero qualunque valore
			* maggiore del minimo, gli elementi del parametro eccedenti il minimo verranno controllati usando ciclicamente gli argomenti `check` eccedenti il minimo
		* se il primo elemento della lista è
			* `or` il parametro dovrà corrispondere ad uno degli argomenti `check` della `or`
			* `and` il parametro dovrà corrispondere a tutti gli argomenti `check` della `and`
			* `<` `<=` `>=` `>` il parametro dovrà essere rispettivamente `<` `<=` `>=` `>` dell'argomento `Comparable` della lista
* le `Box` sono oggetti diversi dagli `Obj`
* le `Box` sono `Combinable` e combinate con
 	* `()` ritornano il valore associato `(box)`
	* un valore
		* questo diventerà il valore della box `(box value)`
		* possono restituire `#inert`, il valore assegnato (`:rhs`) o il valore precedente (`:prv`), o l'oggetto stesso (`:obj`)  
		specificandolo direttamente nell'espressione `(box (or #inert :rhs :prv :obj) value)`  
		o indirettamente per l'impostazione di `(bndRes (or #inert :rhs :prv))`
* le `DVar` ovvero le dinamic variables estendono le `Box`
* le `DVar` sono gestite da una unico operatore `(%d\\ (#! (Symbol) symbols) . body)`  
	che permette di implementare come macro i vari operatori per le dynamic variables __ddef__ __ddef*__ __progv__ __dlet__ __dlet*__
	* se `body` è
		* `()` assegnarà i valori con cui è combinato, convertiti in `DVar`, ai corrispettivi simboli nel corrente `Env` `((d\\ symbols) . values)`
		* una `List` assegnerà i valori con cui è combinato alle `DVar` associate ai simboli, eseguirà le forms nel corrente `Env` ed alla fine ripristinerà i valori delle
		`DVar` a quelli precedenti l'assegnazione `((d\\ symbols form . forms) . values)`
	* se è combinato con `()` utilizzerà come valore da assegnare quello ottenibile da `(boxDft)`
* __catchTagWth__ e __throwTag__ possono essere indifferentemente operatori (`Opv`) o funzioni (`Apv`) in dipendenza di `(ctApv (or #t #f))`
* l'handler di __catchTagWth__ può essere oltre che una funzione di un argomento (`Apv1`) anche un qualunque valore che verrà tornato in caso un `Error` venga catched 
* l'operatore __finally__ è derivato come macro dall'operatore __atEnd__ `(atEnd (#! Apv0 cleaner) form . forms)`
* __takeSubcont__ __pushPrompt__ __pushDelimSubcont__ e __pushSubcontDelim__ sono operatori e non necessitano di completamento
* `Supplier` `Function` `BiFunction` `Executable` e `Field`, genericamente java function, sono implicitamente considerate funzioni `Apv`
* __wrap__ non wrappa gli `Apv` e le java function ma wrappa i restanti `Combinator`
* __unwrap__ unwrappa gli `Apv`, wrappa le java function in una `JFun` e non unwrappa i restanti `Combinator`
* le `String` possono essere interned con `(intStr (or #f #t))`, se interned possono essere confrontate con `==`
* __defMacro__ __defVau__ __def\\__ __def*\\__ __rec\\__ __let1\\__ __let1rec\\__ __let\\__ __letrec\\__ permettono la definizione dei `Combinable` con due forme equivalenti
	* `(___ name parameters . body)`
	* `(___ (name . parameters) . body)`
* __rec__ __rec\\__ __let1rec__ __let1rec\\__ __letrec__ __letrec\\__ inizializzano a `#inert` le definizioni prima della valutazione
* unificate le varie tipologie di commenti interni alle definizioni di lispx con il solo commento multiriga #| ... |#
* possibilità di attivare e disattivare la Tail Call Ottimization `(doTco (or #t #f))`
