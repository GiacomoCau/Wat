|#
	Field
	
		field = (@getField class fieldName) = ((@getField Class String) class fieldName)
		field = (.fieldName class) = (@getField class fieldName) 
	
	Method
	
		method = (@getMethod class methodName ... class) = ((@getMethod Class String ... Class) class methodName ... class)
		method = (@methodName class ... class) = (@getMethod class methodName ... class)
	
	Constructor
		
		constructor = (@getConstructor class ... class) = ((@getConstructor Class ... Class) class ... class)   
		constructor = (@new class ... class) = (@getConstructor class ... class)
	
	Object
		object = (@newInstance constructor class ... args)
		
		object = (constructor class ... args) = (@newInstance constructor class ... args)
		object = (@new class ... args) = ((@new class (getClasses ... args)) class ... args)
	
	Call
		value = (@invoke method object ... args)
		
		value = (method object ... args) = (@invoke method object ... args)
		value = (@methodName object ... args) = ((@methodName (@getClass object) (getClasses ... args)) object ... args)
		
	Getter	
		value = (@get field object)
		
		value = (field object) = (@get field object)
		value = (.fieldName object) = ((.fieldName (@getClass object)) object)
	
	Setter
		void = (@set field object value)
		
		void = (field object value) = (@set field object value)
		void = (.fieldName object value) = ((.fieldName (@getClass object)) object value)
#|

;(prtrc 2)

&Wat.Prova
&Wat.Prova$Box

(@new &Wat.Prova$Box)
(@new &Wat.Prova$Box 1)
($define! box (@new &Wat.Prova$Box 1))
box
	
(.i box)
(.i box 2)
(.i box)
	
(@get box)
(@set box 3)
(@get box)

(@getMethod &Wat.Prova$Box "get")
(@getMethod &Wat.Prova$Box "set" &int)
(@getConstructor &Wat.Prova$Box)
(@getConstructor &Wat.Prova$Box &int)
((@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]) &Wat.Prova$Box)
((@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]) &Wat.Prova$Box &int)

($define! box (@new &Wat.Prova$Box 1))
((@getMethod &Wat.Prova$Box "get") box)
((@getMethod &Wat.Prova$Box "set" &int) box 5)
((@getMethod &Wat.Prova$Box "get") box)

($define! box (@new &Wat.Prova$Box 1))
(@invoke (@getMethod &Wat.Prova$Box "get") box)
(@invoke (@getMethod &Wat.Prova$Box "set" &int) box 4)
(@invoke (@getMethod &Wat.Prova$Box "get") box)
	
((@getMethod &java.lang.Class "newInstance") &Wat.Prova$Box) ; Class.newInstance di Class permette di istanziare solo classi con un costruttore senza parametri

(def getConstructor (@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]))
(def newInstance (@getMethod &java.lang.reflect.Constructor "newInstance" &java.lang.Object[]))
(newInstance (getConstructor  &Wat.Prova$Box))
(newInstance (getConstructor &Wat.Prova$Box &int) 1)
((@getMethod &java.lang.reflect.Constructor "newInstance" &java.lang.Object[]) (getConstructor &Wat.Prova$Box &int) 1)

(@newInstance &java.lang.reflect.Array &int 1)
(@newInstance &java.lang.reflect.Array &int 1 2 3)
	
(@forName &java.lang.Class "Wat.Prova$Box")

($define! int &int)
($define! Array &java.lang.reflect.Array)
($define! Object &java.lang.Object)
($define! a (@newInstance Array int 4))	
($define! get (@getMethod Array "get" Object int))
($define! set (@getMethod Array "set" Object int Object))
($define! length (@getMethod Array "getLength" Object))
(@invoke get #null a 2)	
(@invoke set #null a 2 4)
(@invoke get #null a 2)
(get #null a 2)
(set #null a 2 6)
(get #null a 2)
(length #null a)

((@getField &Wat.Prova$Box "i") (@new &Wat.Prova$Box 3))

; recupero ed esecuzione costruttore innerclass Wat.Vm$DVar (vanno resi public classe e costruttore)!
;((@getConstructor &Wat.Vm$DVar (@getClass (this)) &java.lang.Object) (this) 1)

; recupero dei costruttori di &Wat.Prova$Box
(@getConstructors &Wat.Prova$Box)
((@getConstructor &Wat.Prova$Box))
((@getConstructor &Wat.Prova$Box &int) 1)
(def getConstructor (@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]))
((getConstructor &Wat.Prova$Box))
((getConstructor &Wat.Prova$Box &int) 1)

((@getMethod &java.lang.Class "getField" &java.lang.String) &Wat.Prova$Box "i")
(def getField (@getMethod &java.lang.Class "getField" &java.lang.String))
(getField &Wat.Prova$Box "i")
((getField &Wat.Prova$Box "i") box)

((@getMethod &java.lang.Class "getMethod" &java.lang.String &java.lang.Class[]) &Wat.Prova$Box "get")
(def getMethod (@getMethod &java.lang.Class "getMethod" &java.lang.String &java.lang.Class[]))
(def get (getMethod &Wat.Prova$Box "get"))
(get box)

;(prtrc 0)
