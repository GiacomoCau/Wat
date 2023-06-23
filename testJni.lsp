#|
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
|#

;(prtrc 2)

&Wat.Prova
&Wat.Prova$Cls

(@new &Wat.Prova$Cls)
(@new &Wat.Prova$Cls 1)
(%def cls (@new &Wat.Prova$Cls 1))
cls
	
(.i cls)
(.i cls 2)
(.i cls)
	
(@get cls)
(@set cls 3)
(@get cls)

(@getMethod &Wat.Prova$Cls "get")
(@getMethod &Wat.Prova$Cls "set" &int)
(@getConstructor &Wat.Prova$Cls)
(@getConstructor &Wat.Prova$Cls &int)
((@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]) &Wat.Prova$Cls)
((@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]) &Wat.Prova$Cls &int)

(%def cls (@new &Wat.Prova$Cls 1))
((@getMethod &Wat.Prova$Cls "get") cls)
((@getMethod &Wat.Prova$Cls "set" &int) cls 5)
((@getMethod &Wat.Prova$Cls "get") cls)

(%def cls (@new &Wat.Prova$Cls 1))
(@invoke (@getMethod &Wat.Prova$Cls "get") cls)
(@invoke (@getMethod &Wat.Prova$Cls "set" &int) cls 4)
(@invoke (@getMethod &Wat.Prova$Cls "get") cls)
	
((@getMethod &java.lang.Class "newInstance") &Wat.Prova$Cls) ; Class.newInstance di Class permette di istanziare solo classi con un costruttore senza parametri

(%def getConstructor (@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]))
(%def newInstance (@getMethod &java.lang.reflect.Constructor "newInstance" &java.lang.Object[]))
(newInstance (getConstructor  &Wat.Prova$Cls))
(newInstance (getConstructor &Wat.Prova$Cls &int) 1)
((@getMethod &java.lang.reflect.Constructor "newInstance" &java.lang.Object[]) (getConstructor &Wat.Prova$Cls &int) 1)

(@newInstance &java.lang.reflect.Array &int 1)
(@newInstance &java.lang.reflect.Array &int 1 2 3)
	
(@forName &java.lang.Class "Wat.Prova$Cls")

(%def int &int)
(%def Array &java.lang.reflect.Array)
(%def Object &java.lang.Object)
(%def a (@newInstance Array int 4))	
(%def get (@getMethod Array "get" Object int))
(%def set (@getMethod Array "set" Object int Object))
(%def length (@getMethod Array "getLength" Object))
(@invoke get #null a 2)	
(@invoke set #null a 2 4)
(@invoke get #null a 2)
(get #null a 2)
(set #null a 2 6)
(get #null a 2)
(length #null a)

((@getField &Wat.Prova$Cls "i") (@new &Wat.Prova$Cls 3))

; recupero ed esecuzione costruttore innerclass Wat.Vm$DVar (vanno resi public classe e costruttore)!
;((@getConstructor &Wat.Vm$DVar (@getClass vm) &java.lang.Object) vm 1)

; recupero dei costruttori di &Wat.Prova$Cls
(@getConstructors &Wat.Prova$Cls)
((@getConstructor &Wat.Prova$Cls))
((@getConstructor &Wat.Prova$Cls &int) 1)
(%def getConstructor (@getMethod &java.lang.Class "getConstructor" &java.lang.Class[]))
((getConstructor &Wat.Prova$Cls))
((getConstructor &Wat.Prova$Cls &int) 1)

((@getMethod &java.lang.Class "getField" &java.lang.String) &Wat.Prova$Cls "i")
(%def getField (@getMethod &java.lang.Class "getField" &java.lang.String))
(getField &Wat.Prova$Cls "i")
((getField &Wat.Prova$Cls "i") cls)

((@getMethod &java.lang.Class "getMethod" &java.lang.String &java.lang.Class[]) &Wat.Prova$Cls "get")
(%def getMethod (@getMethod &java.lang.Class "getMethod" &java.lang.String &java.lang.Class[]))
(%def get (getMethod &Wat.Prova$Cls "get"))
(get cls)

;(prtrc 0)
