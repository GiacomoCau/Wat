
Wat.Prova
	class Wat.Prova
Wat.Prova$Box
	class Wat.Prova$Box
	
(@new Wat.Prova$Box)
	Wat.Prova$Box@68de145
(@new Wat.Prova$Box 1)
	Wat.Prova$Box@446cdf90
	
($define! box (@new Wat.Prova$Box 1))
	#ignore
box
	Wat.Prova$Box@799f7e29
	
(.i box)
	1
(.i box 2)
	#null
(.i box)
	2
	
(@get box)
	2
(@set box 3)
	#null
(@get box)
	3

(@getMethod Wat.Prova$Box "get")
	public int Wat.Prova$Box.get()
(@getMethod Wat.Prova$Box "set" int)
	public void Wat.Prova$Box.set(int)

(@getConstructor Wat.Prova$Box)
	public Wat.Prova$Box()
(@getConstructor Wat.Prova$Box int)
	public Wat.Prova$Box(int)
	
($define! box (@new Wat.Prova$Box 1))
	#ignore	
((@getMethod Wat.Prova$Box "get") box)
	1
((@getMethod Wat.Prova$Box "set" int) box 5)
	#null
((@getMethod Wat.Prova$Box "get") box)
	5

($define! box (@new Wat.Prova$Box 1))
	#ignore
(@invoke (@getMethod Wat.Prova$Box "get") box)
	1
(@invoke (@getMethod Wat.Prova$Box "set" int) box 4)
	#null
(@invoke (@getMethod Wat.Prova$Box "get") box)
	4
	
(@newInstance (@getConstructor Wat.Prova$Box))
	Wat.Prova$Box@3fb6a447
(@newInstance (@getConstructor Wat.Prova$Box int) 1)
	Wat.Prova$Box@26ba2a48
	
(@forName java.lang.Class "Wat.Prova$Box")
	class java.lang.Integer	