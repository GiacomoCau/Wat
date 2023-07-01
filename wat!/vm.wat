;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctapv #f)
(prtrc 0)
(bndres 0)
(boxdft #null)

(%def Null #null)
(%def Any &Wat.Vm$Any)
(%def Apv &Wat.Vm$Apv)
(%def Opv &Wat.Vm$Opv)
(%def Box &Wat.Vm$Box)
(%def Cons &Wat.Vm$Cons)
(%def DVar &Wat.Vm$DVar)
(%def JFun &Wat.Vm$JFun)
(%def Obj &Wat.Vm$Obj)
(%def Inert &Wat.Vm$Inert)
(%def Ignore &Wat.Vm$Ignore)
(%def List &Wat.Vm$List)
(%def Error &Wat.Vm$Error)
(%def Symbol &Wat.Vm$Symbol)
(%def Keyword &Wat.Vm$Keyword)
(%def Boolean &java.lang.Boolean)
(%def Date &java.util.Date)
(%def Number &java.lang.Number)
(%def Integer &java.lang.Integer)
(%def Double &java.lang.Double)
(%def Object &java.lang.Object)
(%def String &java.lang.String)
(%def Math &java.lang.Math)
(%def System &java.lang.System)


(load "testVm.lsp");
(load "testJni.lsp");
(load "wat!/boot.wat");
(load "wat!/test.wat");

(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
(%$ "vm started in " (%$ milli "ms"))