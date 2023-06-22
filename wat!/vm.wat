;

(%resetEnv)

(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctapv #f)
(prtrc 0)
(bndres 0)
(bdft #null)

(load "testVm.lsp");
(load "testJni.lsp");
(load "wat!/boot.wat");
(load "wat!/test.wat");

(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
(%$ "vm started in " (%$ milli "ms"))