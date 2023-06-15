;

(%resetEnv)

;(%def milli (@currentTimeMillis &java.lang.System))

(@clear (.methods vm))
(ctapv #f)
(prtrc 0)
(bndres 0)

(load "testVm.lsp");
(load "lsp/boot.lsp");
(load "lsp/test.lsp");
(load "lsp/testJni.lsp");

;(%def milli (%- (@currentTimeMillis &java.lang.System) milli))
;(%$ "vm started in " (%$ milli "ms"))

"vm started!"