;

(%resetEnv)
(@clear (.methods vm))
(ctapv #f)
(prtrc 0)
(bndres 0)

(load "lsp/testVm.lsp");
(load "lsp/boot.lsp");
(load "lsp/test.lsp");
(load "lsp/testJni.lsp");
