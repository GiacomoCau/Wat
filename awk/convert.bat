rem su wat e nibble non hanno test allo stesso posto
rem wat    lispx *.lispx test\*.lispx
rem nibble src   *.lispx ..\test\*.lispx
set AWKPATH=D:\Git\Wat\awk
del /Q /E theVm.mjs & for %%a in (vm eval control) do (gawk -f brace.awk %a.mjs) | (gawk -f format.awk) | (gawk -f comment.awk) >> theVm.mjs
rem al prompt togliere il doppio %%
for %%a in (*.lispx ..\test\*.lispx) do ((gawk -f docstr2cmt.awk %a) | y > %a)