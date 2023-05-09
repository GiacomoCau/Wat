{
	gsub("#'","") # via le costanti funzioni
	gsub("#void", "#inert")
	gsub("#nil", "#null") # namespace funzioni
	#namespace classi in & iniziale maiuscola
	while (match($0, "#\\^([-a-z]+)", a) != 0) sub("#\\^" a[1], camelize(a[1]))
	if (match($0, /defclass ([-a-z]+)/, a)) sub(a[1], camelize(a[1]))

	
#	sub(/\|[^|]+\|/, ";&"); # solo in boot-test.lispx
#	sub(/defun/,"def\")
#	sub(/defexpr/,"defVau")
#	sub(/defmacro/,"defMacro")
}

function camelize(w, a) {
	while (match(w, /(^|-)([a-z])/, a) != 0) sub(a[0], toupper(a[2]), w)
	return w
}


# scarto le righe già commentate
/^[[:blank:]]*;/ { print; next }

# scarto stringhe che iniziano e finiscono sulla linea
#/^[[:blank:]]*"[^"]+"\)?$/            { sub(/^[[:blank:]]+/,"&;"); sub(/\)$/,"\n)"); print; next }
#/^[[:blank:]]*\(:documentation .*\)$/ { sub(/^[[:blank:]]+/,"&;"); sub(/\)\)$/,")\n)"); print; next }
/^[[:blank:]]*"((\\")|[^"])*"\)?)?$/  { print; next }
/^[[:blank:]]*\(:documentation .*\)$/ { print; next }

# modifico
#instr { if (/"\)?$/)  instr=0; sub(/^[[:blank:]]*/,spc";"); sub(/"\)$/,"\"\n"substr(spc,3)")"); print; next }
#indoc { if (/\)\)?$/) indoc=0; sub(/^[[:blank:]]*/,spc";"); sub(/\)\)$/,")\n"substr(spc,3)")"); print; next }
 instr {	if (/"\)?$/)  instr=0; sub(/^[[:blank:]]*/,spc"|"); sub(/"\)$/,"\""substr(spc,3)")"); print; next }
 indoc {	if (/\)\)?$/) indoc=0; sub(/^[[:blank:]]*/,spc"|"); sub(/\)\)$/,")"substr(spc,3)")"); print; next }

#/^[[:blank:]]*"/                 { instr=1; spc=substr($0, 1, match($0,/[^[:blank:]]/)-1); sub(/^[[:blank:]]*/,spc";"); print; next }
#/^[[:blank:]]*\(:documentation / { indoc=1; spc=substr($0, 1, match($0,/[^[:blank:]]/)-1); sub(/^[[:blank:]]*/,spc";"); print; next }
/^[[:blank:]]*"/                 { instr=1; spc=substr($0, 1, match($0,/[^[:blank:]]/)-1); sub(/^[[:blank:]]*/,spc); print; next }
/^[[:blank:]]*\(:documentation / { indoc=1; spc=substr($0, 1, match($0,/[^[:blank:]]/)-1); sub(/^[[:blank:]]*/,spc); print; next }

# tutte le altre
{ print }