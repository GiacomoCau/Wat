
mlcmt { mlcmt=0; sub(/ \*/, "/*"); print; next }
/^[[:blank:]]+\/\*[[:blank:]]*$/ { mlcmt=1; next }

/\/\*+/ { sub(/\/\*\*\*[[:blank:]]*/, "// "); sub(/[[:blank:]]*\*+\//, ""); print "\n" $0; next }

/^[[:blank:]]+\}/ { print gensub(/(^[[:blank:]]+)} (.*)/, "\\1}\n\\1\\2", "g"); next  }

{ sub(/;$/,""); print gensub(/\((\w+))[[:blank:]]*=>/, "\\1 =>", "g") } # parametro singolo senza parentesi




