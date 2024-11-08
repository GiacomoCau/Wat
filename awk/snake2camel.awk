{
	while (match($0, "([a-z]+(-[a-z]+)+)", a)) {
		#print a[0] "|" a[1] "|" camelize(a[1])
		sub(a[1], camelize(a[1]), $0)
	}	
	print
}

function camelize(w, a) {
	while (match(w, /(-)([a-z])/, a) != 0) sub(a[0], toupper(a[2]), w);
	return w
}
