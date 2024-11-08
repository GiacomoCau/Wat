{
	while (match($0, "[ |]([A-Z][-A-Z]+[12]?)([ ,.]|$)", a)) {
		# print a[0]
		sub(a[1], "<b>" camelize(tolower(a[1])) "</b>")
	}
	print
}

function camelize(w, a) {
	while (match(w, /(-)([a-z])/, a) != 0) sub(a[0], toupper(a[2]), w)
	return w
}
