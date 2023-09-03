# Wat

### Java Wat Kernel

Java port of [wat-js](https://github.com/GiacomoCau/wat-js)
with a shadow of [nybble-lisp](https://github.com/nybble-lisp/nybble-lisp)
and the tco suggestion of [jscheme](https://github.com/chidiwilliams/jscheme)


* distinzione fra liste proprie ed improprie (List <: Cons)
* eseguibili esclusivamente le liste proprie
* autoquote: le liste dove il car non è un Combinabile sono costanti
* if else con forme multiple
