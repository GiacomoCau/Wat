
(assert (car 1) Error @getMessage "not a Cons: 1 combining %Car with (1)")
(assert (+ 1 #t) Error @getMessage "not a Number: #t combining %+ with (1 #t)")
(assert (+ 1 "0") Error @getMessage "not a Number: \"0\" combining %+ with (1 \"0\")")
(assert (< "0" 1) Error @getMessage "error executing: %< with: (\"0\" 1)")

(assert (def a (+ 1 "0")) Error @getMessage "not a Number: \"0\" combining %+ with (1 \"0\")" )
(assert (def (#: String a) 1) Error @getMessage "not a String: 1 binding: (#: String a) of: (%Def (#: String a) 1) with: 1")
(assert (set! (#: String a) 1) Error @getMessage "not a String: 1 setting: (#: String a) of: (%Set! (#: String a) 1) with: 1")

(assert ((\ ((#: (< "1") a)) a) 0) Error @getMessage "not a (< \"1\"): 0 binding: ((#: (< \"1\") a)) of: {%Opv ((#: (< \"1\") a)) #ignore a} with: (0)")
(assert ((\ ((#: (< 1) a)) a) "0") Error @getMessage "not a (< 1): \"0\" binding: ((#: (< 1) a)) of: {%Opv ((#: (< 1) a)) #ignore a} with: (\"0\")")
(assert ((\ ((#: (and Integer (< 1)) a)) a) "0") Error @getMessage "not a (and Integer (< 1)): \"0\" binding: ((#: (and Integer (< 1)) a)) of: {%Opv ((#: (and Integer (< 1)) a)) #ignore a} with: (\"0\")")
(assert ((\ ((#: (and Integer (< 1)) a) b) a) "0" #t) Error @getMessage "not a (and Integer (< 1)): \"0\" binding: ((#: (and Integer (< 1)) a) b) of: {%Opv ((#: (and Integer (< 1)) a) b) #ignore a} with: (\"0\" #t)")

(assert (check 1 String) Error @getMessage "not a String: 1")
(assert (check (1) (String)) Error @getMessage "not a String: 1 checking (1) with (String)")
