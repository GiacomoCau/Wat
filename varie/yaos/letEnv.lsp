(defMacro (defEnv* lhs* . rhs*)
  (list* 'def*
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map (\ (lhs rhs) (if (cons? lhs) (list* '\ (cdr lhs) rhs) (list* 'begin rhs))) lhs* rhs*) ))

(defMacro (defEnv* lhs* . rhs*)
  (list* 'def*
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map (\ (lhs rhs) (if (cons? lhs) (list* '\ (cdr lhs) rhs) (null? rhs) () (null? (cdr rhs)) (car rhs) (list* 'begin rhs))) lhs* rhs*) ))

(def\ (->\||begin lhs . rhs)
  (if (null? rhs) 
    (let1 ((lhs . rhs) lhs) (if (cons? lhs) (list (car lhs) (list* '\ (cdr lhs) rhs)) (list lhs (cons 'begin rhs))))
    (let1  (rhs (car! rhs)) (if (cons? lhs) (list* '\ (cdr lhs) rhs) (cons 'begin rhs))) ))

(defMacro (defEnv* lhs* . rhs*)
  (list* 'def*
    (map (\ (lhs) (if (cons? lhs) (car lhs) lhs)) lhs*)
    (map ->\||begin lhs* rhs*) ))

(defMacro (letEnv bindings . forms)
  (list* 'let (map ->\||begin bindings) 
    forms ))

(defMacro (letEnvRec bindings . forms)
  (list* 'let (map ->name+#inert bindings)
    (list* 'defEnv* (map car bindings) (map cdr bindings))
    forms ))

(assert (expand (defEnv* (a (b c)) (1) ((log c)))) '(def* (a b) (begin 1) (\ (c) (log c))))
(assert (expand (letEnv ((a 1) ((b c) (log c))) (theEnv))) '(let ((a (begin 1)) (b (\ (c) (log c)))) (theEnv)))
(assert (expand (letEnvRec ((a 1) ((b c) (log c))) (theEnv))) '(let ((a #inert) (b #inert)) (defEnv* (a (b c)) (1) ((log c))) (theEnv)))
