
(def\ (->begin lhs . rhs)
   (def rhs (if (null? rhs) (cdr lhs) (car! rhs))) 
   (if (null? rhs) () (null? (cdr rhs)) (car rhs) (cons 'begin rhs)))

(def\ (->\||begin lhs . rhs)
  (if (null? rhs) 
    (let1 ((lhs . rhs) lhs) (if (cons? lhs) (list (car lhs) (list* '\ (cdr lhs) rhs)) (list lhs (->begin lhs rhs))))
    (let1  (rhs (car! rhs)) (if (cons? lhs) (list* '\ (cdr lhs) rhs) (->begin lhs rhs))) ))

(defMacro (defEnv* lhs* . rhs*)
  (list* 'def*
    (map ->name lhs*)
    (map ->\||begin lhs* rhs*) ))

(defMacro (letEnv bindings . forms)
  (list* 'let (map ->\||begin bindings) 
    forms ))

(defMacro (letrecEnv bindings . forms)
  (list* 'let (map ->name+#inert bindings)
    (list* 'defEnv* (map car bindings) (map cdr bindings))
    forms ))

(assert (expand (defEnv* (a (b c)) (1) ((log c)))) '(def* (a b) 1 (\ (c) (log c))))
(assert (expand (letEnv ((a 1) ((b c) (log c))) (theEnv))) '(let ((a 1) (b (\ (c) (log c)))) (theEnv)))
(assert (expand (letrecEnv ((a 1) ((b c) (log c))) (theEnv))) '(let ((a #inert) (b #inert)) (defEnv* (a (b c)) (1) ((log c))) (theEnv)))

#;(let () ;; esercizio di recursione ...

  (defVau (defEnv . bindings) env ;; 13ms
    (if (null? bindings) env
      (let1 ( ((lhs . rhs) . bindings) bindings)
        (env (->name lhs) (eval (apply* ->\||begin lhs rhs) env))
        (apply defEnv bindings env) )))
  
  (let () (defEnv (a 1) (b 1 2) ((c c) (1+ c))) (assert a 1) (assert b 2) (assert (c 3) 4) )
  (time 100 (defEnv (a 1) (b 1 2) ((c c) (1+ c))) #inert)
    
  (defMacro (defEnv . bindings) ;; 21ms
    (let1 ( (lhs . rhs)
            ( (rec\ (self l r bindings)
               (if (null? bindings) (cons (reverse l) (reverse r))
                 (let1 (((lhs . rhs) . bindings) bindings)
                   (self (cons (->name lhs) l) (cons (->\||begin lhs rhs) r) bindings) )))
              () () bindings ) )
      (list* 'def* lhs rhs) ))
  
  (assert (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) '(def* (a b c) 1 (begin 1 2) (\ (c) (1+ c))))
  (time 100 (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) #inert)
  
  (defMacro (defEnv . bindings) ;; 24ms
    (let1 ( (lhs . rhs)
            ( (rec\ (self (l . r) bindings)
               (if (null? bindings) (cons (reverse l) (reverse r))
                 (let1 (((lhs . rhs) . bindings) bindings)
                   (self (cons (cons (->name lhs) l) (cons (->\||begin lhs rhs) r)) bindings) )))
              (()) bindings ) )
      (list* 'def* lhs rhs) ))
  
  (assert (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) '(def* (a b c) 1 (begin 1 2) (\ (c) (1+ c))))
  (time 100 (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) #inert)
  
  (defMacro (defEnv . bindings) ;; 31ms
    (let ( (lhs ( (rec\ (self bindings)
                    (if (null? bindings) ()
                      (let1 (((lhs . rhs) . bindings) bindings)
                         (cons (->name lhs) (self bindings)))))
                  bindings ))
           (rhs ( (rec\ (self bindings)
                    (if (null? bindings) ()
                      (let1 (((lhs . rhs) . bindings) bindings)
                        (cons (->\||begin lhs rhs) (self bindings)))))
                  bindings )) )
      (list* 'def* lhs rhs) ))
  
  (assert (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) '(def* (a b c) 1 (begin 1 2) (\ (c) (1+ c))))
  (time 100 (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) #inert)
  
  (defMacro (defEnv . bindings) ;; 38ms
    (let1 ( (lhs . rhs)
            ( (rec\ (self bindings)
                (if (null? bindings) (())
                  (let* ( (((lhs . rhs) . bindings) bindings)
                          ((l . r) (self bindings)) ) 
                    (cons (cons (->name lhs) l) (cons (->\||begin lhs rhs) r)) )))
              bindings ))
      (list* 'def* lhs rhs) ))
  
  (assert (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) '(def* (a b c) 1 (begin 1 2) (\ (c) (1+ c))))
  (time 100 (expand (defEnv (a 1) (b 1 2) ((c c) (1+ c)))) #inert)
)

(defVau (defEnv . bindings) env ;; 13ms
  (forEach (\ ((lhs . rhs)) (env (->name lhs) (eval (apply* ->\||begin lhs rhs) env))) bindings)
  env ) 

(let () (defEnv (a 1) (b 1 2) ((c c) (1+ c))) (assert a 1) (assert b 2) (assert (c 3) 4) )

(defVau (defrecEnv . bindings) env
  (forEach (\ ((lhs . #_)) (env (->name lhs) #inert)) bindings)
  (apply defEnv bindings env) )
