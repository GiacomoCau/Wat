
(def\ (<- env . keys)
  (if (null? keys) env
    (apply** <- (env (car keys)) (cdr keys)))) 

(def\ (instance cls . args) (apply (cls :init) args))




(defVau (class extend . bindings) env
  (def\ (->\||begin static lhs rhs)
    (if (cons? lhs)
      (list* '\ (cdr lhs) rhs)
      (->begin (cons lhs rhs)) ))
  (def\ (->\||begin static lhs rhs)
    (if (cons? lhs)
      (if (|| static (== (car lhs) 'new))
        (list* '\ (cdr lhs) rhs)
        (list 'wrap (list 'vau 'args 'env :method (list 'eval (list 'list* (list 'quote (list* '\ (cdr lhs) rhs)) 'args) '(env :this))))
        ;(list 'wrap (list 'vau 'args 'this (list 'eval (list 'list* (list 'quote (list* '\ (cdr lhs) rhs)) 'args) 'this)))
        ;(list '\ '(this . args) :method (list 'log 'args 'this) (list 'apply (list 'log (list 'eval (list 'quote (list* '\ (cdr lhs) rhs)) 'this)) 'args 'this))
        )
      (->begin (cons lhs rhs)) ))
  (def\ (object static bindings super)
    (def this (newEnv super))
    (forEach (\ (b) (this (if (cons? b) (->name (car b)) b) #inert)) bindings)
    (forEach (\ (b) (when (cons? b) (let1 ((lhs . rhs) b) (this (->name lhs) (eval (apply* ->\||begin static lhs rhs) this))))) bindings)
    this )
  (def static (if (|| (null? bindings) (atom? (car bindings)) (!= (caar bindings) 'static)) () (prog1 (cdar bindings) (def bindings (cdr bindings)))))
  (def hasnew? ((rec\ (loop b) (if (null? b) #f (atom? (car b)) (loop (cdr b)) (let1 (((n . #_) . b) b) (if (&& (cons? n) (== (car n) 'new)) #t (loop b))))) bindings))
  (def extend (if (null? extend) () (eval extend env)))
  (if (&& (!null? extend) (extend :hasnew?) (! hasnew?)) (error "new not defined!"))
  (def class (object #t static (if (null? extend) env extend)))
  (class :cnt :hasnew? hasnew? :init  
    (\ args
      (def proxi (newEnv env (newObj class)))
      (def obj ((object #f bindings proxi) :cnt :static class))
      (def new (@remove (.map obj) "new"))
      (if (null? new)
        (if (null? extend)
          (obj :this obj :super env)
          (let1 (super (apply instance (cons extend args) env))
            (.parent proxi super) (obj :this obj :super super) ))
        (if (null? extend)
          (apply new args (obj :cnt :this obj :super env))
          (let1\ (!this #_ (error "this not yet defined!"))
            ;; new invocation (new must invoke (super ...) if (&& (!null? extend) (extend :hasnew?)))
            (apply new args  
              (obj :cnt :this !this :super
                (\ args
                  (def super (apply instance (cons extend args) env))
                  (.parent proxi super) (obj :this obj :super super) )))
            (when (== (obj :this) !this) (error "super not invoked!")) )))
      obj )))

(def\ (is? instance class)
  (if (&& (type? instance Env) (!null? (def static :rhs (@get (.map instance) "static"))))
    (extend? static class)
    (error "is not an instance!") ))

(def\ (is? instance class)
  (if (&& (type? instance Env) (!null? (def static :rhs (value :static (newObj instance)))))
    (extend? static class)
    (error "is not an instance!") ))

(def\ (extend? class superClass)
  (if (null? class) #f (== class superClass) #t (extend? (.parent class) superClass)) )

(def\ (class? class)
  (&& (type? class Env) (@containsKey (.map class) "init")) )

(def\ (class? class)
  (&& (type? class Env) (bound? :init (newObj class))) )

  
(def\ (instance? obj)
  (&& (type? class Env) (@containsKey (.map class) "super")) )

(defMacro (invoke obj method . args)
  (list* (list obj method) args) )

(defMacro (invoke method obj . args)
  (if (class? obj)
    (list* (list obj method) args)
    (list 'let1 (list 'this obj) (list* (list 'this method) args)) ))

(defMacro (invoke method obj . args)
  (def expr (list* (list obj method) args))
  (if (class? obj) expr (list 'let1 (list 'this obj) expr)) )

(def\ (invoke method obj . args)
  (def method (obj method))
  (if (class? obj)
    (apply method args obj)
    (let1 (this obj) (apply method args obj))) )

(def\ (invoke method obj . args)
  (apply (obj method) args obj) )



#;(def\ (invoke method obj . args)
  (def method (obj method)) 
  (unless (class? obj) (.e (.cmb method) obj))
  (apply method args) )

#;(def\ (invoke method obj . args)
  (def method (obj method)) 
  (when (instance? obj) (.e (.cmb method) obj))
  (apply method args) )

#;(def\ (invoke method obj . args)
  (def method (obj method))
  (log method args)
  ;(let1 (xs (.xs (unwrap method))) (|| (null? xs) (!= (car xs) :method)))
  ;(let1 (xs (.xs (unwrap method))) (&& (cons? xs) (== (car xs) :method)))
  ;(!= (.pt (unwrap method)) '(this . args)) 
  ;(== (.pt (unwrap method)) '(this . args)) 
  (if (log (|| (class? obj) (let1 (xs (.xs (unwrap method))) (|| (null? xs) (!= (car xs) :method))) ))
    (apply method args obj)
    (apply method (cons obj args) obj) ))

#;(def\ (invoke method obj . args)
  (def method (obj method))
  (if ((\ (xs) (&& (cons? xs) (== (car xs) :method))) (.xs (unwrap method)))
    (apply method (cons obj args) obj)
    (apply method args obj) ))


;(load "varie/yaos/yaos.lsp")

(begenv
  (def A (class () (a 1)))
  (def B (class A (b 2)))
  (def b (instance B))
  (assert (b :static) B)
  (assert (.parent B) A)
  (assert (is? b A) #t)
  (assert (extend? B A) #t) )

(begenv
  (def A (class () (a 1) (B (class () ((new b) (this :b b)) (b 0) ((f c) (+ a b c))))))
  (def a (instance A))
  (def b (instance (a :B) 2)) 
  (assert (invoke :f b 3) 6)
)
(begenv
  (def A (class () (a 1)))
  (def B (class A (b 2)))
  (def C (class B (c 3) ((f d) (+ a b c d))))
  (def a (instance A))
  (def b (instance B))
  (def c (instance C))
  (assert (invoke :f c 4) 10)
)
(begenv
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) (super 1)) (b 2)))
  (def C (class B ((new) (super)) (c 3) (a 5) ((f d) (+ (super :a) b c d))))
  (def a (instance A 1))
  (def b (instance B))
  (def c (instance C))
  (assert (invoke :f c 4) 10)
)
(begenv
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) #;(super 1)) (b 2)))
  (assert (instance B) Error @getMessage "super not invoked!")
)
(begenv
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) (this) (super 1)) (b 2)))
  (assert (instance B) Error @getMessage "this not yet defined!")
)
(begenv
  (def A (class () (a 1)))
  (def B (class A (static (a 0)) (b 2) ((f c) (+ (super :a) b c)) ((g c) (+ (static :a) b c)) ((h c) (+ a b c))))
  (def b (instance B))
  (assert (invoke :f b 3) 6)
  (assert (invoke :g b 3) 5)
  (assert (invoke :h b 3) 5)
)
(begenv
  (def A (class () (static (a 0)) ((new) (++ a) (++ static a) (++ A a)) ((f) (++ static a) (++ A a))))
  (instance A)
  (def a (instance A))
  (assert (A :a) 6)
  (assert (invoke :f a) 8) 
)
(begenv
  (def A (class () (static (a 1))))
  (def B (class A  (b 2)))
  (def b1 (instance B))
  (def b2 (instance B))
  (b1 :set! :a 3)
  (assert (== (A :a) (b1 :a) (b2 :a)) #t)
)
(begenv
  (def A (class () (static ((a) 1)) ((a) 2)))
  (def B (class A  (static ((b) (a))) ))
  (def b (instance B))
  (assert (invoke :b B) 1)
  (assert (invoke :b b) 1)
)
(begenv
  (def A (class () (static ((a) 1)) ((a) 2)))
  (def B (class A  (static ((b) (a))) ((b) (a)) ))
  (def b (instance B))
  (assert (invoke :b B) 1)
  (assert (invoke :b b) 2)
  (assert ((<- b :static :b)) 1)
)
(begenv
  (def A (class () ((new))))
  (assert (class A (b 1)) Error @getMessage "new not defined!")
)
(begenv
  (def A (class () ((new))))
  (def B (class A ((new) ) (b 1)))
  (assert (instance B) Error @getMessage "super not invoked!")
)
(begenv
  (def A (class () a (b 1) ((f) (list a b))))
  (assert (invoke :f (instance A)) (#inert 1))
)
(begenv
   (def A (class () ((f) a) (a 1)))
   (def a (instance A))
   (def B (class A (a 2)))
   (def b (instance B))
   (assert (invoke :f a) 1) 
   (assert (invoke :f b) 2)
)
(begenv
   (def A (class () ((f) (g)) ((g) 1)))
   (def a (instance A))
   (def B (class A ((g) 2)))
   (def b (instance B))
   (assert (invoke :f a) 1)
   (assert (invoke :f b) 2)
)
(begenv
  (def A (class () (a 0) ((= b) b) ((new a) (this :a (= a)))))
  (def a (instance A 2))
  (assert (a :a) 2)
)

(begenv (def A (class () ((f) (g)) ((g) 1))) (def a (instance A)) (assert (invoke :f a) 1))

