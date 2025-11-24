
(def\ (<- env . keys)
  (if (null? keys) env
    (apply** <- (env (car keys)) (cdr keys)))) 

(def\ (instance cls . args) (apply (cls :init) args))

(defVau (class extend . bindings) env
  (def\ (object bindings super)
    (def this (newEnv super))
    (forEach (\ ((name . #ignore)) (this name #inert)) bindings)
    (forEach (\ ((name . forms))
      (if (cons? name)
        (this (car name) (eval (list* '\ (cdr name) forms) this)) ;; method definition
        (this name (eval (cons 'begin forms) this)) )) ;; attribute definition
      bindings )
    this )
  (def static (if (!= (caar bindings) 'static) () (prog1 (cdar bindings) (def bindings (cdr bindings)))))
  (def class (object static (if (null? extend) env (eval extend env))))
  (class :cnt :init  
   (\ args
     (def proxi (newEnv env (newObj class)))
     (def obj ((object bindings proxi) :cnt :static class))
     (def new (@remove (.map obj) "new"))
     (if (null? new)
       (let1 (super (if (null? extend) env (apply instance (cons (eval extend env)) env)))
         (.parent proxi super) (obj :this obj :super super) )
       (let1\ (noThis args (error "this not yet defined!"))
         (if (null? extend)
           (obj :this obj :super env)
           (obj :this noThis :super
             (\ args
               (def super (apply instance (cons (eval extend env) args) env))
               (.parent proxi super) (obj :this obj :super super) )))
         (apply new args obj) ;; new invocation (new must invoke (super ...) if (!null? extend))
         (when (== (obj :this) noThis) (error "super not invoked!")) ))
     obj )))

(prEnv 0)

(begenv
  (def A (class () (a 1) (B (class () ((new b) (this :b b)) (b 0) ((f c) (+ a b c))))))
  (def objA (instance A))
  (def objB (instance (objA :B) 2)) 
  (assert ((objB :f) 3) 6)
)
(begenv  
  (def A (class () (a 1)))
  (def B (class A (b 2)))
  (def C (class B (c 3) ((f d) (+ a b c d))))
  (def objA (instance A))
  (def objB (instance B))
  (def objC (instance C))
  (assert ((objC :f) 4) 10)
)
(begenv  
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) (super 1)) (b 2)))
  (def C (class B (c 3) (a 5) ((f d) (+ (super :a) b c d))))
  (def objA (instance A 1))
  (def objB (instance B))
  (def objC (instance C))
  (assert ((objC :f) 4) 10)
)
(begenv  
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) #;(super 1)) (b 2)))
  (assert (instance B)) ;; super not invoked!
)
(begenv  
  (def A (class () ((new a) (this :a a))))
  (def B (class A ((new) (this) (super 1)) (b 2)))
  (assert (instance B)) ;; this not yet defined!
)
(begenv  
  (def A (class () (a 1)))
  (def B (class A (static (a 0)) (b 2) ((f c) (+ (super :a) b c)) ((g c) (+ (static :a) b c)) ((h c) (+ a b c))))
  (def objB (instance B))
  (assert ((objB :f) 3) 6)
  (assert ((objB :g) 3) 5)
  (assert ((objB :h) 3) 5)
)
(begenv
  (def A (class () (static (a 0)) ((new) (++ a) (++ static a) (++ A a)) ((f) (++ static a) (++ A a))))
  (instance A)
  (def objA (instance A))
  (assert (A :a) 6)
  (assert ((objA :f)) 8) 
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
  (assert ((B :b)) 1)
  (assert ((b :b)) 1)
)
(begenv
  (def A (class () (static ((a) 1)) ((a) 2)))
  (def B (class A  (static ((b) (a))) ((b) (a)) ))
  (def b (instance B))
  (assert ((B :b)) 1)
  (assert ((b :b)) 2)
  (assert ((<- b :static :b)) 1)
)


