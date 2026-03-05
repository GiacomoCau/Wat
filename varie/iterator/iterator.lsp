
(defVau (iterate (var val) . forms) env
  (def* (val env) (eval val env) (newEnv env var #inert))
  (def (next? next)
    (caseType val
      (Null (list (\ () #f) (\ ())))
      (List (list (\ () (list? val)) (\ () (prog1 (car val) (set! val (cdr val))))))
      (String (let ((i 0) (l (@length val))) (list (\ () (< i l)) (\ () (@charAt val (++. i))))))
      (Object[] (let ((i 0) (l (.length val))) (list (\ () (< i l)) (\ () (@get Array val (++. i))))))
      (Generator (let1 (b #t) (list (\ () b) (\ () (if (type? (next* val) Generator) (val :value) (set! b :prv #f) val)))))
      (else (error "tipo non previsto")) ))
  (while (next?)
    (apply begin forms (env :cnt var (next))) ))

(def\ (iterator val)
  (caseType val
    (Null (list (\ () #f) (\ ())))
    (List (list (\ () (list? val)) (\ () (prog1 (car val) (set! val (cdr val))))))
    (String (let ((i 0) (l (@length val))) (list (\ () (< i l)) (\ () (@charAt val (++. i))))))
    (Object[] (let ((i 0) (l (.length val))) (list (\ () (< i l)) (\ () (@get Array val (++. i))))))
    (Generator (let1 (b #t) (list (\ () b) (\ () (if (type? (next* val) Generator) (val :value) (set! b :prv #f) val)))))
    (else (error "tipo non previsto")) ))

(defVau (iterate ((#: Symbol var) val) . forms) env
  (def (next? next) (iterator (eval val env)))
  (def env (newEnv env))
  (while (next?)
    (apply begin forms (env :cnt var (next))) ))
  
(defVau (iterate ((#: Symbol var) val) . forms) env
  (def* ((next? next) env) (iterator (eval val env)) (newEnv env))
  (while (next?)
    (apply begin forms (env :cnt var (next))) ))


;(load "varie/iterator/iterator.lsp")

(iterate (a ()) (log a))
(iterate (a "abcd") (log a))
(iterate (a '(a b c d)) (log a))
(iterate (a  (array 'a 'b 'c 'd)) (log a))
(iterate (a  (\** (yield* 'a) (yield* 'b) (yield* 'c) 'd)) (log a))

