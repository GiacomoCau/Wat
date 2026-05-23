
(defClass Iterator ()
  (next? next) )

(def\ (iterator val)
  (caseType val
    (Null (new Iterator :next? (\ () #f) :next (\ ())))
    (List (new Iterator :next? (\ () (list? val)) :next (\ () (prog1 (car val) (set! val (cdr val))))))
    (String (let ((i 0) (l (@length val))) (new Iterator :next? (\ () (< i l)) :next (\ () (@charAt val (++. i))))))
    (Object[] (let ((i 0) (l (.length val))) (new Iterator :next? (\ () (< i l)) :next (\ () (@get Array val (++. i))))))
    (Generator (let1 (b #t) (new Iterator :next? (\ () b) :next (\ () (if (type? (next* val) Generator) (val :value) (set! b :prv #f) val)))))
    (Iterator val)
    (else (error "tipo non iterabile")) ))

(defVau (iterate ((#: Symbol var) val) . forms) env
  (def* ((#: Iterator next? next) env) (iterator (eval val env)) (newEnv env))
  (while (next?)
    (apply begin forms (env :cnt var (next))) ))


;(load "varie/iterator/iterator.lsp")

(iterate (a ()) (log a))
(iterate (a "abcd") (log a))
(iterate (a '(a b c d)) (log a))
(iterate (a  (array 'a 'b 'c 'd)) (log a))
(iterate (a  (\** (yield* 'a) (yield* 'b) (yield* 'c) 'd)) (log a))

