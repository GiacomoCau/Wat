#|
(%fun (%\ (a) (%$ "_" a)))
((%fun (%\ (a) (%$ "_" a))) 2)
((%fun (%\ (a) (%+ 2 a))) 2)
((%fun (%\ (a) (%+ 2 a))) (%+ 1 2))
(@stream &java.util.Arrays (%list->array (1 2 3)))

(@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (%\ (a) (%$ "_" a))))

(@toArray (@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (%\ (a) (%$ "_" a)))))
(@toArray (@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (%\ (a) (%* 2 a)))))

(%fun (a) (%$ "_" a))
((%fun (a) (%$ "_" a)) 2)
((%fun (a) (%+ 2 a)) 2)
((%fun (a) (%+ 2 a)) (%+ 1 2))

(@stream &java.util.Arrays (%list->array (1 2 3)))
(@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (a) (%$ "_" a)))
(@toArray (@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (a) (%$ "_" a))))
(@toArray (@map (@stream &java.util.Arrays (%list->array (1 2 3))) (%fun (a) (%* 2 a))))
|#

(%fun (a) ($ "_" a))
((%fun (a) ($ "_" a)) 2)
((%fun (a) (+ 2 a)) 2)
((%fun (a) (+ 2 a)) (%+ 1 2))

(@stream &java.util.Arrays (array 1 2 3))
(@map (@stream &java.util.Arrays (array 1 2 3)) (%fun (a) ($ "_" a)))
(@toArray (@map (@stream &java.util.Arrays (array 1 2 3)) (%fun (a) ($ "_" a))))
(@toArray (@map (@stream &java.util.Arrays (array 1 2 3)) (%fun (a) (* 2 a))))

(def obj (@new &java.util.HashMap)) (@put obj "a" 1) (@put obj "b" 2) obj
(@forEach obj (%bicons (a b) (print a ":" b)))
(@forEach (.map (newObj :a 1 :b 2)) (%bicons (a b) (print a ":" b)))
(@forEach (@of &java.util.Map "a" 1 "b" 2) (%bicons (a b) (print a ":" b))) 
