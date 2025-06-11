(def defMacro
  #|Defines into the current environment the named macro <b>name</b> with the given <b>parameterTree</b> and <b>forms</b> as body.
   |
   |($nm name parameterTree . forms)
   |($nm (name parameterTree) . forms)
   |(type macro)
   |#
  (macro (lhs . rhs)
    (if (cons? lhs)
      (list 'def (car lhs) (list* 'macro (cdr lhs) rhs))
      (list 'def lhs (cons 'macro rhs)) )))
