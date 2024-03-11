// TODO move to actual SMTLIB format
(fixpoint "--eliminate=horn")

(constraint
  (forall ((x Str) (x == "cat"))
    (forall ((y Str) (y == "dog"))
      (and ((x = "cat"))
           ((y = "dog"))
      )
    )
  )
)
