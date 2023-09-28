// TODO move to actual SMTLIB format
(fixpoint "--eliminate=horn")

(constraint
  (forall ((m1 (Map_t Int Int)) (m1 == (Map_default 0)))
    (and
      (forall ((v Int) (v == (Map_select m1 100)))
        ((v == 0))
      )
    )
  )
)
