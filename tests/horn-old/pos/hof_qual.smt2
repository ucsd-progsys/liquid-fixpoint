(fixpoint "--allowho")
(fixpoint "--allowhoqs")

(qualif MyQual ((v @(0)) (p @(1))) (p v))

(var $k0 ((int) ((func(0, [int;bool])))))

(constraint
  (forall ((p0 (func(0, [int;bool]))) (true))
    (and
      (forall ((x int) (p0 x))
        (($k0 x p0)))
      (forall ((y int) ($k0 y p0))
        (forall ((v int) ((v = y)))
         (($k0 v p0))
        )
      )
      (forall ((z int) ($k0 z p0))
        (tag (p0 z) "h")
      )
    )
  )
)
