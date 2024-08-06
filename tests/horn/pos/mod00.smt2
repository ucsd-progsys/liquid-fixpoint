// Tag 0: Ret at 15:5: 15:15

(qualif EqZero ((v int)) ((= v 0)))
(qualif GtZero ((v int)) ((> v 0)))
(qualif GeZero ((v int)) ((>= v 0)))
(qualif LtZero ((v int)) ((< v 0)))
(qualif LeZero ((v int)) ((<= v 0)))
(qualif Eq ((a int) (b int)) ((= a b)))
(qualif Gt ((a int) (b int)) ((> a b)))
(qualif Ge ((a int) (b int)) ((>= a b)))
(qualif Lt ((a int) (b int)) ((< a b)))
(qualif Le ((a int) (b int)) ((<= a b)))
(qualif Le1 ((a int) (b int)) ((<= a ((- b 1)))))
(constant gt (func 1 (@(0) @(0) ) bool))
(constant ge (func 1 (@(0) @(0) ) bool))
(constant lt (func 1 (@(0) @(0) ) bool))
(constant le (func 1 (@(0) @(0) ) bool))
(var $k0 (int)) // orig: $k0

(constraint
  (and
    (and
      (forall ((a0 int) ((= a0 4)))
        ($k0 a0)
      )
      (forall ((a1 int) ((= a1 10)))
        ($k0 a1)
      )
    )
    (forall ((a2 int) (true))
      (forall ((_ int) (and ($k0 a2) ((>= a2 4)) ((>= a2 10))))
        (tag ((= ((mod a2 2)) 0)) "0")
      )
    )
  )
)

