(fixpoint "--allowhoqs")

;; test for wild-literal pattern in qualifiers,
;; where `a1` will be instantiated with EVERY
;; `Int` literal seen in the constraint.

(qualif Wild1 ((a0 Int) (a1# Int)) (= a0 a1))

(var $k0 (Int))

(cut $k0)

(constraint
  (and
    ($k0 5)
    (forall ((x Int) ($k0 x))
      ((= x 5))
    )
  )
)
