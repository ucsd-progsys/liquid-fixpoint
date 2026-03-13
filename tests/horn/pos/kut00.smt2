; (fixpoint "--eliminate=horn")

(qualif Foo ((v Int)) (= v 10))
(qualif Foo ((v Int)) (= v 20))
(qualif Foo ((v Int)) (= v 30))

(var $k1 (Int))

(cut $k1)

(constraint
  (and
    (forall ((x Int) ((= x 5)))
      (forall ((y Int) ((= y x)))
        (forall ((v Int) ((= v (+ x y))))
          ($k1 v))))
    (forall ((z Int) ($k1 z))
      ((< 99 105))) ;; silly constraint to make sure $k1 doesn't get "sliced out"
  )
)
