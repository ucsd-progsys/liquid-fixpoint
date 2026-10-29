(fixpoint "--eliminate=horn")

(constant f_add (func 0 (Int Int) Int))

(axiom (forall ((x Int) (y Int))
  (! (=> (and (> x 0) (> y 0)) (= (f_add x y) (+ x y)))
    :pattern ((f_add x y)))))

(constraint
  (and
    (forall ((x Int) ((> x 0)))
      (forall ((y Int) ((> y x)))
        (forall ((v Int) ((= v (f_add x y))))
          ((> v 0)))))))