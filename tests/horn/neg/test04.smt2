(fixpoint "--eliminate=horn")

(constant c0 (func 0 (int int int ) bool))

(constraint
  (forall ((x Int) ((> x 0)))
    (forall ((y Int) ((> y 0)))
      (forall ((_$ int) ((c0 x y)))
	 ((> 2 4))
      ))))
