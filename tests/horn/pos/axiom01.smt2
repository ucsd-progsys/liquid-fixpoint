(fixpoint "--eliminate=horn")

(constant M (func 0 (Int Int) Int))

(axiom (forall ((x Int) (y Int) (c Int))
  (! (=> (and (> c 0) (= y (- x c))) (= (M y c) (M x c)))
    :pattern ((M x c) (M y c)))))

(constraint
  (and
    (forall ((c Int) ((> c 0)))
      (forall ((x Int) (true))
        (forall ((y Int) ((= y (- x c))))
          (forall ((z Int) ((= z (- y c))))
            (forall ((a Int) ((= a (M x c))))
              (forall ((b Int) ((= b (M y c))))
                (forall ((d Int) ((= d (M z c))))
                  ((= a d)))))))))))
