(constraint
    (forall ((x Int) (true))
        ((let ((y 2))
            (= (* x y) (+ x x))))))
