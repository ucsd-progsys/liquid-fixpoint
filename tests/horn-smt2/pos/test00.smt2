(fixpoint  "--eliminate=horn")

(qualif Foo ((v int) (x int)) (=  v  x))
(qualif Bar ((v int) (x int)) (>  v  x))

(var $k1 (int int int))
(var $k2 (int int int))
(var $k3 (int int int))


(constraint
   (and
      (forall ((x Int) ((>  x  0)))
         (forall ((y Int) ((>  y  x)))
            (forall ((v Int) ((=  v  (+  x  y))))
               ((>  v  0)))))))