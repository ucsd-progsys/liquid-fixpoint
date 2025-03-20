(define_fun max ((a Int) (b Int)) Int
  (if (>= a b) a b))

(define_fun mmmax ((a Int) (b Int)) Int
  (max a b))


(constraint
  (and
    (forall ((x Int) ((= x 5)))
      (forall ((y Int) ((= y 6)))
        ((= ((mmmax x) y) 62))))))
