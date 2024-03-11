(fixpoint "--rewrite")
 
 
 
 
(constant adder (func 0 (Int Int) Int))
 
(define adder ((x Int) (y Int)) Int (+ x y))
 
 
 
(constraint
  (and
    (forall ((x Int) ((= x 5)))
      (forall ((y Int) ((= y 6)))
        ((= ((adder x) y) 12))))))