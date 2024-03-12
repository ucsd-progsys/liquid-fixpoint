(constraint
  (forall ((x int) (x > 0))
    (forall ((y int) (y > x))
      (forall ((b bool) (b /= (x <= 0 || y <= 0)))
         ((b))))))

(constraint
  (forall ((x int) (x > 0))
    (forall ((y int) (y > x))
      (forall ((b bool) (b != (x <= 0 || y <= 0)))
         ((b))))))
