 
 
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>  x  0)))
         (forall ((y (int)) ((>  y  x)))
            (forall ((b (bool)) ((not  (<=>  b  (or  (<=  x  0)  (<=  y  0))))))
               (b))))
      (forall ((x (int)) ((>  x  0)))
         (forall ((y (int)) ((>  y  x)))
            (forall ((b (bool)) ((not  (<=>  b  (or  (<=  x  0)  (<=  y  0))))))
               (b))))))