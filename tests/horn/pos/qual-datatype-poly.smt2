(datatype (B 1) ((mkB ((bval @(0))))))

(qualif Q ((v (B int))) ((>= (bval v) 0)))

(var $k0 ((B int)))

(constraint
 (and
  (forall ((x (B int)) ((>= (bval x) 5)))
   ($k0 x))
  (forall ((y (B int)) ($k0 y))
   (forall ((z (B int)) ((= (bval z) (+ (bval y) 1))))
    ($k0 z)))
  (forall ((y (B int)) ($k0 y))
   ((>= (bval y) 0)))))
