(fixpoint "--allowho")
(fixpoint "--allowhoqs")
 
 
(qualif MyQual ((v @(0)) (p @(1))) (p v))
 
(var $k0 (Int (func 0 (Int) bool)))
 
 
 
 
 
(constraint
  (and
    (forall ((p0 (func 0 (Int) bool)) (true))
      (and
        (forall ((x Int) ((p0 x)))
          ($k0 x p0))
        (forall ((y Int) ($k0 y p0))
          (forall ((v Int) ((= v y)))
            ($k0 v p0)))
        (forall ((z Int) ($k0 z p0))
          ((p0 z)))))))