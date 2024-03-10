 
(var $kx ((int)))
(var $ky ((int)))
 
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>=  x  0)))
         (and
            (forall ((n (int)) ((==  n  (-  x  1))))
               (forall ((p (int)) ((==  p  (+  x  1))))
                  (and
                     (forall ((v (int)) ((==  v  n)))
                        (($kx  v)))
                     (forall ((v (int)) ((==  v  p)))
                        (($ky  v)))
                     (forall ((v (int)) (($kx  p)))
                        (($ky  v))))))
            (forall ((y (int)) (($ky  y)))
               (forall ((v (int)) ((==  v  (+  y  1))))
                  ((>=  v  0))))))))