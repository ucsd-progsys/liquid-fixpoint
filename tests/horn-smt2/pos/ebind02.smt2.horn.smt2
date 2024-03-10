 
(var $k ((int)))
 
 
 
 
(constraint
   (and
      (forall ((m (int)) (true))
         (forall ((z (int)) ((==  z  (-  m  1))))
            (and
               (forall ((v1 (int)) ((==  v1  (+  z  2))))
                  (($k  v1)))
               (exists ((x1 (int)) (true))
                  (and
                     (forall ((v2 (int)) (($k  v2)))
                        ((==  v2  x1)))
                     (forall ((v3 (int)) ((==  v3  (+  x1  1))))
                        ((==  v3  (+  m  2)))))))))))