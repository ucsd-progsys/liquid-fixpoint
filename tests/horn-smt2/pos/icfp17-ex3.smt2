(fixpoint  "--eliminate=horn")
 
 
(var $ka ((int)))
(var $kb ((int)))
(var $kc ((int)))
 
 
 
 
 
(constraint
   (and
      (and
         (forall ((a int) (($ka  a)))
            (forall ((v int) ((==  v  (-  a  1))))
               (($kb  v))))
         (forall ((b int) (($kb  b)))
            (forall ((v int) ((==  v  (+  b  1))))
               (($kc  v))))
         (forall ((v int) ((>=  v  0)))
            (($ka  v)))
         (forall ((v int) (($kc  v)))
            ((>=  v  0))))))