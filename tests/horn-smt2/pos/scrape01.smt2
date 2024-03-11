(fixpoint  "--scrape=head")
 
(qualif Le ((a0 (int)) (a1 (int))) ((<=  a0  a1)))
 
(var $k0 ((int) (int) (int)))
(var $k1 ((int) (int) (int) (int)))
 
 
 
(datatype (Pair 2)
 ((Pair ((fst @(0))  (snd @(1))))))
(datatype (Unit 0)
 ((Unit ())))
 
 
(constraint
   (and
      (forall ((a0 int) (true))
         (forall ((a1 int) (true))
            (forall ((_ Unit) ((>=  a0  0)))
               (forall ((_ Unit) ((<=  a0  a1)))
                  (forall ((_ Unit) ((>=  a1  0)))
                     (and
                        (forall ((a2 int) ((==  a2  0)))
                           (and
                              (($k0  a0  a0  a1))
                              (($k1  a2  a0  a0  a1))))
                        (forall ((a3 int) (true))
                           (forall ((a4 int) (true))
                              (forall ((_ Unit) ((and  ($k0  a4  a0  a1)  ($k1  a3  a4  a0  a1))))
                                 (and
                                    (forall ((_ Unit) ((not  (<  a4  a1))))
                                       ((==  a3  (-  a1  a0))))
                                    (forall ((_ Unit) ((<  a4  a1)))
                                       (forall ((a5 int) ((==  a5  (+  a3  1))))
                                          (forall ((a6 int) ((==  a6  (+  a4  1))))
                                             (and
                                                (($k0  a6  a0  a1))
                                                (($k1  a5  a6  a0  a1))))))))))))))))))