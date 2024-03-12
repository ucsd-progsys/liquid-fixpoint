(fixpoint "--scrape=head")
 
 
(qualif Le ((a0 Int) (a1 Int)) (<= a0 a1))
 
(var $k0 (Int Int Int))
(var $k1 (Int Int Int Int))
 
 
 
(datatype (Pair 2)
 ((Pair ((fst @(0)) (snd @(1))))))
(datatype (Unit 0)
 ((Unit ())))
 
 
(constraint
  (and
    (forall ((a0 Int) (true))
      (forall ((a1 Int) (true))
        (forall ((_ Unit) ((>= a0 0)))
          (forall ((_ Unit) ((<= a0 a1)))
            (forall ((_ Unit) ((>= a1 0)))
              (and
                (forall ((a2 Int) ((= a2 0)))
                  (and
                    ($k0 a0 a0 a1)
                    ($k1 a2 a0 a0 a1)))
                (forall ((a3 Int) (true))
                  (forall ((a4 Int) (true))
                    (forall ((_ Unit) (and ($k0 a4 a0 a1) ($k1 a3 a4 a0 a1)))
                      (and
                        (forall ((_ Unit) ((not (< a4 a1))))
                          ((= a3 (- a1 a0))))
                        (forall ((_ Unit) ((< a4 a1)))
                          (forall ((a5 Int) ((= a5 (+ a3 1))))
                            (forall ((a6 Int) ((= a6 (+ a4 1))))
                              (and
                                ($k0 a6 a0 a1)
                                ($k1 a5 a6 a0 a1)))))))))))))))))