 
 
 
 
 
 
 
 
(constraint
  (and
    (forall ((a0 Int) (true))
      (and
        (forall ((a1 Int) (true))
          (forall ((_ Int) ((not (not (> a1 0)))))
            ((<=> (> a1 0) true))))
        ((<=> (= (+ a1 1) (+ a1 1)) true))))))