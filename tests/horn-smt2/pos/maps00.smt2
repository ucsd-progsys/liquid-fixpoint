(fixpoint "--eliminate=horn")
 
 
 
 
 
 
 
 
(constraint
  (and
    (forall ((m1 (Map_t Int Int)) ((= m1 (Map_default 0))))
      (and
        (forall ((v Int) ((= v ((Map_select m1) 100))))
          ((= v 0)))
        (forall ((m2 (Map_t Int Int)) ((=
                                        m2
                                        (((Map_store (((Map_store m1) 10) 1)) 20) 2))))
          (and
            (forall ((v Int) ((= v ((Map_select m2) 10))))
              ((= v 1)))
            (forall ((v Int) ((= v ((Map_select m2) 20))))
              ((= v 2)))
            (forall ((v Int) ((= v ((Map_select m2) 30))))
              ((= v 0)))))))))