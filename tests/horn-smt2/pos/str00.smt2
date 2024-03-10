(fixpoint  "--eliminate=horn")
 
 
 
 
 
 
(constraint
   (and
      (forall ((x (Str)) ((==  x  "cat")))
         (forall ((y (Str)) ((==  y  "dog")))
            (and
               ((==  x  "cat"))
               ((==  y  "dog")))))))