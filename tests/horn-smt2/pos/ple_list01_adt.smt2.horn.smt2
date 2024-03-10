(fixpoint  --rewrite  --save)
 
 
 
(constant len ((func 1 (((Vec  @(0)))) int)))
 
(define len ((l ((Vec  (obj a))))) int ((ite  (is$VNil  l)  0  (+  1  (len  (tail  l))))))
 
(datatype (Vec 1)
 ((VNil ())
  (VCons ((head (@(0)))  (tail ((Vec  @(0))))))))
 
(constraint
   (and
      (forall ((x (int)) (true))
         (forall ((y (int)) ((==  y  2)))
            (forall ((z (int)) ((==  z  3)))
               ((==  (len  ((VCons  x)  ((VCons  y)  ((VCons  z)  VNil))))  3)))))))