(fixpoint "--rewrite")
(fixpoint "--save")
 
 
 
 
(constant len (func 1 ((Vec @(0))) Int))
 
(define len ((l (Vec a))) Int (if
                               (is$VNil l)
                               0
                               (+ 1 (len (tail l)))))
 
(datatype (Vec 1)
 ((VNil ())
  (VCons ((head @(0)) (tail (Vec @(0)))))))
 
 
(constraint
  (and
    (and
      (forall ((x Int) (true))
        (forall ((y Int) ((= y 2)))
          (forall ((z Int) ((= z 3)))
            ((= (len ((VCons x) ((VCons y) ((VCons z) VNil)))) 3))))))))