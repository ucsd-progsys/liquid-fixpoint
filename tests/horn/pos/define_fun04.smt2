(qualif Q ((v int)) ((aligned v)))

(define_fun aligned ((x int)) bool ((= (mod x 16) 0)))

(var $k0 (int))

(constraint
 (and
  (forall ((x int) ((aligned x)))
   ($k0 x))
  (forall ((y int) ($k0 y))
   (forall ((z int) ((= z (+ y 16))))
    ($k0 z)))
  (forall ((y int) ($k0 y))
   ((aligned y)))))
