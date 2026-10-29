(datatype (P 0) ((mkP ((pfst int)))))

(qualif Q ((v P)) ((>= (pfst v) 0)))

(var $k0 (P))

(constraint
 (and
  (forall ((x P) ((>= (pfst x) 5)))
   ($k0 x))
  (forall ((y P) ($k0 y))
   (forall ((z P) ((= (pfst z) (+ (pfst y) 1))))
    ($k0 z)))
  (forall ((y P) ($k0 y))
   ((>= (pfst y) 0)))))
