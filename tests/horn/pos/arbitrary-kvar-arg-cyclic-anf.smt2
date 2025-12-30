;; Tag 0: Ret at 26:5: 26:11

(datatype (Adt0 0) ((mkadt0$0 ((fld00 int) (fld01 int)))))
(qualif EqTrue ((a0 bool)) (a0))
(qualif EqFalse ((a0 bool)) ((not a0)))
(qualif EqZero ((a0 int)) ((= a0 0)))
(qualif GtZero ((a0 int)) ((> a0 0)))
(qualif GeZero ((a0 int)) ((>= a0 0)))
(qualif LtZero ((a0 int)) ((< a0 0)))
(qualif LeZero ((a0 int)) ((<= a0 0)))
(qualif Eq ((a0 int) (a1 int)) ((= a0 a1)))
(qualif Gt ((a0 int) (a1 int)) ((> a0 a1)))
(qualif Ge ((a0 int) (a1 int)) ((>= a0 a1)))
(qualif Lt ((a0 int) (a1 int)) ((< a0 a1)))
(qualif Le ((a0 int) (a1 int)) ((<= a0 a1)))
(qualif Le1 ((a0 int) (a1 int)) ((<= a0 (- a1 1))))
(constant gt (func 1 (@(0) @(0) ) bool))
(constant ge (func 1 (@(0) @(0) ) bool))
(constant lt (func 1 (@(0) @(0) ) bool))
(constant le (func 1 (@(0) @(0) ) bool))
(var $k0 (int int int int int)) ;; orig: $k0
(var $k1 (int int int int)) ;; orig: $k0
(var $k2 (int int int)) ;; orig: $k0

(constraint
 (forall ((p0 (Adt0)) (true))
  (forall ((p00 int) ((= p00 (fld00 p0))))
   (forall ((p01 int) ((= p01 (fld01 p0))))
    (and
     (and
      ($k0 0 p00 p01 p00 p01)
      ($k1 p00 p01 p00 p01)
      ($k2 p01 p00 p01))
     (forall ((a0 int) (true))
      (forall ((a1 (Adt0)) (true))
       (forall ((a10 int) ((= a10 (fld00 a1))))
        (forall ((a11 int) ((= a11 (fld01 a1))))
         (forall ((_$ int) (and ($k0 a0 a10 a11 p00 p01) ($k1 a10 a11 p00 p01) ($k2 a11 p00 p01)))
          (and
           (forall ((_$ int) ((not (< a0 10))))
            (tag ((= a10 p00)) "0"))
           (forall ((_$ int) ((< a0 10)))
            (forall  ((a0_plus int) ((= a0_plus (+ a0 1))))
             (forall  ((a11_plus int) ((= a11_plus (+ a11 1))))
              (and
               ($k0 a0_plus a10 a11_plus p00 p01)
               ($k1 a10 a11_plus p00 p01)
               ($k2 a11_plus p00 p01))))))))))))))))