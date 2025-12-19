;; Tag 0: Ret at 15:5: 15:11

(datatype (Adt0 0) ((mkadt0$0 ((fld0$0 int) (fld0$1 int)))))
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
(var $k0 (int int int int bool)) ;; orig: $k0
(var $k1 (int int int bool)) ;; orig: $k0

(constraint
 (forall ((reftgen$p$0 (Adt0)) (true))
  (forall ((a0 bool) (true))
   (and
    (forall ((_$ int) ((not a0)))
     (and
      ($k0 (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) a0)
      ($k1 (fld0$1 reftgen$p$0) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) a0)))
    (forall ((_$ int) (a0))
     (and
      ($k0 (fld0$0 reftgen$p$0) (+ (fld0$1 reftgen$p$0) 1) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) true)
      ($k1 (+ (fld0$1 reftgen$p$0) 1) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) true)))
    (forall ((a1 (Adt0)) (true))
     (forall ((_$ int) (and ($k0 (fld0$0 a1) (fld0$1 a1) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) a0) ($k1 (fld0$1 a1) (fld0$0 reftgen$p$0) (fld0$1 reftgen$p$0) a0)))
      (tag ((= (fld0$0 a1) (fld0$0 reftgen$p$0))) "0")))))))

