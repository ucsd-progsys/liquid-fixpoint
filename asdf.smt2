(var $k0 (Int))
(var $k1 (Int))
(var $k2 (Int))
(var $k3 (Int))
(var $k4 (Int))
(var $k5 (Int))
(var $k6 (Int))
(var $k7 (Int))
(var $k8 (Int))
(var $k9 (Int))
(var $k10 (Int))
(var $k11 (Int))
(var $k12 (Int))
(var $k13 (Int))
(var $k14 (Int))
(var $k15 (Int))
(var $k16 (Int Int Int))
(var $k17 (Int Int Int))
(var $k18 (Int Int Int))
(var $k19 (Int Int Int))
(var $k20 (Int Int Int))
(var $k21 (Int Int Int))
(var $k22 (Int Int Int))
(var $k23 (Int Int Int Int))
(var $k24 (Int Int Int Int))
(var $k25 (Int Int Int Int))
(var $k26 (Int Int Int Int))
(var $k27 (Int Int Int Int))
(var $k28 (Int Int Int Int))
(var $k29 (Int Int Int Int))

(constraint (and
 (forall ((x##4 int) ($k0 x##4))
  (and
   ((true))
   (forall ((anf##0 int) (anf##0 == 1))
    (and
     ((true))
     (forall ((VV##3 int) (VV##3 == x##4 + anf##0))
      (($k4 VV##3 x##4)))))))
 (and
  (and
   (and
    ((true))
    (forall ((anf##1 int) (anf##1 == 7))
     (and
      (and
       ((true))
       (forall ((VV##7 int) (VV##7 == anf##1))
        ((true))))
      (forall ((v##3 int) (v##3 == anf##1 + 1))
       (($k8 v##3))))))
   (forall ((moo##5 int) (moo##5 == 8))
    (and
     (and
      (forall ((x##12 @(8)) (true))
       ((true)))
      (and
       (and
        (and
         ((true))
         (forall ((anf##3 int) (anf##3 == 7))
          (and
           (and
            ((true))
            (and
             (and
              (and
               ((true))
               (forall ((VV##15 int) (VV##15 == anf##3))
                ((and
                  ($k13 VV##15 anf##3 moo##5)
                  (true)))))
              (forall ((anf##4 int) (and ($k13 anf##4 anf##3 moo##5) (true)))
               (and
                (and
                 ((true))
                 (forall ((VV##14 int) (and ($k13 VV##14 anf##3 moo##5) (true)))
                  ((true))))
                (forall ((v##3 int) (v##3 == anf##4 + 1))
                 (($k16 v##3 anf##3 moo##5))))))
             (forall ((VV##17 int) ($k16 VV##17 anf##3 moo##5))
              (($k18 VV##17 anf##3 moo##5)))))
           (forall ((VV##19 int) ($k18 VV##19 anf##3 moo##5))
            (($k20 VV##19 moo##5))))))
        (forall ((bar##13 int) (bar##13 == 8))
         (and
          ((true))
          (forall ((VV##22 Unit) (true))
           (($k23 VV##22 moo##5)))))
        (forall ((VV##21 int) ($k20 VV##21 moo##5))
         (((VV##21 == 8)))))
       (forall ((VV##24 Unit) ($k23 VV##24 moo##5))
        (($k25 VV##24 moo##5))))
      (and
       (forall ((v##10 @(8)) (true))
        ((true)))
       (forall ((fresh0##9 @(8)) (true))
        (forall ((VV##11 @(8)) (VV##11 == fresh0##9))
         ((true))))))
     (forall ((VV##26 Unit) ($k25 VV##26 moo##5))
      (($k27 VV##26)))))
   (forall ((VV##9 int) ($k8 VV##9))
    (((VV##9 == 8)))))
  (forall ((VV##28 Unit) ($k27 VV##28))
   (($k29 VV##28))))
 (and
  (forall ((fresh0##2 int) (true))
   (($k0 fresh0##2)))
  (forall ((x##1 int) (true))
   (forall ((VV##5 int) ($k4 VV##5 x##1))
    (((VV##5 == x##1 + 1))))))))
