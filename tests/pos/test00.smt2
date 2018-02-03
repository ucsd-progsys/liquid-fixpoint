(declare-qualif Zog ((v a)) (<= 10 v))

(declare-relation k0 (Int))

(assert (forall ((v Int)) (=> (= v 10) (k0 v))))
(assert (forall ((v Int)) (=> (= v 20) (k0 v))))
(assert (forall ((v Int)) (=> (k0 v)   (<= 10 v))))
