(declare-qualif Zog ((v a)) (<= 10 v))

(declare-fun k0 (Int) Bool)

(assert (forall ((v Int)) (=> (= v 10) (k0 v))))
(assert (forall ((v Int)) (=> (= v 20) (k0 v))))
(assert (forall ((v Int)) (=> (k0 v)   (<= 10 v))))


