(constant magic (func 0 (int int ) bool))
(define_fun c0 ((a0 int)) bool ((and (and (and (magic 0 a0) (magic 1 a0)) (magic 2 a0)) (magic 3 a0))))
(define_fun c1 ((a1 int)) bool ((or (or (or (= 0 a1) (= 1 a1)) (= 2 a1)) (= 3 a1))))

(constraint
 (forall ((n0 int) (true))
  (forall ((_$ int) ((and (and (and (magic 0 n0) (magic 1 n0)) (magic 2 n0)) (magic 3 n0))))
   (tag ((magic 3 n0)) "0"))))
