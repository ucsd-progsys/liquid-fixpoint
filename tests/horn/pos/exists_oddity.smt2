(var $k0 (int)) ;; orig: $k0

(constraint
  (and
    (forall ((a0 int) ((= a0 0)))
      ($k0 a0))
    (forall ((a1 int) (true))
      (forall ((_$ int) ($k0 a1))
        (tag ((= a1 0)) "0")))
    (forall ((a2 int) (true))
      (forall ((_$ int) ((= a2 0)))
        ($k0 a2)))))
