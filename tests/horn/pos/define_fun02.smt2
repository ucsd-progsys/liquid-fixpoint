(define_fun c0 () (Set_Set int) ((Set_empty 0)))
(define_fun c1 ((a2 int) (a3 (Set_Set int))) (Set_Set int) ((Set_cup (Set_sng a2) a3)))
(define_fun c2 ((a4 (Set_Set int))) bool ((= a4 (Set_empty 0))))

(constraint
 (forall ((xs (Set_Set int)) (true))
  (and
   (forall ((_$ int) ((= xs (Set_empty 0))))
    (tag ((= true (= xs (Set_empty 0)))) "0"))
   (forall ((a0 int) (true))
    (forall ((a1 (Set_Set int)) (true))
     (forall ((_$ int) ((= xs (Set_cup (Set_sng a0) a1))))
      (tag ((= false (= xs (Set_empty 0)))) "1")))))))