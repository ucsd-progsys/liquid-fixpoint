;; Tag 0: Ret at 57:1: 57:2 (ESpan { span: tests/tests/pos/enums/list01.rs:51:48: 51:79 (#0), base: None })

(var $k0 ((Set_Set int) (Set_Set int) (Set_Set int))) ;; orig: $k0
(var $k1 ((Set_Set int) (Set_Set int) (Set_Set int) int (Set_Set int))) ;; orig: $k1

(constraint
 (forall ((xs1 (Set_Set int)) (true))
  (forall ((xs2 (Set_Set int)) (true))
   (and
    (forall ((_$ int) ((= xs1 (Set_empty 0))))
     ($k0 xs2 xs1 xs2))
    (forall ((a0 int) (true))
     (forall ((a1 (Set_Set int)) (true))
      (forall ((_$ int) ((= xs1 (Set_cup (Set_sng a0) a1))))
       (and
        ($k1 (Set_cup a1 xs2) xs1 xs2 a0 a1)
        (forall ((a2 (Set_Set int)) (true))
         (forall ((_$ int) ($k1 a2 xs1 xs2 a0 a1))
          ($k0 (Set_cup (Set_sng a0) a2) xs1 xs2)))))))
    (forall ((a3 (Set_Set int)) (true))
     (forall ((_$ int) ($k0 a3 xs1 xs2))
      (tag ((= a3 (Set_cup xs1 xs2))) "0")))))))
