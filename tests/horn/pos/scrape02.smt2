(fixpoint "--scrape=both")

(datatype (Adt0 0) ((mkadt0$0 ()) (mkadt0$1 ())))
(datatype (Adt1 0) ((mkadt1$0 ((fld1$0 int) (fld1$1 int)))))

(constant f$get_mode$0 (func 0 ((BitVec Size32) int ) (Adt0)))  ;; flux def: FluxId { parent: DefId(0:230 ~ flux_demo[a644]::typestate_addr), name: "get_mode" }

(var $k0 (int (BitVec Size32) int)) ;; orig: $k5
(var $k1 (int int int (BitVec Size32) int)) ;; orig: $k0
(var $k2 (int int (BitVec Size32) int)) ;; orig: $k0
(var $k3 (int (BitVec Size32) int)) ;; orig: $k0
(var $k4 (int int int int (BitVec Size32) int)) ;; orig: $k2
(var $k5 (int (BitVec Size32) int int int int)) ;; orig: $k6

(constraint
 (forall ((reftgen$modes$0 (BitVec Size32)) (true))
  (forall ((a0 int) (true))
   (forall ((_$ int) (true))
    (forall ((_$ int) (true))
     (and
      (forall ((a1 int) (true))
       (forall ((_$ int) (((= (f$get_mode$0 reftgen$modes$0 a1) (mkadt0$0 )))))
         ($k0 a1 reftgen$modes$0 a0)))
      (forall ((a4 int) (true))
       (forall ((_$ int) ($k0 a4 reftgen$modes$0 a0))
        (forall ((a5 int) (true))
         (forall ((a6 int) (true))
          ($k4 a4 a5 a6 a0 reftgen$modes$0 a0)))))
      (forall ((a7 int) (true))
       (forall ((a8 (Adt1)) (true))
        (forall ((a9 int) (true))
         (forall ((a10 int) (true))
          (forall ((_$ int) (and ($k1 a7 a9 a10 reftgen$modes$0 a0) ($k2 a9 a10 reftgen$modes$0 a0) ($k3 a10 reftgen$modes$0 a0)))
           (and
            (forall ((a11 int) (true))
             (forall ((a12 int) (true))
              (forall ((a13 int) (true))
               (forall ((_$ int) ($k4 a11 a7 a12 a13 reftgen$modes$0 a0))
                (forall ((a14 int) (true))
                 (forall ((a15 int) (true))
                  ($k5 a11 reftgen$modes$0 a0 a7 a14 a15)))))))
            (forall ((a16 (Adt1)) (true))
               (forall ((a17 int) (true))
                (forall ((a18 int) (true))
                 (forall ((a19 int) (true))
                  (forall ((_$ int) ($k5 a17 reftgen$modes$0 a0 a7 a18 a19))
                    (and
                     (tag ((= (f$get_mode$0 reftgen$modes$0 a17) (mkadt0$0 ))) "1")
                     (forall ((a20 bool) (true))
                       (and
                        (forall ((a21 int) (true))
                         (forall ((a22 int) (true))
                          (forall ((a23 int) (true))
                           (and
                            ($k1 a21 a22 a23 reftgen$modes$0 a0)
                            ($k2 a22 a23 reftgen$modes$0 a0)
                            ($k3 a23 reftgen$modes$0 a0)))))
                        (forall ((a24 int) (true))
                         (forall ((a25 int) (true))
                          (forall ((a26 int) (true))
                           (forall ((_$ int) ($k5 a24 reftgen$modes$0 a0 a7 a25 a26))
                            (forall ((a27 int) (true))
                             (forall ((a28 int) (true))
                              (forall ((a29 int) (true))
                               ($k4 a24 a27 a28 a29 reftgen$modes$0 a0))))))))))
                               ))))))))))))))))))
