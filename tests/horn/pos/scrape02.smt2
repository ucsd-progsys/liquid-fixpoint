(fixpoint "--scrape=both")

;; Tag 0: Call at 253:9: 253:33 (ESpan { span: /Users/rjhala/research/flux/lib/flux-rs/src/lib.rs:7:15: 7:19 (#0), base: None })
;; Tag 1: Call at 254:18: 254:33 (ESpan { span: src/typestate_addr.rs:143:39: 143:74 (#0), base: None })
;; Tag 2: Call at 254:18: 254:33 (ESpan { span: src/typestate_addr.rs:83:26: 83:32 (#0), base: None })
;; Tag 3: Call at 254:18: 254:33 (ESpan { span: src/typestate_addr.rs:83:36: 83:42 (#0), base: None })

(datatype (Adt0 0) ((mkadt0$0 ()) (mkadt0$1 ())))
(datatype (Adt1 0) ((mkadt1$0 ((fld1$0 int) (fld1$1 int)))))

; (qualif ModeQual0 ((modes (BitVec Size32)) (pin int))
;    ((= (f$get_mode$0 modes pin) (mkadt0$0 ))))

; (qualif ModeQual1 ((pin int) (modes (BitVec Size32)))
;    ((= (f$get_mode$0 modes pin) (mkadt0$0 ))))

(qualif MyQ1 ((a30 int) (a31 int) (a32 int)) ((= a30 (+ a31 a32))))
(qualif MyQ2 ((a33 int) (a34 int) (a35 int)) ((= a33 (- a34 a35))))
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
(constant f$get_mode$0 (func 0 ((BitVec Size32) int ) (Adt0)))  ;; flux def: FluxId { parent: DefId(0:230 ~ flux_demo[a644]::typestate_addr), name: "get_mode" }
(constant gt (func 1 (@(0) @(0) ) bool))
(constant ge (func 1 (@(0) @(0) ) bool))
(constant lt (func 1 (@(0) @(0) ) bool))
(constant le (func 1 (@(0) @(0) ) bool))
(var $k0 (int (BitVec Size32) int)) ;; orig: $k5
(var $k1 (int int int (BitVec Size32) int)) ;; orig: $k0
(var $k2 (int int (BitVec Size32) int)) ;; orig: $k0
(var $k3 (int (BitVec Size32) int)) ;; orig: $k0
(var $k4 (int int int int (BitVec Size32) int)) ;; orig: $k2
(var $k5 (int (BitVec Size32) int int int int)) ;; orig: $k6

(constraint
 (forall ((reftgen$modes$0 (BitVec Size32)) (true))
  (forall ((a0 int) (true))
   (forall ((_$ int) ((>= a0 0)))
    (forall ((_$ int) (true))
     (and
      (forall ((a1 int) (true))
       (forall ((_$ int) (and ((> a1 3)) ((= (f$get_mode$0 reftgen$modes$0 a1) (mkadt0$0 )))))
        (forall ((_$ int) (and ((<= 0 a1)) ((< a1 32))))
         ($k0 a1 reftgen$modes$0 a0))))
      (forall ((a2 int) ((= a2 0)))
       (forall ((a3 int) ((= a3 0)))
        (and
         ($k1 a2 a3 a0 reftgen$modes$0 a0)
         ($k2 a3 a0 reftgen$modes$0 a0)
         ($k3 a0 reftgen$modes$0 a0))))
      (forall ((a4 int) (true))
       (forall ((_$ int) ($k0 a4 reftgen$modes$0 a0))
        (forall ((a5 int) ((= a5 0)))
         (forall ((a6 int) ((= a6 0)))
          ($k4 a4 a5 a6 a0 reftgen$modes$0 a0)))))
      (forall ((a7 int) (true))
       (forall ((a8 (Adt1)) (true))
        (forall ((a9 int) ((= a9 (fld1$0 a8))))
         (forall ((a10 int) ((= a10 (fld1$1 a8))))
          (forall ((_$ int) (and ($k1 a7 a9 a10 reftgen$modes$0 a0) ($k2 a9 a10 reftgen$modes$0 a0) ($k3 a10 reftgen$modes$0 a0)))
           (and
            (forall ((a11 int) (true))
             (forall ((a12 int) ((= a12 (fld1$0 a8))))
              (forall ((a13 int) ((= a13 (fld1$1 a8))))
               (forall ((_$ int) ($k4 a11 a7 a12 a13 reftgen$modes$0 a0))
                (forall ((a14 int) ((= a14 (fld1$0 a8))))
                 (forall ((a15 int) ((= a15 (fld1$1 a8))))
                  ($k5 a11 reftgen$modes$0 a0 a7 a14 a15)))))))
            (forall ((a16 (Adt1)) (true))
             (forall ((_$ int) (and ((= (+ (fld1$0 a8) 1) (fld1$0 a16))) ((= (fld1$1 a8) (fld1$1 a16)))))
              (forall ((_$ int) ((= (< (fld1$0 a8) (fld1$1 a8)) true)))
               (forall ((a17 int) (true))
                (forall ((a18 int) ((= a18 (fld1$0 a8))))
                 (forall ((a19 int) ((= a19 (fld1$1 a8))))
                  (forall ((_$ int) ($k5 a17 reftgen$modes$0 a0 a7 a18 a19))
                   (forall ((_$ int) ((>= a17 0)))
                    (and
                     (tag ((= (> a17 3) true)) "0")
                     (tag ((= (f$get_mode$0 reftgen$modes$0 a17) (mkadt0$0 ))) "1")
                     (and
                      (tag ((<= 0 a17)) "2")
                      (tag ((< a17 32)) "3"))
                     (forall ((a20 bool) (true))
                      (forall ((_$ int) ((<= 0 (+ a7 1))))
                       (and
                        (forall ((a21 int) ((= a21 (+ a7 1))))
                         (forall ((a22 int) ((= a22 (fld1$0 a16))))
                          (forall ((a23 int) ((= a23 (fld1$1 a16))))
                           (and
                            ($k1 a21 a22 a23 reftgen$modes$0 a0)
                            ($k2 a22 a23 reftgen$modes$0 a0)
                            ($k3 a23 reftgen$modes$0 a0)))))
                        (forall ((a24 int) (true))
                         (forall ((a25 int) ((= a25 (fld1$0 a8))))
                          (forall ((a26 int) ((= a26 (fld1$1 a8))))
                           (forall ((_$ int) ($k5 a24 reftgen$modes$0 a0 a7 a25 a26))
                            (forall ((a27 int) ((= a27 (+ a7 1))))
                             (forall ((a28 int) ((= a28 (fld1$0 a16))))
                              (forall ((a29 int) ((= a29 (fld1$1 a16))))
                               ($k4 a24 a27 a28 a29 reftgen$modes$0 a0))))))))))))))))))))))))))))))))
