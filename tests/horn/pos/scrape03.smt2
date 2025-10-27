;; test that `--scrape` works with ADTs and bit-vectors
(fixpoint "--scrape=both")

(datatype (Adt0 0) ((mkadt0$0 ()) (mkadt0$1 ())))

(var $k0 (bool)) ;; orig: $k0

(constraint
 (forall ((a0 bool) (true))
  (and
   ($k0 a0)
   (forall ((_$ int) ($k0 a0))
    (forall ((a1 (BitVec Size32)) (true))
     (forall ((a2 (BitVec Size32)) (true))
      (forall ((a3 (BitVec Size32)) (true))
       (and
        (tag ((= (let ((a8 (bvand (bvlshr (let ((a4 (int_to_bv32 5))) (bvand (let ((a5 (int_to_bv32 1))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand (let ((a6 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a6))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a6)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a5))) (bvor (let ((a7 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a7))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a7)))) (bvshl (lit "#x00000001" (BitVec Size32)) a5)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a4)))) (int_to_bv32 0)) (lit "#x00000001" (BitVec Size32))))) (if (= a8 (lit "#x00000000" (BitVec Size32))) (mkadt0$0 ) (mkadt0$1 ))) (mkadt0$1 ))) "0")
        (tag ((= (let ((a13 (bvand (bvlshr (let ((a9 (int_to_bv32 5))) (bvand (let ((a10 (int_to_bv32 1))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand (let ((a11 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a11))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a11)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a10))) (bvor (let ((a12 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a12))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a12)))) (bvshl (lit "#x00000001" (BitVec Size32)) a10)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a9)))) (int_to_bv32 1)) (lit "#x00000001" (BitVec Size32))))) (if (= a13 (lit "#x00000000" (BitVec Size32))) (mkadt0$0 ) (mkadt0$1 ))) (mkadt0$1 ))) "1")
        (tag ((= (let ((a18 (bvand (bvlshr (let ((a14 (int_to_bv32 5))) (bvand (let ((a15 (int_to_bv32 1))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand (let ((a16 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a16))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a16)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a15))) (bvor (let ((a17 (int_to_bv32 0))) (if (= (mkadt0$1 ) (mkadt0$0 )) (bvand a1 (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a17))) (bvor a1 (bvshl (lit "#x00000001" (BitVec Size32)) a17)))) (bvshl (lit "#x00000001" (BitVec Size32)) a15)))) (bvnot (bvshl (lit "#x00000001" (BitVec Size32)) a14)))) (int_to_bv32 5)) (lit "#x00000001" (BitVec Size32))))) (if (= a18 (lit "#x00000000" (BitVec Size32))) (mkadt0$0 ) (mkadt0$1 ))) (mkadt0$0 ))) "2")))))))))
