(fixpoint "--rewrite")

(constant pow2 (func(0, [BitVec Size32, bool])))

(define pow2(x : BitVec Size32) : bool = {
     (bvand x (bvsub x (int_to_bv32 1))) <=> (int_to_bv32 0)
 })

(constraint
(forall ((size Int) (1 <= size))
(forall ((index Int) (0 <= index))
(forall ((size_  (BitVec Size32)) (and (size_ = (int_to_bv32 size)) (pow2 size_) ))
(forall ((index_ (BitVec Size32)) (index_ = (int_to_bv32 index)))
(forall ((one_ (BitVec Size32)) (one_ = (int_to_bv32 1)))
(forall ((zero_ (BitVec Size32)) (zero_ = (int_to_bv32 0)))
(forall ((mask_ (BitVec Size32)) (mask_ = (bvsub size_ one_)))
(forall ((res_ (BitVec Size32)) (res_ = (bvand index_ mask_)))
(forall ((res Int) (res = (bv32_to_int res_)))
((res < size))))))))))))
