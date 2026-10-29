(constraint
  ; bv128_to_int ((int_to_bv128 2^100) + (int_to_bv128 1)) != 2^100
  (forall ((x Int) ((= x 1267650600228229401496703205376)))
    (forall ((y Int) ((= y 1)))
      (forall ((x_ (BitVec Size128)) ((= x_ (int_to_bv128 x))))
        (forall ((y_ (BitVec Size128)) ((= y_ (int_to_bv128 y))))
          (forall ((res_ (BitVec Size128)) ((= res_ (bvadd x_ y_))))
            ((= (bv128_to_int res_) 1267650600228229401496703205376))))))))
