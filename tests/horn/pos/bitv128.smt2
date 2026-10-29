(constraint
  (and
    ; bv128_to_int ((int_to_bv128 2^100) + (int_to_bv128 1)) == 2^100 + 1
    (forall ((x Int) ((= x 1267650600228229401496703205376)))
      (forall ((y Int) ((= y 1)))
        (forall ((x_ (BitVec Size128)) ((= x_ (int_to_bv128 x))))
          (forall ((y_ (BitVec Size128)) ((= y_ (int_to_bv128 y))))
            (forall ((res_ (BitVec Size128)) ((= res_ (bvadd x_ y_))))
              ((= (bv128_to_int res_) 1267650600228229401496703205377)))))))

    ; bv128_to_int ((int_to_bv128 (2^128 - 1)) + (int_to_bv128 1)) == 0
    (forall ((max Int) ((= max 340282366920938463463374607431768211455)))
      (forall ((one Int) ((= one 1)))
        (forall ((max_ (BitVec Size128)) ((= max_ (int_to_bv128 max))))
          (forall ((one_ (BitVec Size128)) ((= one_ (int_to_bv128 one))))
            (forall ((res_ (BitVec Size128)) ((= res_ (bvadd max_ one_))))
              ((= (bv128_to_int res_) 0)))))))
  )
)
