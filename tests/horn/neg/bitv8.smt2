(constraint
  (and

    (forall ((x Int) ((= x 1)))
      (forall ((y Int) ((= y 2)))
        (forall ((x_ (BitVec Size8)) ((= x_ (int_to_bv8 x))))
          (forall ((y_ (BitVec Size8)) ((= y_ (int_to_bv8 y))))
            (forall ((res_ (BitVec Size8)) ((= res_ (bvadd x_ y_))))
              ((= (bv8_to_int res_) 2)))))))

    (forall ((x Int) ((= x 1)))
      (forall ((y Int) ((= y 2)))
        (forall ((x_ (BitVec Size16)) ((= x_ (int_to_bv16 x))))
          (forall ((y_ (BitVec Size16)) ((= y_ (int_to_bv16 y))))
            (forall ((res_ (BitVec Size16)) ((= res_ (bvadd x_ y_))))
              ((= (bv16_to_int res_) 2)))))))

  )
)