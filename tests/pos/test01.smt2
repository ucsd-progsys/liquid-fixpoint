(declare-fun inv (Int Int) Bool)

(declare-const f (Array Int Int))

(assert (forall 
	  ((x Int) (y Int)) 
	    (=> (= x y) (inv x y))))

(assert (forall 
	  ((x Int) (y Int) (x1 Int) (y1 Int))
    	    (=> (and (inv x y) (and (= x1 ((select f x))) (= y1 (select f y))))
     	        (inv x1 y1)))

(assert (forall 
          ((x Int) (y Int)) 
	    (=> (inv x y) (= x y))))
