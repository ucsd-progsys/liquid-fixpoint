(define_fun foo ((m (Map_t int int))) bool ((= 99 (Map_select m 0))))

(constraint
 (forall ((moo (Map_t int int)) ((foo moo)))
   (and
     (tag ((= (Map_select moo 10) (Map_select moo (+ 1 9)))) "1")
     (tag ((foo moo)) "0")
   )
  ))