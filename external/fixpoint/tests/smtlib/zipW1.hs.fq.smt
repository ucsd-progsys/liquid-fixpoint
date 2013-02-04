(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_31 Int))

:extrafuns ((VV_34 Int))
:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.24 Int))

:extrafuns ((dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.29 Int))

:extrafuns ((dummy.pos.zipw1.hs.3.19 Int))
:extrafuns ((xs Int))

:extrafuns ((ys Int))
:extrafuns ((VV_26 Int))
:extrafuns ((VV_24 Int))

:extrafuns ((VV_22 Int))
:extrafuns ((LT_6S Int))
:extrafuns ((GT_6W Int))

:extrafuns ((EQ_6U Int))
; constant 
:extrapreds ((papp2 Int Int Int))


; constant 
:extrapreds ((papp1 Int Int))


; constant 
:extrafuns ((len Int Int))


; constant 
:extrapreds ((isJust Int))


; constant 
:extrafuns ((fromJust Int Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_27 Int Int Int Int))

:extrapreds ((k_25 Int Int Int Int))

:extrapreds ((k_23 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_23 EQ_6U GT_6W LT_6S dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.24) (and (k_25 EQ_6U GT_6W LT_6S dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.29) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))))) (k_27 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_31) 0) true))))) (k_23 EQ_6U GT_6W LT_6S VV_F2))



; cid = 3
:assumption
(implies ((and (not (= (len VV_F3) (len xs))) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len xs) 0) (and (= (len VV_F3) (len xs)) true))))))) false)



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_34) (len xs)) (and (>= (len VV_34) 0) (and (>= (len xs) 0) true))))))) (k_25 EQ_6U GT_6W LT_6S VV_F4))



; cid = 5
:assumption
(implies ((and (not (= (len VV_F5) (len xs))) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len xs) 0) (and (= (len ys) (len xs)) (and (>= (len ys) 0) (and (= (len VV_F5) (len xs)) true))))))))) false)

)