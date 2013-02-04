(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))

:extrafuns ((bar_r9H Int))
:extrafuns ((foo_r9G Int))
:extrafuns ((i Int))

:extrafuns ((lq_anf__dbf Int))
:extrafuns ((lq_anf__dbg Int))

:extrafuns ((n Int))
:extrafuns ((n_ab6 Int))
:extrafuns ((n_ab7 Int))

:extrafuns ((n_ab7 Int))
:extrafuns ((n_ab6 Int))
:extrafuns ((foo_r9G Int))

:extrafuns ((bar_r9H Int))
:extrafuns ((VV_34 Int))
:extrafuns ((VV_31 Int))

:extrafuns ((VV_28 Int))
:extrafuns ((VV_25 Int))
:extrafuns ((LT_6S Int))

:extrafuns ((I__6c Int))
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

:extrapreds ((k_35 Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int))

:extrapreds ((k_29 Int Int Int Int Int))

:extrapreds ((k_26 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_32 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (not (< n VV_F2)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_35 EQ_6U GT_6W LT_6S VV_F2 n) true)))))) false)



; cid = 3
:assumption
(implies ((and (k_32 EQ_6U GT_6W LT_6S n_ab7) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbg 1) (and (= VV_F3 (+ n_ab7 lq_anf__dbg)) true))))))) (k_35 EQ_6U GT_6W LT_6S VV_F3 n_ab7))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_26 EQ_6U GT_6W LT_6S VV_F4))



; cid = 5
:assumption
(implies ((and (not (< i VV_F5)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_29 EQ_6U GT_6W LT_6S VV_F5 i) true)))))) false)



; cid = 6
:assumption
(implies ((and (k_26 EQ_6U GT_6W LT_6S n_ab6) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbf 1) (and (= VV_F6 (+ n_ab6 lq_anf__dbf)) true))))))) (k_29 EQ_6U GT_6W LT_6S VV_F6 n_ab6))

)