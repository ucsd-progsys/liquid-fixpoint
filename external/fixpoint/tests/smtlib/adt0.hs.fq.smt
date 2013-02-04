(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_32 Int))
:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((goo_rui Int))

:extrafuns ((lq_anf__dvT Int))
:extrafuns ((z_avL Int))

:extrafuns ((z_avL Int))
:extrafuns ((goo_rui Int))
:extrafuns ((VV_28 Int))

:extrafuns ((VV_26 Int))
:extrafuns ((VV_23 Int))
:extrafuns ((VV_20 Int))

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


; constant 
:extrafuns ((P_ruh Int Int Int))

:extrapreds ((k_29 Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int Int))

:extrapreds ((k_21 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dvT 10) true))))) (k_29 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dvT 10) (and (k_24 EQ_6U GT_6W LT_6S VV_F2 lq_anf__dvT) true)))))) (k_27 EQ_6U GT_6W LT_6S VV_F2 VV_32))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dvT 10) (and (= VV_F3 10) (and (= VV_F3 lq_anf__dvT) true))))))) (k_21 EQ_6U GT_6W LT_6S VV_F3))



; cid = 4
:assumption
(implies ((and (k_21 EQ_6U GT_6W LT_6S z_avL) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_21 EQ_6U GT_6W LT_6S VV_F4) (and (= VV_F4 z_avL) true))))))) (k_24 EQ_6U GT_6W LT_6S VV_F4 z_avL))

)