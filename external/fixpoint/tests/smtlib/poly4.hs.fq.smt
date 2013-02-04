(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((True_6u Int))
:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((choose_rh Int))
:extrafuns ((liquidAssertB_r7 Int))

:extrafuns ((lq_anf__dlO Int))
:extrafuns ((lq_anf__dlP Int))

:extrafuns ((prop_rkl Int))
:extrafuns ((prop_rkl Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((choose_rh Int))

:extrafuns ((VV_34 Int))
:extrafuns ((VV_30 Int))
:extrafuns ((VV_28 Int))

:extrafuns ((VV_24 Int))
:extrafuns ((True_6u Int))
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

:extrapreds ((k_35 Int Int Int Int Int Int))

:extrapreds ((k_31 Int Int Int Int Int))

:extrapreds ((k_29 Int Int Int Int Int))

:extrapreds ((k_25 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_29 EQ_6U GT_6W LT_6S True_6u prop_rkl) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dlP 0) true))))))) (k_35 EQ_6U GT_6W LT_6S True_6u VV_F1 prop_rkl))



; cid = 2
:assumption
(implies ((and (k_31 EQ_6U GT_6W LT_6S True_6u lq_anf__dlO) (and (k_25 EQ_6U GT_6W LT_6S True_6u lq_anf__dlO) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dlO True_6u) (and (Prop VV_F2) true))))))))) (k_29 EQ_6U GT_6W LT_6S True_6u VV_F2))



; cid = 3
:assumption
(implies ((and (not (Prop VV_F3)) (and (k_31 EQ_6U GT_6W LT_6S True_6u lq_anf__dlO) (and (k_25 EQ_6U GT_6W LT_6S True_6u lq_anf__dlO) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dlO True_6u) (and (k_31 EQ_6U GT_6W LT_6S True_6u VV_F3) (and (k_25 EQ_6U GT_6W LT_6S True_6u VV_F3) (and (= VV_F3 True_6u) (and (= VV_F3 lq_anf__dlO) true))))))))))))) false)



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Prop VV_F4) (and (= VV_F4 True_6u) true))))))) (k_25 EQ_6U GT_6W LT_6S True_6u VV_F4))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Prop VV_F4) (and (= VV_F4 True_6u) true))))))) (k_31 EQ_6U GT_6W LT_6S True_6u VV_F4))

)