(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F2 Int))
:extrafuns ((lq_tmp_x2 Int))

:extrafuns ((lq_tmp_x4 Int))
:extrafuns ((plusOne_r0 Int))

:extrafuns ((plusOne_r0 Int))
:extrafuns ((VV_27 Int))

:extrafuns ((VV_25 Int))
:extrafuns ((VV_23 Int))
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

:extrapreds ((k_28 Int Int Int Int))

:extrapreds ((k_26 Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_28 EQ_6U GT_6W LT_6S lq_tmp_x4) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> VV_F1 lq_tmp_x4) true)))))) (k_24 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (k_24 EQ_6U GT_6W LT_6S lq_tmp_x2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> VV_F2 lq_tmp_x2) true)))))) (k_26 EQ_6U GT_6W LT_6S VV_F2))

)