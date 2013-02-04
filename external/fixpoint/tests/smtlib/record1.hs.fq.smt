(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_39 Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((lq_anf__db0 Int))

:extrafuns ((lq_tmp_x25 Int))
:extrafuns ((unpackCString__0k Int))

:extrafuns ((unpackCString__0k Int))
:extrafuns ((lq_tmp_x35 Int))

:extrafuns ((lq_tmp_x25 Int))
:extrafuns ((lq_anf__db0 Int))

:extrafuns ((VV_36 Int))
:extrafuns ((VV_33 Int))
:extrafuns ((VV_31 Int))

:extrafuns ((VV_29 Int))
:extrafuns ((VV_26 Int))
:extrafuns ((VV_23 Int))

:extrafuns ((VV_21 Int))
:extrafuns ((VV_19 Int))
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
:extrapreds ((isBin Int))


; constant 
:extrafuns ((fromJust Int Int))


; constant 
:extrafuns ((fix__36__36__34_TODO_34_ Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_37 Int Int Int Int Int))

:extrapreds ((k_34 Int Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int Int))

:extrapreds ((k_22 Int Int Int Int Int))

:extrapreds ((k_20 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len lq_anf__db0) 0) (and (k_37 EQ_6U GT_6W LT_6S VV_F1 lq_anf__db0) true)))))) (k_27 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (k_37 EQ_6U GT_6W LT_6S VV_39 lq_anf__db0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len lq_anf__db0) 0) (and (k_30 EQ_6U GT_6W LT_6S VV_F2 VV_39 lq_anf__db0) true))))))) (k_20 EQ_6U GT_6W LT_6S VV_F2 VV_39))



; cid = 3
:assumption
(implies ((and (k_37 EQ_6U GT_6W LT_6S VV_39 lq_anf__db0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len lq_anf__db0) 0) (and (k_32 EQ_6U GT_6W LT_6S VV_F3 VV_39 lq_anf__db0) true))))))) (k_22 EQ_6U GT_6W LT_6S VV_F3 VV_39))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len lq_anf__db0) 0) (and (k_34 EQ_6U GT_6W LT_6S VV_F4 lq_anf__db0 lq_tmp_x25) true)))))) (k_24 EQ_6U GT_6W LT_6S VV_F4 lq_tmp_x25))

)