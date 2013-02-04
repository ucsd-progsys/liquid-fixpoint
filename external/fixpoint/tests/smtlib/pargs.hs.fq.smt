(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((dummy.pos.pargs.hs.4.14 Int))

:extrafuns ((dummy.pos.pargs.hs.4.35 Int))
:extrafuns ((f_aa6 Int))

:extrafuns ((i_aa7 Int))
:extrafuns ((lq_tmp_x17 Int))
:extrafuns ((p Int))

:extrafuns ((x0 Int))
:extrafuns ((x0 Int))
:extrafuns ((p Int))

:extrafuns ((lq_tmp_x17 Int))
:extrafuns ((f_aa6 Int))

:extrafuns ((VV_23 Int))
:extrafuns ((VV_20 Int))
:extrafuns ((VV_18 Int))

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

:extrapreds ((k_24 Int Int Int Int Int Int))

:extrapreds ((k_21 Int Int Int Int Int Int Int))

:extrapreds ((k_19 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_19 EQ_6U GT_6W LT_6S lq_tmp_x17 p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p VV_F1 lq_tmp_x17) true)))))) (k_21 EQ_6U GT_6W LT_6S VV_F1 lq_tmp_x17 p x0))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= VV_F2 0) true))))) (k_24 EQ_6U GT_6W LT_6S VV_F2 p x0))



; cid = 3
:assumption
(implies ((and (not (papp2 p VV_F3 0)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= dummy.pos.pargs.hs.4.35 0) (and (k_21 EQ_6U GT_6W LT_6S VV_F3 dummy.pos.pargs.hs.4.35 p x0) true))))))) false)



; cid = 4
:assumption
(implies ((and (k_24 EQ_6U GT_6W LT_6S i_aa7 p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_24 EQ_6U GT_6W LT_6S VV_F4 p x0) (and (= VV_F4 i_aa7) true))))))) (k_19 EQ_6U GT_6W LT_6S VV_F4 p x0))

)