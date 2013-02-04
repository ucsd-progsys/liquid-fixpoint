(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_31 Int))
:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((lq_anf__dbw Int))
:extrafuns ((lq_anf__dbx Int))

:extrafuns ((lq_anf__dby Int))
:extrafuns ((lq_anf__dby Int))

:extrafuns ((lq_anf__dbx Int))
:extrafuns ((lq_anf__dbw Int))

:extrafuns ((VV_28 Int))
:extrafuns ((VV_23 Int))
:extrafuns ((VV_21 Int))

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
:extrafuns ((F_r9H Int Int Int Int))

:extrapreds ((k_29 Int Int Int Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int))

:extrapreds ((k_22 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbw 1) (and (= lq_anf__dbx 2) (and (= lq_anf__dby 3) true))))))) (k_24 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbw 1) (and (= lq_anf__dbx 2) (and (= lq_anf__dby 3) (and (k_29 EQ_6U GT_6W LT_6S VV_F2 lq_anf__dbw lq_anf__dbx lq_anf__dby) true)))))))) (k_22 EQ_6U GT_6W LT_6S VV_F2 VV_31))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbw 1) (and (= lq_anf__dbx 2) (and (= lq_anf__dby 3) (and (= VV_F3 3) (and (= VV_F3 lq_anf__dby) true))))))))) (k_29 EQ_6U GT_6W LT_6S VV_F3 lq_anf__dbw lq_anf__dbx lq_anf__dby))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbw 1) (and (= lq_anf__dbx 2) (and (= lq_anf__dby 3) (and (= VV_F4 2) (and (= VV_F4 lq_anf__dbx) true))))))))) (k_29 EQ_6U GT_6W LT_6S VV_F4 lq_anf__dbw lq_anf__dbx lq_anf__dby))



; cid = 5
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dbw 1) (and (= lq_anf__dbx 2) (and (= lq_anf__dby 3) (and (= VV_F5 1) (and (= VV_F5 lq_anf__dbw) true))))))))) (k_29 EQ_6U GT_6W LT_6S VV_F5 lq_anf__dbw lq_anf__dbx lq_anf__dby))

)