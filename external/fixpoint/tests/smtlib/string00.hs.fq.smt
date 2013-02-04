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

:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))

:extrafuns ((VV_F8 Int))
:extrafuns ((foo_ruf Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((lq_anf__dFJ Int))

:extrafuns ((lq_anf__dFK Int))
:extrafuns ((lq_anf__dFL Int))

:extrafuns ((lq_anf__dFM Int))
:extrafuns ((lq_anf__dFN Int))

:extrafuns ((lq_anf__dFO Int))
:extrafuns ((prop1_rug Int))

:extrafuns ((unpackCString__0k Int))
:extrafuns ((unpackCString__0k Int))

:extrafuns ((prop1_rug Int))
:extrafuns ((lq_anf__dFN Int))

:extrafuns ((lq_anf__dFM Int))
:extrafuns ((lq_anf__dFK Int))

:extrafuns ((lq_anf__dFJ Int))
:extrafuns ((liquidAssertB_r7 Int))

:extrafuns ((foo_ruf Int))
:extrafuns ((VV_43 Int))
:extrafuns ((VV_39 Int))

:extrafuns ((VV_35 Int))
:extrafuns ((VV_31 Int))
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
:extrafuns ((fix__36__36__34_dog_34_ Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_44 Int Int Int Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int))

:extrapreds ((k_36 Int Int Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_32 EQ_6U GT_6W LT_6S prop1_rug foo_ruf) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFM 1) (and (= lq_anf__dFN 0) (and (and (implies (Prop lq_anf__dFO) (not (= lq_anf__dFM lq_anf__dFN))) (and (implies (not (= lq_anf__dFM lq_anf__dFN)) (Prop lq_anf__dFO)) true)) (and (Prop VV_F1) true)))))))))) (k_40 EQ_6U GT_6W LT_6S VV_F1 foo_ruf prop1_rug))



; cid = 2
:assumption
(implies ((and (not (Prop VV_F2)) (and (k_32 EQ_6U GT_6W LT_6S prop1_rug foo_ruf) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFM 1) (and (= lq_anf__dFN 0) (and (and (implies (Prop lq_anf__dFO) (not (= lq_anf__dFM lq_anf__dFN))) (and (implies (not (= lq_anf__dFM lq_anf__dFN)) (Prop lq_anf__dFO)) true)) (and (and (implies (Prop VV_F2) (not (= lq_anf__dFM lq_anf__dFN))) (and (implies (not (= lq_anf__dFM lq_anf__dFN)) (Prop VV_F2)) true)) (and (= VV_F2 lq_anf__dFO) true)))))))))))) false)



; cid = 3
:assumption
(implies ((and (k_32 EQ_6U GT_6W LT_6S prop1_rug foo_ruf) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFM 1) (and (= lq_anf__dFN 0) (and (= VV_F3 0) (and (= VV_F3 lq_anf__dFN) true)))))))))) (k_44 EQ_6U GT_6W LT_6S VV_F3 foo_ruf lq_anf__dFM lq_anf__dFN prop1_rug))



; cid = 4
:assumption
(implies ((and (k_32 EQ_6U GT_6W LT_6S prop1_rug foo_ruf) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFM 1) (and (= lq_anf__dFN 0) (and (= VV_F4 1) (and (= VV_F4 lq_anf__dFM) true)))))))))) (k_44 EQ_6U GT_6W LT_6S VV_F4 foo_ruf lq_anf__dFM lq_anf__dFN prop1_rug))



; cid = 5
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFJ 0) (and (= lq_anf__dFK 0) (and (and (implies (Prop lq_anf__dFL) (= lq_anf__dFJ lq_anf__dFK)) (and (implies (= lq_anf__dFJ lq_anf__dFK) (Prop lq_anf__dFL)) true)) (and (Prop VV_F5) true))))))))) (k_32 EQ_6U GT_6W LT_6S VV_F5 foo_ruf))



; cid = 6
:assumption
(implies ((and (not (Prop VV_F6)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFJ 0) (and (= lq_anf__dFK 0) (and (and (implies (Prop lq_anf__dFL) (= lq_anf__dFJ lq_anf__dFK)) (and (implies (= lq_anf__dFJ lq_anf__dFK) (Prop lq_anf__dFL)) true)) (and (and (implies (Prop VV_F6) (= lq_anf__dFJ lq_anf__dFK)) (and (implies (= lq_anf__dFJ lq_anf__dFK) (Prop VV_F6)) true)) (and (= VV_F6 lq_anf__dFL) true))))))))))) false)



; cid = 7
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFJ 0) (and (= lq_anf__dFK 0) (and (= VV_F7 0) (and (= VV_F7 lq_anf__dFK) true))))))))) (k_36 EQ_6U GT_6W LT_6S VV_F7 foo_ruf lq_anf__dFJ lq_anf__dFK))



; cid = 8
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len foo_ruf) 0) (and (= lq_anf__dFJ 0) (and (= lq_anf__dFK 0) (and (= VV_F8 0) (and (= VV_F8 lq_anf__dFJ) true))))))))) (k_36 EQ_6U GT_6W LT_6S VV_F8 foo_ruf lq_anf__dFJ lq_anf__dFK))

)