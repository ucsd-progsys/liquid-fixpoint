(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((False_68 Int))
:extrafuns ((GT_6W Int))

:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))

:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))

:extrafuns ((dummy.pos.alias01.hs.7.14 Int))
:extrafuns ((incr_r9H Int))

:extrafuns ((lq_anf__dhG Int))
:extrafuns ((lq_anf__dhH Int))

:extrafuns ((lq_anf__dhI Int))
:extrafuns ((lq_anf__dhJ Int))

:extrafuns ((lq_anf__dhK Int))
:extrafuns ((myabs_r9G Int))

:extrafuns ((x Int))
:extrafuns ((x_ahv Int))
:extrafuns ((x_ahw Int))

:extrafuns ((x_ahw Int))
:extrafuns ((x_ahv Int))

:extrafuns ((myabs_r9G Int))
:extrafuns ((lq_anf__dhG Int))

:extrafuns ((incr_r9H Int))
:extrafuns ((VV_56 Int))

:extrafuns ((VV_53 Int))
:extrafuns ((VV_46 Int))
:extrafuns ((VV_43 Int))

:extrafuns ((VV_40 Int))
:extrafuns ((True_6u Int))
:extrafuns ((LT_6S Int))

:extrafuns ((I__6c Int))
:extrafuns ((GT_6W Int))

:extrafuns ((False_68 Int))

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

:extrapreds ((k_57 Int Int Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int Int Int))

:extrapreds ((k_47 Int Int Int Int Int Int Int Int))

:extrapreds ((k_44 Int Int Int Int Int Int Int))

:extrapreds ((k_41 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_54 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1))



; cid = 2
:assumption
(implies ((and (not (<= x VV_F2)) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_57 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2 x) true)))))))) false)



; cid = 3
:assumption
(implies ((and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x_ahw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhK 1) (and (= VV_F3 (+ x_ahw lq_anf__dhK)) true))))))))) (k_57 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 x_ahw))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_41 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4))



; cid = 5
:assumption
(implies ((and (not (<= 0 VV_F5)) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_44 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 dummy.pos.alias01.hs.7.14) true)))))))) false)



; cid = 6
:assumption
(implies ((and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u x_ahv) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhG 0) (and (and (implies (Prop lq_anf__dhH) (> x_ahv lq_anf__dhG)) (and (implies (> x_ahv lq_anf__dhG) (Prop lq_anf__dhH)) true)) (and (and (implies (Prop lq_anf__dhI) (> x_ahv lq_anf__dhG)) (and (implies (> x_ahv lq_anf__dhG) (Prop lq_anf__dhI)) true)) (and (= lq_anf__dhI lq_anf__dhH) (and (Prop lq_anf__dhI) (and (Prop lq_anf__dhI) (and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6) (and (= VV_F6 x_ahv) true))))))))))))))) (k_44 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 x_ahv))



; cid = 7
:assumption
(implies ((and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u x_ahv) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhG 0) (and (and (implies (Prop lq_anf__dhH) (> x_ahv lq_anf__dhG)) (and (implies (> x_ahv lq_anf__dhG) (Prop lq_anf__dhH)) true)) (and (and (implies (Prop lq_anf__dhI) (> x_ahv lq_anf__dhG)) (and (implies (> x_ahv lq_anf__dhG) (Prop lq_anf__dhI)) true)) (and (= lq_anf__dhI lq_anf__dhH) (and (not (Prop lq_anf__dhI)) (and (not (Prop lq_anf__dhI)) (and (= lq_anf__dhJ 0) (and (= VV_F7 (- lq_anf__dhJ x_ahv)) true))))))))))))))) (k_44 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 x_ahv))



; cid = 8
:assumption
(implies ((and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u x_ahv) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhG 0) (and (= VV_F8 0) (and (= VV_F8 lq_anf__dhG) true)))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 lq_anf__dhG x_ahv))



; cid = 9
:assumption
(implies ((and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u x_ahv) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhG 0) (and (k_41 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9) (and (= VV_F9 x_ahv) true)))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 lq_anf__dhG x_ahv))

)