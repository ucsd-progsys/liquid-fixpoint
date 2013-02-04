(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((z Int))
:extrafuns ((z_alV Int))

:extrafuns ((z_alV Int))
:extrafuns ((VV_27 Int))
:extrafuns ((VV_25 Int))

:extrafuns ((VV_22 Int))
:extrafuns ((True_6u Int))
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
:extrapreds ((isJustS Int))


; constant 
:extrapreds ((isJust Int))


; constant 
:extrafuns ((fromJustS Int Int))


; constant 
:extrafuns ((fromJust Int Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_28 Int Int Int Int Int))

:extrapreds ((k_26 Int Int Int Int Int Int))

:extrapreds ((k_23 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Prop VV_F1) (and (= VV_F1 True_6u) true))))))) (k_28 EQ_6U GT_6W LT_6S True_6u VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_23 EQ_6U GT_6W LT_6S True_6u VV_F2))



; cid = 3
:assumption
(implies ((and (not (= (fromJustS VV_F3) z)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (fromJustS VV_F3) z) (and (and (implies (isJustS VV_F3) true) (and (implies true (isJustS VV_F3)) true)) true)))))))) false)



; cid = 4
:assumption
(implies ((and (k_23 EQ_6U GT_6W LT_6S True_6u z_alV) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_23 EQ_6U GT_6W LT_6S True_6u VV_F4) (and (= VV_F4 z_alV) true)))))))) (k_26 EQ_6U GT_6W LT_6S True_6u VV_F4 z_alV))

)