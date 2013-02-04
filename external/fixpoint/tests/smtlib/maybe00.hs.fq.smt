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
:extrafuns ((z_abl Int))

:extrafuns ((z_abl Int))
:extrafuns ((VV_24 Int))
:extrafuns ((VV_22 Int))

:extrafuns ((VV_19 Int))
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
:extrapreds ((isJust Int))


; constant 
:extrafuns ((fromJust Int Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))


; constant 
:extrafuns ((Just_r1a Int Int))

:extrapreds ((k_25 Int Int Int Int Int))

:extrapreds ((k_23 Int Int Int Int Int Int))

:extrapreds ((k_20 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Prop VV_F1) (and (= VV_F1 True_6u) true))))))) (k_25 EQ_6U GT_6W LT_6S True_6u VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_20 EQ_6U GT_6W LT_6S True_6u VV_F2))



; cid = 3
:assumption
(implies ((and (not (= (fromJust VV_F3) z)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (fromJust VV_F3) z) (and (and (implies (isJust VV_F3) true) (and (implies true (isJust VV_F3)) true)) true)))))))) false)



; cid = 4
:assumption
(implies ((and (k_20 EQ_6U GT_6W LT_6S True_6u z_abl) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_20 EQ_6U GT_6W LT_6S True_6u VV_F4) (and (= VV_F4 z_abl) true)))))))) (k_23 EQ_6U GT_6W LT_6S True_6u VV_F4 z_abl))

)