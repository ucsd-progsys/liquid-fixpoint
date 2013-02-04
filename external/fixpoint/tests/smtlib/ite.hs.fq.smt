(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((False_68 Int))
:extrafuns ((GT_6W Int))

:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))

:extrafuns ((VV_64 Int))
:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))

:extrafuns ((VV_F9 Int))
:extrafuns ((ds_dhj Int))

:extrafuns ((lq_anf__dhk Int))
:extrafuns ((lq_anf__dhl Int))

:extrafuns ((lq_anf__dhm Int))
:extrafuns ((lq_anf__dhn Int))

:extrafuns ((myabs_r9G Int))
:extrafuns ((realWorld__0f Int))

:extrafuns ((x Int))
:extrafuns ((x_ahb Int))
:extrafuns ((x_ahb Int))

:extrafuns ((realWorld__0f Int))
:extrafuns ((myabs_r9G Int))

:extrafuns ((lq_anf__dhm Int))
:extrafuns ((lq_anf__dhl Int))

:extrafuns ((lq_anf__dhk Int))
:extrafuns ((ds_dhj Int))

:extrafuns ((VV_51 Int))
:extrafuns ((VV_48 Int))
:extrafuns ((VV_46 Int))

:extrafuns ((VV_41 Int))
:extrafuns ((VV_38 Int))
:extrafuns ((VV_35 Int))

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

:extrapreds ((k_52 Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_47 Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_42 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_39 Int Int Int Int Int Int Int Int))

:extrapreds ((k_36 Int Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_36 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1 realWorld__0f))



; cid = 2
:assumption
(implies ((and (not (and (implies (> x 0) (= VV_F2 x)) (and (implies (not (> x 0)) (= (+ VV_F2 x) 0)) true))) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_39 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2 realWorld__0f x) true)))))))) false)



; cid = 3
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (and (implies (Prop lq_anf__dhl) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhl)) true)) (and (and (implies (Prop lq_anf__dhm) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhm)) true)) (and (= lq_anf__dhm lq_anf__dhl) (and (Prop lq_anf__dhm) (and (Prop lq_anf__dhm) (and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 realWorld__0f) (and (= VV_F3 x_ahb) true))))))))))))))) (k_39 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 realWorld__0f x_ahb))



; cid = 4
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (and (implies (Prop lq_anf__dhl) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhl)) true)) (and (and (implies (Prop lq_anf__dhm) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhm)) true)) (and (= lq_anf__dhm lq_anf__dhl) (and (not (Prop lq_anf__dhm)) (and (not (Prop lq_anf__dhm)) (and (k_52 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 realWorld__0f lq_anf__dhk lq_anf__dhl lq_anf__dhm realWorld__0f x_ahb) true)))))))))))))) (k_39 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 realWorld__0f x_ahb))



; cid = 5
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (and (implies (Prop lq_anf__dhl) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhl)) true)) (and (and (implies (Prop lq_anf__dhm) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhm)) true)) (and (= lq_anf__dhm lq_anf__dhl) (and (not (Prop lq_anf__dhm)) (and (not (Prop lq_anf__dhm)) (and (= VV_F5 realWorld__0f) true)))))))))))))) (k_49 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 lq_anf__dhk lq_anf__dhl lq_anf__dhm realWorld__0f x_ahb))



; cid = 6
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_64 realWorld__0f) (and (= lq_anf__dhk 0) (and (and (implies (Prop lq_anf__dhl) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhl)) true)) (and (and (implies (Prop lq_anf__dhm) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhm)) true)) (and (= lq_anf__dhm lq_anf__dhl) (and (not (Prop lq_anf__dhm)) (and (not (Prop lq_anf__dhm)) true)))))))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 VV_64 lq_anf__dhk lq_anf__dhl lq_anf__dhm realWorld__0f x_ahb))



; cid = 7
:assumption
(implies ((and (k_49 EQ_6U False_68 GT_6W LT_6S True_6u ds_dhj lq_anf__dhk lq_anf__dhl lq_anf__dhm realWorld__0f x_ahb) (and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (and (implies (Prop lq_anf__dhl) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhl)) true)) (and (and (implies (Prop lq_anf__dhm) (> x_ahb lq_anf__dhk)) (and (implies (> x_ahb lq_anf__dhk) (Prop lq_anf__dhm)) true)) (and (= lq_anf__dhm lq_anf__dhl) (and (not (Prop lq_anf__dhm)) (and (not (Prop lq_anf__dhm)) (and (= lq_anf__dhn 0) (and (= VV_F7 (- lq_anf__dhn x_ahb)) true)))))))))))))))) (k_52 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 ds_dhj lq_anf__dhk lq_anf__dhl lq_anf__dhm realWorld__0f x_ahb))



; cid = 8
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (= VV_F8 0) (and (= VV_F8 lq_anf__dhk) true)))))))))) (k_42 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 lq_anf__dhk realWorld__0f x_ahb))



; cid = 9
:assumption
(implies ((and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u x_ahb realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhk 0) (and (k_36 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 realWorld__0f) (and (= VV_F9 x_ahb) true)))))))))) (k_42 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 lq_anf__dhk realWorld__0f x_ahb))

)