(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((False_68 Int))
:extrafuns ((GT_6W Int))

:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))

:extrafuns ((VV_102 Int))
:extrafuns ((VV_99 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F13 Int))

:extrafuns ((VV_F14 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))

:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))

:extrafuns ((dummy.pos.alias00.hs.4.21 Int))

:extrafuns ((dummy.pos.alias00.hs.9.22 Int))
:extrafuns ((lq_anf__dhO Int))

:extrafuns ((lq_anf__dhP Int))
:extrafuns ((lq_anf__dhQ Int))

:extrafuns ((lq_anf__dhR Int))
:extrafuns ((lq_anf__dhS Int))

:extrafuns ((lq_tmp_x46 Int))
:extrafuns ((lq_tmp_x60 Int))

:extrafuns ((myabs_r9G Int))
:extrafuns ((x_ahy Int))

:extrafuns ((x_ahz Int))
:extrafuns ((x_ahz Int))
:extrafuns ((x_ahy Int))

:extrafuns ((myabs_r9G Int))
:extrafuns ((lq_tmp_x60 Int))

:extrafuns ((lq_tmp_x53 Int))
:extrafuns ((lq_tmp_x46 Int))

:extrafuns ((lq_anf__dhP Int))
:extrafuns ((lq_anf__dhO Int))

:extrafuns ((VV_68 Int))
:extrafuns ((VV_65 Int))
:extrafuns ((VV_62 Int))

:extrafuns ((VV_58 Int))
:extrafuns ((VV_56 Int))
:extrafuns ((VV_51 Int))

:extrafuns ((VV_49 Int))
:extrafuns ((VV_47 Int))
:extrafuns ((VV_44 Int))

:extrafuns ((VV_42 Int))
:extrafuns ((VV_39 Int))
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
:extrafuns ((fix__91__93__35_6m  Int))


; constant 
:extrafuns ((fix__58__35_64 Int Int Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_69 Int Int Int Int Int Int Int Int))

:extrapreds ((k_66 Int Int Int Int Int Int Int))

:extrapreds ((k_63 Int Int Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_57 Int Int Int Int Int Int Int Int))

:extrapreds ((k_52 Int Int Int Int Int Int Int Int))

:extrapreds ((k_50 Int Int Int Int Int Int Int))

:extrapreds ((k_48 Int Int Int Int Int Int Int))

:extrapreds ((k_45 Int Int Int Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_63 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1))



; cid = 2
:assumption
(implies ((and (not (>= VV_F2 0)) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_66 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2 dummy.pos.alias00.hs.4.21) true)))))))) false)



; cid = 3
:assumption
(implies ((and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u x_ahy) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhP 0) (and (and (implies (Prop lq_anf__dhQ) (> x_ahy lq_anf__dhP)) (and (implies (> x_ahy lq_anf__dhP) (Prop lq_anf__dhQ)) true)) (and (and (implies (Prop lq_anf__dhR) (> x_ahy lq_anf__dhP)) (and (implies (> x_ahy lq_anf__dhP) (Prop lq_anf__dhR)) true)) (and (= lq_anf__dhR lq_anf__dhQ) (and (Prop lq_anf__dhR) (and (Prop lq_anf__dhR) (and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3) (and (= VV_F3 x_ahy) true))))))))))))))) (k_66 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 x_ahy))



; cid = 4
:assumption
(implies ((and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u x_ahy) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhP 0) (and (and (implies (Prop lq_anf__dhQ) (> x_ahy lq_anf__dhP)) (and (implies (> x_ahy lq_anf__dhP) (Prop lq_anf__dhQ)) true)) (and (and (implies (Prop lq_anf__dhR) (> x_ahy lq_anf__dhP)) (and (implies (> x_ahy lq_anf__dhP) (Prop lq_anf__dhR)) true)) (and (= lq_anf__dhR lq_anf__dhQ) (and (not (Prop lq_anf__dhR)) (and (not (Prop lq_anf__dhR)) (and (= lq_anf__dhS 0) (and (= VV_F4 (- lq_anf__dhS x_ahy)) true))))))))))))))) (k_66 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 x_ahy))



; cid = 5
:assumption
(implies ((and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u x_ahy) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhP 0) (and (= VV_F5 0) (and (= VV_F5 lq_anf__dhP) true)))))))))) (k_69 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 lq_anf__dhP x_ahy))



; cid = 6
:assumption
(implies ((and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u x_ahy) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dhP 0) (and (k_63 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6) (and (= VV_F6 x_ahy) true)))))))))) (k_69 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 lq_anf__dhP x_ahy))



; cid = 7
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7))



; cid = 8
:assumption
(implies ((and (not (> (len VV_F8) 0)) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_48 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 dummy.pos.alias00.hs.9.22) true)))))))) false)



; cid = 9
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (= (len VV_F9) (+ 1 (len lq_anf__dhO))) true)))))))))) (k_48 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 x_ahz))



; cid = 10
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len VV_99) (+ 1 (len lq_anf__dhO))) (and (>= (len VV_99) 0) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_57 EQ_6U False_68 GT_6W LT_6S True_6u VV_F10 lq_anf__dhO x_ahz) true)))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F10 VV_99 x_ahz))



; cid = 11
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 lq_anf__dhO lq_tmp_x46 x_ahz) true)))))))))) (k_45 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 lq_tmp_x46 x_ahz))



; cid = 12
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len VV_102) 0) (and (>= (len VV_102) 0) (and (= VV_102 lq_anf__dhO) (and (>= (len VV_102) 0) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_50 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 x_ahz) true)))))))))))))) (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 lq_anf__dhO x_ahz x_ahz))



; cid = 12
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len VV_102) 0) (and (>= (len VV_102) 0) (and (= VV_102 lq_anf__dhO) (and (>= (len VV_102) 0) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_50 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 x_ahz) true)))))))))))))) (k_57 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 lq_anf__dhO x_ahz))



; cid = 13
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_52 EQ_6U False_68 GT_6W LT_6S True_6u VV_F13 lq_tmp_x60 x_ahz) true)))))))))) (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F13 lq_anf__dhO lq_tmp_x60 x_ahz))



; cid = 14
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_ahz) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len lq_anf__dhO) 0) (and (>= (len lq_anf__dhO) 0) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F14) (and (= VV_F14 x_ahz) true))))))))))) (k_57 EQ_6U False_68 GT_6W LT_6S True_6u VV_F14 lq_anf__dhO x_ahz))

)