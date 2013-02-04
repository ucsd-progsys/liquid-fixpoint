(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((False_68 Int))
:extrafuns ((GT_6W Int))

:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))
:extrafuns ((VV_55 Int))

:extrafuns ((VV_58 Int))
:extrafuns ((VV_64 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))

:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))

:extrafuns ((ds_dnk Int))
:extrafuns ((ds_dnn Int))

:extrafuns ((lq_anf__dnr Int))
:extrafuns ((lq_anf__dns Int))

:extrafuns ((realWorld__0f Int))
:extrafuns ((x Int))

:extrafuns ((x_an5 Int))
:extrafuns ((x_an6 Int))

:extrafuns ((realWorld__0f Int))
:extrafuns ((lq_anf__dns Int))

:extrafuns ((ds_dnp Int))
:extrafuns ((ds_dnn Int))

:extrafuns ((ds_dnk Int))
:extrafuns ((VV_51 Int))
:extrafuns ((VV_48 Int))

:extrafuns ((VV_46 Int))
:extrafuns ((VV_42 Int))
:extrafuns ((VV_39 Int))

:extrafuns ((VV_37 Int))
:extrafuns ((VV_31 Int))
:extrafuns ((VV_28 Int))

:extrafuns ((VV_26 Int))
:extrafuns ((True_6u Int))
:extrafuns ((LT_6S Int))

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
:extrafuns ((fix__36__36__34_pos_47_maybe0.hs_58_7_58_1_45_17_124_function_32_foo_34_ Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))


; constant 
:extrafuns ((Nothing_r7s  Int))


; constant 
:extrafuns ((Just_r7t Int Int))

:extrapreds ((k_52 Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_47 Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int Int Int Int Int))

:extrapreds ((k_29 Int Int Int Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1 realWorld__0f))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_38 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2 VV_55 realWorld__0f))



; cid = 3
:assumption
(implies ((and (not (implies (isJust x) (= (fromJust x) VV_F3))) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 x realWorld__0f) true)))))))) false)



; cid = 4
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnn realWorld__0f) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dns realWorld__0f) (and (k_38 EQ_6U False_68 GT_6W LT_6S True_6u x_an5 lq_anf__dns realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dns ds_dnn) (and (= lq_anf__dns (Just_r7t x_an5)) (and (= (fromJust lq_anf__dns) x_an5) (and (and (implies (isJust lq_anf__dns) true) (and (implies true (isJust lq_anf__dns)) true)) (and (k_38 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 lq_anf__dns realWorld__0f) (and (= VV_F4 x_an5) true))))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 ds_dnn realWorld__0f))



; cid = 5
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnn realWorld__0f) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dns realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dns ds_dnn) (and (= lq_anf__dns (Nothing_r7s )) (and (and (implies (isJust lq_anf__dns) false) (and (implies false (isJust lq_anf__dns)) true)) (and (k_52 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 ds_dnn realWorld__0f lq_anf__dns realWorld__0f) true)))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 ds_dnn realWorld__0f))



; cid = 6
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnn realWorld__0f) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dns realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dns ds_dnn) (and (= lq_anf__dns (Nothing_r7s )) (and (and (implies (isJust lq_anf__dns) false) (and (implies false (isJust lq_anf__dns)) true)) (and (= VV_F6 realWorld__0f) true)))))))))))) (k_49 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 ds_dnn lq_anf__dns realWorld__0f))



; cid = 7
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnn realWorld__0f) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dns realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_58 realWorld__0f) (and (= lq_anf__dns ds_dnn) (and (= lq_anf__dns (Nothing_r7s )) (and (and (implies (isJust lq_anf__dns) false) (and (implies false (isJust lq_anf__dns)) true)) true)))))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 VV_58 ds_dnn lq_anf__dns realWorld__0f))



; cid = 8
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_29 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 realWorld__0f))



; cid = 9
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_27 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 VV_64 realWorld__0f))



; cid = 10
:assumption
(implies ((and (not (and (implies (isJust x) (Prop VV_F10)) (and (implies (Prop VV_F10) (isJust x)) true))) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F10 x realWorld__0f) true)))))))) false)



; cid = 11
:assumption
(implies ((and (k_29 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnk realWorld__0f) (and (k_29 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dnr realWorld__0f) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u x_an6 lq_anf__dnr realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dnr ds_dnk) (and (= lq_anf__dnr (Just_r7t x_an6)) (and (= (fromJust lq_anf__dnr) x_an6) (and (and (implies (isJust lq_anf__dnr) true) (and (implies true (isJust lq_anf__dnr)) true)) (and (Prop VV_F11) (and (= VV_F11 True_6u) true))))))))))))))) (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 ds_dnk realWorld__0f))



; cid = 12
:assumption
(implies ((and (k_29 EQ_6U False_68 GT_6W LT_6S True_6u ds_dnk realWorld__0f) (and (k_29 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dnr realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dnr ds_dnk) (and (= lq_anf__dnr (Nothing_r7s )) (and (and (implies (isJust lq_anf__dnr) false) (and (implies false (isJust lq_anf__dnr)) true)) (and (not (Prop VV_F12)) (and (= VV_F12 False_68) true))))))))))))) (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 ds_dnk realWorld__0f))

)