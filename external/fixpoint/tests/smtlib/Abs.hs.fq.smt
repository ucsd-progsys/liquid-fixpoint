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

:extrafuns ((a_asn Int))
:extrafuns ((absI_r9H Int))

:extrafuns ((lq_anf__dsI Int))
:extrafuns ((lq_anf__dsJ Int))

:extrafuns ((lq_anf__dsK Int))
:extrafuns ((lq_anf__dsL Int))

:extrafuns ((lq_anf__dsM Int))
:extrafuns ((lq_anf__dsN Int))

:extrafuns ((lq_anf__dsO Int))
:extrafuns ((lq_anf__dsP Int))

:extrafuns ((lq_anf__dsQ Int))
:extrafuns ((lq_anf__dsR Int))

:extrafuns ((x_asa Int))
:extrafuns ((x_asb Int))
:extrafuns ((x0_r9I Int))

:extrafuns ((x0_r9I Int))
:extrafuns ((x_asb Int))
:extrafuns ((x_asa Int))

:extrafuns ((lq_anf__dsN Int))
:extrafuns ((lq_anf__dsJ Int))

:extrafuns ((lq_anf__dsI Int))
:extrafuns ((absI_r9H Int))

:extrafuns ((a_asn Int))
:extrafuns ((VV_70 Int))
:extrafuns ((VV_63 Int))

:extrafuns ((VV_60 Int))
:extrafuns ((VV_57 Int))
:extrafuns ((VV_53 Int))

:extrafuns ((VV_46 Int))
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
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_71 Int Int Int Int Int Int Int))

:extrapreds ((k_64 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_61 Int Int Int Int Int Int Int Int))

:extrapreds ((k_58 Int Int Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int Int Int))

:extrapreds ((k_47 Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsQ 5) (and (k_61 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1 lq_anf__dsR x0_r9I) true))))))))) (k_71 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1 x0_r9I))



; cid = 2
:assumption
(implies ((and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsQ 5) (and (= VV_F2 lq_anf__dsR) true))))))))) (k_58 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2 x0_r9I))



; cid = 3
:assumption
(implies ((and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u x_asb x0_r9I) (and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsN 0) (and (and (implies (Prop lq_anf__dsO) (> x_asb lq_anf__dsN)) (and (implies (> x_asb lq_anf__dsN) (Prop lq_anf__dsO)) true)) (and (and (implies (Prop lq_anf__dsP) (> x_asb lq_anf__dsN)) (and (implies (> x_asb lq_anf__dsN) (Prop lq_anf__dsP)) true)) (and (= lq_anf__dsP lq_anf__dsO) (and (Prop lq_anf__dsP) (and (Prop lq_anf__dsP) (and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 x0_r9I) (and (= VV_F3 x_asb) true)))))))))))))))) (k_61 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 x_asb x0_r9I))



; cid = 4
:assumption
(implies ((and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u x_asb x0_r9I) (and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsN 0) (and (and (implies (Prop lq_anf__dsO) (> x_asb lq_anf__dsN)) (and (implies (> x_asb lq_anf__dsN) (Prop lq_anf__dsO)) true)) (and (and (implies (Prop lq_anf__dsP) (> x_asb lq_anf__dsN)) (and (implies (> x_asb lq_anf__dsN) (Prop lq_anf__dsP)) true)) (and (= lq_anf__dsP lq_anf__dsO) (and (not (Prop lq_anf__dsP)) (and (not (Prop lq_anf__dsP)) true)))))))))))))) (k_61 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 x_asb x0_r9I))



; cid = 5
:assumption
(implies ((and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u x_asb x0_r9I) (and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsN 0) (and (= VV_F5 0) (and (= VV_F5 lq_anf__dsN) true))))))))))) (k_64 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 lq_anf__dsN x_asb x0_r9I))



; cid = 6
:assumption
(implies ((and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u x_asb x0_r9I) (and (k_54 EQ_6U False_68 GT_6W LT_6S True_6u x0_r9I) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsN 0) (and (k_58 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 x0_r9I) (and (= VV_F6 x_asb) true))))))))))) (k_64 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 lq_anf__dsN x_asb x0_r9I))



; cid = 7
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsM 4) (and (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 a_asn lq_anf__dsM) true)))))))) (k_54 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7))



; cid = 8
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsM 4) (and (= VV_F8 4) (and (= VV_F8 lq_anf__dsM) true))))))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 a_asn))



; cid = 9
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_asa a_asn) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsI 0) (and (= lq_anf__dsJ lq_anf__dsI) (and (and (implies (Prop lq_anf__dsK) (> x_asa lq_anf__dsJ)) (and (implies (> x_asa lq_anf__dsJ) (Prop lq_anf__dsK)) true)) (and (and (implies (Prop lq_anf__dsL) (> x_asa lq_anf__dsJ)) (and (implies (> x_asa lq_anf__dsJ) (Prop lq_anf__dsL)) true)) (and (= lq_anf__dsL lq_anf__dsK) (and (Prop lq_anf__dsL) (and (Prop lq_anf__dsL) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 a_asn) (and (= VV_F9 x_asa) true)))))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 a_asn x_asa))



; cid = 10
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_asa a_asn) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsI 0) (and (= lq_anf__dsJ lq_anf__dsI) (and (and (implies (Prop lq_anf__dsK) (> x_asa lq_anf__dsJ)) (and (implies (> x_asa lq_anf__dsJ) (Prop lq_anf__dsK)) true)) (and (and (implies (Prop lq_anf__dsL) (> x_asa lq_anf__dsJ)) (and (implies (> x_asa lq_anf__dsJ) (Prop lq_anf__dsL)) true)) (and (= lq_anf__dsL lq_anf__dsK) (and (not (Prop lq_anf__dsL)) (and (not (Prop lq_anf__dsL)) true)))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F10 a_asn x_asa))



; cid = 11
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_asa a_asn) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsI 0) (and (= lq_anf__dsJ lq_anf__dsI) (and (= VV_F11 lq_anf__dsI) (and (= VV_F11 lq_anf__dsJ) true))))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 a_asn lq_anf__dsI lq_anf__dsJ x_asa))



; cid = 12
:assumption
(implies ((and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u x_asa a_asn) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dsI 0) (and (= lq_anf__dsJ lq_anf__dsI) (and (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 a_asn) (and (= VV_F12 x_asa) true))))))))))) (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 a_asn lq_anf__dsI lq_anf__dsJ x_asa))

)