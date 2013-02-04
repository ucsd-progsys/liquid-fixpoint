(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))

:extrafuns ((VV_55 Int))
:extrafuns ((VV_58 Int))
:extrafuns ((VV_65 Int))

:extrafuns ((VV_67 Int))
:extrafuns ((VV_69 Int))
:extrafuns ((VV_71 Int))

:extrafuns ((VV_73 Int))
:extrafuns ((VV_76 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F13 Int))

:extrafuns ((VV_F14 Int))
:extrafuns ((VV_F15 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))

:extrafuns ((VV_F9 Int))
:extrafuns ((hi Int))
:extrafuns ((hi_abH Int))

:extrafuns ((lo Int))
:extrafuns ((lo_abG Int))

:extrafuns ((lq_anf__dbV Int))
:extrafuns ((lq_anf__dbW Int))

:extrafuns ((lq_anf__dbW Int))
:extrafuns ((lq_anf__dbV Int))

:extrafuns ((lo_abG Int))
:extrafuns ((hi_abI Int))

:extrafuns ((hi_abH Int))
:extrafuns ((VV_53 Int))
:extrafuns ((VV_50 Int))

:extrafuns ((VV_48 Int))
:extrafuns ((VV_45 Int))
:extrafuns ((VV_43 Int))

:extrafuns ((VV_41 Int))
:extrafuns ((VV_38 Int))
:extrafuns ((VV_36 Int))

:extrafuns ((VV_33 Int))
:extrafuns ((VV_31 Int))
:extrafuns ((VV_28 Int))

:extrafuns ((VV_26 Int))
:extrafuns ((VV_23 Int))
:extrafuns ((VV_21 Int))

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

:extrapreds ((k_54 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_51 Int Int Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_46 Int Int Int Int Int Int Int))

:extrapreds ((k_44 Int Int Int Int Int Int Int Int))

:extrapreds ((k_42 Int Int Int Int Int Int Int))

:extrapreds ((k_39 Int Int Int Int Int Int))

:extrapreds ((k_37 Int Int Int Int Int Int Int))

:extrapreds ((k_34 Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int Int Int))

:extrapreds ((k_29 Int Int Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int Int))

:extrapreds ((k_22 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_34 EQ_6U GT_6W LT_6S True_6u VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_F2 (fromJust VV_55)) (and (isJust VV_55) true))))))) (k_32 EQ_6U GT_6W LT_6S True_6u VV_F2 VV_55))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (implies (and (isJust lo) (and (isJust VV_F3) true)) (>= (fromJust VV_F3) (fromJust lo))) true)))))) (k_39 EQ_6U GT_6W LT_6S True_6u VV_F3 lo))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (implies (and (isJust lo) (and (isJust VV_58) true)) (>= (fromJust VV_58) (fromJust lo))) (and (= VV_F4 (fromJust VV_58)) (and (isJust VV_58) true)))))))) (k_37 EQ_6U GT_6W LT_6S True_6u VV_F4 VV_58 lo))



; cid = 5
:assumption
(implies ((and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (k_51 EQ_6U GT_6W LT_6S True_6u lq_anf__dbW hi_abH lo_abG lq_anf__dbV) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dbV hi_abH) (and (= lq_anf__dbW lo_abG) true))))))))))) (k_42 EQ_6U GT_6W LT_6S True_6u VV_F5 hi_abH lo_abG))



; cid = 6
:assumption
(implies ((and (not (implies (isJust lq_anf__dbV) (<= VV_F6 (fromJust lq_anf__dbV)))) (and (k_51 EQ_6U GT_6W LT_6S True_6u VV_65 hi_abH lo_abG lq_anf__dbV) (and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (k_51 EQ_6U GT_6W LT_6S True_6u lq_anf__dbW hi_abH lo_abG lq_anf__dbV) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_65 lo_abG) (and (= VV_65 lq_anf__dbW) (and (= lq_anf__dbV hi_abH) (and (= lq_anf__dbW lo_abG) (and (k_49 EQ_6U GT_6W LT_6S True_6u VV_F6 VV_65 hi_abH lo_abG lq_anf__dbV) true)))))))))))))))) false)



; cid = 6
:assumption
(implies ((and (k_51 EQ_6U GT_6W LT_6S True_6u VV_65 hi_abH lo_abG lq_anf__dbV) (and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (k_51 EQ_6U GT_6W LT_6S True_6u lq_anf__dbW hi_abH lo_abG lq_anf__dbV) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_65 lo_abG) (and (= VV_65 lq_anf__dbW) (and (= lq_anf__dbV hi_abH) (and (= lq_anf__dbW lo_abG) (and (k_49 EQ_6U GT_6W LT_6S True_6u VV_F6 VV_65 hi_abH lo_abG lq_anf__dbV) true))))))))))))))) (k_54 EQ_6U GT_6W LT_6S True_6u VV_F6 hi_abH lo_abG lq_anf__dbV lq_anf__dbW))



; cid = 7
:assumption
(implies ((and (k_46 EQ_6U GT_6W LT_6S True_6u VV_67 hi_abH lo_abG) (and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (k_51 EQ_6U GT_6W LT_6S True_6u lq_anf__dbW hi_abH lo_abG lq_anf__dbV) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_67 hi_abH) (and (= VV_67 lq_anf__dbV) (and (= lq_anf__dbV hi_abH) (and (= lq_anf__dbW lo_abG) (and (k_44 EQ_6U GT_6W LT_6S True_6u VV_F7 VV_67 hi_abH lo_abG) true))))))))))))))) (k_54 EQ_6U GT_6W LT_6S True_6u VV_F7 hi_abH lo_abG lq_anf__dbV lq_anf__dbW))



; cid = 8
:assumption
(implies ((and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dbV hi_abH) (and (k_34 EQ_6U GT_6W LT_6S True_6u VV_F8) (and (= VV_F8 lo_abG) true))))))))))) (k_51 EQ_6U GT_6W LT_6S True_6u VV_F8 hi_abH lo_abG lq_anf__dbV))



; cid = 9
:assumption
(implies ((and (k_34 EQ_6U GT_6W LT_6S True_6u VV_69) (and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (k_46 EQ_6U GT_6W LT_6S True_6u lq_anf__dbV hi_abH lo_abG) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_69 lo_abG) (and (= lq_anf__dbV hi_abH) (and (k_32 EQ_6U GT_6W LT_6S True_6u VV_F9 VV_69) true)))))))))))) (k_49 EQ_6U GT_6W LT_6S True_6u VV_F9 VV_69 hi_abH lo_abG lq_anf__dbV))



; cid = 10
:assumption
(implies ((and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (k_39 EQ_6U GT_6W LT_6S True_6u VV_F10 lo_abG) (and (= VV_F10 hi_abH) true))))))))) (k_46 EQ_6U GT_6W LT_6S True_6u VV_F10 hi_abH lo_abG))



; cid = 11
:assumption
(implies ((and (k_39 EQ_6U GT_6W LT_6S True_6u VV_71 lo_abG) (and (k_39 EQ_6U GT_6W LT_6S True_6u hi_abH lo_abG) (and (k_34 EQ_6U GT_6W LT_6S True_6u lo_abG) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= VV_71 hi_abH) (and (k_37 EQ_6U GT_6W LT_6S True_6u VV_F11 VV_71 lo_abG) true)))))))))) (k_44 EQ_6U GT_6W LT_6S True_6u VV_F11 VV_71 hi_abH lo_abG))



; cid = 12
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_24 EQ_6U GT_6W LT_6S True_6u VV_F12))



; cid = 13
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_22 EQ_6U GT_6W LT_6S True_6u VV_F13 VV_73))



; cid = 14
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))) (k_29 EQ_6U GT_6W LT_6S True_6u VV_F14 hi))



; cid = 15
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (implies (isJust hi) (<= VV_F15 (fromJust hi))) true)))))) (k_27 EQ_6U GT_6W LT_6S True_6u VV_F15 VV_76 hi))

)