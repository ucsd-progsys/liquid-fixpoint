(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_69 Int))

:extrafuns ((VV_71 Int))
:extrafuns ((VV_75 Int))
:extrafuns ((VV_78 Int))

:extrafuns ((VV_82 Int))
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

:extrafuns ((ds_deo Int))
:extrafuns ((ds_dep Int))

:extrafuns ((ds_deq Int))
:extrafuns ((ds_der Int))

:extrafuns ((fail_des Int))
:extrafuns ((lq_anf__deA Int))

:extrafuns ((lq_anf__deB Int))
:extrafuns ((lq_anf__dex Int))

:extrafuns ((lq_anf__dey Int))
:extrafuns ((lq_anf__dez Int))

:extrafuns ((lq_tmp_x31 Int))
:extrafuns ((lq_tmp_x41 Int))

:extrafuns ((realWorld__0f Int))
:extrafuns ((x_aec Int))

:extrafuns ((realWorld__0f Int))
:extrafuns ((lq_tmp_x41 Int))

:extrafuns ((lq_tmp_x38 Int))
:extrafuns ((lq_tmp_x31 Int))

:extrafuns ((ds_det Int))
:extrafuns ((ds_deo Int))
:extrafuns ((VV_52 Int))

:extrafuns ((VV_49 Int))
:extrafuns ((VV_47 Int))
:extrafuns ((VV_45 Int))

:extrafuns ((VV_42 Int))
:extrafuns ((VV_39 Int))
:extrafuns ((VV_36 Int))

:extrafuns ((VV_34 Int))
:extrafuns ((VV_32 Int))
:extrafuns ((VV_29 Int))

:extrafuns ((VV_27 Int))
:extrafuns ((VV_25 Int))
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
:extrafuns ((fix__91__93__35_6m  Int))


; constant 
:extrafuns ((fix__58__35_64 Int Int Int))


; constant 
:extrafuns ((fix__40__44__35_74 Int Int Int))


; constant 
:extrafuns ((fix__36__36__34_pos_47_grty3.hs_58_4_58_1_45_16_124_function_32_bar_34_ Int))


; constant 
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_53 Int Int Int Int Int Int Int))

:extrapreds ((k_50 Int Int Int Int Int Int))

:extrapreds ((k_48 Int Int Int Int Int Int Int))

:extrapreds ((k_46 Int Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int))

:extrapreds ((k_37 Int Int Int Int Int Int))

:extrapreds ((k_35 Int Int Int Int Int Int Int))

:extrapreds ((k_33 Int Int Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int Int Int))

:extrapreds ((k_28 Int Int Int Int Int Int Int))

:extrapreds ((k_26 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_43 EQ_6U GT_6W LT_6S VV_F1 realWorld__0f))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_26 EQ_6U GT_6W LT_6S VV_F2 VV_69 realWorld__0f))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_33 EQ_6U GT_6W LT_6S VV_F3 VV_69 realWorld__0f))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_71) 0) true))))) (k_28 EQ_6U GT_6W LT_6S VV_F4 VV_71 VV_69 realWorld__0f))



; cid = 5
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_30 EQ_6U GT_6W LT_6S VV_F5 VV_69 lq_tmp_x31 realWorld__0f))



; cid = 6
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_40 EQ_6U GT_6W LT_6S VV_F6 lq_tmp_x41 realWorld__0f))



; cid = 7
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_75) 0) true))))) (k_35 EQ_6U GT_6W LT_6S VV_F7 VV_75 lq_tmp_x41 realWorld__0f))



; cid = 8
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_37 EQ_6U GT_6W LT_6S VV_F8 lq_tmp_x38 realWorld__0f))



; cid = 9
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S lq_anf__deA VV_39 ds_dep realWorld__0f) (and (k_30 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dex x_aec realWorld__0f) (and (k_37 EQ_6U GT_6W LT_6S lq_anf__deA ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S x_aec lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S x_aec VV_39 ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_deq) 0) (and (>= (len ds_der) 0) (and (>= (len lq_anf__deB) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__58__35_64 x_aec ds_der)) (and (= (len lq_anf__dey) (+ 1 (len ds_der))) (and (>= (len lq_anf__dey) 0) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez ds_der) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez (fix__58__35_64 lq_anf__deA lq_anf__deB)) (and (= (len lq_anf__dez) (+ 1 (len lq_anf__deB))) (and (>= (len lq_anf__dez) 0) (and (k_53 EQ_6U GT_6W LT_6S VV_F9 ds_deo realWorld__0f realWorld__0f) true))))))))))))))))))))))))))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F9 ds_deo realWorld__0f))



; cid = 10
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S lq_anf__deA VV_39 ds_dep realWorld__0f) (and (k_30 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dex x_aec realWorld__0f) (and (k_37 EQ_6U GT_6W LT_6S lq_anf__deA ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S x_aec lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S x_aec VV_39 ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_deq) 0) (and (>= (len ds_der) 0) (and (>= (len lq_anf__deB) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__58__35_64 x_aec ds_der)) (and (= (len lq_anf__dey) (+ 1 (len ds_der))) (and (>= (len lq_anf__dey) 0) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez ds_der) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez (fix__58__35_64 lq_anf__deA lq_anf__deB)) (and (= (len lq_anf__dez) (+ 1 (len lq_anf__deB))) (and (>= (len lq_anf__dez) 0) (and (= VV_F10 realWorld__0f) true))))))))))))))))))))))))))))))))))) (k_50 EQ_6U GT_6W LT_6S VV_F10 ds_deo realWorld__0f))



; cid = 11
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S lq_anf__deA VV_39 ds_dep realWorld__0f) (and (k_30 EQ_6U GT_6W LT_6S lq_anf__deA lq_anf__dex x_aec realWorld__0f) (and (k_37 EQ_6U GT_6W LT_6S lq_anf__deA ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S x_aec lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S x_aec VV_39 ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= VV_78 realWorld__0f) (and (>= (len ds_deq) 0) (and (>= (len ds_der) 0) (and (>= (len lq_anf__deB) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__58__35_64 x_aec ds_der)) (and (= (len lq_anf__dey) (+ 1 (len ds_der))) (and (>= (len lq_anf__dey) 0) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez ds_der) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez (fix__58__35_64 lq_anf__deA lq_anf__deB)) (and (= (len lq_anf__dez) (+ 1 (len lq_anf__deB))) (and (>= (len lq_anf__dez) 0) true))))))))))))))))))))))))))))))))))) (k_48 EQ_6U GT_6W LT_6S VV_F11 VV_78 ds_deo realWorld__0f))



; cid = 12
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (k_28 EQ_6U GT_6W LT_6S x_aec lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S x_aec VV_39 ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_deq) 0) (and (>= (len ds_der) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__58__35_64 x_aec ds_der)) (and (= (len lq_anf__dey) (+ 1 (len ds_der))) (and (>= (len lq_anf__dey) 0) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez ds_der) (and (>= (len lq_anf__dez) 0) (and (= lq_anf__dez (fix__91__93__35_6m )) (and (= (len lq_anf__dez) 0) (and (>= (len lq_anf__dez) 0) (and (k_28 EQ_6U GT_6W LT_6S VV_F12 lq_anf__dey lq_anf__dex realWorld__0f) (and (k_35 EQ_6U GT_6W LT_6S VV_F12 VV_39 ds_dep realWorld__0f) (and (= VV_F12 x_aec) true)))))))))))))))))))))))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F12 ds_deo realWorld__0f))



; cid = 13
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_deq) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__91__93__35_6m )) (and (= (len lq_anf__dey) 0) (and (>= (len lq_anf__dey) 0) (and (k_53 EQ_6U GT_6W LT_6S VV_F13 ds_deo realWorld__0f realWorld__0f) true))))))))))))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F13 ds_deo realWorld__0f))



; cid = 14
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_deq) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__91__93__35_6m )) (and (= (len lq_anf__dey) 0) (and (>= (len lq_anf__dey) 0) (and (= VV_F14 realWorld__0f) true))))))))))))))))))))) (k_50 EQ_6U GT_6W LT_6S VV_F14 ds_deo realWorld__0f))



; cid = 15
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_deo realWorld__0f) (and (k_26 EQ_6U GT_6W LT_6S ds_dep lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S ds_deq lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S ds_deq ds_dep realWorld__0f) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dex realWorld__0f) (and (k_33 EQ_6U GT_6W LT_6S lq_anf__dey lq_anf__dex realWorld__0f) (and (k_40 EQ_6U GT_6W LT_6S lq_anf__dey ds_dep realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= VV_82 realWorld__0f) (and (>= (len ds_deq) 0) (and (= lq_anf__dex ds_deo) (and (= lq_anf__dex (fix__40__44__35_74 ds_dep ds_deq)) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey ds_deq) (and (>= (len lq_anf__dey) 0) (and (= lq_anf__dey (fix__91__93__35_6m )) (and (= (len lq_anf__dey) 0) (and (>= (len lq_anf__dey) 0) true))))))))))))))))))))) (k_48 EQ_6U GT_6W LT_6S VV_F15 VV_82 ds_deo realWorld__0f))

)