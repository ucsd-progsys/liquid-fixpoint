(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_102 Int))
:extrafuns ((VV_111 Int))

:extrafuns ((VV_124 Int))
:extrafuns ((VV_127 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F13 Int))

:extrafuns ((VV_F14 Int))
:extrafuns ((VV_F15 Int))

:extrafuns ((VV_F16 Int))
:extrafuns ((VV_F17 Int))

:extrafuns ((VV_F18 Int))
:extrafuns ((VV_F19 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))

:extrafuns ((VV_F9 Int))
:extrafuns ((choose_rh Int))

:extrafuns ((ds_dsy Int))
:extrafuns ((ds_dsz Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((lq_anf__dsC Int))

:extrafuns ((lq_anf__dsD Int))
:extrafuns ((lq_anf__dsE Int))

:extrafuns ((lq_anf__dsF Int))
:extrafuns ((lq_anf__dsG Int))

:extrafuns ((lq_anf__dsH Int))
:extrafuns ((lq_anf__dsI Int))

:extrafuns ((lq_tmp_x36 Int))
:extrafuns ((lq_tmp_x55 Int))

:extrafuns ((lq_tmp_x69 Int))
:extrafuns ((lq_tmp_x76 Int))

:extrafuns ((x_asd Int))
:extrafuns ((x_ase Int))
:extrafuns ((xs_roE Int))

:extrafuns ((xs_roE Int))
:extrafuns ((x_asd Int))

:extrafuns ((lq_tmp_x76 Int))
:extrafuns ((lq_tmp_x69 Int))

:extrafuns ((lq_tmp_x62 Int))
:extrafuns ((lq_tmp_x55 Int))

:extrafuns ((lq_tmp_x36 Int))
:extrafuns ((lq_anf__dsH Int))

:extrafuns ((lq_anf__dsG Int))
:extrafuns ((lq_anf__dsF Int))

:extrafuns ((lq_anf__dsE Int))
:extrafuns ((lq_anf__dsD Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((ds_dsy Int))

:extrafuns ((choose_rh Int))
:extrafuns ((VV_90 Int))

:extrafuns ((VV_87 Int))
:extrafuns ((VV_84 Int))
:extrafuns ((VV_81 Int))

:extrafuns ((VV_77 Int))
:extrafuns ((VV_74 Int))
:extrafuns ((VV_72 Int))

:extrafuns ((VV_67 Int))
:extrafuns ((VV_65 Int))
:extrafuns ((VV_60 Int))

:extrafuns ((VV_58 Int))
:extrafuns ((VV_56 Int))
:extrafuns ((VV_53 Int))

:extrafuns ((VV_51 Int))
:extrafuns ((VV_48 Int))
:extrafuns ((VV_40 Int))

:extrafuns ((VV_37 Int))
:extrafuns ((VV_34 Int))
:extrafuns ((VV_32 Int))

:extrafuns ((LT_6S Int))
:extrafuns ((I__6c Int))
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
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_91 Int Int Int Int Int Int Int))

:extrapreds ((k_88 Int Int Int Int Int Int))

:extrapreds ((k_85 Int Int Int Int Int))

:extrapreds ((k_82 Int Int Int Int Int Int))

:extrapreds ((k_78 Int Int Int Int))

:extrapreds ((k_75 Int Int Int Int Int))

:extrapreds ((k_73 Int Int Int Int Int))

:extrapreds ((k_68 Int Int Int Int Int Int Int))

:extrapreds ((k_66 Int Int Int Int Int Int))

:extrapreds ((k_61 Int Int Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int))

:extrapreds ((k_57 Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int Int Int))

:extrapreds ((k_52 Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int))

:extrapreds ((k_41 Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int))

:extrapreds ((k_35 Int Int Int Int Int))

:extrapreds ((k_33 Int Int Int Int Int))


; cid = 16
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_61 EQ_6U GT_6W LT_6S VV_F16 lq_tmp_x69 x_asd) true)))))))) (k_68 EQ_6U GT_6W LT_6S VV_F16 lq_anf__dsD lq_tmp_x69 x_asd))



; cid = 1
:assumption
(implies ((and (k_41 EQ_6U GT_6W LT_6S lq_anf__dsH xs_roE) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (and (implies (Prop lq_anf__dsI) (= lq_anf__dsG lq_anf__dsH)) (and (implies (= lq_anf__dsG lq_anf__dsH) (Prop lq_anf__dsI)) true)) (and (>= (len xs_roE) 0) (and (Prop VV_F1) true)))))))))) (k_85 EQ_6U GT_6W LT_6S VV_F1 xs_roE))



; cid = 17
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_49 EQ_6U GT_6W LT_6S VV_F17) (and (= VV_F17 x_asd) true))))))))) (k_66 EQ_6U GT_6W LT_6S VV_F17 lq_anf__dsD x_asd))



; cid = 2
:assumption
(implies ((and (not (Prop VV_F2)) (and (k_41 EQ_6U GT_6W LT_6S lq_anf__dsH xs_roE) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (and (implies (Prop lq_anf__dsI) (= lq_anf__dsG lq_anf__dsH)) (and (implies (= lq_anf__dsG lq_anf__dsH) (Prop lq_anf__dsI)) true)) (and (>= (len xs_roE) 0) (and (and (implies (Prop VV_F2) (= lq_anf__dsG lq_anf__dsH)) (and (implies (= lq_anf__dsG lq_anf__dsH) (Prop VV_F2)) true)) (and (= VV_F2 lq_anf__dsI) true)))))))))))) false)



; cid = 18
:assumption
(implies ((and (k_38 EQ_6U GT_6W LT_6S ds_dsy) (and (k_38 EQ_6U GT_6W LT_6S lq_anf__dsC) (and (k_33 EQ_6U GT_6W LT_6S x_ase lq_anf__dsC) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dsy) 0) (and (>= (len ds_dsz) 0) (and (>= (len lq_anf__dsC) 0) (and (= lq_anf__dsC ds_dsy) (and (>= (len lq_anf__dsC) 0) (and (= lq_anf__dsC (fix__58__35_64 x_ase ds_dsz)) (and (= (len lq_anf__dsC) (+ 1 (len ds_dsz))) (and (>= (len lq_anf__dsC) 0) (and (= VV_F18 1) true)))))))))))))))) (k_41 EQ_6U GT_6W LT_6S VV_F18 ds_dsy))



; cid = 3
:assumption
(implies ((and (k_41 EQ_6U GT_6W LT_6S lq_anf__dsH xs_roE) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (k_41 EQ_6U GT_6W LT_6S VV_F3 xs_roE) (and (= VV_F3 lq_anf__dsH) true)))))))))) (k_91 EQ_6U GT_6W LT_6S VV_F3 lq_anf__dsG lq_anf__dsH xs_roE))



; cid = 19
:assumption
(implies ((and (k_38 EQ_6U GT_6W LT_6S ds_dsy) (and (k_38 EQ_6U GT_6W LT_6S lq_anf__dsC) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dsy) 0) (and (>= (len lq_anf__dsC) 0) (and (= lq_anf__dsC ds_dsy) (and (>= (len lq_anf__dsC) 0) (and (= lq_anf__dsC (fix__91__93__35_6m )) (and (= (len lq_anf__dsC) 0) (and (>= (len lq_anf__dsC) 0) (and (= VV_F19 0) true)))))))))))))) (k_41 EQ_6U GT_6W LT_6S VV_F19 ds_dsy))



; cid = 4
:assumption
(implies ((and (k_41 EQ_6U GT_6W LT_6S lq_anf__dsH xs_roE) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (= VV_F4 1) (and (= VV_F4 lq_anf__dsG) true)))))))))) (k_91 EQ_6U GT_6W LT_6S VV_F4 lq_anf__dsG lq_anf__dsH xs_roE))



; cid = 5
:assumption
(implies ((and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (k_78 EQ_6U GT_6W LT_6S VV_F5) (and (>= (len VV_F5) 0) (and (= VV_F5 xs_roE) true)))))))))) (k_38 EQ_6U GT_6W LT_6S VV_F5))



; cid = 6
:assumption
(implies ((and (k_78 EQ_6U GT_6W LT_6S VV_102) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_102) 0) (and (= VV_102 xs_roE) (and (>= (len VV_102) 0) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (k_73 EQ_6U GT_6W LT_6S VV_F6 VV_102) true)))))))))))) (k_33 EQ_6U GT_6W LT_6S VV_F6 VV_102))



; cid = 6
:assumption
(implies ((and (k_78 EQ_6U GT_6W LT_6S VV_102) (and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_102) 0) (and (= VV_102 xs_roE) (and (>= (len VV_102) 0) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (k_73 EQ_6U GT_6W LT_6S VV_F6 VV_102) true)))))))))))) (k_88 EQ_6U GT_6W LT_6S VV_F6 lq_anf__dsG xs_roE))



; cid = 7
:assumption
(implies ((and (k_78 EQ_6U GT_6W LT_6S xs_roE) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsG 1) (and (>= (len xs_roE) 0) (and (k_75 EQ_6U GT_6W LT_6S VV_F7 lq_tmp_x36) true)))))))) (k_35 EQ_6U GT_6W LT_6S VV_F7 lq_tmp_x36))



; cid = 8
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsE 0) (and (k_57 EQ_6U GT_6W LT_6S VV_F8 lq_anf__dsF) true)))))) (k_78 EQ_6U GT_6W LT_6S VV_F8))



; cid = 9
:assumption
(implies ((and (k_57 EQ_6U GT_6W LT_6S VV_111 lq_anf__dsF) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_111) 0) (and (= lq_anf__dsE 0) (and (k_82 EQ_6U GT_6W LT_6S VV_F9 lq_anf__dsE lq_anf__dsF) (and (k_52 EQ_6U GT_6W LT_6S VV_F9 VV_111 lq_anf__dsF) true))))))))) (k_73 EQ_6U GT_6W LT_6S VV_F9 VV_111))



; cid = 10
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsE 0) (and (k_54 EQ_6U GT_6W LT_6S VV_F10 lq_tmp_x76 lq_anf__dsF) true)))))) (k_75 EQ_6U GT_6W LT_6S VV_F10 lq_tmp_x76))



; cid = 11
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsE 0) (and (= VV_F11 lq_anf__dsF) true)))))) (k_49 EQ_6U GT_6W LT_6S VV_F11))



; cid = 11
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dsE 0) (and (= VV_F11 lq_anf__dsF) true)))))) (k_82 EQ_6U GT_6W LT_6S VV_F11 lq_anf__dsE lq_anf__dsF))



; cid = 12
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (= (len VV_F12) (+ 1 (len lq_anf__dsD))) true)))))))) (k_57 EQ_6U GT_6W LT_6S VV_F12 x_asd))



; cid = 13
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_124) (+ 1 (len lq_anf__dsD))) (and (>= (len VV_124) 0) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_66 EQ_6U GT_6W LT_6S VV_F13 lq_anf__dsD x_asd) true)))))))))) (k_52 EQ_6U GT_6W LT_6S VV_F13 VV_124 x_asd))



; cid = 14
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_68 EQ_6U GT_6W LT_6S VV_F14 lq_anf__dsD lq_tmp_x55 x_asd) true)))))))) (k_54 EQ_6U GT_6W LT_6S VV_F14 lq_tmp_x55 x_asd))



; cid = 15
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_127) 0) (and (>= (len VV_127) 0) (and (= VV_127 lq_anf__dsD) (and (>= (len VV_127) 0) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_59 EQ_6U GT_6W LT_6S VV_F15 x_asd) true)))))))))))) (k_68 EQ_6U GT_6W LT_6S VV_F15 lq_anf__dsD x_asd x_asd))



; cid = 15
:assumption
(implies ((and (k_49 EQ_6U GT_6W LT_6S x_asd) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_127) 0) (and (>= (len VV_127) 0) (and (= VV_127 lq_anf__dsD) (and (>= (len VV_127) 0) (and (= (len lq_anf__dsD) 0) (and (>= (len lq_anf__dsD) 0) (and (k_59 EQ_6U GT_6W LT_6S VV_F15 x_asd) true)))))))))))) (k_66 EQ_6U GT_6W LT_6S VV_F15 lq_anf__dsD x_asd))

)