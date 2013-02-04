(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_102 Int))
:extrafuns ((VV_86 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F13 Int))

:extrafuns ((VV_F14 Int))
:extrafuns ((VV_F15 Int))

:extrafuns ((VV_F16 Int))
:extrafuns ((VV_F17 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))

:extrafuns ((VV_F9 Int))
:extrafuns ((baz_rkm Int))

:extrafuns ((chk_rkp Int))
:extrafuns ((choose_rh Int))

:extrafuns ((ds_dp1 Int))
:extrafuns ((incr_rkl Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((lq_anf__dp3 Int))

:extrafuns ((lq_anf__dp4 Int))
:extrafuns ((lq_anf__dp5 Int))

:extrafuns ((lq_anf__dp6 Int))
:extrafuns ((lq_anf__dp7 Int))

:extrafuns ((lq_anf__dp8 Int))
:extrafuns ((n_rkn Int))

:extrafuns ((x_aoI Int))
:extrafuns ((x_aoJ Int))
:extrafuns ((x_aoK Int))

:extrafuns ((y_aoL Int))
:extrafuns ((y_aoL Int))
:extrafuns ((x_aoK Int))

:extrafuns ((x_aoJ Int))
:extrafuns ((x_aoI Int))
:extrafuns ((n_rkn Int))

:extrafuns ((lq_anf__dp6 Int))
:extrafuns ((lq_anf__dp5 Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((incr_rkl Int))

:extrafuns ((ds_dp1 Int))
:extrafuns ((choose_rh Int))

:extrafuns ((chk_rkp Int))
:extrafuns ((baz_rkm Int))

:extrafuns ((VV_80 Int))
:extrafuns ((VV_76 Int))
:extrafuns ((VV_70 Int))

:extrafuns ((VV_67 Int))
:extrafuns ((VV_65 Int))
:extrafuns ((VV_63 Int))

:extrafuns ((VV_60 Int))
:extrafuns ((VV_58 Int))
:extrafuns ((VV_55 Int))

:extrafuns ((VV_53 Int))
:extrafuns ((VV_51 Int))
:extrafuns ((VV_48 Int))

:extrafuns ((VV_44 Int))
:extrafuns ((VV_41 Int))
:extrafuns ((VV_37 Int))

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
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))


; constant 
:extrafuns ((P_rkk Int Int Int))

:extrapreds ((k_81 Int Int Int Int Int))

:extrapreds ((k_77 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_71 Int Int Int Int Int Int))

:extrapreds ((k_68 Int Int Int Int Int))

:extrapreds ((k_66 Int Int Int Int Int Int))

:extrapreds ((k_64 Int Int Int Int Int Int))

:extrapreds ((k_61 Int Int Int Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int Int Int))

:extrapreds ((k_56 Int Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int Int Int Int))

:extrapreds ((k_52 Int Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int Int))

:extrapreds ((k_45 Int Int Int Int Int Int))

:extrapreds ((k_42 Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int))


; cid = 16
:assumption
(implies ((and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_42 EQ_6U GT_6W LT_6S x_aoI n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp4 1) (and (= VV_F16 (+ x_aoI lq_anf__dp4)) true)))))))) (k_45 EQ_6U GT_6W LT_6S VV_F16 n_rkn x_aoI))



; cid = 1
:assumption
(implies ((and (k_56 EQ_6U GT_6W LT_6S lq_anf__dp8 n_rkn n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_71 EQ_6U GT_6W LT_6S VV_F1 lq_anf__dp8 n_rkn) true))))))) (k_81 EQ_6U GT_6W LT_6S VV_F1 n_rkn))



; cid = 17
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp3 0) true))))) (k_38 EQ_6U GT_6W LT_6S VV_F17))



; cid = 2
:assumption
(implies ((and (k_56 EQ_6U GT_6W LT_6S lq_anf__dp8 n_rkn n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_56 EQ_6U GT_6W LT_6S VV_F2 n_rkn n_rkn) (and (= VV_F2 lq_anf__dp8) true)))))))) (k_68 EQ_6U GT_6W LT_6S VV_F2 n_rkn))



; cid = 3
:assumption
(implies ((and (k_56 EQ_6U GT_6W LT_6S VV_86 n_rkn n_rkn) (and (k_56 EQ_6U GT_6W LT_6S lq_anf__dp8 n_rkn n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= VV_86 lq_anf__dp8) (and (k_52 EQ_6U GT_6W LT_6S VV_F3 VV_86 n_rkn n_rkn) true))))))))) (k_64 EQ_6U GT_6W LT_6S VV_F3 VV_86 n_rkn))



; cid = 4
:assumption
(implies ((and (k_56 EQ_6U GT_6W LT_6S VV_86 n_rkn n_rkn) (and (k_56 EQ_6U GT_6W LT_6S lq_anf__dp8 n_rkn n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= VV_86 lq_anf__dp8) (and (k_54 EQ_6U GT_6W LT_6S VV_F4 VV_86 n_rkn n_rkn) true))))))))) (k_66 EQ_6U GT_6W LT_6S VV_F4 VV_86 n_rkn))



; cid = 5
:assumption
(implies ((and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_38 EQ_6U GT_6W LT_6S VV_F5) (and (= VV_F5 n_rkn) true))))))) (k_49 EQ_6U GT_6W LT_6S VV_F5 n_rkn))



; cid = 6
:assumption
(implies ((and (k_68 EQ_6U GT_6W LT_6S ds_dp1 n_rkn) (and (k_68 EQ_6U GT_6W LT_6S lq_anf__dp6 n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_64 EQ_6U GT_6W LT_6S x_aoK lq_anf__dp6 n_rkn) (and (k_66 EQ_6U GT_6W LT_6S y_aoL lq_anf__dp6 n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp6 ds_dp1) (and (= lq_anf__dp6 (P_rkk x_aoK y_aoL)) (and (and (implies (Prop lq_anf__dp7) (<= x_aoK y_aoL)) (and (implies (<= x_aoK y_aoL) (Prop lq_anf__dp7)) true)) (and (Prop VV_F6) true))))))))))))) (k_71 EQ_6U GT_6W LT_6S VV_F6 ds_dp1 n_rkn))



; cid = 7
:assumption
(implies ((and (not (Prop VV_F7)) (and (k_68 EQ_6U GT_6W LT_6S ds_dp1 n_rkn) (and (k_68 EQ_6U GT_6W LT_6S lq_anf__dp6 n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_64 EQ_6U GT_6W LT_6S x_aoK lq_anf__dp6 n_rkn) (and (k_66 EQ_6U GT_6W LT_6S y_aoL lq_anf__dp6 n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp6 ds_dp1) (and (= lq_anf__dp6 (P_rkk x_aoK y_aoL)) (and (and (implies (Prop lq_anf__dp7) (<= x_aoK y_aoL)) (and (implies (<= x_aoK y_aoL) (Prop lq_anf__dp7)) true)) (and (and (implies (Prop VV_F7) (<= x_aoK y_aoL)) (and (implies (<= x_aoK y_aoL) (Prop VV_F7)) true)) (and (= VV_F7 lq_anf__dp7) true))))))))))))))) false)



; cid = 8
:assumption
(implies ((and (k_68 EQ_6U GT_6W LT_6S ds_dp1 n_rkn) (and (k_68 EQ_6U GT_6W LT_6S lq_anf__dp6 n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_64 EQ_6U GT_6W LT_6S x_aoK lq_anf__dp6 n_rkn) (and (k_66 EQ_6U GT_6W LT_6S y_aoL lq_anf__dp6 n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp6 ds_dp1) (and (= lq_anf__dp6 (P_rkk x_aoK y_aoL)) (and (k_66 EQ_6U GT_6W LT_6S VV_F8 lq_anf__dp6 n_rkn) (and (= VV_F8 y_aoL) true))))))))))))) (k_77 EQ_6U GT_6W LT_6S VV_F8 ds_dp1 lq_anf__dp6 n_rkn x_aoK y_aoL))



; cid = 9
:assumption
(implies ((and (k_68 EQ_6U GT_6W LT_6S ds_dp1 n_rkn) (and (k_68 EQ_6U GT_6W LT_6S lq_anf__dp6 n_rkn) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_64 EQ_6U GT_6W LT_6S x_aoK lq_anf__dp6 n_rkn) (and (k_66 EQ_6U GT_6W LT_6S y_aoL lq_anf__dp6 n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dp6 ds_dp1) (and (= lq_anf__dp6 (P_rkk x_aoK y_aoL)) (and (k_64 EQ_6U GT_6W LT_6S VV_F9 lq_anf__dp6 n_rkn) (and (= VV_F9 x_aoK) true))))))))))))) (k_77 EQ_6U GT_6W LT_6S VV_F9 ds_dp1 lq_anf__dp6 n_rkn x_aoK y_aoL))



; cid = 10
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S lq_anf__dp5 n_rkn x_aoJ) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true))))))) (k_56 EQ_6U GT_6W LT_6S VV_F10 n_rkn x_aoJ))



; cid = 11
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S lq_anf__dp5 n_rkn x_aoJ) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_59 EQ_6U GT_6W LT_6S VV_F11 lq_anf__dp5 n_rkn x_aoJ) true)))))))) (k_52 EQ_6U GT_6W LT_6S VV_F11 VV_102 n_rkn x_aoJ))



; cid = 12
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S lq_anf__dp5 n_rkn x_aoJ) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_61 EQ_6U GT_6W LT_6S VV_F12 lq_anf__dp5 n_rkn x_aoJ) true)))))))) (k_54 EQ_6U GT_6W LT_6S VV_F12 VV_102 n_rkn x_aoJ))



; cid = 13
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S lq_anf__dp5 n_rkn x_aoJ) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_45 EQ_6U GT_6W LT_6S VV_F13 n_rkn x_aoJ) (and (= VV_F13 lq_anf__dp5) true))))))))) (k_61 EQ_6U GT_6W LT_6S VV_F13 lq_anf__dp5 n_rkn x_aoJ))



; cid = 14
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S lq_anf__dp5 n_rkn x_aoJ) (and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_49 EQ_6U GT_6W LT_6S VV_F14 n_rkn) (and (= VV_F14 x_aoJ) true))))))))) (k_59 EQ_6U GT_6W LT_6S VV_F14 lq_anf__dp5 n_rkn x_aoJ))



; cid = 15
:assumption
(implies ((and (k_38 EQ_6U GT_6W LT_6S n_rkn) (and (k_49 EQ_6U GT_6W LT_6S x_aoJ n_rkn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_49 EQ_6U GT_6W LT_6S VV_F15 n_rkn) (and (= VV_F15 x_aoJ) true)))))))) (k_42 EQ_6U GT_6W LT_6S VV_F15 n_rkn))

)