(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_64 Int))
:extrafuns ((VV_67 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))
:extrafuns ((VV_F2 Int))

:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))

:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))

:extrafuns ((VV_F9 Int))
:extrafuns ((dummy.pos.grty0.hs.10.22 Int))

:extrafuns ((lq_anf__db3 Int))
:extrafuns ((lq_tmp_x31 Int))

:extrafuns ((lq_tmp_x45 Int))
:extrafuns ((stupid_r9G Int))

:extrafuns ((x Int))
:extrafuns ((x_aaK Int))
:extrafuns ((x_aaK Int))

:extrafuns ((stupid_r9G Int))
:extrafuns ((lq_tmp_x45 Int))

:extrafuns ((lq_tmp_x38 Int))
:extrafuns ((lq_tmp_x31 Int))

:extrafuns ((lq_anf__db3 Int))
:extrafuns ((VV_50 Int))

:extrafuns ((VV_47 Int))
:extrafuns ((VV_43 Int))
:extrafuns ((VV_41 Int))

:extrafuns ((VV_36 Int))
:extrafuns ((VV_34 Int))
:extrafuns ((VV_32 Int))

:extrafuns ((VV_29 Int))
:extrafuns ((VV_27 Int))
:extrafuns ((VV_24 Int))

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

:extrapreds ((k_51 Int Int Int Int))

:extrapreds ((k_48 Int Int Int Int))

:extrapreds ((k_44 Int Int Int Int Int Int Int))

:extrapreds ((k_42 Int Int Int Int Int Int))

:extrapreds ((k_37 Int Int Int Int Int Int))

:extrapreds ((k_35 Int Int Int Int Int))

:extrapreds ((k_33 Int Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int Int))

:extrapreds ((k_28 Int Int Int Int Int Int))

:extrapreds ((k_25 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_51 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_48 EQ_6U GT_6W LT_6S VV_F2))



; cid = 3
:assumption
(implies ((and (not (= VV_F3 x)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_48 EQ_6U GT_6W LT_6S VV_F3) (and (= VV_F3 x) true))))))) false)



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_25 EQ_6U GT_6W LT_6S VV_F4))



; cid = 5
:assumption
(implies ((and (not (> (len VV_F5) 0)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_33 EQ_6U GT_6W LT_6S VV_F5 dummy.pos.grty0.hs.10.22) true)))))) false)



; cid = 6
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (= (len VV_F6) (+ 1 (len lq_anf__db3))) true)))))))) (k_33 EQ_6U GT_6W LT_6S VV_F6 x_aaK))



; cid = 7
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_64) (+ 1 (len lq_anf__db3))) (and (>= (len VV_64) 0) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_42 EQ_6U GT_6W LT_6S VV_F7 lq_anf__db3 x_aaK) true)))))))))) (k_28 EQ_6U GT_6W LT_6S VV_F7 VV_64 x_aaK))



; cid = 8
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_44 EQ_6U GT_6W LT_6S VV_F8 lq_anf__db3 lq_tmp_x31 x_aaK) true)))))))) (k_30 EQ_6U GT_6W LT_6S VV_F8 lq_tmp_x31 x_aaK))



; cid = 9
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_67) 0) (and (>= (len VV_67) 0) (and (= VV_67 lq_anf__db3) (and (>= (len VV_67) 0) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_35 EQ_6U GT_6W LT_6S VV_F9 x_aaK) true)))))))))))) (k_44 EQ_6U GT_6W LT_6S VV_F9 lq_anf__db3 x_aaK x_aaK))



; cid = 9
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_67) 0) (and (>= (len VV_67) 0) (and (= VV_67 lq_anf__db3) (and (>= (len VV_67) 0) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_35 EQ_6U GT_6W LT_6S VV_F9 x_aaK) true)))))))))))) (k_42 EQ_6U GT_6W LT_6S VV_F9 lq_anf__db3 x_aaK))



; cid = 10
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_37 EQ_6U GT_6W LT_6S VV_F10 lq_tmp_x45 x_aaK) true)))))))) (k_44 EQ_6U GT_6W LT_6S VV_F10 lq_anf__db3 lq_tmp_x45 x_aaK))



; cid = 11
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len lq_anf__db3) 0) (and (>= (len lq_anf__db3) 0) (and (k_25 EQ_6U GT_6W LT_6S VV_F11) (and (= VV_F11 x_aaK) true))))))))) (k_42 EQ_6U GT_6W LT_6S VV_F11 lq_anf__db3 x_aaK))

)