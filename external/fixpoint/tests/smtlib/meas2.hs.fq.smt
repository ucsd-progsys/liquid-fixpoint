(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_56 Int))
:extrafuns ((VV_68 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))

:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))
:extrafuns ((ds_dlY Int))

:extrafuns ((ds_dlZ Int))
:extrafuns ((lq_anf__dm2 Int))

:extrafuns ((lq_anf__dm3 Int))
:extrafuns ((lq_anf__dm4 Int))

:extrafuns ((lq_tmp_x31 Int))
:extrafuns ((lq_tmp_x41 Int))

:extrafuns ((xs_alQ Int))
:extrafuns ((xs_alQ Int))

:extrafuns ((lq_tmp_x41 Int))
:extrafuns ((lq_tmp_x31 Int))

:extrafuns ((lq_anf__dm3 Int))
:extrafuns ((lq_anf__dm2 Int))

:extrafuns ((ds_dlZ Int))
:extrafuns ((ds_dlY Int))
:extrafuns ((VV_53 Int))

:extrafuns ((VV_45 Int))
:extrafuns ((VV_42 Int))
:extrafuns ((VV_39 Int))

:extrafuns ((VV_37 Int))
:extrafuns ((VV_34 Int))
:extrafuns ((VV_32 Int))

:extrafuns ((VV_29 Int))
:extrafuns ((VV_27 Int))
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

:extrapreds ((k_54 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_46 Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int Int))

:extrapreds ((k_35 Int Int Int Int Int))

:extrapreds ((k_33 Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int))

:extrapreds ((k_28 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_33 EQ_6U GT_6W LT_6S VV_F1) true))))) (k_43 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S VV_56) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_56) 0) (and (k_28 EQ_6U GT_6W LT_6S VV_F2 VV_56) true))))))) (k_38 EQ_6U GT_6W LT_6S VV_F2 VV_56))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_30 EQ_6U GT_6W LT_6S VV_F3 lq_tmp_x41) true))))) (k_40 EQ_6U GT_6W LT_6S VV_F3 lq_tmp_x41))



; cid = 4
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S ds_dlY) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dlY) 0) (and (k_46 EQ_6U GT_6W LT_6S VV_F4 ds_dlY) true))))))) (k_35 EQ_6U GT_6W LT_6S VV_F4 ds_dlY))



; cid = 5
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_38 EQ_6U GT_6W LT_6S ds_dlZ lq_anf__dm2) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (k_35 EQ_6U GT_6W LT_6S lq_anf__dm4 xs_alQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__58__35_64 ds_dlZ xs_alQ)) (and (= (len lq_anf__dm2) (+ 1 (len xs_alQ))) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm3 1) (and (>= (len xs_alQ) 0) (and (= VV_F5 (+ lq_anf__dm3 lq_anf__dm4)) true)))))))))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F5 ds_dlY))



; cid = 6
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_38 EQ_6U GT_6W LT_6S ds_dlZ lq_anf__dm2) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__58__35_64 ds_dlZ xs_alQ)) (and (= (len lq_anf__dm2) (+ 1 (len xs_alQ))) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm3 1) (and (>= (len xs_alQ) 0) (and (>= (len VV_F6) 0) (and (= VV_F6 xs_alQ) true)))))))))))))))))) (k_33 EQ_6U GT_6W LT_6S VV_F6))



; cid = 7
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_38 EQ_6U GT_6W LT_6S ds_dlZ lq_anf__dm2) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_68) 0) (and (= VV_68 xs_alQ) (and (>= (len VV_68) 0) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__58__35_64 ds_dlZ xs_alQ)) (and (= (len lq_anf__dm2) (+ 1 (len xs_alQ))) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm3 1) (and (>= (len xs_alQ) 0) (and (k_38 EQ_6U GT_6W LT_6S VV_F7 lq_anf__dm2) (and (k_40 EQ_6U GT_6W LT_6S VV_F7 ds_dlZ) true))))))))))))))))))))) (k_28 EQ_6U GT_6W LT_6S VV_F7 VV_68))



; cid = 7
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_38 EQ_6U GT_6W LT_6S ds_dlZ lq_anf__dm2) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_68) 0) (and (= VV_68 xs_alQ) (and (>= (len VV_68) 0) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__58__35_64 ds_dlZ xs_alQ)) (and (= (len lq_anf__dm2) (+ 1 (len xs_alQ))) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm3 1) (and (>= (len xs_alQ) 0) (and (k_38 EQ_6U GT_6W LT_6S VV_F7 lq_anf__dm2) (and (k_40 EQ_6U GT_6W LT_6S VV_F7 ds_dlZ) true))))))))))))))))))))) (k_54 EQ_6U GT_6W LT_6S VV_F7 ds_dlY ds_dlZ lq_anf__dm2 lq_anf__dm3 xs_alQ))



; cid = 8
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_38 EQ_6U GT_6W LT_6S ds_dlZ lq_anf__dm2) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__58__35_64 ds_dlZ xs_alQ)) (and (= (len lq_anf__dm2) (+ 1 (len xs_alQ))) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm3 1) (and (>= (len xs_alQ) 0) (and (k_40 EQ_6U GT_6W LT_6S VV_F8 lq_tmp_x31) true))))))))))))))))) (k_30 EQ_6U GT_6W LT_6S VV_F8 lq_tmp_x31))



; cid = 9
:assumption
(implies ((and (k_43 EQ_6U GT_6W LT_6S ds_dlY) (and (k_43 EQ_6U GT_6W LT_6S lq_anf__dm2) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len ds_dlY) 0) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 ds_dlY) (and (>= (len lq_anf__dm2) 0) (and (= lq_anf__dm2 (fix__91__93__35_6m )) (and (= (len lq_anf__dm2) 0) (and (>= (len lq_anf__dm2) 0) (and (= VV_F9 0) true)))))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F9 ds_dlY))

)