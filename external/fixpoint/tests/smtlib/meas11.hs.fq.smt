(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((False_68 Int))
:extrafuns ((GT_6W Int))

:extrafuns ((LT_6S Int))
:extrafuns ((True_6u Int))
:extrafuns ((VV_74 Int))

:extrafuns ((VV_80 Int))
:extrafuns ((VV_83 Int))
:extrafuns ((VV_86 Int))

:extrafuns ((VV_91 Int))
:extrafuns ((VV_94 Int))
:extrafuns ((VV_99 Int))

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

:extrafuns ((VV_F20 Int))
:extrafuns ((VV_F21 Int))

:extrafuns ((VV_F22 Int))
:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))

:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))
:extrafuns ((ds_dct Int))

:extrafuns ((dummy.pos.meas11.hs.5.17 Int))
:extrafuns ((f_acl Int))

:extrafuns ((lq_anf__dcw Int))
:extrafuns ((lq_anf__dcx Int))

:extrafuns ((lq_anf__dcy Int))
:extrafuns ((lq_anf__dcz Int))

:extrafuns ((lq_tmp_x23 Int))
:extrafuns ((lq_tmp_x33 Int))

:extrafuns ((lq_tmp_x41 Int))
:extrafuns ((lq_tmp_x68 Int))

:extrafuns ((x_acn Int))
:extrafuns ((xs Int))
:extrafuns ((xs_aco Int))

:extrafuns ((xs_aco Int))
:extrafuns ((x_acn Int))

:extrafuns ((lq_tmp_x68 Int))
:extrafuns ((lq_tmp_x50 Int))

:extrafuns ((lq_tmp_x41 Int))
:extrafuns ((lq_tmp_x33 Int))

:extrafuns ((lq_tmp_x23 Int))
:extrafuns ((lq_anf__dcz Int))

:extrafuns ((lq_anf__dcy Int))
:extrafuns ((lq_anf__dcx Int))

:extrafuns ((lq_anf__dcw Int))
:extrafuns ((f_acl Int))

:extrafuns ((ds_dct Int))
:extrafuns ((VV_66 Int))
:extrafuns ((VV_64 Int))

:extrafuns ((VV_61 Int))
:extrafuns ((VV_58 Int))
:extrafuns ((VV_48 Int))

:extrafuns ((VV_46 Int))
:extrafuns ((VV_42 Int))
:extrafuns ((VV_39 Int))

:extrafuns ((VV_37 Int))
:extrafuns ((VV_34 Int))
:extrafuns ((VV_31 Int))

:extrafuns ((VV_29 Int))
:extrafuns ((VV_26 Int))
:extrafuns ((VV_24 Int))

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
:extrafuns ((listElts Int Int))


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

:extrapreds ((k_67 Int Int Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_65 Int Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_62 Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_47 Int Int Int Int Int Int Int Int))

:extrapreds ((k_43 Int Int Int Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int Int Int Int Int))

:extrapreds ((k_35 Int Int Int Int Int Int))

:extrapreds ((k_32 Int Int Int Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int Int Int Int))

:extrapreds ((k_25 Int Int Int Int Int Int))


; cid = 16
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (not (Prop lq_anf__dcy)) (and (not (Prop lq_anf__dcy)) (and (>= (len xs_aco) 0) true))))))))))))))))))))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F16 ds_dct lq_tmp_x41))



; cid = 1
:assumption
(implies ((and (k_25 EQ_6U False_68 GT_6W LT_6S True_6u lq_tmp_x23) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true))))))) (k_27 EQ_6U False_68 GT_6W LT_6S True_6u VV_F1 lq_tmp_x23))



; cid = 17
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len VV_94) 0) (and (= VV_94 xs_aco) (and (>= (len VV_94) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (not (Prop lq_anf__dcy)) (and (not (Prop lq_anf__dcy)) (and (>= (len xs_aco) 0) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u VV_F17 lq_anf__dcw) (and (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F17 x_acn) true)))))))))))))))))))))))))))) (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F17 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_35 EQ_6U False_68 GT_6W LT_6S True_6u VV_F2))



; cid = 18
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (not (Prop lq_anf__dcy)) (and (not (Prop lq_anf__dcy)) (and (>= (len xs_aco) 0) (and (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F18 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco) true)))))))))))))))))))))))) (k_25 EQ_6U False_68 GT_6W LT_6S True_6u VV_F18))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len VV_74) 0) true))))))) (k_30 EQ_6U False_68 GT_6W LT_6S True_6u VV_F3 VV_74))



; cid = 19
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (>= (len xs_aco) 0) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u VV_F19 lq_anf__dcw) (and (= VV_F19 x_acn) true)))))))))))))))))))) (k_25 EQ_6U False_68 GT_6W LT_6S True_6u VV_F19))



; cid = 4
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) true)))))) (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F4 lq_tmp_x33))



; cid = 20
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__91__93__35_6m )) (and (= (len lq_anf__dcw) 0) (and (Set_emp (listElts lq_anf__dcw)) (and (>= (len lq_anf__dcw) 0) (and (= (len VV_F20) 0) (and (Set_emp (listElts VV_F20)) true)))))))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F20 ds_dct))



; cid = 5
:assumption
(implies ((and (not (Set_sub (listElts VV_F5) (listElts xs))) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len xs) 0) (and (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F5 xs) true))))))))) false)



; cid = 21
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len VV_99) 0) (and (Set_emp (listElts VV_99)) (and (>= (len VV_99) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__91__93__35_6m )) (and (= (len lq_anf__dcw) 0) (and (Set_emp (listElts lq_anf__dcw)) (and (>= (len lq_anf__dcw) 0) (and (k_47 EQ_6U False_68 GT_6W LT_6S True_6u VV_F21 ds_dct lq_anf__dcw) true)))))))))))))))))))) (k_38 EQ_6U False_68 GT_6W LT_6S True_6u VV_F21 VV_99 ds_dct))



; cid = 6
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (= (len VV_F6) (+ 1 (len lq_anf__dcz))) (and (= (listElts VV_F6) (Set_cup (Set_sng x_acn) (listElts lq_anf__dcz))) true))))))))))))))))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F6 ds_dct))



; cid = 22
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__91__93__35_6m )) (and (= (len lq_anf__dcw) 0) (and (Set_emp (listElts lq_anf__dcw)) (and (>= (len lq_anf__dcw) 0) (and (k_49 EQ_6U False_68 GT_6W LT_6S True_6u VV_F22 ds_dct lq_anf__dcw lq_tmp_x41) true))))))))))))))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F22 ds_dct lq_tmp_x41))



; cid = 7
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= (len VV_80) (+ 1 (len lq_anf__dcz))) (and (= (listElts VV_80) (Set_cup (Set_sng x_acn) (listElts lq_anf__dcz))) (and (>= (len VV_80) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (k_65 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz x_acn xs_aco) true))))))))))))))))))))))))))))) (k_38 EQ_6U False_68 GT_6W LT_6S True_6u VV_F7 VV_80 ds_dct))



; cid = 8
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (k_67 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz lq_tmp_x41 x_acn xs_aco) true)))))))))))))))))))))))))) (k_40 EQ_6U False_68 GT_6W LT_6S True_6u VV_F8 ds_dct lq_tmp_x41))



; cid = 9
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Set_sub (listElts VV_83) (listElts xs_aco)) (and (>= (len VV_83) 0) (and (= VV_83 lq_anf__dcz) (and (>= (len VV_83) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (k_62 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco) true)))))))))))))))))))))))))))))) (k_67 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz x_acn x_acn xs_aco))



; cid = 9
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Set_sub (listElts VV_83) (listElts xs_aco)) (and (>= (len VV_83) 0) (and (= VV_83 lq_anf__dcz) (and (>= (len VV_83) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (k_62 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco) true)))))))))))))))))))))))))))))) (k_65 EQ_6U False_68 GT_6W LT_6S True_6u VV_F9 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz x_acn xs_aco))



; cid = 10
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) true))))))))))))))))))))))))) (k_67 EQ_6U False_68 GT_6W LT_6S True_6u VV_F10 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz lq_tmp_x68 x_acn xs_aco))



; cid = 11
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (Set_sub (listElts lq_anf__dcz) (listElts xs_aco)) (and (>= (len lq_anf__dcz) 0) (and (>= (len xs_aco) 0) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 lq_anf__dcw) (and (= VV_F11 x_acn) true))))))))))))))))))))))))))) (k_65 EQ_6U False_68 GT_6W LT_6S True_6u VV_F11 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy lq_anf__dcz x_acn xs_aco))



; cid = 12
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len VV_86) 0) (and (= VV_86 xs_aco) (and (>= (len VV_86) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (>= (len xs_aco) 0) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 lq_anf__dcw) (and (k_32 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 x_acn) true)))))))))))))))))))))))))))) (k_62 EQ_6U False_68 GT_6W LT_6S True_6u VV_F12 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco))



; cid = 13
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (Prop lq_anf__dcy) (and (Prop lq_anf__dcy) (and (>= (len xs_aco) 0) (and (k_62 EQ_6U False_68 GT_6W LT_6S True_6u VV_F13 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco) true)))))))))))))))))))))))) (k_25 EQ_6U False_68 GT_6W LT_6S True_6u VV_F13))



; cid = 14
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (not (Prop lq_anf__dcy)) (and (not (Prop lq_anf__dcy)) (and (>= (len xs_aco) 0) (and (Set_sub (listElts VV_F14) (listElts xs_aco)) true)))))))))))))))))))))))) (k_43 EQ_6U False_68 GT_6W LT_6S True_6u VV_F14 ds_dct))



; cid = 15
:assumption
(implies ((and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u ds_dct) (and (k_35 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcw) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcx x_acn) (and (k_27 EQ_6U False_68 GT_6W LT_6S True_6u lq_anf__dcy x_acn) (and (k_30 EQ_6U False_68 GT_6W LT_6S True_6u x_acn lq_anf__dcw) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (Set_sub (listElts VV_91) (listElts xs_aco)) (and (>= (len VV_91) 0) (and (>= (len ds_dct) 0) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw ds_dct) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcw (fix__58__35_64 x_acn xs_aco)) (and (= (len lq_anf__dcw) (+ 1 (len xs_aco))) (and (= (listElts lq_anf__dcw) (Set_cup (Set_sng x_acn) (listElts xs_aco))) (and (>= (len lq_anf__dcw) 0) (and (= lq_anf__dcy lq_anf__dcx) (and (not (Prop lq_anf__dcy)) (and (not (Prop lq_anf__dcy)) (and (>= (len xs_aco) 0) (and (k_59 EQ_6U False_68 GT_6W LT_6S True_6u VV_F15 ds_dct lq_anf__dcw lq_anf__dcx lq_anf__dcy x_acn xs_aco) true)))))))))))))))))))))))))) (k_38 EQ_6U False_68 GT_6W LT_6S True_6u VV_F15 VV_91 ds_dct))

)