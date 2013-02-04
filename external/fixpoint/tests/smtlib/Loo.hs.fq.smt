(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))

:extrafuns ((VV_F8 Int))
:extrafuns ((a_adG Int))

:extrafuns ((lq_anf__ddV Int))
:extrafuns ((lq_anf__ddW Int))

:extrafuns ((lq_anf__ddX Int))
:extrafuns ((lq_tmp_x6 Int))

:extrafuns ((lq_tmp_x8 Int))
:extrafuns ((plusFour_rbh Int))

:extrafuns ((plusOne_r9G Int))
:extrafuns ((plusTwo_raM Int))

:extrafuns ((pp_rbi Int))
:extrafuns ((x_adw Int))
:extrafuns ((x_adx Int))

:extrafuns ((z Int))
:extrafuns ((x_adx Int))
:extrafuns ((x_adw Int))

:extrafuns ((pp_rbi Int))
:extrafuns ((plusTwo_raM Int))

:extrafuns ((plusOne_r9G Int))
:extrafuns ((plusFour_rbh Int))

:extrafuns ((a_adG Int))
:extrafuns ((VV_61 Int))
:extrafuns ((VV_59 Int))

:extrafuns ((VV_57 Int))
:extrafuns ((VV_54 Int))
:extrafuns ((VV_52 Int))

:extrafuns ((VV_50 Int))
:extrafuns ((VV_47 Int))
:extrafuns ((VV_44 Int))

:extrafuns ((VV_39 Int))
:extrafuns ((VV_36 Int))
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

:extrapreds ((k_62 Int Int Int Int))

:extrapreds ((k_60 Int Int Int Int))
:extrapreds ((k_58 Int Int Int Int))

:extrapreds ((k_55 Int Int Int Int))
:extrapreds ((k_53 Int Int Int Int))

:extrapreds ((k_51 Int Int Int Int))

:extrapreds ((k_48 Int Int Int Int Int))

:extrapreds ((k_45 Int Int Int Int))

:extrapreds ((k_40 Int Int Int Int Int Int))

:extrapreds ((k_37 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_62 EQ_6U GT_6W LT_6S lq_tmp_x8) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true))))) (k_58 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (k_58 EQ_6U GT_6W LT_6S lq_tmp_x6) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> VV_F2 lq_tmp_x6) true)))))) (k_60 EQ_6U GT_6W LT_6S VV_F2))



; cid = 3
:assumption
(implies ((and (k_55 EQ_6U GT_6W LT_6S lq_tmp_x8) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true))))) (k_51 EQ_6U GT_6W LT_6S VV_F3))



; cid = 4
:assumption
(implies ((and (k_51 EQ_6U GT_6W LT_6S lq_tmp_x6) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true))))) (k_53 EQ_6U GT_6W LT_6S VV_F4))



; cid = 5
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_45 EQ_6U GT_6W LT_6S VV_F5))



; cid = 6
:assumption
(implies ((and (not (> VV_F6 z)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_48 EQ_6U GT_6W LT_6S VV_F6 z) true)))))) false)



; cid = 7
:assumption
(implies ((and (k_45 EQ_6U GT_6W LT_6S x_adw) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> lq_anf__ddX x_adw) (and (> VV_F7 lq_anf__ddX) true))))))) (k_48 EQ_6U GT_6W LT_6S VV_F7 x_adw))



; cid = 8
:assumption
(implies ((and (k_37 EQ_6U GT_6W LT_6S x_adx a_adG) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__ddV 1) (and (= lq_anf__ddW lq_anf__ddV) (and (= VV_F8 (+ x_adx lq_anf__ddW)) true)))))))) (k_40 EQ_6U GT_6W LT_6S VV_F8 a_adG x_adx))

)