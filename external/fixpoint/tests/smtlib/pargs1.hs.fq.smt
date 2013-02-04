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

:extrafuns ((dummy.pos.pargs1.hs.4.14 Int))
:extrafuns ((f_aa9 Int))

:extrafuns ((i_aaa Int))
:extrafuns ((ii Int))
:extrafuns ((j_aab Int))

:extrafuns ((jj Int))
:extrafuns ((lq_tmp_x17 Int))

:extrafuns ((lq_tmp_x20 Int))
:extrafuns ((p Int))
:extrafuns ((x0 Int))

:extrafuns ((x0 Int))
:extrafuns ((p Int))
:extrafuns ((lq_tmp_x20 Int))

:extrafuns ((lq_tmp_x17 Int))
:extrafuns ((i_aaa Int))

:extrafuns ((f_aa9 Int))
:extrafuns ((VV_29 Int))
:extrafuns ((VV_26 Int))

:extrafuns ((VV_23 Int))
:extrafuns ((VV_21 Int))
:extrafuns ((VV_18 Int))

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

:extrapreds ((k_30 Int Int Int Int Int Int Int))

:extrapreds ((k_27 Int Int Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int Int Int Int Int))

:extrapreds ((k_22 Int Int Int Int Int Int Int))

:extrapreds ((k_19 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_19 EQ_6U GT_6W LT_6S lq_tmp_x17 p x0) (and (k_22 EQ_6U GT_6W LT_6S lq_tmp_x20 lq_tmp_x17 p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p VV_F1 (+ lq_tmp_x17 lq_tmp_x20)) true))))))) (k_24 EQ_6U GT_6W LT_6S VV_F1 lq_tmp_x17 lq_tmp_x20 p x0))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_27 EQ_6U GT_6W LT_6S VV_F2 p x0))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) true)))) (k_30 EQ_6U GT_6W LT_6S VV_F3 ii p x0))



; cid = 4
:assumption
(implies ((and (not (papp2 p VV_F4 (+ ii jj))) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_24 EQ_6U GT_6W LT_6S VV_F4 ii jj p x0) true)))))) false)



; cid = 5
:assumption
(implies ((and (k_27 EQ_6U GT_6W LT_6S i_aaa p x0) (and (k_30 EQ_6U GT_6W LT_6S j_aab i_aaa p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_30 EQ_6U GT_6W LT_6S VV_F5 i_aaa p x0) (and (= VV_F5 j_aab) true)))))))) (k_22 EQ_6U GT_6W LT_6S VV_F5 i_aaa p x0))



; cid = 6
:assumption
(implies ((and (k_27 EQ_6U GT_6W LT_6S i_aaa p x0) (and (k_30 EQ_6U GT_6W LT_6S j_aab i_aaa p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_27 EQ_6U GT_6W LT_6S VV_F6 p x0) (and (= VV_F6 i_aaa) true)))))))) (k_19 EQ_6U GT_6W LT_6S VV_F6 p x0))

)