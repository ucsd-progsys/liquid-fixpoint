(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_31 Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((b_aaF Int))

:extrafuns ((ds_daP Int))
:extrafuns ((dummy.pos.ex01.hs.14.15 Int))

:extrafuns ((lq_anf__daR Int))
:extrafuns ((p Int))
:extrafuns ((x0 Int))

:extrafuns ((ys Int))
:extrafuns ((x0 Int))
:extrafuns ((p Int))

:extrafuns ((ds_daP Int))
:extrafuns ((b_aaF Int))
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


; constant 
:extrafuns ((Nil_r9H  Int))

:extrapreds ((k_27 Int Int Int Int Int Int Int Int))

:extrapreds ((k_24 Int Int Int Int Int Int Int))

:extrapreds ((k_22 Int Int Int Int Int Int Int Int))

:extrapreds ((k_19 Int Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p VV_F1 (Nil_r9H )) true))))) (k_19 EQ_6U GT_6W LT_6S VV_F1 p x0))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p dummy.pos.ex01.hs.14.15 (Nil_r9H )) true))))) (k_24 EQ_6U GT_6W LT_6S VV_F2 dummy.pos.ex01.hs.14.15 p x0))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p dummy.pos.ex01.hs.14.15 (Nil_r9H )) true))))) (k_22 EQ_6U GT_6W LT_6S VV_F3 VV_31 dummy.pos.ex01.hs.14.15 p x0))



; cid = 4
:assumption
(implies ((and (not (papp2 p VV_F4 ys)) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (papp2 p dummy.pos.ex01.hs.14.15 (Nil_r9H )) (and (k_27 EQ_6U GT_6W LT_6S VV_F4 dummy.pos.ex01.hs.14.15 ys p x0) true))))))) false)



; cid = 5
:assumption
(implies ((and (k_19 EQ_6U GT_6W LT_6S b_aaF p x0) (and (k_24 EQ_6U GT_6W LT_6S ds_daP b_aaF p x0) (and (k_24 EQ_6U GT_6W LT_6S lq_anf__daR b_aaF p x0) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__daR ds_daP) (and (= lq_anf__daR (Nil_r9H )) (and (k_19 EQ_6U GT_6W LT_6S VV_F5 p x0) (and (= VV_F5 b_aaF) true))))))))))) (k_27 EQ_6U GT_6W LT_6S VV_F5 b_aaF ds_daP p x0))

)