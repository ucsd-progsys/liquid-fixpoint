(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

:extrafuns ((VV_F1 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((incr_r9G Int))
:extrafuns ((lq_anf__daP Int))

:extrafuns ((x Int))
:extrafuns ((x_aaK Int))
:extrafuns ((x_aaK Int))

:extrafuns ((incr_r9G Int))
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
:extrafuns ((cmp Int Int))


; constant 
:extrapreds ((Prop Int))

:extrapreds ((k_28 Int Int Int Int Int))

:extrapreds ((k_25 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> VV_F1 0) true))))) (k_25 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (not (and (> VV_F2 0) (and (not (< VV_F2 x)) true))) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (> x 0) (and (k_28 EQ_6U GT_6W LT_6S VV_F2 x) true))))))) false)



; cid = 3
:assumption
(implies ((and (k_25 EQ_6U GT_6W LT_6S x_aaK) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__daP 1) (and (= VV_F3 (+ x_aaK lq_anf__daP)) true))))))) (k_28 EQ_6U GT_6W LT_6S VV_F3 x_aaK))

)