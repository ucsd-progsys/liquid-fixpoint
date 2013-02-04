(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_50 Int))

:extrafuns ((VV_57 Int))
:extrafuns ((VV_63 Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))
:extrafuns ((VV_F4 Int))

:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))
:extrafuns ((VV_F7 Int))

:extrafuns ((dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.24 Int))

:extrafuns ((dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.29 Int))

:extrafuns ((lq_anf__dbk Int))
:extrafuns ((lq_tmp_x39 Int))

:extrafuns ((zs_abg Int))
:extrafuns ((zs_abg Int))

:extrafuns ((lq_tmp_x39 Int))
:extrafuns ((lq_tmp_x31 Int))

:extrafuns ((lq_anf__dbk Int))
:extrafuns ((VV_47 Int))

:extrafuns ((VV_45 Int))
:extrafuns ((VV_43 Int))
:extrafuns ((VV_40 Int))

:extrafuns ((VV_37 Int))
:extrafuns ((VV_35 Int))
:extrafuns ((VV_32 Int))

:extrafuns ((VV_29 Int))
:extrafuns ((VV_27 Int))
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

:extrapreds ((k_48 Int Int Int Int Int))

:extrapreds ((k_46 Int Int Int Int Int))

:extrapreds ((k_44 Int Int Int Int Int))

:extrapreds ((k_41 Int Int Int Int Int))

:extrapreds ((k_38 Int Int Int Int Int Int))

:extrapreds ((k_36 Int Int Int Int Int Int))

:extrapreds ((k_33 Int Int Int Int))

:extrapreds ((k_30 Int Int Int Int Int))

:extrapreds ((k_28 Int Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len zs_abg) 0) (and (= (len VV_F1) (len zs_abg)) true))))))) (k_41 EQ_6U GT_6W LT_6S VV_F1 zs_abg))



; cid = 2
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= (len VV_50) (len zs_abg)) (and (>= (len VV_50) 0) (and (>= (len zs_abg) 0) (and (k_48 EQ_6U GT_6W LT_6S VV_F2 zs_abg) true))))))))) (k_36 EQ_6U GT_6W LT_6S VV_F2 VV_50 zs_abg))



; cid = 3
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len zs_abg) 0) true)))))) (k_38 EQ_6U GT_6W LT_6S VV_F3 lq_tmp_x39 zs_abg))



; cid = 4
:assumption
(implies ((and (not (= (len VV_F4) (len zs_abg))) (and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len zs_abg) 0) (and (k_33 EQ_6U GT_6W LT_6S VV_F4) (and (>= (len VV_F4) 0) (and (= VV_F4 zs_abg) true)))))))))) false)



; cid = 5
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S VV_57) (and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_57) 0) (and (= VV_57 zs_abg) (and (>= (len VV_57) 0) (and (>= (len zs_abg) 0) (and (k_28 EQ_6U GT_6W LT_6S VV_F5 VV_57) true))))))))))) (k_46 EQ_6U GT_6W LT_6S VV_F5 zs_abg))



; cid = 6
:assumption
(implies ((and (k_33 EQ_6U GT_6W LT_6S VV_63) (and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len VV_63) 0) (and (= VV_63 zs_abg) (and (>= (len VV_63) 0) (and (>= (len zs_abg) 0) (and (k_28 EQ_6U GT_6W LT_6S VV_F6 VV_63) true))))))))))) (k_44 EQ_6U GT_6W LT_6S VV_F6 zs_abg))



; cid = 7
:assumption
(implies ((and (k_44 EQ_6U GT_6W LT_6S dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.24 zs_abg) (and (k_46 EQ_6U GT_6W LT_6S dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.29 zs_abg) (and (k_33 EQ_6U GT_6W LT_6S zs_abg) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (>= (len zs_abg) 0) (and (= VV_F7 (+ dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.24 dummy..home.rjhala.research.liquidhaskell.include.ghc.list.lhs.716.29)) true))))))))) (k_48 EQ_6U GT_6W LT_6S VV_F7 zs_abg))

)