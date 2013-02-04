(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((LT_6S Int))
:extrafuns ((VV_F1 Int))

:extrafuns ((VV_F10 Int))
:extrafuns ((VV_F11 Int))

:extrafuns ((VV_F12 Int))
:extrafuns ((VV_F13 Int))

:extrafuns ((VV_F14 Int))
:extrafuns ((VV_F2 Int))
:extrafuns ((VV_F3 Int))

:extrafuns ((VV_F4 Int))
:extrafuns ((VV_F5 Int))
:extrafuns ((VV_F6 Int))

:extrafuns ((VV_F7 Int))
:extrafuns ((VV_F8 Int))
:extrafuns ((VV_F9 Int))

:extrafuns ((fix__38__38__35_r1w Int))
:extrafuns ((liquidAssertB_r7 Int))

:extrafuns ((lq_anf__dGl Int))
:extrafuns ((lq_anf__dGm Int))

:extrafuns ((lq_anf__dGn Int))
:extrafuns ((lq_anf__dGo Int))

:extrafuns ((lq_anf__dGp Int))
:extrafuns ((lq_anf__dGq Int))

:extrafuns ((lq_anf__dGr Int))
:extrafuns ((lq_anf__dGs Int))

:extrafuns ((lq_anf__dGt Int))
:extrafuns ((lq_anf__dGu Int))

:extrafuns ((lq_anf__dGv Int))
:extrafuns ((s_aFN Int))

:extrafuns ((s1_aFP Int))
:extrafuns ((x Int))
:extrafuns ((x_aFO Int))

:extrafuns ((x_aFQ Int))
:extrafuns ((x_aFQ Int))
:extrafuns ((x_aFO Int))

:extrafuns ((s1_aFP Int))
:extrafuns ((s_aFN Int))

:extrafuns ((lq_anf__dGt Int))
:extrafuns ((lq_anf__dGs Int))

:extrafuns ((lq_anf__dGr Int))
:extrafuns ((lq_anf__dGq Int))

:extrafuns ((lq_anf__dGp Int))
:extrafuns ((lq_anf__dGn Int))

:extrafuns ((lq_anf__dGm Int))
:extrafuns ((liquidAssertB_r7 Int))

:extrafuns ((fix__38__38__35_r1w Int))
:extrafuns ((VV_81 Int))

:extrafuns ((VV_77 Int))
:extrafuns ((VV_73 Int))
:extrafuns ((VV_71 Int))

:extrafuns ((VV_66 Int))
:extrafuns ((VV_63 Int))
:extrafuns ((VV_60 Int))

:extrafuns ((VV_58 Int))
:extrafuns ((VV_53 Int))
:extrafuns ((VV_50 Int))

:extrafuns ((VV_48 Int))
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

:extrapreds ((k_82 Int Int Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_78 Int Int Int Int Int Int Int Int Int))

:extrapreds ((k_74 Int Int Int Int Int Int Int))

:extrapreds ((k_72 Int Int Int Int Int Int Int))

:extrapreds ((k_67 Int Int Int Int Int Int Int))

:extrapreds ((k_64 Int Int Int Int Int Int))

:extrapreds ((k_61 Int Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int Int))

:extrapreds ((k_51 Int Int Int Int))

:extrapreds ((k_49 Int Int Int Int))


; cid = 1
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (and (implies (Prop lq_anf__dGs) (> s_aFN lq_anf__dGr)) (and (implies (> s_aFN lq_anf__dGr) (Prop lq_anf__dGs)) true)) (and (= lq_anf__dGt 4) (and (and (implies (Prop lq_anf__dGu) (< s1_aFP lq_anf__dGt)) (and (implies (< s1_aFP lq_anf__dGt) (Prop lq_anf__dGu)) true)) (and (and (implies (Prop lq_anf__dGv) (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true))) (and (implies (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true)) (Prop lq_anf__dGv)) true)) (and (Prop VV_F1) true)))))))))))))) (k_49 EQ_6U GT_6W LT_6S VV_F1))



; cid = 2
:assumption
(implies ((and (not (Prop VV_F2)) (and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (and (implies (Prop lq_anf__dGs) (> s_aFN lq_anf__dGr)) (and (implies (> s_aFN lq_anf__dGr) (Prop lq_anf__dGs)) true)) (and (= lq_anf__dGt 4) (and (and (implies (Prop lq_anf__dGu) (< s1_aFP lq_anf__dGt)) (and (implies (< s1_aFP lq_anf__dGt) (Prop lq_anf__dGu)) true)) (and (and (implies (Prop lq_anf__dGv) (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true))) (and (implies (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true)) (Prop lq_anf__dGv)) true)) (and (and (implies (Prop VV_F2) (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true))) (and (implies (and (Prop lq_anf__dGs) (and (Prop lq_anf__dGu) true)) (Prop VV_F2)) true)) (and (= VV_F2 lq_anf__dGv) true)))))))))))))))) false)



; cid = 3
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (and (implies (Prop lq_anf__dGs) (> s_aFN lq_anf__dGr)) (and (implies (> s_aFN lq_anf__dGr) (Prop lq_anf__dGs)) true)) (and (= lq_anf__dGt 4) (and (= VV_F3 4) (and (= VV_F3 lq_anf__dGt) true))))))))))))) (k_82 EQ_6U GT_6W LT_6S VV_F3 lq_anf__dGn lq_anf__dGq lq_anf__dGr lq_anf__dGs lq_anf__dGt s_aFN s1_aFP))



; cid = 4
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (and (implies (Prop lq_anf__dGs) (> s_aFN lq_anf__dGr)) (and (implies (> s_aFN lq_anf__dGr) (Prop lq_anf__dGs)) true)) (and (= lq_anf__dGt 4) (and (k_74 EQ_6U GT_6W LT_6S VV_F4 lq_anf__dGn lq_anf__dGq s_aFN) (and (= VV_F4 s1_aFP) true))))))))))))) (k_82 EQ_6U GT_6W LT_6S VV_F4 lq_anf__dGn lq_anf__dGq lq_anf__dGr lq_anf__dGs lq_anf__dGt s_aFN s1_aFP))



; cid = 5
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (= VV_F5 3) (and (= VV_F5 lq_anf__dGr) true))))))))))) (k_78 EQ_6U GT_6W LT_6S VV_F5 lq_anf__dGn lq_anf__dGq lq_anf__dGr s_aFN s1_aFP))



; cid = 6
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_74 EQ_6U GT_6W LT_6S s1_aFP lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= lq_anf__dGr 3) (and (k_61 EQ_6U GT_6W LT_6S VV_F6 lq_anf__dGn) (and (= VV_F6 s_aFN) true))))))))))) (k_78 EQ_6U GT_6W LT_6S VV_F6 lq_anf__dGn lq_anf__dGq lq_anf__dGr s_aFN s1_aFP))



; cid = 7
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (= VV_F7 4) (and (= VV_F7 lq_anf__dGq) true))))))))) (k_72 EQ_6U GT_6W LT_6S VV_F7 lq_anf__dGn lq_anf__dGq s_aFN))



; cid = 8
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (k_72 EQ_6U GT_6W LT_6S VV_F8 lq_anf__dGn lq_anf__dGq s_aFN) true)))))))) (k_64 EQ_6U GT_6W LT_6S VV_F8 lq_anf__dGn s_aFN))



; cid = 9
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_72 EQ_6U GT_6W LT_6S x lq_anf__dGn lq_anf__dGq s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGq 4) (and (k_67 EQ_6U GT_6W LT_6S VV_F9 lq_anf__dGn s_aFN x) true))))))))) (k_74 EQ_6U GT_6W LT_6S VV_F9 lq_anf__dGn lq_anf__dGq s_aFN))



; cid = 10
:assumption
(implies ((and (k_61 EQ_6U GT_6W LT_6S s_aFN lq_anf__dGn) (and (k_64 EQ_6U GT_6W LT_6S x_aFQ lq_anf__dGn s_aFN) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= lq_anf__dGo 1) (and (= VV_F10 (- x_aFQ lq_anf__dGo)) true))))))))) (k_67 EQ_6U GT_6W LT_6S VV_F10 lq_anf__dGn s_aFN x_aFQ))



; cid = 11
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (= VV_F11 3) (and (= VV_F11 lq_anf__dGn) true))))))) (k_59 EQ_6U GT_6W LT_6S VV_F11 lq_anf__dGn))



; cid = 12
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (k_59 EQ_6U GT_6W LT_6S VV_F12 lq_anf__dGn) true)))))) (k_51 EQ_6U GT_6W LT_6S VV_F12))



; cid = 13
:assumption
(implies ((and (k_59 EQ_6U GT_6W LT_6S x lq_anf__dGn) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGn 3) (and (k_54 EQ_6U GT_6W LT_6S VV_F13 x) true))))))) (k_61 EQ_6U GT_6W LT_6S VV_F13 lq_anf__dGn))



; cid = 14
:assumption
(implies ((and (k_51 EQ_6U GT_6W LT_6S x_aFO) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dGl 1) (and (= VV_F14 (+ x_aFO lq_anf__dGl)) true))))))) (k_54 EQ_6U GT_6W LT_6S VV_F14 x_aFO))

)