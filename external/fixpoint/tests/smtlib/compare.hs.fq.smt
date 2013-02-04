(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((EQ_6U Int))

:extrafuns ((GT_6W Int))
:extrafuns ((I__6c Int))
:extrafuns ((LT_6S Int))

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
:extrafuns ((choose_rh Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((lq_anf__dzr Int))

:extrafuns ((lq_anf__dzs Int))
:extrafuns ((lq_anf__dzt Int))

:extrafuns ((lq_anf__dzu Int))
:extrafuns ((lq_anf__dzv Int))

:extrafuns ((lq_anf__dzw Int))
:extrafuns ((lq_anf__dzx Int))

:extrafuns ((m_ayT Int))
:extrafuns ((n_ayS Int))
:extrafuns ((x_ayQ Int))

:extrafuns ((y_ayR Int))
:extrafuns ((y_ayR Int))
:extrafuns ((x_ayQ Int))

:extrafuns ((n_ayS Int))
:extrafuns ((m_ayT Int))

:extrafuns ((lq_tmp_x55 Int))
:extrafuns ((lq_tmp_x52 Int))

:extrafuns ((lq_anf__dzx Int))
:extrafuns ((lq_anf__dzw Int))

:extrafuns ((lq_anf__dzs Int))
:extrafuns ((lq_anf__dzr Int))

:extrafuns ((liquidAssertB_r7 Int))
:extrafuns ((choose_rh Int))

:extrafuns ((VV_88 Int))
:extrafuns ((VV_82 Int))
:extrafuns ((VV_78 Int))

:extrafuns ((VV_74 Int))
:extrafuns ((VV_70 Int))
:extrafuns ((VV_65 Int))

:extrafuns ((VV_60 Int))
:extrafuns ((VV_58 Int))
:extrafuns ((VV_56 Int))

:extrafuns ((VV_53 Int))
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

:extrapreds ((k_89 Int Int Int Int Int Int Int Int))

:extrapreds ((k_83 Int Int Int Int))

:extrapreds ((k_79 Int Int Int Int Int Int Int Int))

:extrapreds ((k_75 Int Int Int Int Int Int Int Int))

:extrapreds ((k_71 Int Int Int Int Int Int Int Int))

:extrapreds ((k_66 Int Int Int Int Int Int))

:extrapreds ((k_61 Int Int Int Int))

:extrapreds ((k_59 Int Int Int Int Int Int))

:extrapreds ((k_57 Int Int Int Int Int))

:extrapreds ((k_54 Int Int Int Int))


; cid = 16
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_57 EQ_6U GT_6W LT_6S VV_F16 x_ayQ) (and (= VV_F16 y_ayR) true)))))))) (k_66 EQ_6U GT_6W LT_6S VV_F16 x_ayQ y_ayR))



; cid = 1
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dzw 1) (and (= lq_anf__dzx 0) (and (k_59 EQ_6U GT_6W LT_6S VV_F1 n_ayS m_ayT) true))))))) (k_83 EQ_6U GT_6W LT_6S VV_F1))



; cid = 17
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (k_54 EQ_6U GT_6W LT_6S VV_F17) (and (= VV_F17 x_ayQ) true)))))))) (k_66 EQ_6U GT_6W LT_6S VV_F17 x_ayQ y_ayR))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dzw 1) (and (= lq_anf__dzx 0) (and (= VV_F2 m_ayT) true))))))) (k_57 EQ_6U GT_6W LT_6S VV_F2 n_ayS))



; cid = 2
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dzw 1) (and (= lq_anf__dzx 0) (and (= VV_F2 m_ayT) true))))))) (k_89 EQ_6U GT_6W LT_6S VV_F2 lq_anf__dzw lq_anf__dzx m_ayT n_ayS))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dzw 1) (and (= lq_anf__dzx 0) (and (= VV_F3 n_ayS) true))))))) (k_54 EQ_6U GT_6W LT_6S VV_F3))



; cid = 3
:assumption
(implies ((and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (= lq_anf__dzw 1) (and (= lq_anf__dzx 0) (and (= VV_F3 n_ayS) true))))))) (k_89 EQ_6U GT_6W LT_6S VV_F3 lq_anf__dzw lq_anf__dzx m_ayT n_ayS))



; cid = 4
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs GT_6W) (and (= (cmp lq_anf__dzs) GT_6W) (and (and (implies (Prop lq_anf__dzv) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (Prop lq_anf__dzv)) true)) (and (Prop VV_F4) true))))))))))))))))) (k_59 EQ_6U GT_6W LT_6S VV_F4 x_ayQ y_ayR))



; cid = 5
:assumption
(implies ((and (not (Prop VV_F5)) (and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs GT_6W) (and (= (cmp lq_anf__dzs) GT_6W) (and (and (implies (Prop lq_anf__dzv) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (Prop lq_anf__dzv)) true)) (and (and (implies (Prop VV_F5) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (Prop VV_F5)) true)) (and (= VV_F5 lq_anf__dzv) true))))))))))))))))))) false)



; cid = 6
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs GT_6W) (and (= (cmp lq_anf__dzs) GT_6W) (and (k_57 EQ_6U GT_6W LT_6S VV_F6 x_ayQ) (and (= VV_F6 y_ayR) true))))))))))))))))) (k_79 EQ_6U GT_6W LT_6S VV_F6 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))



; cid = 7
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs GT_6W) (and (= (cmp lq_anf__dzs) GT_6W) (and (k_54 EQ_6U GT_6W LT_6S VV_F7) (and (= VV_F7 x_ayQ) true))))))))))))))))) (k_79 EQ_6U GT_6W LT_6S VV_F7 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))



; cid = 8
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs EQ_6U) (and (= (cmp lq_anf__dzs) EQ_6U) (and (and (implies (Prop lq_anf__dzu) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (Prop lq_anf__dzu)) true)) (and (Prop VV_F8) true))))))))))))))))) (k_59 EQ_6U GT_6W LT_6S VV_F8 x_ayQ y_ayR))



; cid = 9
:assumption
(implies ((and (not (Prop VV_F9)) (and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs EQ_6U) (and (= (cmp lq_anf__dzs) EQ_6U) (and (and (implies (Prop lq_anf__dzu) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (Prop lq_anf__dzu)) true)) (and (and (implies (Prop VV_F9) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (Prop VV_F9)) true)) (and (= VV_F9 lq_anf__dzu) true))))))))))))))))))) false)



; cid = 10
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs EQ_6U) (and (= (cmp lq_anf__dzs) EQ_6U) (and (k_57 EQ_6U GT_6W LT_6S VV_F10 x_ayQ) (and (= VV_F10 y_ayR) true))))))))))))))))) (k_75 EQ_6U GT_6W LT_6S VV_F10 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))



; cid = 11
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs EQ_6U) (and (= (cmp lq_anf__dzs) EQ_6U) (and (k_54 EQ_6U GT_6W LT_6S VV_F11) (and (= VV_F11 x_ayQ) true))))))))))))))))) (k_75 EQ_6U GT_6W LT_6S VV_F11 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))



; cid = 12
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs LT_6S) (and (= (cmp lq_anf__dzs) LT_6S) (and (and (implies (Prop lq_anf__dzt) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (Prop lq_anf__dzt)) true)) (and (Prop VV_F12) true))))))))))))))))) (k_59 EQ_6U GT_6W LT_6S VV_F12 x_ayQ y_ayR))



; cid = 13
:assumption
(implies ((and (not (Prop VV_F13)) (and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs LT_6S) (and (= (cmp lq_anf__dzs) LT_6S) (and (and (implies (Prop lq_anf__dzt) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (Prop lq_anf__dzt)) true)) (and (and (implies (Prop VV_F13) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (Prop VV_F13)) true)) (and (= VV_F13 lq_anf__dzt) true))))))))))))))))))) false)



; cid = 14
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs LT_6S) (and (= (cmp lq_anf__dzs) LT_6S) (and (k_57 EQ_6U GT_6W LT_6S VV_F14 x_ayQ) (and (= VV_F14 y_ayR) true))))))))))))))))) (k_71 EQ_6U GT_6W LT_6S VV_F14 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))



; cid = 15
:assumption
(implies ((and (k_54 EQ_6U GT_6W LT_6S x_ayQ) (and (k_57 EQ_6U GT_6W LT_6S y_ayR x_ayQ) (and (= (cmp EQ_6U) EQ_6U) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (and (implies (= lq_anf__dzr LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzr LT_6S)) true)) (and (and (implies (= lq_anf__dzr GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzr GT_6W)) true)) (and (and (implies (= lq_anf__dzr EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzr EQ_6U)) true)) (and (and (implies (= lq_anf__dzs LT_6S) (< x_ayQ y_ayR)) (and (implies (< x_ayQ y_ayR) (= lq_anf__dzs LT_6S)) true)) (and (and (implies (= lq_anf__dzs GT_6W) (> x_ayQ y_ayR)) (and (implies (> x_ayQ y_ayR) (= lq_anf__dzs GT_6W)) true)) (and (and (implies (= lq_anf__dzs EQ_6U) (= x_ayQ y_ayR)) (and (implies (= x_ayQ y_ayR) (= lq_anf__dzs EQ_6U)) true)) (and (= lq_anf__dzs lq_anf__dzr) (and (= lq_anf__dzs LT_6S) (and (= (cmp lq_anf__dzs) LT_6S) (and (k_54 EQ_6U GT_6W LT_6S VV_F15) (and (= VV_F15 x_ayQ) true))))))))))))))))) (k_71 EQ_6U GT_6W LT_6S VV_F15 lq_anf__dzr lq_anf__dzs x_ayQ y_ayR))

)