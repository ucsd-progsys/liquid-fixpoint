(fixpoint "--rewrite")
 
 
 
 
(constant head (func 1 ((list @(0))) @(0)))
(constant tail (func 1 ((list @(0))) (list @(0))))
(constant ints2 (list Int))
(constant isCons (func 1 ((list @(0))) bool))
(constant ints0 (list Int))
(constant isPos (func 0 (Int) bool))
(constant
 filter
 (func 1 ((func 0 (@(0)) bool) (list @(0))) (list @(0))))
(constant Cons (func 1 (@(0) (list @(0))) (list @(0))))
(constant Nil (func 1 () (list @(0))))
(constant isNil (func 1 ((list @(0))) bool))
 
(define ints2 () (list Int) ((Cons 1) ((Cons 20) Nil)))
(define filter ((lq1 (func 0 (a##a29r) bool))
                (lq2 (list a##a29r))) (list a##a29r) (if
                                                      (isNil lq2)
                                                      Nil
                                                      (if
                                                       (lq1 (head lq2))
                                                       ((Cons (head lq2)) ((filter lq1) (tail lq2)))
                                                       ((filter lq1) (tail lq2)))))
(define ints0 () (list Int) ((Cons 0) ((Cons 1) ((Cons 2) Nil))))
(define isPos ((lq1 Int)) bool (> lq1 0))
 
 
(match isCons (Cons x xs) true)
(match isNil (Cons x xs) false)
(match isCons (Nil) false)
(match isNil (Nil) true)
(match tail (Cons x xs) xs)
(match head (Cons x xs) x)
 
(constraint
  (and
    ((= ((filter isPos) ints0) ints2))))