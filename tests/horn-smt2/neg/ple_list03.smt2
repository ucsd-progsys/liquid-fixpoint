(fixpoint  --rewrite)
 
 
 
(constant head ((func 1 ((([]  @(0)))) @(0))))
(constant tail ((func 1 ((([]  @(0)))) ([]  @(0)))))
(constant ints2 (([]  int)))
(constant isCons ((func 1 ((([]  @(0)))) bool)))
(constant ints0 (([]  int)))
(constant isPos ((func 0 ((int)) bool)))
(constant filter ((func 1 (((func 0 ((@(0))) bool))  (([]  @(0)))) ([]  @(0)))))
(constant Cons ((func 1 ((@(0))  (([]  @(0)))) ([]  @(0)))))
(constant Nil ((func 1 () ([]  @(0)))))
(constant isNil ((func 1 ((([]  @(0)))) bool)))
 
(define ints2 () ([]  int) (((Cons  1)  ((Cons  20)  Nil))))
(define filter ((lq1 ((func 0 (((obj a##a29r))) bool)))  (lq2 (([]  (obj a##a29r))))) ([]  (obj a##a29r)) ((ite  (isNil  lq2)  Nil  (ite  (lq1  (head  lq2))  ((Cons  (head  lq2))  ((filter  lq1)  (tail  lq2)))  ((filter  lq1)  (tail  lq2))))))
(define ints0 () ([]  int) (((Cons  0)  ((Cons  1)  ((Cons  2)  Nil)))))
(define isPos ((lq1 (int))) bool ((>  lq1  0)))
 
 
(constraint
   (and
      ((==  ((filter  isPos)  ints0)  ints2))))