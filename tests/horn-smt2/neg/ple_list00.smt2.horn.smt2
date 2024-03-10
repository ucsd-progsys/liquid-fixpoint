 
 
(constant len ((func 1 (((Main.List  @(0)))) int)))
(constant Cons ((func 2 ((@(0))  ((Main.List  @(0)))) (Main.List  @(0)))))
(constant Nil ((Main.List  @(0))))
 
 
 
(constraint
   (and
      ((==  (len  ((Cons  1)  ((Cons  2)  ((Cons  3)  Nil))))  4))))