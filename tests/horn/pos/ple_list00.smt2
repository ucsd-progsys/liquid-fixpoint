(fixpoint "--rewrite")
 
 
 
 
(constant len (func 1 ((MyList @(0))) Int))
(constant Cons (func 2 (@(0) (MyList @(0))) (MyList @(0))))
(constant Nil (MyList @(0)))
 
 
 
(match len (Nil) 0)
(match len (Cons x xs) (+ 1 (len xs)))
 
(constraint
  (and
    (forall ((x Int) (true))
      (forall ((y Int) ((= y 2)))
        (forall ((z Int) ((= z 3)))
          ((= (len ((Cons x) ((Cons y) ((Cons z) Nil)))) 3)))))))