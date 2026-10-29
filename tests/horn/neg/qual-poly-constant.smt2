; With --allowhoqs, polymorphic constants like `gt` are candidates for qualifier
; parameters. Instantiating `b` with `gt` must not bind `@(0)` to the
; uninstantiated sort of `gt`, which mentions its own `@(0)`: applying that
; binding to the sort of `f` would loop forever.

(fixpoint "--allowhoqs")

(qualif Q ((a int) (b @(0)) (f (func 0 (@(0)) int))) ((= a (f b))))
(qualif GeZero ((v int)) ((>= v 0)))

(constant gt (func 1 (@(0) @(0)) bool))

(var $k (int))

(constraint
  (and
    ($k 0)
    (forall ((x int) ($k x))
      (and
        ($k (+ x 1))
        ((> x 0))))))
