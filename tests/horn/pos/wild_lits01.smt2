
(fixpoint "--allowhoqs")

;; use wild-literal pattern `a1` will get instantiated with any `Int` literals in this constraint,
;; and `a2` with any str literals.

(qualif Wild1 ((a0 Adt0) (a1# Int)) (= (fld0$0 a0) a1))
(qualif Wild2 ((a0 Adt0) (a1# Int) (a2# Str)) (= (Map_select (fld0$1 a0) a1) a2))

(var $k0 (Adt0))
(var $k3 (Int (Map_t Int Str)))
(var $k5 (Int (Map_t Int Str)))


(datatype (Adt0 0)
 ((mkadt0$0 ((fld0$0 Int) (fld0$1 (Map_t Int Str))))))

(cut $k0)

(constraint
  (and
    (and
      (and
        (forall ((a0 Adt0) (true))
          (forall ((_$ Int) ($k0 a0))
            (and
              (forall ((_$ Int) (true))
                (and
                  ((= (fld0$0 a0) 99))
                  ((= (Map_select (fld0$1 a0) 1) "HELLO"))
                  ((= (Map_select (fld0$1 a0) 2) "PROFILE"))
                  ((= (Map_select (fld0$1 a0) 3) "WORLD"))
                )

               )
              ((< 0 10))
              ((= (< 0 10) (< 0 10))))))
        (forall ((a0 Adt0) (true))
          (and
            ((= a0 a0))
            ((= 99 99))
            ((= 99 (fld0$0 ((mkadt0$0 99) (fld0$1 a0)))))
            ((= (fld0$1 a0) (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))))
            ((= 1 1))
            ((= "HELLO" "HELLO"))
            ((=
              (fld0$0 ((mkadt0$0 99) (fld0$1 a0)))
              (fld0$0
               ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO")))))
            ((=
              (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO")
              (fld0$1
               ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO")))))
            ((= 2 2))
            ((= "PROFILE" "PROFILE"))
            ((=
              (fld0$0
               ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO")))
              (fld0$0
               ((mkadt0$0
                 (fld0$0
                  ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                   (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                (((Map_store
                   (fld0$1
                    ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                     (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                  2)
                 "PROFILE")))))
            ((=
              (((Map_store
                 (fld0$1
                  ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                   (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                2)
               "PROFILE")
              (fld0$1
               ((mkadt0$0
                 (fld0$0
                  ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                   (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                (((Map_store
                   (fld0$1
                    ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                     (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                  2)
                 "PROFILE")))))
            ((= 3 3))
            ((= "WORLD" "WORLD"))
            (forall ((_$ Int) (false))
              (true))
            ($k0
             ((mkadt0$0
               (fld0$0
                ((mkadt0$0
                  (fld0$0
                   ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                    (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                 (((Map_store
                    (fld0$1
                     ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                      (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                   2)
                  "PROFILE"))))
              (((Map_store
                 (fld0$1
                  ((mkadt0$0
                    (fld0$0
                     ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                      (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                   (((Map_store
                      (fld0$1
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     2)
                    "PROFILE"))))
                3)
               "WORLD")))
            ((=
              (fld0$0
               ((mkadt0$0
                 (fld0$0
                  ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                   (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                (((Map_store
                   (fld0$1
                    ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                     (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                  2)
                 "PROFILE")))
              (fld0$0
               ((mkadt0$0
                 (fld0$0
                  ((mkadt0$0
                    (fld0$0
                     ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                      (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                   (((Map_store
                      (fld0$1
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     2)
                    "PROFILE"))))
                (((Map_store
                   (fld0$1
                    ((mkadt0$0
                      (fld0$0
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     (((Map_store
                        (fld0$1
                         ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                          (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                       2)
                      "PROFILE"))))
                  3)
                 "WORLD")))))
            ((=
              (((Map_store
                 (fld0$1
                  ((mkadt0$0
                    (fld0$0
                     ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                      (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                   (((Map_store
                      (fld0$1
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     2)
                    "PROFILE"))))
                3)
               "WORLD")
              (fld0$1
               ((mkadt0$0
                 (fld0$0
                  ((mkadt0$0
                    (fld0$0
                     ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                      (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                   (((Map_store
                      (fld0$1
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     2)
                    "PROFILE"))))
                (((Map_store
                   (fld0$1
                    ((mkadt0$0
                      (fld0$0
                       ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                        (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                     (((Map_store
                        (fld0$1
                         ((mkadt0$0 (fld0$0 ((mkadt0$0 99) (fld0$1 a0))))
                          (((Map_store (fld0$1 ((mkadt0$0 99) (fld0$1 a0)))) 1) "HELLO"))))
                       2)
                      "PROFILE"))))
                  3)
                 "WORLD")))))
            ($k3 (fld0$0 a0) (fld0$1 a0))))))))