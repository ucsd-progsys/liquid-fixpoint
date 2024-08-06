(var $k0 (int))


(constraint
  (and // this is a random comment
    (and
      (forall ((a0 int) ((= a0 4)))
        ($k0 a0)
      )
      (forall ((a1 int) ((= a1 10)))
        ($k0 a1)
      )
    )
    (forall ((a2 int) (true))
      (forall ((_ int) (and ($k0 a2) ((>= a2 4)) ((>= a2 10))))
        (tag ((= ((mod a2 2)) 0)) "0")
      )
    )
  )
)
