; this is a comment too.

(var $k0 (int))


; this is a comment
(constraint
  (and ; this is a random comment
    (and
      (forall ((a0 int) ((= a0 4)))
        ($k0 a0)
      )
      (forall ((a1 int) ((= a1 10)))
        ; and yet another comment
        ($k0 a1)
      )
    )
    (forall ((a2 int) (true))
    ; sprinkle sprinkle
      (forall ((_ int) (and ($k0 a2) ((>= a2 4)) ((>= a2 10))))
        (tag ((= ((mod a2 2)) 0)) "0")  ; lets stick a comment here too!
      )
    )
  )
)
