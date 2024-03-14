(constraint
  (forall ((a0 int) (true))
    (and
      (forall ((a1 int) (true))
        (forall ((_ int) (~(~(a1 > 0))))
          (((a1 > 0) = true))
        )
      )
      (((a1 + 1 = a1 + 1) = true))
    )
  )
)
