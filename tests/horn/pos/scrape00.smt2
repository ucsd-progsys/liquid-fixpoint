// Tag 0: Goto(bb1) at 27:9: 27:16
// Tag 1: Ret at 33:5: 33:8
// Tag 2: Goto(bb1) at 28:18: 32:6
// (qualif AUTO1 ((a0 int)) (a0 >= 0))
// (qualif AUTO2 ((a1 int)) (a1 >= 0))
// (qualif AUTO3 ((a1 int) (a0 int)) (a0 <= a1))
// (qualif AUTO4 ((a2 int)) (a2 = 0))
// (qualif AUTO6 ((a4 int) (a1 int)) (~ ((a4 < a1))))
// (qualif AUTO7 ((a4 int) (a1 int)) (a4 < a1))
// (qualif AUTO8 ((a5 int) (a3 int)) (a5 = (a3 + 1)))
// (qualif AUTO9 ((a6 int) (a4 int)) (a6 = (a4 + 1)))
// (qualif AUTO5 ((a3 int) (a1 int) (a0 int)) (a3 = (a1 - a0)))

// This is the qualifier we want to auto-scrape
// (qualif MyQ1 ((a0 int) (a1 int) (a2 int)) (a0 = (a1 - a2)))

// (qualif GeZero ((a0 int)) (a0 >= 0))
// (qualif EqZero ((a0 int)) (a0 = 0))
// (qualif GtZero ((a0 int)) (a0 > 0))
// (qualif GeZero ((a0 int)) (a0 >= 0))
// (qualif LtZero ((a0 int)) (a0 < 0))
// (qualif LeZero ((a0 int)) (a0 <= 0))
// (qualif Eq ((a0 int) (a1 int)) (a0 = a1))
// (qualif Gt ((a0 int) (a1 int)) (a0 > a1))
// (qualif Ge ((a0 int) (a1 int)) (a0 >= a1))
// (qualif Lt ((a0 int) (a1 int)) (a0 < a1))
// (qualif Le1 ((a0 int) (a1 int)) (a0 <= (a1 - 1)))

(qualif Le ((a0 int) (a1 int)) (a0 <= a1))

(data Pair 2 = [| Pair { fst: @(0), snd: @(1) } ])
(data Unit 0 = [| Unit { }])
(var $k0 ((int) (int) (int))) // orig: $k0
(var $k1 ((int) (int) (int) (int))) // orig: $k0

(constraint
  (forall ((a0 int) (true))
    (forall ((a1 int) (true))
      (forall ((_ Unit) (a0 >= 0))
        (forall ((_ Unit) (a0 <= a1))
          (forall ((_ Unit) (a1 >= 0))
            (and
              (forall ((a2 int) (a2 = 0))
                (and
                  (tag ($k0 a0 a0 a1) "0")
                  (tag ($k1 a2 a0 a0 a1) "0")
                )
              )
              (forall ((a3 int) (true))
                (forall ((a4 int) (true))
                  (forall ((_ Unit) (and ($k0 a4 a0 a1) ($k1 a3 a4 a0 a1)))
                    (and
                      (forall ((_ Unit) (~(a4 < a1)))
                        (tag (a3 = (a1 - a0)) "1")
                      )
                      (forall ((_ Unit) (a4 < a1))
                        (forall ((a5 int) (a5 = (a3 + 1)))
                          (forall ((a6 int) (a6 = (a4 + 1)))
                            (and
                              (tag ($k0 a6 a0 a1) "2")
                              (tag ($k1 a5 a6 a0 a1) "2")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
