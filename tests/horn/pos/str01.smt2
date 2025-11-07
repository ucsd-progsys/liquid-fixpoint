;; (fixpoint "--stringtheory")

(constraint
  (and
    (forall ((x Str) ((= x "cat")))
      (forall ((y Str) ((= y "dogeral")))
        (and
          ((= (strLen x) 3))
          ((= (strLen y) 7)))))

    (forall ((x Str) ((= x "cat")))
      (forall ((y Str) ((= y "caterpillar")))
        ((strPrefixOf x y))))

    (forall ((x Str) ((= x "pillar")))
      (forall ((y Str) ((= y "caterpillar")))
        ((strSuffixOf x y))))

    (forall ((x Str) ((= x "pill")))
      (forall ((y Str) ((= y "caterpillar")))
        ((strContains y x))))

    (forall ((x Str) ((= x "hot")))
      (forall ((y Str) ((= y "dog")))
        (forall ((z Str) ((= z "hotdog")))
          (and
            ((= (strConcat x y) z))))))
  )
)
