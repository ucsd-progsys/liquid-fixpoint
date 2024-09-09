(datatype (MyData 1) ((mkdata ((field1 @(0))))))

(constraint
  (forall ((x (MyData (Set_Set int))) ((Set_mem 0 (field1 x))))
    ((Set_mem 0 (field1 x)))
  )
)
