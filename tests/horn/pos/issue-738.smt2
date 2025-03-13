(datatype (Data 2) ((mk ((acc (Map_t @(0) @(1)))))))

(constraint
 (forall ((x (Data int int)) (true))
   (forall ((y (Map_t int int)) ((= y (acc x))))
     ((> 2 2)))))