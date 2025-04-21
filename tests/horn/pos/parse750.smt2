; test that using names which contain reserved names and operator
; does not make parsing fail.

(define_fun fmod_a
    ((x int) (y int))
    bool
    (= x y))

(define_fun blob
    ((a int))
    bool
    (fmod_a (+ 1 a) (+ 2 a)))


(define_fun moda
    ((x int) (y int))
    bool
    (= x y))

(define_fun clob
    ((a int))
    bool
    (moda (+ 1 a) (+ 2 a)))
