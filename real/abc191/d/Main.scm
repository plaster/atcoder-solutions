;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (exact4 X)
  (/ (round->exact (* X 10000)) 10000))

(define (parse)
  (let* [[ X (read)]
         [ Y (read)]
         [ R (read)]]
    (values
      (exact4 X)
      (exact4 Y)
      (exact4 R)
      )))

(use srfi-42)
(define (solve X Y R)
  (sum-ec
    (: x ($ ceiling->exact $ - X R) ($ floor->exact $ + X R 1))
    (:let dy (sqrt (- (* R R) (* (- x X) (- x X)))))
    ;(if (real? dy))
    (:let T ($ ceiling->exact $ + Y dy))
    (:let B ($ floor->exact $ - Y dy))
    (:let T (if (< (* R R) (+ (* (- x X) (- x X))
                              (* (- T Y) (- T Y))))
              (- T 1) T))
    (:let B (if (< (* R R) (+ (* (- x X) (- x X))
                              (* (- B Y) (- B Y))))
              (+ B 1) B))
    ($ + 1 $ - T B)
    ))

(define (voice output) (print output))
