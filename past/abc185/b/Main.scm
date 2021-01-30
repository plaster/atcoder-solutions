;;; common libs
(use util.match)
;;; entry point
(define (main _)
  (call-with-values (cut call-with-values parse solve)
    emit) 0)
;;; solution body

(use srfi-42)
(define (parse)
  (let* [[N (read)]
         [M (read)]
         [T (read)]
         ]
    (values N
            (append
              (list-ec (: _ M)
                       (:let A (read))
                       (:let B (read))
                       (cons A B))
              `((,T))
              ))))

(define (solve N ABs)
  (let loop [[ n N]
             [ T0 0]
             [ ABs ABs]]
    (match ABs
      [ ((A))
       (let1 n ($ - n $ - A T0)
         (positive? n))
       ]
      [ (( A . B ) . ABs)
       (let1 n ($ - n $ - A T0)
         (and (positive? n)
              (loop ($ min N $ + n $ - B A)
                    B
                    ABs)))
       ])))

(define (emit x) (print (if x 'Yes 'No)))
