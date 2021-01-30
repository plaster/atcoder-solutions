;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
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
    (values N T
            (list-ec (: _ M)
                     (:let A (read))
                     (:let B (read))
                     (cons A B)))
    ))

(define (solve N T ABs)
  (let loop [[ n N]
             [ T0 T]
             [ ABs ABs]]
    (match ABs
      [ ()
       (let1 n ($ - n $ - T T0)
         (positive? n))
       ]
      [ (( A . B ) . ABs)
       (let1 n ($ - n $ - A T0)
         (and (positive? n)
              (solve ($ + n $ - B A)
                     B
                     ABs)))
       ])))

(define (emit x) (print (if x 'Yes 'No)))
