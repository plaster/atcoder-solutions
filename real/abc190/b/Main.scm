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
         [S (read)]
         [D (read)]]
    (values S D
            (list-ec (: _ N)
                     (:let X (read))
                     (:let Y (read))
                     (cons X Y))))
    )

(define (solve S D XYs)
  ($ positive?
     $ count (^ (XY) (and (> S (car XY))
                          (< D (cdr XY))))
     XYs
     ))

(define (emit t) (print (if t 'Yes 'No)))
