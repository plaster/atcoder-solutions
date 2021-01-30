;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define (parse)
  (let* [[N (read)]
         [M (read)]
         [ABs (list-ec (: _ M)
                       (:let A (read))
                       (:let B (read))
                       (cons A B)) ]
         [K (read)]
         [Cs (list-ec (: _ K) (read))]
         ]
    (values N ABs Cs)))

(define (solve N ABs Cs)
  (error "not implemented")
  )

(define voice print)
