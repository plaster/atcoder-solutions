;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse) (read))

(use srfi-42)

(define (solve N)
  (let1 expable (make-hash-table)
    (do-ec
      (:while (: a 2 N) ($ >= N $ * a a))
      (:while (: b 2 N) ($ >= N $ expt a b))
      (hash-table-put! expable (expt a b) 1)
      )
    ($ - N $ length $ hash-table-keys expable)
    ))

(define (voice output) (print output))
