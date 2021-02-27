;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(define (parse)
  (let1 T (read)
    (list-ec
      (: _ T)
      (:let X (read))
      (:let Y (read))
      (:let P (read))
      (:let Q (read))
      (list X Y P Q)
      )))

(define (solve cases)
  cases
  )

(define (voice output) (print output))
