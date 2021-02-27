;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define (parse)
  (let1 N (read)
    (list-ec
      (: _ N)
      (:let A (read))
      (:let P (read))
      (:let X (read))
      (if (> X A))
      (list A P X)
      )))

(define (solve apxs)
  (if (null? apxs)
    -1
    ($ apply min $ map cadr apxs)))

(define (voice output) (print output))
