;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [
         [ A (read)]
         [ B (read)]
         [ C (read)]
         [ D (read)]
         ]
    (values A B C D)))

(define (solve A B C D)
  (- (* A D) (* B C))
  )

(define (voice output) (print output))
