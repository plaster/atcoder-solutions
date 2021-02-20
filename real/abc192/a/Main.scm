;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (read)
  )

(define (solve input)
  (- 100 (modulo input 100))
  )

(define (voice output) (print output))
