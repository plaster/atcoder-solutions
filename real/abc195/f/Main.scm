;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[ A (read) ]
         [ B (read) ]
         ]
    (values A B)))

(define (solve input)
  (error "not implemented")
  )

(define (voice output) (print output))
