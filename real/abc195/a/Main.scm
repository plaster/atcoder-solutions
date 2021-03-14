;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[ M (read)]
         [ H (read)]]
    (values M H)))

(define (solve M H)
  ($ zero? $ modulo H M)
  )

(define (voice output) (print (if output 'Yes 'No)))
