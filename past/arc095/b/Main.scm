;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(define (parse)
  (let1 N (read)
    (values N (list-ec (: _ N) (read))))
  )

(define (solve N As)
  (error "not implemented")
  )

(define (voice output) (print output))
