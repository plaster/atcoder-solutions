;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _)
  (call-with-values (cut call-with-values parse solve)
    emit) 0)
;;; solution body

(define (parse)
  (error "not implemented")
  )

(define (solve data)
  (error "not implemented")
  )

(define emit print)
