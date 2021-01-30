;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _)
  (call-with-values (cut call-with-values parse solve)
    emit) 0)
;;; solution body

(define (parse)
  (values (read) (read) (read))
  )

(define (solve A B C)
  (cond
    [ (> A B)
     'Takahashi ]
    [ (< A B)
     'Aoki ]
    [ (zero? C)
     'Aoki ]
    [ else 'Takahashi ])
  )

(define emit print)
