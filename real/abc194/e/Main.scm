;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define T (make-tree-map))

(use srfi-42)
(define (parse)
  (let* [[ N (read) ]
         [ M (read) ]]
    (list-ec
      (: i N)
      (:let A (read))
      A
      ))))

(define (solve input)
  (error "not implemented")
  )

(define (voice output) (print output))
