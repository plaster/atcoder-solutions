;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define-inline (%u gap new-left old-left)
  (and old-left
       ($ >= gap $ - new-left old-left)
       new-left))

(define (parse)
  (let* [[ N (read) ]
         [ M (read) ]
         [ V (make-vector N -1) ]
         ]
    (do-ec
      (: i N)
      (:let A (read))
      ($ vector-set! V A $ %u M i $ vector-ref V A))
    (do-ec
      (: A N)
      ($ vector-set! V A $ %u M N $ vector-ref V A))
    (values N M V)
    ))

(define (solve N M V)
  (first-ec
    N
    (: A N)
    (if ($ not $ vector-ref V A))
    A
    ))

(define (voice output) (print output))
