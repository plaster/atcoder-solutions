;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define (parse)
  (let* [[ N (read) ]
         [ M (read) ]
         [ V (make-vector N '(-1)) ]
         ]
    (do-ec
      (: i N)
      (:let A (read))
      (push! (vector-ref V A) i)
      )
    (do-ec
      (: A N)
      (push! (vector-ref V A) N))
    (values N M V)
    ))

(define (sparse? indices M)
  (any (^ (i1 i0)
          (< M (- i1 i0)))
       indices (cdr indices)))

(define (solve N M V)
  (first-ec
    N
    (: A N)
    (if (sparse? (vector-ref V A) M))
    A
    ))

(define (voice output) (print output))
