;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use scheme.vector)
(use srfi-42)

(define *KMAX* 200000)
(define P2 (make-vector (+ *KMAX* 1) #f))

(define (parse)
  (read)
  )

(define (%calc! k)
  (rlet1 p2 k
    (vector-set! P2 k p2)))

(define (calc! k)
  (rlet1 p2
    (or (vector-ref P2 k)
        (sum-ec
          (: b k 0 -1)
          ($ %calc! $ quotient k b)
          ))
    (vector-set! P2 k p2)))

(define (solve K)
  (sum-ec
    (: a K 0 -1)
    ($ calc! $ quotient K a)
    ))

(define (voice output) (print output))
