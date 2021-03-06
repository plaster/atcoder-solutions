;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[A (read)]
         [B (read)]
         ]
    (values (+ A B) B)))

(define (solve A+B B)
  (cond
    [ (and (>= A+B 15)
           (>= B 8))
     1]
    [ (and (>= A+B 10)
           (>= B 3))
     2]
    [ (>= A+B 3) 3]
    [else 4]))

(define (voice output) (print output))
