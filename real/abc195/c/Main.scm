;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse) (read))

(use srfi-42)

(define (solve N)
  (sum-ec
    (: E 1 16)
    (:let b0 (- (expt 10       E) 1))
    (:let b1 (- (expt 10 (+ 1 E)) 1))
    (:let c ($ max 0 $ quotient E 3))
    (* c
       (cond
         [ (< N b0) 0 ]
         [else (- (min b1 N) b0) ]
         ))))

(define (voice output) (print output))
