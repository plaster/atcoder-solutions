;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (read)
  )

(use srfi-42)
(define (solve N)
  ($ + -1
     $ * N
     (let loop [[n N]
                [S 0]]
       (if (zero? n)
         S
         ($ loop (- n 1)
            $ + S
            $ /. 1 n)))))

(define (voice output) (print output))
