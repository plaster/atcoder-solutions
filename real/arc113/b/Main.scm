;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[A (read)]
         [B (read)]
         [C (read)]]
    (values A B C)))

(define *T*
  '#(
     #(0)
     #(1)
     #(2 4 8 6)
     #(3 9 7 1)
     #(4 6)
     #(5)
     #(6)
     #(7 9 3 1)
     #(8 4 2 6)
     #(9 1)
     ))

(define (solve A B C)
  (let* [[ period-A ($ vector-length $ vector-ref *T* $ modulo A 10) ]
         [ period-B ($ vector-length $ vector-ref *T* $ modulo B period-A) ]
         [ B^C ($ expt B $ modulo C $ lcm period-A period-B) ]
         [ A^B^C ($ expt A $ modulo B^C period-A) ]
         ]
    (modulo A^B^C 10)
    ))

(define (voice output) (print output))
