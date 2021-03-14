;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[ A (read)]
         [ B (read)]
         [ W ($ * 1000 $ read)]
         ]
    (values A B W)))

(define (solve A B W)
  (let [[ cA (floor->exact (/ W A)) ]
        [ cB (ceiling->exact (/ W B)) ]]
    (and (<= cB cA)
         (cons cB cA))))

(define (voice output)
  (if output
    (print (car output) " " (cdr output))
    (print 'UNSATISFIABLE)
    ))
