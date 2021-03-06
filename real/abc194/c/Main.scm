;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(define (parse)
  (let1 N (read)
    (let loop [[ N N ]
               [ C 0 ]
               [ S 0 ]
               [ S2 0 ]
               [ D 0 ]
               ]
      (if (zero? N)
        D
        (let1 A (read)
          (loop (- N 1)
                (+ C 1)
                (+ S A)
                (+ S2 (* A A))
                (+ D
                   (* -2 A S)
                   S2
                   (* C A A)
                   )))))))

(define (solve S)
  S)

(define (voice output) (print output))
