;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (receive (N X)
    (with-input-from-string (read-line)
      (^ () (let* [[ N (read)]
                   [ X (read) ]]
              (values N X))))
    (values N X (read-line))))


(use srfi-13)
(define (solve N X S)
  (string-fold (^ (C X)
                  ($ max 0 $ + X
                     (case C
                       [(#\o) 1]
                       [(#\x) -1])))
               X S))

(define (voice output) (print output))
