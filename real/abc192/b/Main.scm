;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  ($ string->list $ read-line)
  )

(define (solve cs)
  (let loop [[cs cs]
             [i 0]]
    (match cs
      [ () #t ]
      [ (c . cs)
       (and (if (odd? i)
              (char-upper-case? c)
              (char-lower-case? c))
            (loop cs (+ i 1))) ])))

(define (voice output) (print (if output 'Yes 'No)))
