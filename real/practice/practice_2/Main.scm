;;; common libs

(define *cs* (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (parse)
  (let* [[N (read)]
         [Q (read)]]
    (values (take *cs* N) Q)
    ))

(define (comp? c0 c1)
  (print "? " c0 " " c1) (flush)
  (eq? '< (read))
  )
(define (solve cs Q)
  (print "! " (list->string (sort cs comp?))) (flush)
  )

(define (main)
  (receive (cs Q) (parse)
    (solve cs Q))
  0)
