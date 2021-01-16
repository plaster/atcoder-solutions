;;; common libs
(use srfi-1) (use util.match)
;;; self-made library: I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define line-read$ (pa$ pa$ line-read))
(define (replist n proc) (let loop [[n n] [acc '()] ] (if (zero? n) (reverse acc) (loop (- n 1) (cons (proc) acc)))))
(define replist$ (pa$ pa$ replist)) 
(define (read-matrix rows cols :optional [reader read-char]) (replist rows (line-read$ (replist$ cols reader))))
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define (parse)
  (map (.$ string->symbol string) (string->list (read-line))))

(define (solve cs)
  (match cs
    [ () #t ]
    [ ( 'd 'r 'e 'a 'm . (and cs ( 'e 'r 'a . _ )))
     (solve cs) ]
    [ ( 'd 'r 'e 'a 'm 'e 'r . cs)
     (solve cs) ]
    [ ( 'd 'r 'e 'a 'm . cs)
     (solve cs) ]
    [ ( 'e 'r 'a 's 'e 'r . cs)
     (solve cs) ]
    [ ( 'e 'r 'a 's 'e . cs)
     (solve cs) ]
    [ else #f ]
    ))

(define (emit res)
  (print (if res 'YES 'NO)))
