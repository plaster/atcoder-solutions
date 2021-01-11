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
  (read)
  )

(define (solve X)
  (cond
    (( <= 1800 X) 1)
    (( <= 1600 X) 2)
    (( <= 1400 X) 3)
    (( <= 1200 X) 4)
    (( <= 1000 X) 5)
    (( <= 800  X) 6)
    (( <= 600  X) 7)
    (( <= 400  X) 8)
    (else #f)
    ))

(define emit print)
