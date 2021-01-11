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
  (replist 5 read-char)
  )

(define (solve cs)
  (let loop [[ n #f ]
             [ cs cs ]
             [ c #f ]
             ]
    (cond
      [ (eqv? n 3) c ]
      [else
        (match cs
          [ (c1 . cs)
           (loop
             (if (eqv? c1 c)
               (+ n 1) 1)
             cs c1) ]
          [ () #f ]
          )])) 
  )

(define (emit c)
  (print (or c 'draw)))
