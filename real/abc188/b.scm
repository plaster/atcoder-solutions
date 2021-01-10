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
  (let* [[N (line-read)]
         [As (line-read (replist$ N read))]
         [Bs (line-read (replist$ N read))]
         ]
    (values As Bs)
  ))

(define (solve As Bs)
  ($ zero?
     $ apply +
     $ map * As Bs
     ))

(define (emit b) (print (if b 'Yes 'No)))
