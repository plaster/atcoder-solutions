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
  (let* [[A (read)]
         [B (read)]
         [C (read)]
         [X (read)]
         ]
    (values A B C X)
    ))

(use srfi-42)

(define (solve A B C X)
  (sum-ec
    (: a 0 (+ 1 A))
    (: b 0 (+ 1 B))
    (: c 0 (+ 1 C))
    (if (= X (+ (* a 500) (* b 100) (* c 50))))
    1))

(define emit print)
