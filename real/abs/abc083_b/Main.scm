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
  (apply values (replist 3 read))
  )

(define (dsum x)
  (let loop [[s 0]
             [x x]]
    (if (zero? x) s
      (loop (+ s (modulo x 10))
            (quotient x 10)))))

(use srfi-42)

(define (solve N A B)
  (sum-ec
    (:range n 1 (+ 1 N))
    (if (<= A (dsum n) B))
    n
    ))

(define emit print)
