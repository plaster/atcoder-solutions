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
  (apply values (replist 2 read))
  )

(use srfi-42)
(define (solve N Y)
  (first-ec '(-1 -1 -1)
    (: x 0 (+ N 1))
    (:let Y (- Y (* 10000 x)))
    (:let N (- N x))
    (if (<= 0 Y (* N 5000)))
    (: y 0 (+ N 1))
    (:let Y (- Y (* 5000 y)))
    (:let N (- N y))
    (if (= Y (* N 1000)))
    (list x y N)
    ))

(define emit (.$ print (cut string-join <> " ") (map$ x->string)))
