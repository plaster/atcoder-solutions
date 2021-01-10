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

(use gauche.collection)

(define (parse)
  (let* [[N (read)]
         [pN (ash 1 N)]
         [As (replist pN read)]]
    (values pN As)
    ))

(define (solve pN As)
  (let* [[As (zip As (iota pN 1))]
         [As0 (take As (/ pN 2))]
         [As1 (drop As (/ pN 2))]
         ]
    (find-min (list (find-max As0 :key car)
                    (find-max As1 :key car)
                    )
              :key car)
    ))

(define (emit A) (print (cadr A)))
