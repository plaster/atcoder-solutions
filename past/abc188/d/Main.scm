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
  (let* [[ N (read) ]
         [ C (read) ]
         [ Ss (replist N (replist$ 3 read)) ]
         ]
    (values C Ss)
    ))

(define ((put-event$ events) S)
  (match S
    [(a b c)
     (tree-map-update! events a       (^ (c-old) (+ c-old c)) 0)
     (tree-map-update! events (+ b 1) (^ (c-old) (- c-old c)) 0)
     ]))

(define ((replay-event$ C) . args)
  (match args
    [(k c-delta (k-old c-old c-sum))
     ($ list k (+ c-old c-delta)
        $ + c-sum $ * (min C c-old) $ - k k-old)]))

(define (solve C Ss)
  (let1 events (make-tree-map)
    (for-each (put-event$ events) Ss)
    (tree-map-fold events (replay-event$ C) '(0 0 0))
    ))

(define emit (.$ print caddr))
