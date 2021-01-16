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
         [cards (replist N read)]]
    ($ reverse $ sort cards)
    ))

(define (solve cards)
  (let loop [[ a 0 ]
             [ b 0 ]
             [ cards cards ]]
    (match cards
      [ ()
       (- a b)
       ]
      [ (ca)
       (loop (+ ca a) b '())
       ]
      [ (ca cb . cards)
       (loop (+ ca a) (+ cb b) cards)
       ]

      )))

(define emit print)
