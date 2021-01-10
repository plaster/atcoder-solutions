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
  (let* [[ N (read) ]
         [ C (read) ]
         [ Ss (replist N (replist$ 3 read)) ]
         ]
    (values C Ss)
    ))

(define ((service-cost-of-date date) S)
  (match S
    [(a b c)
     (if (<= a date b) c 0)
     ]))

(define ((cost-of-date C Ss) date)
  (min C (apply + (map (service-cost-of-date date) Ss))))

(define (solve C Ss)
  (let* [[ first-date (apply min (map car Ss)) ]
         [ last-date (apply max (map cadr Ss)) ]
         ]
    (apply + (map (cost-of-date C Ss)
                  (iota ($ + 1 $ - last-date first-date) first-date)))
    ))

(define emit print)
