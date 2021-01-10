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

(define (date->cost C Ss date)
  (min C (apply + (map (service-cost-of-date date) Ss))))

(define (accum . args)
  (match args
    [ (date cost #(date-prev cost-prev cost-sum))
     ($ vector date cost $ + cost-sum $ * cost-prev $ - date date-prev)
     ]))

(define (solve C Ss)
  (let1 cost-of-date (make-tree-map)
    ($ for-each (cut tree-map-put! cost-of-date <> 0) $ map car Ss)
    ($ for-each (cut tree-map-put! cost-of-date <> 0) $ map (pa$ + 1) $ map cadr Ss)
    ($ for-each (^ (date) (tree-map-put! cost-of-date date (date->cost C Ss date)))
       $ tree-map-keys cost-of-date)
    (tree-map-fold cost-of-date accum '#(0 0 0))
    ))

(define emit (.$ print (cut vector-ref <> 2)))
