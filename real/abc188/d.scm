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
         [ Ss (replist N (.$ list->vector (replist$ 3 read))) ]
         ]
    (values C Ss)
    ))

(define ((service-cost-of-date date) S)
  (match S
    [#(a b c)
     (if (<= a date b) c 0)
     ]))

(define (date->cost C Ss date)
  (min C (apply + (map (service-cost-of-date date) Ss))))

(define (solve C Ss)
  (let1 cost-of-date (make-tree-map)
    ($ for-each (cut tree-map-put! cost-of-date <> 0) $ map (cut vector-ref <> 0) Ss)
    ($ for-each (cut tree-map-put! cost-of-date <> 0) $ map (pa$ + 1) $ map (cut vector-ref <> 1) Ss)
    (let loop [[date-prev 0]
               [cost-prev 0]
               [cost-sum 0]
               [dates (tree-map-keys cost-of-date) ]
               ]
      (match dates
        [() cost-sum]
        [(date . dates)
         (loop date
               (date->cost C Ss date)
               ($ + cost-sum $ * cost-prev $ - date date-prev) 
               dates)
         ]
        ))))

(define emit print)
