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
  (let1 N (line-read)
    (replist N (.$ parse-var read-line))
    ))

(define (parse-var s)
  (if (char=? #\! (string-ref s 0))
    (list (string-copy s 1) #f)
    (list s #t)
    ))

(define (test-var return v vars0 vars1)
  (if (tree-map-exists? vars1 v)
    (return v)
    (tree-map-put! vars0 v v)
    ))

(define (solve ss)
  (let/cc return
    (let [[t-vars (make-tree-map)]
          [f-vars (make-tree-map)]
          ]
      (for-each (^(s)
                  (match s
                    [ (v tf)
                     (test-var return v (if tf t-vars f-vars) (if tf f-vars t-vars))
                     ]))
                ss)
      #f
      )))

(define (emit x)
  (print (if x x 'satisfiable)))

