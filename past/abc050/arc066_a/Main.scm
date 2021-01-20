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
    ($ values N
       $ reverse
       $ sort $ replist N read)
    ))

(define (assert-list N As)
  (match As
    [ () (= N 0) ]
    [ (A) (and (= N 1)
               (= A 0)) ]
    [ (A B . As)
     (and (= A B (- N 1))
          (assert-list (- N 2) As))
     ]
    ))

(define (calc N p)
  (cond
    [ (< N 2) p]
    [ (calc (- N 2)
            (modulo (* p 2)
                    1000000007
                    )) ]))

(define (solve N As)
  (if (assert-list N As)
    (calc N 1)
    0))

(define emit print)
