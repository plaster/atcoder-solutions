;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common libs
(use srfi-1)
(use util.match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; self-made library

;;; I/O helper

(define (line-read :optional [reader read])
  (with-input-from-string (read-line) reader))

(define line-read$ (pa$ pa$ line-read))

(define (replist n proc)
  (let loop [[n n]
             [acc '()]
             ]
    (if (zero? n)
      (reverse acc)
      (loop (- n 1) (cons (proc) acc)))))

(define replist$ (pa$ pa$ replist))

(define (read-matrix rows cols :optional [reader read-char])
  (replist rows (line-read$ (replist$ cols reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solution body

(define (parse)
  (let1 N (line-read)
    ($ sort-by-x
       $ read-matrix N 2 read))
  )

(define (sort-by-x mat)
  (sort mat <= car))

(define ((test-point p1) p2)
  (match p1
    [( x1 y1 )
     (match p2
       [( x2 y2 )
        ($ >= 1
           $ abs
           $ / (- y2 y1) (- x2 x1)
           )])]))
        
(define (solve mat)
  (match mat
    [ () 0 ]
    [ (_) 0 ]
    [ (p1 . mat )
     (+ (solve mat)
        (count (test-point p1) mat))
     ]))

(define emit print)
