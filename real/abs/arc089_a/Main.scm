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
  (let* [[N (line-read)]]
    (cons '(0 0 0)
          (read-matrix N 3 read))
    ))

(define (solve points)
  (match points
    [ ((_ _ _)) #t ]
    [ ((t0 x0 y0) . (and points ((t1 x1 y1) . _)))
     (let [[ dxy (+ (abs (- x1 x0))
                    (abs (- y1 y0))) ]
           [ dt (- t1 t0) ]]
       (and (<= dxy dt)
            (zero? (modulo (- dt dxy) 2))
            (solve points))
       )]))

(define (emit res)
  (print (if res 'Yes 'No)))
