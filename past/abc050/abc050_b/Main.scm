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
  (let* [[ N (line-read) ]
         [ Ts (line-read (replist$ N read)) ]
         [ M (line-read) ]
         [ PXs (read-matrix M 2 read) ]
         ]
    (values Ts PXs)))

(use gauche.sequence)
(define ((%solve$ Ts) PX)
  (match PX
    [ ( P X )
     (fold-with-index
       (^ (i0 T sum)
          (+ sum (if ($ = P $ + 1 i0) X T)))
       0 Ts) ]))

(define (solve Ts PXs)
  (map (%solve$ Ts) PXs)
  )

(define emit (for-each$ print))
