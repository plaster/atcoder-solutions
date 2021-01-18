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
         [ Q (read) ]
         [ As (replist N read) ]
         [ Ss (replist Q read) ]
         ]
    (values As Ss)))

(use gauche.sequence)

(define ((%solve$ As) S)
  (let/cc return
    (fold-with-index
      (^ (i A X)
         (let1 X (gcd X A)
           (if (= 1 X)
             ($ return $ + 1 i))
           X))
      S As)))

(define (solve As Ss)
  (map (%solve$ As) Ss)
  )

(define emit (for-each$ print))
