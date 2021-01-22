;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define ((line-read$ reader)) (line-read reader))
(define (replist n proc) ($ generator->list $ gunfold zero? (^ (_) (proc)) ($ + -1 $) n))
(define ((replist$ n proc)) (replist n proc))
(define (read-matrix rows cols :optional [reader read]) (replist rows (line-read$ (replist$ cols reader))))
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define parse read)

(use math.prime)

(define (histogram PS)
  (let loop [[ PS PS ]
             [ PPREV #f ]
             [ C 0 ]
             [ CS '() ]]
    (match PS
      [ () (if (zero? C) CS (cons C CS)) ]
      [ (P . PS)
       (if (equal? P PPREV)
         (loop PS PPREV (+ C 1) CS)
         (loop PS P 1 (if (zero? C) CS (cons C CS))))
       ])))

(use srfi-42)
(define (triangle-level C)
  (max-ec (: L 1 (+ C 1))
          (if (>= (* 2 C) (* L (+ L 1))))
          L
          ))


(define (solve N)
  ($ apply + $ map triangle-level $ histogram $ remove ($ = 1 $) $ mc-factorize N ))

(define emit print)
