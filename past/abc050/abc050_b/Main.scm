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
