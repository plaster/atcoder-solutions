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
  (match (line-read (replist$ 2 read))
    [ (N X)
     (values X (read-matrix N 2))
     ]))

(define (solve X VPs)
  (let loop [[X X]
             [VPs VPs]
             [i 0]
             ]
    (if (negative? X)
      i
      (match VPs
        [ () -1 ]
        [ ((V P) . VPs)
         (loop (- X (* V P 1/100))
               VPs
               (+ i 1))
         ]
        ))))

(define emit print)
