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
  (let1 N (read)
    ($ sort $ unfold zero? (^ (_) (read)) ($ + -1 $) N)))

(define (solve As)
  (match As
    [ (A . As)
     (let loop [[ As As]
                [ S 0 ]
                [ N 1 ]
                [ G A ]
                ]
       (match As
         [ () S ]
         [ (A . As)
           (loop As
                 ($ + S $ * N $ - A $ / G N)
                 (+ N 1)
                 (+ G A)
                 )]))]))

(define emit print)
