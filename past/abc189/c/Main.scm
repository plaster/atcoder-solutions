#!/bin/gosh
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
    (values N (list->vector (replist N read)))))

#|
(use srfi-42)
(define (solve As)
  (max-ec
    (: L 0 (vector-length As))
    (:let x (vector-ref As L))
    (: R L (vector-length As))
    (:let x (min x (vector-ref As R))) ;; NG - x cannot take over prev As[R]
    ($ * x $ + 1 $ - R L)
    ))
|#

(define (solve N As)
  (let loop-L [[ L 0 ] [m 0]]
    (if (>= L N) m
      (let loop-R [[ R L ] [m m] [x (vector-ref As L)]]
        (if (>= R N) (loop-L (+ L 1) m)
          (let1 x (min x (vector-ref As R))
            (loop-R (+ R 1) ($ max m $ * x $ + 1 $ - R L) x)))))))


(define emit print)
