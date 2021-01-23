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
    ($ reverse $ replist N read)
    ))

(define (solve ops)
  (match ops
    [ () 1]
    [ ('OR . ops)
     ($ + (solve ops) 
        $ ash 1 $ + 1 $ length ops) ]
    [ ('AND . ops)
     (solve ops) ]
    ))

(define emit print)
