;;; common libs
(use srfi-1) (use util.match)
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define ((line-read$ reader)) (line-read reader))
(define (replist n proc) (unfold zero? (^ (_) (proc)) (pa$ + -1) n))
(define ((replist$ n proc)) (replist n proc))
(define (read-matrix rows cols :optional [reader read]) (replist rows (line-read$ (replist$ cols reader))))
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define (parse)
  (error "not implemented")
  )

(define (solve data)
  (error "not implemented")
  )

(define emit print)
