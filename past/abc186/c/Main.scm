;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define (parse) (read))

(use srfi-42)

(define (solve N)
  (sum-ec
    (: i 1 (+ N 1))
    (if ($ not $ string-scan (number->string i) #\7))
    (if ($ not $ string-scan (number->string i 8) #\7))
    1))

(define emit print)
