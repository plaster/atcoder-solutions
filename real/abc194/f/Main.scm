;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

;gosh> (char->integer #\0)
;48
;gosh> (char->integer #\A)
;65
(define *T*
  '#(
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
     0  1  2  3  4  5  6  7  8  9  #f #f #f #f #f #f
     #f 10 11 12 13 14 15
     ))

(define (parse-char c)
  ($ vector-ref *T* $ char->integer c))

(define (parse)
  ($ map parse-char $ string->list $ read-line)
  )

(define (solve input)
  input
  )

(define (voice output) (print output))
