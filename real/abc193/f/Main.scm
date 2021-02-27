;;; common libs
(use srfi-1) (use util.match)
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse-char c)
  (case c
    [ (#\B) 1 ]
    [ (#\W) 0 ]
    [ (#\?) #f ]))

(use srfi-42)
(define (parse)
  (let1 N (line-read)
    (list-ec
      (: _ N)
      ($ map parse-char $ string->list $ read-line)
      )))

(define (solve input)
  input
  )

(define (voice output) (print output))
