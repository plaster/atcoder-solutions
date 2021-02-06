;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define (parse)
  (let* [[N (read)]
         [X (read)]
         ]
    (values X
            (list-ec (: _ N) (read)))))

(define (solve X As)
  (remove ($ = X $) As))

(define (voice As)
  (match As
    [ () (print) ]
    [ (A . As)
     (display A)
     (for-each (^ (A) (display " ")
                  (display A))
               As)
     (print)]))
