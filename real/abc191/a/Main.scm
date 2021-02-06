;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [
         [V (read)]
         [T (read)]
         [S (read)]
         [D (read)]
         ]
    (values V T S D)))

(define (solve V T S D)
  (<= (* V T)
      D
      (* V S)
      ))

(define (voice v) (print (if v 'No 'Yes)))
