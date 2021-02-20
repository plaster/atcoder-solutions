;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [[ N (read) ]
         [ K (read) ]]
    (values N K)))

(define (normal n desc?)
  (let f [[ n n ]
          [ ds '() ]]
    (if (zero? n)
      (let g [[ ds ($ drop-while zero?
                     $ (if desc? reverse values)
                     $ sort ds)
                   ]
              [ m 0 ]
              ]
        (match ds
          [ () m ]
          [ (d . ds) (g ds (+ d (* 10 m))) ]))
      (f (quotient n 10)
         (cons (modulo n 10) ds))
      )))

(define (f x)
  (- (normal x #t)
     (normal x #f)))

(define (solve N K)
  (if (zero? K)
    N
    (solve (f N) (- K 1))))

(define (voice output) (print output))
