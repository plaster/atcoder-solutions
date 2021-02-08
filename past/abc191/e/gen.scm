(use math.mt-random)
(use util.match)

(define *mt* (make <mersenne-twister> :seed (sys-time)))

(define (gen N M Cmax)
  (print N " " M)
  (dotimes (_ M)
    (let* [[ A ($ + 1 $ mt-random-integer *mt* N) ]
           [ B ($ + 1 $ mt-random-integer *mt* N) ]
           [ C ($ + 1 $ mt-random-integer *mt* Cmax) ]
           ]
      (print A " " B " " C)
      )))

(define (string->read s) (with-input-from-string s read))

(define main
  (match-lambda
    [ (_ N M Cmax)
     (gen (string->read N)
          (string->read M)
          (string->read Cmax)
          )
     0]
    [else
      (print "usage: gen N M")
      1
      ]))
