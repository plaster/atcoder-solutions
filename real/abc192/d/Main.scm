;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; algorithm: bisection

(define (bisect-first-index n pred)
  (let1 di (let f [[di 1]]
             (if (< di n) (f (ash di 1)) di))
    (let loop [[i 0] [iprev #f] [di di]]
      (let1 di (ash di -1)
        (cond
          [ (not (< i n))
           (if (zero? di)
             iprev
             (loop (- i di) iprev di)) ]
          [ (negative? i) ;; reached here iff (pred 0)
           iprev ]
          [ (pred i)
           (if (zero? di)
             i
             (loop (- i di) i di)) ]
          [else
            (if (zero? di)
              iprev
              (loop (+ i di) iprev di)) ]
          )))))

(define (bisect-last-index n pred)
  (bisect-first-index n ($ not $ pred $))
  )

;;; solution body
(define (parse)
  (let* [[ X- (read-line) ]
         [ X ($ map digit->integer
                $ string->list X-) ]
         [ M (read) ]
         [ D ($ + 1 $ apply max X) ]
         ]
    (values X M D)))

(define (digits->number X D)
  (let loop [[ X X ]
             [ M 0 ]]
    (match X
      [ () M ]
      [ (x . X)
       ($ loop X
          $ + x $ * D M)
       ])))

(define (solve X M D0)
  (match X
    [(x)
     (if (> x M) 0 1)]
    [ X
      (let1 D1 (bisect-first-index
                 100000000000000000000
                 (^ (D1)
                    ($ < M $ digits->number X D1)))
        ($ max 0 $ - D1 D0)) ]
    ))

(define (voice output) (print output))
