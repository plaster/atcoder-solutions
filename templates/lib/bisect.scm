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
