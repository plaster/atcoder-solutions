;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)

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

;;; solution body
(use srfi-42)
(define (parse)
  (let* [[N (read)]
         [ABs (list-ec
                (: _ 0 N)
                (:let A (read))
                (:let B (read))
                (cons A B)
                ) ]
         [As (map car ABs)]
         [Bs (map cdr ABs)]
         ]
    (values As Bs ($ + 1 $ apply max Bs) N)
    ))

(define (count-leftable As C)
  (count ($ >= C $) As))
(define (count-rightable Bs C)
  (count ($ <= C $) Bs))

(define (solve As Bs Bmax+1 N)
  (cond
    [($ zero? $ modulo N 2)
     (error "not implemented.")
     ]
    [else
      (let [[ L (bisect-first-index
                  Bmax+1
                  (^ (C)
                     (<= (/ N 2) (count-leftable As C))
                     ))
                ]
            [ R (bisect-first-index
                  Bmax+1
                  (^ (C)
                     (let1 C (- Bmax+1 C 1)
                       (<= (/ N 2) (count-rightable Bs C))
                       )))
                ]
            ]
        (- R L -1))
        ]))

(define emit print)
