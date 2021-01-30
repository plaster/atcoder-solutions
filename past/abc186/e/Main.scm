(use srfi-42)

(define (solve N S K)
  (let [[Nm (modulo N K)]
        [Sm (modulo S K)]]
    (cond
      [ (and (zero? Nm)
             (> Sm 0))
       -1 ]
      [else
        ])))

(do-ec (: _ (read))
       (:let N (read))
       (:let S (read))
       (:let K (read))
       ($ print $ solve N S K))
