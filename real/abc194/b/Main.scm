;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(define (parse)
  (let1 N (read)
    (list-ec
      (: _ N)
      (:let A (read))
      (:let B (read))
      (cons A B))))

(define (solve ABs)
  (let [[ sort-by-A (sort ABs < car) ]
        [ sort-by-B (sort ABs < cdr) ]
        ]
    (match sort-by-A
      [ ( a0 a1 . _)
       (match sort-by-B
         [ ( b0 b1 . _)
          (cond
            [ (eq? a0 b0)
             (min (max (car a0) (cdr b1))
                  (max (car a1) (cdr b0))
                  (+ (car a0) (cdr b0))
                  )
             ]
            [else
              (max (car a0) (cdr b0))]
            )])])))

(define (voice output) (print output))
