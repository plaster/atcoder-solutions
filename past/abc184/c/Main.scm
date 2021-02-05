;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  (let* [
         [ A (read)]
         [ B (read)]
         [ C (read)]
         [ D (read)]
         ]
    (values A B C D)))

(use srfi-42)

(define (solve r1 c1 r2 c2)
  (let loop [[r1 r1] [c1 c1] [N 0]]
    (cond
      [(>= N 3) 3]
      [(and (= r1 r2) (= c1 c2)) N]
      [($ >= 3 $ +
          ($ abs $ - r1 r2)
          ($ abs $ - c1 c2))
       (+ N 1)]
      [(or (= (+ r1 r2) (+ c1 c2))
           (= (- r1 r2) (- c1 c2)))
       (+ N 1)]
      [(>= N 2) 3]
      [else
        (min (+ N (if (odd? (+ r1 r2 c1 c2)) 3 2))
             (min-ec
               (:list delta '(                              ( 0 . -3)
                                                  ( -1 . -2)( 0 . -2)( 1 . -2)
                                        ( -2 . -1)( -1 . -1)( 0 . -1)( 1 . -1)( 2 . -1)
                              ( -3 .  0)( -2 .  0)( -1 .  0)         ( 1 .  0)( 2 .  0)( 3 .  0)
                                        ( -2 .  1)( -1 .  1)( 0 .  1)( 1 .  1)( 2 .  1)
                                                  ( -1 .  2)( 0 .  2)( 1 .  2)
                                                            ( 0 .  3)
                              ))
               (:let r1 ($ + r1 $ car delta))
               (:let c1 ($ + c1 $ cdr delta))
               (loop r1 c1 (+ N 1))
               ))
          ]
      )))

(define (voice output) (print output))
