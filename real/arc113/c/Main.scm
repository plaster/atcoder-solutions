;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse)
  ($ reverse
     $ map ($ + -97 $ char->integer $)
     $ string->list
     $ read-line))

(use scheme.vector)
(use srfi-42)

(define *H* (make-vector 26 0))

(define (invade! c0)
  (sum-ec
    (: c 0 26)
    (cond [ (= c c0) 0 ]
          [ else
            (rlet1 x (vector-ref *H* c)
              (inc! (vector-ref *H* c0) x)
              (vector-set! *H* c 0)) ]
          )))

(define (solve cs)
  (let loop [[ cs cs ]
             [ x 0]
             ]
    (match cs
      [ (c0 . (and (c1 . _) cs))
       (inc! (vector-ref *H* c0))
       (if (= c0 c1)
         ($ loop cs $ + x
            $ invade! c0
            )
         (loop cs x)
         )
       ]
      [ else x ]
      )))

(define (voice output) (print output))
