;;; common libs
(use srfi-1) (use util.match)
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(define (parse-char c)
  (- (char->ucs c)
     (char->ucs #\1)))

(use scheme.vector)

(define (hand->hist s)
  (rlet1 hist (make-vector 9 0)
    (for-each (^ (i0) (inc! (vector-ref hist i0))) s)))

(define (draw-deck! hist s)
  (for-each (^ (i0) (dec! (vector-ref hist i0))) s))

(define (parse)
  (let* [[K (line-read) ]
         [s ($ map parse-char $ cdr $ sort $ string->list $ read-line) ]
         [t ($ map parse-char $ cdr $ sort $ string->list $ read-line) ]
         ]
    (values (hand->hist s)
            (hand->hist t)
            (rlet1 deck (make-vector 9 K)
              (draw-deck! deck s)
              (draw-deck! deck t))
            ($ + -8 $ * 9 K)
            )
    ))

(use srfi-42)

(define (eval-hand S x)
  (sum-ec
    (: i0 9)
    (:let c ($ + (if (= x i0) 1 0) $ vector-ref S i0))
    (* (+ i0 1) (expt 10 c))))

(define (p-of D D-size x y)
  (cond
    [(= x y)
     (let [[ px (/ (vector-ref D x) D-size)]
           [ py (/ (- (vector-ref D x) 1) (- D-size 1))]
           ]
       (or (and (positive? px) (positive? py)
                (*. px py))
           0)) ]
    [else
      (let [[ px (/ (vector-ref D x) D-size)]
            [ py (/ (vector-ref D y) (- D-size 1)) ]]
        (or (and (positive? px) (positive? py)
                 (*. px py))
            0)) ]
    ))

(define (solve S T D D-size)
  (sum-ec
    (: x 9)
    (: y 9)
    (:let p (p-of D D-size x y))
    (if (positive? p))
    (:let score-s (eval-hand S x))
    (:let score-t (eval-hand T y))
    (cond
      [ (> score-s score-t) p ]
      [else 0]
      )))

(define (voice output) (print output))
