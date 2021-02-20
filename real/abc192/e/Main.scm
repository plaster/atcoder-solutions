;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define *MMAX* 100000)
(define *V* (make-vector *MMAX* #f)) ;; NMAX = MMAX
(define *Oe* (make-vector *MMAX* '())) ;; NMAX = MMAX

(define (parse)
  (let* [[ N (read) ]
         [ M (read) ]
         [ X (- (read) 1)]
         [ Y (- (read) 1)]
         ]
    (do-ec
      (: m M)
      (:let m1 (* m 2))
      (:let m2 (+ m1 1))
      (:let A (- (read) 1))
      (:let B (- (read) 1))
      (:let T (read))
      (:let K (read))
      (:let e1 (list B T K))
      (:let e2 (list A T K))
      (begin
        (push! (vector-ref *Oe* A) e1)
        (push! (vector-ref *Oe* B) e2)
        ))
    (values X Y)
    ))

;;; data: naive binary min-heap

(define-inline (%heap-value-of h i)
  ($ cdr $ vector-ref (cdr h) i))

(define-inline (%heap-value<= v1 v2)
  (<= v1 v2))

(use scheme.vector)
(define (make-bin-heap capa)
  ($ cons 0 $ make-vector capa
     ; $ undefined
     ))
(define (bin-heap-empty? h) (zero? (car h)))

(define (bin-heap-clear! h) (set-car! h 0)
  ; ($ vector-fill! (cdr h) $ undefined)
  )

(define (bin-heap-pop! h)
  (begin0 (vector-ref (cdr h) 0)
          (or ($ positive? $ car h) (error "heap is empty"))
          (dec! (car h))
          ($ vector-swap! (cdr h) 0 (car h))
          ; ($ vector-set! (cdr h) (car h) $ undefined)
          (%down-heap! h 0)
          ))

(define (bin-heap-push! h elem)
  (or ($ < (car h) $ vector-length $ cdr h) (error "heap is full"))
  (inc! (car h))
  (vector-set! (cdr h) ($ + -1 $ car h) elem)
  (%up-heap! h ($ + -1 $ car h)))

(define (%down-heap! h i)
  (let [[ iL ($ + 1 $ * i 2)]
        [ iR ($ + 2 $ * i 2)]]
    (cond
      [ ($ < iR $ car h)
       (let [[ v (%heap-value-of h i) ]
             [ vL (%heap-value-of h iL) ]
             [ vR (%heap-value-of h iR) ] ]
         (if (%heap-value<= vL vR)
           (unless (%heap-value<= v vL)
             (vector-swap! (cdr h) i iL)
             (%down-heap! h iL))
           (unless (%heap-value<= v vR)
             (vector-swap! (cdr h) i iR)
             (%down-heap! h iR))
           )) ]
      [ ($ < iL $ car h)
       (let [[ v (%heap-value-of h i) ]
             [ vL (%heap-value-of h iL) ] ]
         (unless (%heap-value<= v vL)
           (vector-swap! (cdr h) i iL)
           (%down-heap! h iL))
         ) ]
      [else #| no children; stop |# ]
      )))
(define (%up-heap! h i)
  (when (positive? i)
    (let* [[ v (%heap-value-of h i) ]
           [ iP (ash (- i 1) -1) ]
           [ vP (%heap-value-of h iP) ]]
      (unless (%heap-value<= vP v)
        (vector-swap! (cdr h) i iP)
        (%up-heap! h iP))
      )))
;;; algorithm: dijkstra
; requires `heap`

(define-inline (%dijkstra-visited? b)
  (vector-ref *V* b))

(define-inline (%dijkstra-visit! b c)
  (vector-set! *V* b c))

(define-inline (%dijkstra-edge->cost btk t)
  (+ (cadr btk)
     t
     (let1 k (caddr btk)
       (modulo (- k (modulo t k)) k)
       )))

(define-inline (%dijkstra-edges-from a)
  (vector-ref *Oe* a))

(define (dijkstra! s0)
  (let1 Q (rlet1 Q ($ make-bin-heap $ * *MMAX* 20)
            ($ bin-heap-push! Q $ cons s0 0)
            )
    (until (bin-heap-empty? Q)
      (match (bin-heap-pop! Q)
        [ ( a . Cs )
         (unless (%dijkstra-visited? a)
           (%dijkstra-visit! a Cs)
           (for-each
             (^ (btk)
                (let1 b (car btk)
                  (unless (%dijkstra-visited? b)
                    ($ bin-heap-push! Q
                       $ cons b
                       $ %dijkstra-edge->cost btk Cs
                       )
                    )))
             (%dijkstra-edges-from a))
           ) ] ))
    ))

(define (solve X Y)
  (dijkstra! X)
  (or (vector-ref *V* Y) -1)
  )

(define (voice output) (print output))
