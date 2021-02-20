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
