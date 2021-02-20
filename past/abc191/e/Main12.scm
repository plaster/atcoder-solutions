;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values parse solve))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define *V-MAX* 2000)

(define *oL* (make-vector *V-MAX* '() )) ;; outgoing edge list
(define *iL* (make-vector *V-MAX* '() )) ;; incoming edge list
(define *EW* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; edge weight
(define *VC* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; vertex cost

(define *iD* (make-vector *V-MAX* 0)) ;; incoming-edge dimension
(define *oD* (make-vector *V-MAX* 0)) ;; outgoing-edge dimension

(define (index s t) ($ + s $ * t *V-MAX*))

(define (parse)
  (let* [[N (read)]
         [M (read)]
         ]
    (do-ec
      [: _ M]
      (:let A ($ + -1 $ read))
      (:let B ($ + -1 $ read))
      (:let C (read))
      (begin
        (if-let1 C0 ($ vector-ref *EW* $ index A B)
          (vector-set! *EW* (index A B) (min C0 C))
          (begin
            (vector-set! *EW* (index A B) C)
            (inc! (vector-ref *oD* A))
            (inc! (vector-ref *iD* B))
            (push! (vector-ref *oL* A) B)
            (push! (vector-ref *iL* B) A)
            ))
        ))
    (do-ec [: s N]
           (chain! s))
    N))

(define *CN* (make-vector *V-MAX* #f)) ;; chain group
(define *CC* (make-hash-table)) ;; chain cost
(define (chain! s)
  (cond
    [(and ($ = 1 $ vector-ref *iD* s)
          ($ = 1 $ vector-ref *oD* s)
          ($ not $ vector-ref *CN* s)
          )
     (vector-set! *CN* s s)
     (let loop [[ t ($ car $ vector-ref *oL* s) ]]
       (when (and ($ = 1 $ vector-ref *iD* t)
                  ($ = 1 $ vector-ref *oD* t)
                  ($ not $ vector-ref *CN* t)
                  )
         (vector-set! *CN* t s)
         ($ loop $ car $ vector-ref *oL* t)
         ))
     (let loop [[ t ($ car $ vector-ref *iL* s) ]]
       (when (and ($ = 1 $ vector-ref *iD* t)
                  ($ = 1 $ vector-ref *oD* t)
                  ($ not $ vector-ref *CN* t)
                  )
         (vector-set! *CN* t s)
         ($ loop $ car $ vector-ref *iL* t)
         ))
     ]
    [ ($ not $ vector-ref *CN* s)
     (vector-set! *CN* s s)
     ]))


;; naive binary heap

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
;;

(define *Q* ($ make-bin-heap $ * 1000 *V-MAX*))

(define (dijkstra! s0)
  (let1 Q (rlet1 Q *Q*
            (bin-heap-clear! Q)
            ($ bin-heap-push! Q $ cons s0 0) ;; as normal dijkstra
            )
    (until (bin-heap-empty? Q)
      (match (bin-heap-pop! Q)
        [ ( s . Cs )
         (unless ($ vector-ref *VC* $ index s0 s)
           (vector-set! *VC* (index s0 s) Cs)
           (cond
             (for-each
               (^ (t)
                  (unless (and ($ vector-ref *VC* $ index s0 t)
                               ($ not $ = s t))
                    ($ bin-heap-push! Q
                       $ cons t $ + Cs
                       $ vector-ref *EW* $ index s t
                       )
                    ))
               (vector-ref *oL* s))
               )) ] ))
    ))

(define (min* ls)
  (fold (^ (x s) (if (and x s) (min x s) (or x s))) #f ls))
(define (plus* d0 d1)
  (and d0 d1 (+ d0 d1)))

(define (solve N)
  (do-ec
    (: s N)
    (:let chain (vector-ref *CN* s))
    (voice
      (cond
        [ (hash-table-contains? *CC* chain)
         (hash-table-get *CC* chain) ]
        [ else
          (rlet1 c (begin
                     (dijkstra! s)
                     ($ min*
                        $ map (^ (t)
                                 (plus* ($ vector-ref *EW* $ index s t)
                                        ($ vector-ref *VC* $ index t s)
                                        ))
                        $ vector-ref *oL* s))
            (hash-table-put! *CC* chain c))
          ]
        ))))

(define (voice distance)
  (print (or distance -1)))
