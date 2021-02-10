;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define *V-MAX* 2000)

(define *EL* (make-vector *V-MAX* '() )) ;; edge list
(define *EW* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; edge weight
(define *VC* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; vertex cost

(define (uniq-list ls)
  (let1 ht (make-hash-table)
    (for-each (cut hash-table-put! ht <> #t) ls)
    (hash-table-keys ht)))

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
          (vector-set! *EW* (index A B) C))
        (push! (vector-ref *EL* A) B)
        ))
    (do-ec
      [: A N]
      (update! (vector-ref *EL* A) uniq-list))
    (values N M)))

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
            ; ($ bin-heap-push! Q $ cons s0 0) ;; as normal dijkstra
            ; starting from 1-hop nodes
            (for-each
              (^ (t)
                 ($ bin-heap-push! Q $ cons t
                    $ vector-ref *EW* $ index s0 t))
              (vector-ref *EL* s0))
            )
    (until (bin-heap-empty? Q)
      (match (bin-heap-pop! Q)
        [ ( s . Cs )
         (if-let1 Cs0 ($ vector-ref *VC* $ index s s0)
           (vector-set! *VC* (index s s0) (min Cs Cs0))
           (vector-set! *VC* (index s s0) Cs)
           )
         (cond
           [(= s s0)
            (bin-heap-clear! Q) ;; break
            ]
           [else
             (for-each
               (^ (t)
                  (or ($ vector-ref *VC* $ index t s0)
                      ($ bin-heap-push! Q $ cons t $ + Cs
                         $ vector-ref *EW* $ index s t)))
               (vector-ref *EL* s))
             ] ) ] ))))

(define (warshall-floyd! N)
  (dotimes [ s N ]
    (dotimes [t N ]
      ($ vector-set! *VC* (index s t)
         $ vector-ref *EW* $ index s t)))
  (dotimes [ k N ]
    (dotimes [ s N ]
      (dotimes [ t N ]
        (and-let* [[ d-sk ($ vector-ref *VC* $ index s k) ]
                   [ d-kt ($ vector-ref *VC* $ index k t) ]
                   ]
          (vector-set! *VC* (index s t) 
                       (if-let1 d-st ($ vector-ref *VC* $ index s t)
                         ($ min d-st $ + d-sk d-kt)
                         (+ d-sk d-kt)
                         )))))))


(define (solve N M)
  (if (< N 100)
    (warshall-floyd! N)
    (do-ec (: s N) (dijkstra! s))
    )
  (list-ec
    (: s N)
    ($ vector-ref *VC* $ index s s)
    ))

(define (voice output)
  (for-each (^ (distance) (print (or distance -1))) output))
