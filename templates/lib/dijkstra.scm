;;; algorithm: dijkstra
; requires `heap`

(define-inline (%dijkstra-visited? s t)
  (error "not implemented"))

(define-inline (%dijkstra-visit! s t c)
  (error "not implemented"))

(define-inline (%dijkstra-edge->cost s t)
  (error "not implemented"))

(define-inline (%dijkstra-edges-from s)
  (error "not implemented"))

(define *Q* ($ make-bin-heap $ *SIZE*)

(define (dijkstra! s0)
  (let1 Q (rlet1 Q *Q*
            (bin-heap-clear! Q)
            ($ bin-heap-push! Q $ cons s0 0)
            )
    (until (bin-heap-empty? Q)
      (match (bin-heap-pop! Q)
        [ ( s . Cs )
         (unless (%dijkstra-visited? s0 s)
           (%dijkstra-visit! s0 s Cs)
           (for-each
             (^ (t)
                (unless (and (%dijkstra-visited? s0 t)
                             ($ not $ = s t))
                  ($ bin-heap-push! Q
                     $ cons t $ + Cs
                     $ %dijkstra-edge->cost s t
                     )
                  ))
             (%dijkstra-edges-from s))
           ) ] ))
    ))
