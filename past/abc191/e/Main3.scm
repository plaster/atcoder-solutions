;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(use data.sparse)

(define ((make-fw-dict v) s t)
  (sparse-matrix-ref v s t))

(define ((make-rv-dict v) t s)
  (sparse-matrix-ref v s t))

(define fw->cost (undefined)) ;; forward graph (weight)
(define rv->cost (undefined)) ;; reverse graph (weight)
(define fw-edges (make-hash-table integer-comparator)) ;; forward graph (edge list)
(define rv-edges (make-hash-table integer-comparator)) ;; reverse graph (edge list)

(define (parse)
  (let* [[N (read)]
         [M (read)]
         [ve (make-sparse-matrix #f :default +inf.0)]
         ]
    (set! fw->cost (make-fw-dict ve))
    (set! rv->cost (make-rv-dict ve))
    (do-ec
      [: _ M]
      (:let A ($ + -1 $ read))
      (:let B ($ + -1 $ read))
      (:let C (read))
      (begin
        (sparse-matrix-update! ve A B ($ min C $))
        ))
    (sparse-matrix-for-each
      ve
      (^ (A B _)
         (hash-table-push! fw-edges A B)
         (hash-table-push! rv-edges B A)
         ))
    N))

(use data.heap)

(define empty-sv (make-hash-table)) ;; dont modify

(define (dijkstra cost-of edges-from s)
  (let* [[ Q (rlet1 Q (make-binary-heap :key cdr :comparator integer-comparator)
               ($ binary-heap-push! Q $ cons s 0)) ]
         [ V (make-hash-table integer-comparator) ]]
    (until (binary-heap-empty? Q)
      (match (binary-heap-pop-min! Q)
        [ ( s . Cs )
         (hash-table-put! V s Cs)
         (for-each
           (^ (t)
              (or (hash-table-exists? V t)
                  ($ binary-heap-push! Q $ cons t $ + Cs $ cost-of s t)))
           (hash-table-get edges-from s '()))
         ]))
    V))

(define *unreachable-distance* +inf.0)

(define (distance-to V t)
  (hash-table-get V t *unreachable-distance*))

(define (solve N)
  (list-ec
    (: s N)
    (:let fw-costs (dijkstra fw->cost fw-edges s))
    (:let rv-costs (dijkstra rv->cost rv-edges s))
    (min-ec
      (: t N)
      (cond
        [(= s t)
         (fw->cost s t)
         ]
        [else
          (+ (distance-to fw-costs t)
             (distance-to rv-costs t))
          ]))
    ))

(define (voice output)
  (for-each (^ (distance)
               (print
                 (cond
                   [ (>= distance *unreachable-distance*) -1 ]
                   [ else (round->exact distance) ]
                   )))
            output))
