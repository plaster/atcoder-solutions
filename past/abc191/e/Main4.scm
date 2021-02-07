;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(use data.sparse)

(define *V-MAX* 2000)

(define *EW* (make-sparse-matrix #f :default +inf.0)) ;; edge weight
(define *EL* (make-hash-table integer-comparator)) ;; edge list
(define *VC* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; vertex cost

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
        (sparse-matrix-update! *EW* A B ($ min C $))
        ))
    (sparse-matrix-for-each
      *EW*
      (^ (A B _)
         (hash-table-push! *EL* A B)
         ))
    N))

(use data.heap)

(define empty-sv (make-hash-table)) ;; dont modify

(define (dijkstra! s0)
  (let* [[ Q (rlet1 Q (make-binary-heap :key cdr :comparator integer-comparator)
               ($ binary-heap-push! Q $ cons s0 0)) ]
         ]
    (until (binary-heap-empty? Q)
      (match (binary-heap-pop-min! Q)
        [ ( s . Cs )
         (vector-set! *VC* ($ + s $ * *V-MAX* s0) Cs)
         (for-each
           (^ (t)
              (or (vector-ref *VC* ($ + t $ * *V-MAX* s0))
                  ($ binary-heap-push! Q $ cons t $ + Cs
                     $ sparse-matrix-ref *EW* s t)))
           (hash-table-get *EL* s '()))
         ]))))

(define *unreachable-distance* +inf.0)

(define (distance-between s t)
  (or (vector-ref *VC* ($ + s $ * t *V-MAX*))
      *unreachable-distance*))

(define (solve N)
  (do-ec (: s N) (dijkstra! s))
  (list-ec
    (: s N)
    (min-ec
      (: t N)
      (cond
        [(= s t)
         (sparse-matrix-ref *EW* s t)
         ]
        [else
          (+ (distance-between s t)
             (distance-between t s))
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
