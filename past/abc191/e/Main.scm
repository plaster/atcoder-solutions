;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)
(use data.sparse)
(use gauche.dictionary)

(define *E* (make-sparse-vector)) ;; forward graph
(define *R* (make-sparse-vector)) ;; reverse graph

(define (parse)
  (let* [[N (read)]
         [M (read)]]
    (do-ec
      [: _ M]
      (:let A ($ + -1 $ read))
      (:let B ($ + -1 $ read))
      (:let C (read))
      (:let E
        (if (dict-exists? *E* A)
          (dict-get *E* A)
          (rlet1 E (make-sparse-vector)
            (dict-put! *E* A E))))
      (:let R
        (if (dict-exists? *R* B)
          (dict-get *R* B)
          (rlet1 R (make-sparse-vector)
            (dict-put! *R* B R))))
      (begin
        (dict-update! E B ($ min C $) C)
        (dict-update! R A ($ min C $) C)
        ))
    N))

(use data.heap)

(define empty-sv (make-sparse-vector)) ;; dont modify

(define (edges-from edges s)
  (dict-get edges s empty-sv ))

(define (dijkstra edges s)
  (let* [[ Q (rlet1 Q (make-binary-heap :key cdr :comparator integer-comparator)
                    ($ binary-heap-push! Q $ cons s 0)) ]
         [ V (make-sparse-vector) ]]
    (until (binary-heap-empty? Q)
      (match (binary-heap-pop-min! Q)
        [ ( s . Cs )
         (dict-put! V s Cs)
         (dict-for-each
           (edges-from edges s)
           (^ (t Ct)
              (or (dict-exists? V t)
                  ($ binary-heap-push! Q $ cons t $ + Cs Ct))))
         ]))
    V))

(define *unreachable-distance* +inf.0)

(define (distance-to V t)
  (dict-get V t *unreachable-distance*))

(define (solve N)
  (list-ec
    (: s N)
    (:let fw-costs (dijkstra *E* s))
    (:let rv-costs (dijkstra *R* s))
    (min-ec
      (: t N)
      (cond
        [(= s t)
         (dict-get
           (dict-get *E* s empty-sv)
           s *unreachable-distance*)
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
