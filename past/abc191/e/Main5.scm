;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

(use srfi-42)

(define *V-MAX* 2000)

(define *EW* (make-vector (* *V-MAX* *V-MAX*) #f)) ;; edge weight
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
        (let1 C0 ($ vector-ref *EW* $ + A $ * B *V-MAX*)
          (vector-set! *EW* ($ + A $ * B *V-MAX*)
                       (or (and C0 (min C0 C)) C)))
        (hash-table-push! *EL* A B)
        ))
    N))

(use data.heap)

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
              (or ($ vector-ref *VC* $ + t $ * *V-MAX* s0)
                  ($ binary-heap-push! Q $ cons t $ + Cs
                     $ vector-ref *EW* ($ + s $ * t *V-MAX*))))
           (hash-table-get *EL* s '()))
         ]))))

(define (solve N)
  (do-ec (: s N) (dijkstra! s))
  (list-ec
    (: s N)
    (:let d-ss ($ vector-ref *EW* $ + s $ * s *V-MAX*))
    (fold-ec #f

             (: t N)

             (cond [ (= s t) d-ss ]
                   [else 
                     (and-let* [[ d-st ($ vector-ref *VC* $ + s $ * t *V-MAX*) ]
                                [ d-ts ($ vector-ref *VC* $ + t $ * s *V-MAX*) ]]
                       (+ d-st d-ts)) ])

             (^ (d1 d0)
                (or (and d0 d1 (min d1 d0)) d0 d1))
            )))

(define (voice output)
  (for-each (^ (distance) (print (or distance -1))) output))
