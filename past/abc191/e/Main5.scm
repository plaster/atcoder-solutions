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
    N))

(use data.heap)

(define (dijkstra! s0)
  (let* [[ Q (rlet1 Q (make-binary-heap :key cdr :comparator integer-comparator)
               ($ binary-heap-push! Q $ cons s0 0)) ]
         ]
    (until (binary-heap-empty? Q)
      (match (binary-heap-pop-min! Q)
        [ ( s . Cs )
         (if-let1 Cs0 ($ vector-ref *VC* $ index s s0)
           (vector-set! *VC* (index s s0) (min Cs Cs0))
           (vector-set! *VC* (index s s0) Cs)
           )
         (for-each
           (^ (t)
              (or ($ vector-ref *VC* $ index t s0)
                  ($ binary-heap-push! Q $ cons t $ + Cs
                     $ vector-ref *EW* $ index s t)))
           (vector-ref *EL* s))
         ]))))

(define (solve N)
  (do-ec (: s N) (dijkstra! s))
  (list-ec
    (: s N)
    (:let d-ss ($ vector-ref *EW* $ index s s))
    (fold-ec #f

             (: t N)

             (cond [ (= s t) d-ss ]
                   [else 
                     (and-let* [[ d-st ($ vector-ref *VC* $ index s t) ]
                                [ d-ts ($ vector-ref *VC* $ index t s) ]]
                       (+ d-st d-ts)) ])

             (^ (d1 d0)
                (or (and d0 d1 (min d1 d0)) d0 d1))
            )))

(define (voice output)
  (for-each (^ (distance) (print (or distance -1))) output))
