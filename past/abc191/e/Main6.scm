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

;; Skew Heap
(define (skew-heap seq)
  (fold skew-heap-insert '#() seq)
  )

(define (skew-heap-empty) '#())

(define (skew-heap-singleton e)
  (vector e '#() '#()))

(define (skew-heap-union heap0 heap1)
  (match `#(,(force heap0) ,(force heap1))
    [ #( #() _ )
      heap1
      ]
    [ #( _ #() )
      heap0
      ]
    [ #( #(e0 l0 r0) #(e1 l1 r1) )
      (delay
        (if (<= (cdr e0) (cdr e1)) ;; (<= e0 e1)
          (vector e0 (skew-heap-union heap1 r0) l0)
          (vector e1 (skew-heap-union heap0 r1) l1)
          ))
      ]
    ))

(define (skew-heap-insert e heap)
  (skew-heap-union heap (skew-heap-singleton e))
  )

(define (skew-heap-extract-min heap)
  (match (force heap)
    [ #() (values #f #f) ]
	[ #(e l r)
      (values e (skew-heap-union l r))
      ]))
;;

(define (dijkstra! s0)
  (let loop [[ Q ($ skew-heap-singleton $ cons s0 0) ]
			 ]
    (receive (ent Q) (skew-heap-extract-min Q)
      (match ent
        [ #f #f ]
        [ ( s . Cs)
         (if-let1 Cs0 ($ vector-ref *VC* $ index s s0)
           (vector-set! *VC* (index s s0) (min Cs Cs0))
           (vector-set! *VC* (index s s0) Cs)
           )
         (loop (fold (^ (t Q)
                        (if ($ vector-ref *VC* $ index t s0)
                          Q
                          (skew-heap-insert
                            ($ cons t $ + Cs $ vector-ref *EW* $ index s t)
                            Q)))
                     Q
                     (vector-ref *EL* s)))
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
                (or (and d1 d0 (min d1 d0)) d1 d0))
            )))

(define (voice output)
  (for-each (^ (distance) (print (or distance -1))) output))
