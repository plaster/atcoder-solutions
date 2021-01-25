(use srfi-1) (use util.match)
(define (main _) ((.$ emit solve parse)) 0)

(define (parse)
  (let1 N (read)
    ($ reverse $ cons 0
       $ unfold zero? (^ (_) (read)) ($ + -1 $) N
       )))

(define *m* (make-tree-map))

(define (map-update! L0 A0 S)
  (tree-map-update! *m* A0 ($ min L0 $) L0)
  (let loop [[ S S ]]
    (receive (A1 L1) ($ tree-map-ceiling *m* $ + A0 1)
      (cond
        [ A1
          (tree-map-delete! *m* A1)
          (tree-map-update! *m* A0 ($ min L1 $) L0)
          (loop ($ max S
                   $ * A1 $ - L0 L1))
          ]
        [else
          S ]))))

(use gauche.sequence)

(define (solve As)
  (fold-with-index map-update! 0 As))

(define emit print)
