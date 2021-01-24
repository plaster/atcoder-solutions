;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define ((line-read$ reader)) (line-read reader))
(define (replist n proc) ($ generator->list $ gunfold zero? (^ (_) (proc)) ($ + -1 $) n))
(define ((replist$ n proc)) (replist n proc))
(define (read-matrix rows cols :optional [reader read]) (replist rows (line-read$ (replist$ cols reader))))
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define (parse)
  (let* [[ N (line-read) ]
         [ Ps (read-matrix N 2) ]
         [ M (line-read) ]
         [ Qs (replist M (cut call-with-input-string (read-line) port->sexp-list)) ]
         [ Q (line-read) ]
         [ ABs (read-matrix Q 2) ]
         ]
    (values Ps Qs ABs)))
;         n11 n12
;         n21 n22
; m11 m12
; m21 m22
;

(define (mat* mm nn)
  (match mm
    [#( m11 m12 m13
        m21 m22 m23
        )
     (match nn
       [#( n11 n12 n13
           n21 n22 n23
           )

      (vector (+ (* m11 n11) (* m12 n21))
              (+ (* m11 n12) (* m12 n22))
              (+ (* m11 n13) (* m12 n23) m13)
              (+ (* m21 n11) (* m22 n21))
              (+ (* m21 n12) (* m22 n22))
              (+ (* m21 n13) (* m22 n23) m23)
              )
        ])]))

(define (pj mm v)
  (match mm
    [#( m11 m12 m13
        m21 m22 m23
        )
     (match v
       [ ( x y )
        (list (+ (* x m11)
                 (* y m12)
                 (* 1 m13))
              (+ (* x m21)
                 (* y m22)
                 (* 1 m23))
              )])]))

(use gauche.collection)

(define (calc-motion Qs)
  (map-accum
    (^ (Q mm) (let1 mm (mat* (query->matrix Q) mm)
                (values mm mm)))
    E
    Qs))

(define E'#( 1 0 0 
             0 1 0
             ))

(define (query->matrix Q)
  (match Q
    [ (1)
    '#( 0 1 0 -1 0 0) ]
    [ (2)
    '#( 0 -1 0 1 0 0) ]
    [ (3 p)
    `#(-1 0 ,(* 2 p) 0 1 0) ]
    [ (4 p)
    `#(1 0 0 0 -1 ,(* 2 p)) ]
    ))

(define (solve Ps Qs ABs)
  (let [[Ms ($ list->vector
               $ cons E
               $ calc-motion Qs)]
        [Ps (list->vector Ps) ]]
    (for-each (^ (AB)
            (match AB
              [ (A B)
               (match (pj (vector-ref Ms A) (vector-ref Ps (- B 1)))
                 [ (X Y) (print X " " Y) ])
               ]))
         ABs)
    ))

(define (emit _) #f)
