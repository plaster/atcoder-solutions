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

(use gauche.uvector)

(define (mat* mm nn)
  (s64vector
    (+ (* (s64vector-ref mm 0) (s64vector-ref nn 0)) (* (s64vector-ref mm 1) (s64vector-ref nn 3)))
    (+ (* (s64vector-ref mm 0) (s64vector-ref nn 1)) (* (s64vector-ref mm 1) (s64vector-ref nn 4)))
    (+ (* (s64vector-ref mm 0) (s64vector-ref nn 2)) (* (s64vector-ref mm 1) (s64vector-ref nn 5)) (s64vector-ref mm 2))
    (+ (* (s64vector-ref mm 3) (s64vector-ref nn 0)) (* (s64vector-ref mm 4) (s64vector-ref nn 3)))
    (+ (* (s64vector-ref mm 3) (s64vector-ref nn 1)) (* (s64vector-ref mm 4) (s64vector-ref nn 4)))
    (+ (* (s64vector-ref mm 3) (s64vector-ref nn 2)) (* (s64vector-ref mm 4) (s64vector-ref nn 5)) (s64vector-ref mm 5))
    ))

(define (pj mm v)
  (match v
    [ ( x y )
     (list (+ (* x (s64vector-ref mm 0))
              (* y (s64vector-ref mm 1))
              (* 1 (s64vector-ref mm 2)))
           (+ (* x (s64vector-ref mm 3))
              (* y (s64vector-ref mm 4))
              (* 1 (s64vector-ref mm 5)))
           )]))

(use gauche.collection)

(define (calc-motion Qs)
  (map-accum
    (^ (Q mm) (let1 mm (mat* (query->matrix! Q) mm)
                (values mm mm)))
    E
    Qs))

(define E'#s64( 1 0 0 0 1 0 ))

(define *mat3* (s64vector -1 0 0 0 1 0) )
(define *mat4* (s64vector 1 0 0 0 -1 0) )

(define (query->matrix! Q)
  (match Q
    [ (1)
    '#s64( 0 1 0 -1 0 0) ]
    [ (2)
    '#s64( 0 -1 0 1 0 0) ]
    [ (3 p)
     (s64vector-set! *mat3* 2 (* 2 p))
     *mat3* ]
    [ (4 p)
     (s64vector-set! *mat4* 5 (* 2 p))
     *mat4* ]
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
