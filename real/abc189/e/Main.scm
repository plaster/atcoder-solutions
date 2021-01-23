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
    [ ( m11 m12 m13
        m21 m22 m23
        m31 m32 m33 )
     (match nn
       [ ( n11 n12 n13
           n21 n22 n23
           n31 n32 n33 )

        (list (+ (* m11 n11) (* m12 n21) (* m13 n31))
              (+ (* m11 n12) (* m12 n22) (* m13 n32))
              (+ (* m11 n13) (* m12 n23) (* m13 n33))
              (+ (* m21 n11) (* m22 n21) (* m23 n31))
              (+ (* m21 n12) (* m22 n22) (* m23 n32))
              (+ (* m21 n13) (* m22 n23) (* m23 n33))
              0 0 1
              )
        ])]))

(define (pj mm v)
  (match mm
    [ ( m11 m12 m13
        m21 m22 m23
        m31 m32 m33 )
     (match v
       [ ( x y )
        (list (+ (* x m11)
                 (* x m12)
                 (* x m13))
              (+ (* y m21)
                 (* y m22)
                 (* y m23))
              )])]))

(define (calc-motion Qs)
  (let loop [[ res `(,E) ]
             [ Qs Qs ]
             [ mm E ]]
    (match Qs
      [ () (reverse res) ]
      [ (Q . Qs)
       (let1 mm (mat* (query->matrix Q) mm)
         (loop (cons mm res) Qs mm))])))

(define E '( 1 0 0 
             0 1 0
             0 0 1 ))

(define (query->matrix Q)
  (match Q
    [ (1)
     '( 0 -1 0
        1 0 0
        0 0 1 ) ]
    [ (2)
     '( 0 1 0
        -1 0 0
        0 0 1 ) ]
    [ (3 p)
     `(-1 0 ,(* 2 p)
       0 1 0
       0 0 1)
     ]
    [ (4 p)
     `(1 0 0
       0 1 ,(* 2 p)
       0 0 1)
     ]
    ))

(define (solve Ps Qs ABs)
  (let [[Ms (list->vector (calc-motion Qs))]
        [Ps (list->vector Ps) ]]
    (map (^ (AB)
            (match AB
              [ (A B)
               (pj (vector-ref Ms A) (vector-ref Ps (- B 1)))
               ]))
         ABs)
    ))

(define (emit vs) (for-each (^ (v) (print (car v) " " (cadr v))) vs))
