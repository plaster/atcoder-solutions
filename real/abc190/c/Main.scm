;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _)
  (call-with-values (cut call-with-values parse solve)
    emit) 0)
;;; solution body

(use srfi-42)

(define (parse)
  (let* [[N (read)]
         [M (read)]
         [ABs (list-ec (: _ M)
                       (:let A (read))
                       (:let B (read))
                       (cons (- A 1) (- B 1))) ]
         [K (read)]
         [CDs (vector-ec (: _ K)
                         (:let C (read))
                         (:let D (read))
                         (cons (- C 1) (- D 1))) ]
         ]
    (values (map car ABs) (map cdr ABs) CDs)
    ))

(use gauche.sequence)

(define (solve As Bs CDs)
  (max-ec
    (: k-bits (ash 1 (vector-length CDs)))
    (:let balls (fold-with-index
                  (^ (k-i CD m)
                     ($ logior m 
                        $ ash 1
                        (if (logbit? k-i k-bits) (car CD) (cdr CD))))
                  0
                  CDs))
    (count (^ (A B)
              (and (logbit? A balls)
                   (logbit? B balls))
              #|
              ($ logtest
                 $ logand balls
                 $ logior
                 (ash 1 A)
                 (ash 1 B))
              |#
              )
           As Bs)
    ))

(define emit print)
