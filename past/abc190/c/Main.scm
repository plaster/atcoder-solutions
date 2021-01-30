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
                       (logior
                         (ash 1 (- A 1))
                         (ash 1 (- B 1)))
                       ) ]
         [K (read)]
         [CDs (vector-ec (: _ K)
                         (:let C (read))
                         (:let D (read))
                         (cons (ash 1 (- C 1)) (ash 1 (- D 1)))) ]
         ]
    (values ABs CDs)
    ))

(use gauche.sequence)

(define (solve ABs CDs)
  (max-ec
    (: k-bits (ash 1 (vector-length CDs)))
    (:let balls (fold-with-index
                  (^ (k-i CD m)
                     ($ logior m 
                        (if (logbit? k-i k-bits) (car CD) (cdr CD))))
                  0
                  CDs))
    (count (^ (AB)
              #|(and (logbit? A balls)
                     (logbit? B balls))
              |#
              ($ = AB $ logand balls AB)
              )
           ABs)
    ))

(define emit print)
