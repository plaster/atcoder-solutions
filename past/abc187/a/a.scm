(define (S n)
  ($ apply +
         $ map (cute - <> (char->integer #\0))
         $ map char->integer
         $ string->list
         $ number->string n 10
         ))

(define (solve A B)
  (max (S A) (S B)))

(define emit print)
         
(let* [[ A (read) ]
       [ B (read) ]
       ]
  ($ emit $ solve A B))
