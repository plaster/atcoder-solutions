;;; common libs
(use srfi-1) (use util.match) (use gauche.generator)
;;; entry point
(define (main _)
  (call-with-values (cut call-with-values parse solve)
    emit) 0)
;;; solution body

(define (parse)
  (read)
  )

(use math.prime)
(use gauche.collection)

(define (solve N)
  ($ apply * 2
     $ map ($ + 1 $)
     $ map length $ group-collection $ filter ($ < 1 $) $ filter odd? $ mc-factorize N)
  )
#|
1 ->
    1
  * 0, 1
2 ->
    2
  * -1, 0, 1, 2
3 ->
    3
    1, 2
  * 0, 1, 2
  * -2, -1, 0, 1, 2, 3
4 ->
    4
  * -3, -2, -1, 0, 1, 2, 3, 4
5 ->
    5
    2 3
    ...
11 ->
    11
    5 6
    ...
24 ->
    24
    7 8 9
|#

(define emit print)
