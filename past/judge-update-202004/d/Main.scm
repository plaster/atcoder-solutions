;;; common libs
(use srfi-1) (use util.match)
;;; self-made library: I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define line-read$ (pa$ pa$ line-read))
(define (replist n proc) (let loop [[n n] [acc '()] ] (if (zero? n) (reverse acc) (loop (- n 1) (cons (proc) acc)))))
(define replist$ (pa$ pa$ replist)) 
(define (read-matrix rows cols :optional [reader read-char]) (replist rows (line-read$ (replist$ cols reader))))
;;; entry point
(define (main _) ((.$ emit solve parse)) 0)
;;; solution body

(define (parse)
  (let* [[ N (read) ]
         [ Q (read) ]
         [ As (replist N read) ]
         [ Ss (replist Q read) ]
         ]
    (values (gcded As) Ss)))

(define (gcded As)
  (let loop [[ As As]
             [ G #f ]
             [ Gs '() ]]
    (match As
      [ () ($ list->vector $ reverse Gs) ]
      [ (A . As)
       (let1 G (if G (gcd G A) A)
         (loop As G (cons G Gs)))
       ])))

(define (first-index-binary n pred)
  (let1 di (let f [[di 1]]
             (if (< di n)
               (f (ash di 1))
               di))
    (let loop [[i 0] [iprev #f] [di di]]
      (let1 di (ash di -1)
        (cond
          [ (not (< i n))
           (if (zero? di)
             iprev
             (loop (- i di) iprev di)) ]
          [ (negative? i) ;; reached here iff (pred 0)
           iprev ]
          [ (pred i)
           (if (zero? di)
             i
             (loop (- i di) i di)) ]
          [else
            (if (zero? di)
              iprev
              (loop (+ i di) iprev di)) ]
          )))))
; test code
;(use srfi-42)
;(define (t n r) (first-index-binary n (pa$ <= r)))
;(do-ec (: n 0 1000) (: r 0 1000) 
;       (:let x (t n r))
;       (or (if (> n r) (and x (= r x)) (not x))
;           (errorf "~s" (vector n r x))))


(define ((%solve$ As) S)
  (let1 i (first-index-binary (vector-length As)
                              (.$ (pa$ = 1)
                                  (pa$ gcd S)
                                  (pa$ vector-ref As))
                              )
    (if i (+ i 1)
      ($ gcd S
         $ vector-ref As
         $ + -1
         $ vector-length As ))))


(define (solve As Ss)
  (map (%solve$ As) Ss)
  )

(define emit (for-each$ print))
