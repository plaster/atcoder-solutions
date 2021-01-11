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

(define (parse-char c)
  (if (char=? #\# c) 1 0))
(define (parse)
  (match (line-read (replist$ 2 read))
    [ ( H W )
     (let* [[ S (read-matrix H W (.$ parse-char read-char)) ]
            [ T (read-matrix H W (.$ parse-char read-char)) ]
            ]
       (values S T))
     ]))

(define (rot90 mat)
  ($ reverse $ apply (map$ list) mat))

(define (trim-top mat)
  (drop-while (pa$ every zero?) mat))

(define (trim mat)
  ($ rot90 $ trim-top
     $ rot90 $ trim-top
     $ rot90 $ trim-top
     $ rot90 $ trim-top
     mat))

;; 1行チェック
(define (fit1 fail s t)
  (match t
    [ () #t ] ; 他の行で失敗するかもしれないから確定できない
    [ (t0 . t)
     (match s
       [ () (fail #f) ] ; 横がはみ出たら、この回はもうだめ
       [ (s0 . s)
        (and (zero? (logand t0 s0))
             (fit1 fail s t))
        ])]))

;; 全行チェック
(define (fit success fail-h fail-v S T)
  (match T
    [ () (success #t) ]
    [ (t . T)
     (match S
       [ () (fail-v #f) ]
       [ (s . S)
        (and (fit1 fail-h s t)
             (fit success fail-h fail-v S T))
        ])]))

;; 縦へずらしながらチェック
(define (fit* success fail-h S T)
  (let/cc fail-v
    (pair-for-each 
      (^ (S) (fit success fail-h fail-v S T))
      S
      )))


;; 横へずらしながらチェック
(define (fit** success S T)
  (let/cc fail-h
    (apply pair-for-each
      (^ S (fit* success fail-h S T))
      S)))

(define (solve S T)
  (let/cc success
    (let* [[ T (trim T) ]
           [ _ (fit** success S T) ]
           [ T (rot90 T) ]
           [ _ (fit** success S T) ]
           [ T (rot90 T) ]
           [ _ (fit** success S T) ]
           [ T (rot90 T) ]
           [ _ (fit** success S T) ]
           ]
      #f
      )))

(define (emit x) (print (if x 'Yes 'No)))
