;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define (parse)
  (let* [[ N (line-read) ]
         [ S ($ string->list $ read-line) ]
         [ X ($ string->list $ read-line) ]]
    (values S X)
    ))

(define (solve N S X)
  (let loop [[ S S ]
             [ X X ]
             [ M (drop (circular-list 5 4 6 2 3 1) (modulo (- N 1) 6)) ]
             [ b 0 ]
             ]
    (match S
      [ ()
       ]
      [ ( s . S )
       (match X
         [ ( x . X )
          (match M
            [ ( m . M )
             ])])])))

(define (voice output) (print output))
