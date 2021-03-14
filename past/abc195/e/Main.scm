;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body
;;; I/O helper
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))

(use srfi-13)

(define-constant *CP0* (char->integer #\0))

(define (parse-S s)
  (string-fold
    (^ (c S)
       (cons (- (char->integer c) *CP0*) S)
       ) '() s))

(define (parse-X x)
  (string-fold
    (^ (c X)
       (cons (case c
               [ (#\T) 'T ]
               [ (#\A) 'A ] ) X)
       ) '() x))

(define (parse)
  (let* [[ N (line-read) ]
         [ S ($ parse-S $ read-line) ]
         [ X ($ parse-X $ read-line) ]
         ]
    (values N S X)
    ))

(define-inline (mask-of s)
  ($ ash 1 $ modulo s 7))

(use scheme.bitwise)

(define (solve N S X)
  (let loop [[ N N ]
             [ S S ] [ X X ]
             [ m 1 ] [ b 1 ] ]
    (if (zero? N)
      ($ < 0 $ logand 1 b)
      (match S
        ( (s . S)
         (match X
           ( ( x . X)
            (loop (- N 1) S X (modulo (* m 10) 7)
                  (case x
                    ( (T)
                     ($ logior b
                        $ mask-of $ * m s))
                    ( (A)
                     ($ bit-field-rotate b
                        (modulo (* m s) 7)
                        0 7))
                    )))))))))

(define (voice T) (print (if T 'Takahashi 'Aoki)))
