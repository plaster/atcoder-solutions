;;; common libs
(use srfi-1) (use util.match)
;;; entry point
(define %main (cut call-with-values (cut call-with-values parse solve) voice))
(define (main . _) (%main) 0)
;;; solution body

;;; I/O helper
(use gauche.generator)
(define (line-read :optional [reader read]) (with-input-from-string (read-line) reader))
(define ((line-read$ reader)) (line-read reader))
(define (replist n proc) ($ generator->list $ gunfold zero? (^ (_) (proc)) ($ + -1 $) n))
(define ((replist$ n proc)) (replist n proc))
(define (read-matrix rows cols :optional [reader read]) (replist rows (line-read$ (replist$ cols reader))))

(define (parse-read-char)
  (let1 c (read-char)
    (if (char=? c #\.) 'O 'X)))

(define (parse)
  (match (line-read (replist$ 2 read))
    [ ( H W )
     (read-matrix H W parse-read-char) ]))

(define (solve Ss)
  (rlet1 res 0
         (for-each (^ (S1 S2)
                      (pair-for-each
                        (match-lambda*
                          [ ((_)
                             (_))
                           0 ] ;; white-space guaranteed; NOP
                          [ ((_  'O . _)
                             ('O 'X . _))
                           (inc! res 4) ]
                          [ (('O 'O . _)
                             ('X 'X . _))
                           0 ]
                          [ (('X 'O . _)
                             ('X 'X . _))
                           (inc! res 2) ]
                          [ (('X 'X 'O . _)
                             ('O 'X . _))
                           (inc! res 2) ]
                          [ (('X 'X 'X . _)
                             ('O 'X . _))
                           (inc! res 4) ]
                          [ (('O 'X 'O . _)
                             ('O 'X . _))
                           0 ]
                          [ (('O 'X 'X . _)
                             ('O 'X . _))
                           (inc! res 2) ]
                          [ (('O 'X 'O . _)
                             ('X 'X . _))
                           (dec! res 2)
                           ]
                          [ (('X 'X 'O . _)
                             ('X 'X . _))
                           (dec! res 2)
                           ]
                          [ (('O 'X 'X . _)
                             ('X 'X . _))
                           0 ]
                          [ (('X 'X 'X . _)
                             ('X 'X . _))
                           0 ]
                          [ ( _
                             (_ 'O . _))
                           0 ]
                          [ X
                            (for-each print X)
                            (flush)
                            (errorf "not exhaustive: ~s" X) ]
                          )
                        S1 S2))
                   Ss (cdr Ss)
                   )))

(define (voice output) (print output))
