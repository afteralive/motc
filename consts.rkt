#lang racket/base

(provide
    (all-defined-out))

(define bin-mot-long 0)
(define bin-mot-step 1)
(define bin-mot-posx 2)
(define bin-mot-posy 3)
(define bin-mot-part-pos-dir 4)
(define bin-mot-part-pos-dis 5)
(define bin-mot-part-angle 6)
(define bin-mot-part-picture 7)
(define bin-mot-part-scalex 8)
(define bin-mot-part-scaley 9)

(define bin-bnd-long 0)
(define bin-bnd-step 1)
(define bin-bnd-posx 2)
(define bin-bnd-posy 3)
(define bin-bnd-part 4)

(define bin-eof 255)