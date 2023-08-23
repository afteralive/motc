#lang racket/base

(provide
    (all-defined-out))

(define (uint16->bytes n)
    (bytes
        (bitwise-and (arithmetic-shift n -8) 255)
        (bitwise-and n 255)))
(define (uint32->bytes n)
    (bytes
        (bitwise-and (arithmetic-shift n -24) 255)
        (bitwise-and (arithmetic-shift n -16) 255)
        (bitwise-and (arithmetic-shift n -8) 255)
        (bitwise-and n 255)))
(define (int16->bytes n)
    (define m (+ n #x7ff))
    (uint16->bytes m))
(define (int32->bytes n)
    (define m (+ n #x7ffffff))
    (uint32->bytes m))
