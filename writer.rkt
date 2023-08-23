#lang racket/base

(provide
    generate-mot/data
    src-generate-mot/record
    src-write-to)

(require
    racket/match

    "consts.rkt")

(struct src-mot/long (long) #:transparent)

(struct src-mot/root-data (frame value) #:transparent)
(struct src-mot/step src-mot/root-data () #:transparent)
(struct src-mot/posx src-mot/root-data () #:transparent)
(struct src-mot/posy src-mot/root-data () #:transparent)

(struct src-mot/part-data (part frame value) #:transparent)
(struct src-mot/part/dir src-mot/part-data () #:transparent)
(struct src-mot/part/dis src-mot/part-data () #:transparent)
(struct src-mot/part/angle src-mot/part-data () #:transparent)
(struct src-mot/part/picture src-mot/part-data () #:transparent)
(struct src-mot/part/scalex src-mot/part-data () #:transparent)
(struct src-mot/part/scaley src-mot/part-data () #:transparent)

(struct src-mot/record (long data) #:transparent)

(define (cmd-type cmd)
    (car cmd))

(define (set-key cmd)
    (cadr cmd))
(define (set-value cmd)
    (caddr cmd))

(define (offset-key cmd)
    (cadr cmd))
(define (offset-frame cmd)
    (caddr cmd))
(define (offset-value cmd)
    (cadddr cmd))

(define (surronding-part cmd)
    (cadr cmd))
(define (surronding-cmds cmd)
    (caddr cmd))

(define mot/part/attribute-mapping
    (list
        (cons "data_motPartPosDir" src-mot/part/dir)
        (cons "data_motPartPosDis" src-mot/part/dis)
        (cons "data_motPartAngle" src-mot/part/angle)
        (cons "data_motPartScaleX" src-mot/part/scalex) 
        (cons "data_motPartScaleY" src-mot/part/scaley)
        (cons "data_motPartPicture" src-mot/part/picture)))
(define (generate-mot/part-data cmds part)
    (for/list ([cmd cmds])
        ((cdr (assoc (offset-key cmd) mot/part/attribute-mapping)) part (offset-frame cmd) (offset-value cmd))
    ))
(define mot/root/attribute-mapping
    (list
        (cons "data_motStep" src-mot/step)
        (cons "data_motPosX" src-mot/posx)
        (cons "data_motPosY" src-mot/posy)))
(define (generate-mot/root-data cmd)
    ((cdr (assoc (offset-key cmd) mot/root/attribute-mapping)) (offset-frame cmd) (offset-value cmd))
)
(define (generate-mot/data cmds)
    (list-unbound
        (for/list ([cmd cmds])
            (case (cmd-type cmd)
                [(set) (when (equal? (set-key cmd) "data_motLong")
                        (src-mot/long (set-value cmd)))]
                [(set-offset) (generate-mot/root-data cmd)]
                [(with-surronding) (generate-mot/part-data (surronding-cmds cmd) (surronding-part cmd))])
    )))

(define (src-generate-mot/record data)
    (let ([long/set (filter src-mot/long? data)])
        (if (null? long/set)
            (src-mot/record '() data)
            (src-mot/record (src-mot/long-long (car long/set)) (filter (lambda (e) (not (src-mot/long? e))) data)))))

(define (src-write-to record out)
    (match record
        [(? src-mot/record?)
                (let ([data (src-mot/record-data record)])
                    (displayln data)
                    ;;; Write motion length
                    (unless (null? (src-mot/record-long record))
                        (write-byte bin-bnd/long out)
                        (write-bytes (uint16->bytes (src-mot/record-long record)) out))
                    ;;; Write root data
                    (for (
                            [$filter (list src-mot/step? src-mot/posx? src-mot/posy?)]
                            [$con (list uint16->bytes int16->bytes int16->bytes)]
                            [$bin (list bin-mot/step bin-mot/posx bin-mot/posy)])
                        (for ([$item (filter $filter data)])
                            (write-byte $bin out)
                            (write-byte (src-mot/root-data-frame $item) out)
                            (write-bytes ($con (src-mot/root-data-value $item)) out)))
                    ;;; Write part data
                    (for (
                            [$filter (list
                                        src-mot/part/dir?
                                        src-mot/part/dis?
                                        src-mot/part/angle?
                                        src-mot/part/picture?
                                        src-mot/part/scalex?
                                        src-mot/part/scaley?)]
                            [$con (list
                                    int32->bytes
                                    int32->bytes
                                    int16->bytes
                                    int16->bytes
                                    int16->bytes
                                    int16->bytes)]
                            [$bin (list
                                    bin-mot/part-pos-dir
                                    bin-mot/part-pos-dis
                                    bin-mot/part-angle
                                    bin-mot/part-picture
                                    bin-mot/part-scalex
                                    bin-mot/part-scaley)])
                        (for ([$item (filter $filter data)])
                            (write-byte $bin out)
                            (write-byte (src-mot/part-data-frame $item) out)
                            (write-byte (src-mot/part-data-part $item) out)
                            (write-bytes ($con (src-mot/part-data-value $item)) out)))
                    ;;; EOF
                    (write-byte bin-eof out)
                )]
    ))

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

(define (list-unbound lst)
    (match lst
        [(? null?) '()]
        [(? pair?) (append (list-unbound (car lst)) (list-unbound (cdr lst)))]
        [else (list lst)]))