#lang racket/base

(provide
    generate-mot/data
    src-generate-mot/record
    src-write-to)

(require
    racket/match
    racket/list

    "helper.rkt"
    "consts.rkt")

(struct src-mot/long (long) #:transparent)

(struct src-data (frame value) #:transparent)

(struct src-mot/root-data src-data () #:transparent)
(struct src-mot/step src-mot/root-data () #:transparent)
(struct src-mot/posx src-mot/root-data () #:transparent)
(struct src-mot/posy src-mot/root-data () #:transparent)

(struct src-mot/part-data src-data (part) #:transparent)
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
        ((cdr (assoc (offset-key cmd) mot/part/attribute-mapping)) (offset-frame cmd) (offset-value cmd) part)
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
    (flatten
        (for/list ([cmd cmds])
            (case (cmd-type cmd)
                [(set) (when (equal? (set-key cmd) "data_motLong")
                        (src-mot/long (set-value cmd)))]
                [(set-offset) (generate-mot/root-data cmd)]
                [(with-surronding) (generate-mot/part-data (surronding-cmds cmd) (surronding-part cmd))])
    )))

(define (?assoc lst ele)
    (let loop ([lft lst])
        (if ((caar lft) ele)
            (car lft)
            (loop (cdr lft)))))
(define (mot/statement-sorter a b)
    (define order
        (list
            (cons src-mot/step? 0)
            (cons src-mot/posx? 1)
            (cons src-mot/posy? 2)

            (cons src-mot/part/dir? 3)
            (cons src-mot/part/dis? 4)
            (cons src-mot/part/angle? 5)
            (cons src-mot/part/picture? 6)
            (cons src-mot/part/scalex? 7)
            (cons src-mot/part/scaley? 8)))
    (if (equal? (src-data-frame a) (src-data-frame b))
        (<
            (cdr (?assoc order a))
            (cdr (?assoc order b)))
        (< (src-data-frame a) (src-data-frame b))))

(define (src-generate-mot/record data)
    (let ([long/set (filter src-mot/long? data)])
        (if (null? long/set)
            (src-mot/record '() data)
            (src-mot/record
                (src-mot/long-long (car long/set))
                (sort
                    (filter
                        (lambda (e) (not (src-mot/long? e)))
                        data)
                    mot/statement-sorter)))))

(define ((make-root/writer bin con) cmd out)
    (write-byte bin out)
    (write-byte (src-data-frame cmd) out)
    (write-bytes (con (src-data-value cmd)) out))
(define ((make-part/writer bin con) cmd out)
    (write-byte bin out)
    (write-byte (src-data-frame cmd) out)
    (write-byte (src-mot/part-data-part cmd) out)
    (write-bytes (con (src-data-value cmd)) out))
(define mot/writter-mapping
    (list
        (cons src-mot/step? (make-root/writer bin-mot/step uint16->bytes))
        (cons src-mot/posx? (make-root/writer bin-mot/posx int16->bytes))
        (cons src-mot/posy? (make-root/writer bin-mot/posy int16->bytes))

        (cons src-mot/part/dir? (make-part/writer bin-mot/part-pos-dir int32->bytes))
        (cons src-mot/part/dis? (make-part/writer bin-mot/part-pos-dis int32->bytes))
        (cons src-mot/part/angle? (make-part/writer bin-mot/part-angle int16->bytes))
        (cons src-mot/part/picture? (make-part/writer bin-mot/part-picture int16->bytes))
        (cons src-mot/part/scalex? (make-part/writer bin-mot/part-scalex int16->bytes))
        (cons src-mot/part/scaley? (make-part/writer bin-mot/part-scaley int16->bytes))
    ))
(define (mot/write-single cmd out)
    ((cdr (?assoc mot/writter-mapping cmd)) cmd out))

(define (src-write-to record out)
    (match record
        [(? src-mot/record?)
                (let ([data (src-mot/record-data record)])
                    ;;; Write motion length
                    (unless (null? (src-mot/record-long record))
                        (write-byte bin-bnd/long out)
                        (write-bytes (uint16->bytes (src-mot/record-long record)) out))
                    ;;; Write data
                    (for ([cmd data])
                        (mot/write-single cmd out))
                    ;;; EOF
                    (write-byte bin-eof out)
                )]
    ))
