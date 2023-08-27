#lang racket/base

(provide
    generate-mot/data
    src-generate-mot/record

    generate-bnd/data
    src-generate-bnd/record

    src-write-to)

(require
    racket/match
    racket/list

    "helper.rkt"
    "consts.rkt")

(struct src-mot/long (long) #:transparent)
(struct src-bnd/long (long) #:transparent)

(struct src-data (frame value) #:transparent)

;;; Motion data
(struct src-mot/root-data src-data ())
(struct src-mot/step src-mot/root-data ())
(struct src-mot/posx src-mot/root-data ())
(struct src-mot/posy src-mot/root-data ())

(struct src-mot/part-data src-data (part) #:transparent)
(struct src-mot/part/dir src-mot/part-data ())
(struct src-mot/part/dis src-mot/part-data ())
(struct src-mot/part/angle src-mot/part-data ())
(struct src-mot/part/picture src-mot/part-data ())
(struct src-mot/part/scalex src-mot/part-data ())
(struct src-mot/part/scaley src-mot/part-data ())

;;; Bind data

(struct src-bnd/data src-data ())
(struct src-bnd/step src-bnd/data ())
(struct src-bnd/posx src-bnd/data ())
(struct src-bnd/posy src-bnd/data ())
(struct src-bnd/part src-bnd/data ())

;;; Records
(struct src-record (long data) #:transparent)
(struct src-mot/record src-record ())
(struct src-bnd/record src-record ())

;;; Raw Command operations
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

(define (?assoc lst ele)
    (let loop ([lft lst])
        (if ((caar lft) ele)
            (car lft)
            (loop (cdr lft)))))

;;; Motion attribute operations
;;; & Generating data

(define ((statement-sorter order) a b)
    (if (equal? (src-data-frame a) (src-data-frame b))
        (<
            (cdr (?assoc order a))
            (cdr (?assoc order b)))
        (< (src-data-frame a) (src-data-frame b))))

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

(define mot/statement-sorter
    (statement-sorter (list
                        (cons src-mot/step? 0)
                        (cons src-mot/posx? 1)
                        (cons src-mot/posy? 2)

                        (cons src-mot/part/dir? 3)
                        (cons src-mot/part/dis? 4)
                        (cons src-mot/part/angle? 5)
                        (cons src-mot/part/picture? 6)
                        (cons src-mot/part/scalex? 7)
                        (cons src-mot/part/scaley? 8))))

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

;;; Bind attributes operations
;;; & Generating datas

(define bnd/attribute-mapping
    (list
        (cons "data_bndStep" src-bnd/step)
        (cons "data_bndPosX" src-bnd/posx)
        (cons "data_bndPosY" src-bnd/posy)
        (cons "data_bndParts" src-bnd/part)))

(define (generate-bnd/root-data cmd)
    ((cdr (assoc (offset-key cmd) bnd/attribute-mapping)) (offset-frame cmd) (offset-value cmd)))

(define (generate-bnd/data cmds)
    ;;; (displayln cmds)
    (for/list ([cmd cmds])
        (case (cmd-type cmd)
            [(set) (when (equal? (set-key cmd) "data_bndLong")
                    (src-bnd/long (set-value cmd)))]
            [(set-offset) (generate-bnd/root-data cmd)])))

(define bnd/statement-sorter
    (statement-sorter (list
                        (cons src-bnd/step? 0)
                        (cons src-bnd/posx? 1)
                        (cons src-bnd/posy? 2)
                        (cons src-bnd/part? 3))))

(define (src-generate-bnd/record data)
    (let ([long/set (filter src-bnd/long? data)])
        (if (null? long/set)
            (src-bnd/record '() data)
            (src-bnd/record
                (src-bnd/long-long (car long/set))
                (sort
                    (filter
                        (lambda (e) (not (src-bnd/long? e)))
                        data)
                    bnd/statement-sorter)))))

;;; Writers
(define ((make-root/writer bin con) cmd out)
    (write-byte bin out)
    (write-byte (src-data-frame cmd) out)
    (write-bytes (con (src-data-value cmd)) out))
(define ((make-part/writer bin con) cmd out)
    (write-byte bin out)
    (write-byte (src-data-frame cmd) out)
    (write-byte (src-mot/part-data-part cmd) out)
    (write-bytes (con (src-data-value cmd)) out))

(define mot/writer-mapping
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
    ((cdr (?assoc mot/writer-mapping cmd)) cmd out))

(define bnd/writer-mapping
    (list
        (cons src-bnd/step? (make-root/writer bin-bnd/step uint16->bytes))
        (cons src-bnd/posx? (make-root/writer bin-bnd/posx int16->bytes))
        (cons src-bnd/posy? (make-root/writer bin-bnd/posy int16->bytes))
        (cons src-bnd/part? (make-root/writer bin-bnd/part uint16->bytes))))
(define (bnd/write-single cmd out)
    ((cdr (?assoc bnd/writer-mapping cmd)) cmd out))

;;; XXX: union these two similiar conditions?
(define (src-write-to record out)
    (match record
        [(? src-mot/record?)
            (let ([data (src-record-data record)])
                ;;; Write motion length
                (unless (null? (src-record-long record))
                    (write-byte bin-mot/long out)
                    (write-bytes (uint16->bytes (src-record-long record)) out))
                ;;; Write data
                (for ([cmd data])
                    (mot/write-single cmd out))
                ;;; EOF
                (write-byte bin-eof out)
            )]
        [(? src-bnd/record?)
            (let ([data (src-record-data record)])
                ;;; Write motion length
                (unless (null? (src-record-long record))
                    (write-byte bin-bnd/long out)
                    (write-bytes (uint16->bytes (src-record-long record)) out))
                ;;; Write data
                (for ([cmd data])
                    (bnd/write-single cmd out))
                ;;; EOF
                (write-byte bin-eof out))]
    ))
