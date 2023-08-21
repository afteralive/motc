#lang racket/base

(provide
    generate-data)

(require
    "consts.rkt")

(struct src-record-step (frame step) #:transparent)
(struct src-record-posx (frame posx) #:transparent)
(struct src-record-posy (frame posy) #:transparent)

(struct src-record-part-dir (part frame dir) #:transparent)
(struct src-record-part-dis (part frame dis) #:transparent)
(struct src-record-part-angle (part frame angle) #:transparent)
(struct src-record-part-picture (part frame picture) #:transparent)
(struct src-record-part-scalex (part frame scalex) #:transparent)
(struct src-record-part-scaley (part frame scaley) #:transparent)

(struct src-motion-record (long data))

(define (cmd-type cmd)
    (car cmd))
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

(define (generate-part-data cmds part)
    (for/list ([cmd cmds])
        (case (offset-key cmd)
            [("data_motPartPosDir") (src-record-part-dir part (offset-frame cmd) (offset-value cmd))]
            [("data_motPartPosDis") (src-record-part-dis part (offset-frame cmd) (offset-value cmd))]
            [("data_motPartAngle") (src-record-part-angle part (offset-frame cmd) (offset-value cmd))]
            [("data_motPartScaleX") (src-record-part-scalex part (offset-frame cmd) (offset-value cmd))]
            [("data_motPartScaleY") (src-record-part-scaley part (offset-frame cmd) (offset-value cmd))]
            [("data_motPartPicture") (src-record-part-picture part (offset-frame cmd) (offset-value cmd))]
        )))
(define (generate-root-data cmd)
    (case (offset-key cmd)
        [("data_motStep") (src-record-step (offset-frame cmd) (offset-value cmd))]
        [("data_motPosX") (src-record-posx (offset-frame cmd) (offset-value cmd))]
        [("data_motPosY") (src-record-posy (offset-frame cmd) (offset-value cmd))]
    ))
(define (generate-data cmds)
    (for/list ([cmd
                (filter
                    (lambda (cmd)
                        (or (eq? (car cmd) 'set-offset) (eq? (car cmd) 'with-surronding)))
                    cmds)])
        (cond
            [(eq? (cmd-type cmd) 'set-offset) (generate-root-data cmd)]
            [else (generate-part-data (surronding-cmds cmd) (surronding-part cmd))])
        ;;; (case (cmd-type cmd)
        ;;;     [('set-offset) cmd]
        ;;;     [('with-surronding) (generate-part-data (surronding-cmds cmd) (surronding-part cmd))])
    ))
