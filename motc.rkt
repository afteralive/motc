#lang racket/base

;;;
;;; motc (mot Compiler)
;;; (C)opyright 2023 Shan Ran
;;;

(require
    racket/cmdline
    racket/string

    "parser.rkt"
    "writer.rkt")

;;; TODO: Expand Command-line interface

(define output-path (make-parameter '()))
(define mode (make-parameter 'mot))

(define target
    (command-line
        #:program "motc"
        #:once-each
            [("-o") outpath
                "Output file" 
                (output-path outpath)]
            [("-m" "--mode") outmode
                "Compile mode(motion/bind)"
                (mode (if (equal? outmode "bind")
                            'bind
                            'motion))]
        #:args (filename)
        filename))

(when (null? (output-path))
    (output-path (regexp-replace #rx".txt$" target ".mot")))

(let ([out (open-output-file (output-path) #:exists 'replace)])
    (src-write-to 
        (cond
            [(eq? (mode) 'motion)
                (src-generate-mot/record
                    (generate-mot/data
                        (src-parse
                            (open-input-file
                                target))))]
            [(eq? (mode) 'bind)
                (src-generate-bnd/record
                    (generate-bnd/data
                        (src-parse
                            (open-input-file
                                target))))])

            
    out)
    (close-output-port out))