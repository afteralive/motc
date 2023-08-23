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

(define target
    (command-line
        #:program "motc"
        #:once-any
            [("-o") outpath
                "Output file" 
                (output-path outpath)]
        #:args (filename)
        filename))

(when (null? (output-path))
    (output-path (regexp-replace #rx".txt$" target ".mot")))

(let ([out (open-output-file (output-path) #:exists 'replace)])
    (src-write-to 
        (src-generate-mot/record
            (generate-mot/data
                (src-parse
                    (open-input-file
                        target))))
    out)
    (close-output-port out))