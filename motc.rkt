#lang racket/base

;;;
;;; motc (mot Compiler)
;;; (C)opyright 2023 Shan Ran
;;;

(require
    parser-tools/lex
    parser-tools/yacc
    (prefix-in : parser-tools/lex-sre))

;;; basic forms:
;;; X=Y;
;;; X[I]=Y;
;;; with(Process[I]) {...}

(define ?sample-code "")

(define-tokens src-values (NUMBER IDENTITY))
(define-empty-tokens src-surrondings (EOF LSQUARE RSQUARE LBRACE RBRACE LBRACKET RBRACKET END-STATEMENT))
(define-empty-tokens src-operators (EQUAL WITH PROCESS))

(define src-lexer
    (lexer
        [(eof) (token-EOF)]
        ["[" (token-LSQUARE)]
        ["]" (token-RSQUARE)]
        ["(" (token-LBRACE)]
        [")" (token-RBRACE)]
        ["{" (token-LBRACKET)]
        ["}" (token-RBRACKET)]
        ["=" (token-EQUAL)]
        [";" (token-END-STATEMENT)]
        ["with" (token-WITH)]
        ["Process" (token-PROCESS)]
        [(:: (:? "-")
            (:or
                (:: "." (:+ numeric))
                (:: (:* numeric) "." (:+ numeric))
                (:+ numeric)))
            
                    (token-NUMBER (string->number lexeme))]
        [(:+ (:or alphabetic #\_)) (token-IDENTITY lexeme)]
        [whitespace (src-lexer input-port)]
    ))

(define src-parser
    (parser
        [start statements]
        [end EOF]
        [error
            (lambda (tok-ok? tok-name tok-value #:stack trace)
                (displayln (format "ERROR: ~a ~a ~a: ~a" tok-ok? tok-name tok-value trace)))]
        [tokens src-values src-surrondings src-operators]
        [grammar
            [statements
                [() '()]
                [(statement statements) (cons $1 $2)]]
            [statement
                [(assignment) $1]
                [(with-surronding) $1]]

            [assignments
                [() '()]
                [(assignment assignments) (cons $1 $2)]]
            [assignment
                [(IDENTITY EQUAL NUMBER END-STATEMENT) (list 'set $1 $3)]
                [(IDENTITY LSQUARE NUMBER RSQUARE EQUAL NUMBER END-STATEMENT) (list 'set-offset $1 $3 $6)]
            ]

            [process-locator
                [(PROCESS LSQUARE NUMBER RSQUARE) $3]]
            [with-surronding
                [(WITH LBRACE process-locator RBRACE LBRACKET assignments RBRACKET)
                    (list 'with-surronding $3 $6)]]
        ]))

(define ?p (open-input-string ?sample-code))
(define (read-to-end lexer code)
    (define p (open-input-string code))
    (let loop ([lst '()])
        (let ([r (lexer p)])
            (if (not (equal? (token-name r) 'EOF))
                (loop (append lst (list r)))
                (begin
                    (close-input-port p)
                    lst)))))

(define ?tokens (read-to-end src-lexer ?sample-code))