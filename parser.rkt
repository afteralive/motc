#lang racket/base

(require
    racket/match
    racket/contract

    parser-tools/lex
    parser-tools/yacc
    (prefix-in : parser-tools/lex-sre))

(provide
    src-lexer
    src-parser
    (contract-out
        [src-parse (-> (or/c string? input-port?) list?)]))

(define-tokens src-values (NUMBER IDENTITY))
(define-empty-tokens src-surrondings (EOF LSQUARE RSQUARE LBRACE RBRACE LBRACKET RBRACKET END-STATEMENT))
(define-empty-tokens src-operators (EQUAL WITH PROCESS))

;;; Lexer for .txt source file
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

        ;;; Special formal of "with"
        ["with" (token-WITH)]

        ;;; Convenient way to fetch part index
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

;;; Parser for .txt source file
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

(define (src-parse input)
    (match input
        [(? string?) (let* ([p (open-input-string input)]
                            [r (src-parse p)])
                        (close-input-port p)
                        r)]
        [(? input-port?) (src-parser (lambda () (src-lexer input)))]))
