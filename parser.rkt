#lang racket

(require "tokenizer.rkt"
         racket/match)

(provide (all-defined-out))


; functions to call file into DrRacket to parse
(define (parse file)
  (parse-lines (read-file-lines file)))

(define (file-contents file)
  (string-trim (port->string (open-input-file file) #:close? #t)))


; regex to split strings on white space
(define (read-file-lines file)
  (string-split (file-contents file)
                #px"(\r\n|\r|\n)"))


; main parse function to trace line number and deliver results to console
(define (parse-lines lines)
  (define (iter lines line-num)
    (cond
      [(empty? lines) "Accept"]
      [(let ([line-result (parse-line (first lines))])
         (cond
           [(equal? line-result 'accept)
            (iter (rest lines) (+ line-num 1))]
           [(equal? line-result 'invalid)
            (~a "Syntax error found on line " line-num)]
           [(equal? line-result 'eof)
            "Accept"]
           [else (iter lines 1)]))]))
  (if (empty? lines) "File Is Empty: Cannot Parse Program" (iter lines 1)))


; defined in another .rkt - parses single lines at a time
(define (parse-line line)
  (parse-tokens (tokenize line)))


; parses tokens by checking against grammar, token by token through a line
(define (parse-tokens tokens)
  (define (parse-read tokens)
    (cond
      [(empty? tokens) 'invalid]
      [(equal? (first tokens) 'id)
       (if (empty? (rest tokens)) 'accept 'invalid)]
      [else 'invalid]))
  (define (parse-write tokens)
    (cond
      [(empty? tokens) 'invalid]
      [else (parse-expr tokens)]))
  (define (parse-asgn tokens)
    (cond
      [(empty? tokens) 'invalid]
      [(equal? (first tokens) 'asgn-op)
       (parse-expr (rest tokens))]
      [else 'invalid]))
  (define (parse-expr tokens)
    (if (or (equal? (first tokens) 'id)
            (equal? (first tokens) 'num)
            (equal? (first tokens) 'left-paren))
        (let ([rem-tokens (parse-term tokens)])
          (if (equal? rem-tokens 'invalid) 'invalid (parse-term-tail rem-tokens)))
        'invalid))
  (define (parse-term tokens)
    (if (or (equal? (first tokens) 'id)
            (equal? (first tokens) 'num)
            (equal? (first tokens) 'left-paren))
        (let ([rem-tokens (parse-factor tokens)])
          (if (equal? rem-tokens 'invalid) 'invalid (parse-factor-tail rem-tokens)))
        'invalid))
  (define (parse-factor tokens)
    (cond
      [(or (equal? (first tokens) 'id)
           (equal? (first tokens) 'num))
       (rest tokens)]
      [(equal? (first tokens) 'left-paren)
       (let ([rem-tokens (parse-expr (rest tokens))])
         (cond
           [(equal? rem-tokens 'invalid) 'invalid]
           [(equal? (first rem-tokens) 'right-paren) (rest rem-tokens)]
           [else 'invalid]))]
      [else 'invalid]))
  (define (parse-term-tail tokens)
    (cond
      [(empty? tokens) tokens]
      [(or (equal? (first tokens) 'right-paren)
           (equal? (first tokens) 'id)
           (equal? (first tokens) 'num)
           (equal? (first tokens) 'read)
           (equal? (first tokens) 'write)
           (equal? (first tokens) 'EOF))
       tokens]
      [(equal? (first tokens) 'add-op)
       (let ([rem-tokens (parse-term (rest tokens))])
         (if (equal? rem-tokens 'invalid) 'invalid (parse-term-tail rem-tokens)))]
      [else 'invalid]))
  (define (parse-factor-tail tokens)
    (cond
      [(empty? tokens) tokens]
      [(or (equal? (first tokens) 'right-paren)
           (equal? (first tokens) 'id)
           (equal? (first tokens) 'num)
           (equal? (first tokens) 'read)
           (equal? (first tokens) 'write)
           (equal? (first tokens) 'add-op)
           (equal? (first tokens) 'EOF))
       tokens]
      [(equal? (first tokens) 'mult-op)
       (let ([rem-tokens (parse-factor (rest tokens))])
         (if (equal? rem-tokens 'invalid) 'invalid (parse-factor-tail rem-tokens)))]
      [else 'invalid]))
  (define (parse-stmt tokens)
    (cond
      [(empty? tokens) 'accept]
      [(equal? (first tokens) 'read) (parse-read (rest tokens))]
      [(equal? (first tokens) 'id)
       (if (empty? (parse-asgn (rest tokens))) 'accept 'invalid)]
      [(equal? (first tokens) 'num) 'invalid]
      [(equal? (first tokens) 'write)
       (if (empty? (parse-write (rest tokens))) 'accept 'invalid)]
      [(equal? (first tokens) 'asgn-op) 'invalid]
      [(equal? (first tokens) 'mult-op) 'invalid]
      [(equal? (first tokens) 'add-op) 'invalid]
      [(equal? (first tokens) 'EOF) 'eof]
      [else (parse-stmt (rest tokens))]))
  (if (equal? tokens 'invalid) 'invalid (parse-stmt tokens)))