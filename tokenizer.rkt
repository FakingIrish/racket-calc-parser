#lang racket

(provide (all-defined-out))


; token definition
(define (get-token cur-char)
  (cond
    [(equal? #\( cur-char) 'left-paren]
    [(equal? #\) cur-char) 'right-paren]
    [(equal? #\+ cur-char) 'add-op]
    [(equal? #\- cur-char) 'add-op]
    [(equal? #\/ cur-char) 'mult-op]
    [(equal? #\* cur-char) 'mult-op]
    [(equal? #\$ cur-char) 'eof-start]
    [(equal? #\: cur-char) 'asgn-start]
    [(char-alphabetic? cur-char) 'id-start]
    [(char-numeric? cur-char) 'num-start]
    [(char-whitespace? cur-char) 'whitespace]
    [else 'invalid]))


; lexer creating tokens
(define (tokenize line)
  (define (iter chars cur-token tokens)
    (cond
      [(empty? chars)
       (if (list? cur-token)
           (cond
             [(equal? (first cur-token) 'eof-start) 'invalid]
             [(equal? (first cur-token) 'asgn-start) 'invalid]
             [(equal? (first cur-token) 'num-start) (cons 'num tokens)]
             [(equal? (first cur-token) 'id-start)
              (cond
                [(equal? (get-token-data cur-token) "read") (cons 'read tokens)]
                [(equal? (get-token-data cur-token) "write") (cons 'write tokens)]
                [else (cons 'id tokens)])]
             [else 'invalid])
           tokens)]
      [(symbol? cur-token)
       (let ([token (get-token (first chars))])
         (cond
           [(equal? token 'id-start) (iter (rest chars) (list 'id-start (list (first chars))) tokens)]
           [(equal? token 'num-start) (iter (rest chars) (list 'num-start (list (first chars))) tokens)]
           [(equal? token 'eof-start) (iter (rest chars) '(eof-start) tokens)]
           [(equal? token 'asgn-start) (iter (rest chars) '(asgn-start) tokens)]
           [(equal? token 'invalid) 'invalid]
           [(equal? token 'whitespace) (iter (rest chars) 'whitespace tokens)]
           [else (iter (rest chars) token (cons token tokens))]))]
      [else
       (complete-token chars cur-token tokens)]))
  (define (complete-token chars cur-token tokens)
    (cond
      [(equal? (first cur-token) 'id-start)
       (if (alpha-num-terminator? (first chars))
           (cond
             [(equal? (get-token-data cur-token) "read") (iter (rest chars) 'read (cons 'read tokens))]
             [(equal? (get-token-data cur-token) "write") (iter (rest chars) 'write (cons 'write tokens))]
             [else (iter chars 'id (cons 'id tokens))])
           (if (char-alphabetic? (first chars))
               (iter (rest chars) (list 'id-start (cons (first chars) (second cur-token))) tokens)
               'invalid))]
      [(equal? (first cur-token) 'num-start)
       (if (alpha-num-terminator? (first chars))
           (iter chars 'num (cons 'num tokens))
           (if (char-numeric? (first chars))
               (iter (rest chars) (list 'num-start (cons (first chars) (second cur-token))) tokens)
               'invalid))]
      [(equal? (first cur-token) 'eof-start)
       (if (equal? #\$ (first chars))
           (cons 'EOF tokens)
           'invalid)]
      [(equal? (first cur-token) 'asgn-start)
       (if (equal? #\= (first chars))
           (iter (rest chars) 'asgn-op (cons 'asgn-op tokens))
           'invalid)]
      [else 'invalid]))
  (let ([tokens (iter (string->list line) 'start '())])
    (if (list? tokens)
        (reverse tokens)
        tokens)))


; defines acceptable alphanumeric termination characters
(define (alpha-num-terminator? char)
  (not (not (member (get-token char) '(left-paren right-paren asgn-end add-op mult-op whitespace eof-start)))))


; tokens added to list in reverse to fix ordering
(define (get-token-data token)
  (list->string (reverse (second token))))
