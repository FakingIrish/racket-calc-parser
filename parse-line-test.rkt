#lang racket

(require rackunit
         "tokenizer.rkt"
         "parser.rkt")


; parse-line unit tests

(check-equal? (parse-line "read a") 'accept)
(check-equal? (parse-line "$$") 'eof)
(check-equal? (parse-line "^") 'invalid)
(check-equal? (parse-line "A := 1") 'accept)
(check-equal? (parse-line "write A") 'accept)
(check-equal? (parse-line "A + B") 'invalid)
(check-equal? (parse-line "write A + B") 'accept)
(check-equal? (parse-line "write (A + B)") 'accept)
(check-equal? (parse-line "C := A ^ B ") 'invalid)
(check-equal? (parse-line "C := Fred + Wilma - reader + writer * 5") 'accept)
(check-equal? (parse-line "write (A + 1) * + ((B - 2) + A - (B * A))") 'invalid)
(check-equal? (parse-line "write (A + 1) * ((B - 2) + A - (B * A)))))))") 'invalid)
(check-equal? (parse-line "C := Q + R * W + 6 - Z") 'accept)
(check-equal? (parse-line "write (A + 1) * (( B - 2) + A - (B * A))") 'accept)