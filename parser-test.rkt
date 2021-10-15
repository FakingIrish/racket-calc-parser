#lang racket


(require rackunit
         "parser.rkt")


; unit testing for parser program
(check-equal? (parse "exampleprogram.txt")
              "Accept"
              "Parses simple program file")


(define (valid-program lines message)
  (check-equal? (parse-lines lines)
                "Accept"
                message))

(define (invalid-program lines error-lines message)
  (check-equal? (parse-lines lines)
                (~a "Syntax error found on line " error-lines)
                message))

(check-equal? (parse-lines '())
              "File Is Empty: Cannot Parse Program" 
              "Empty file")

(valid-program '("$$")
              "Simplest valid program")

(valid-program '("$$"
                 "This ^")
               "Accept: Ignore comments past '$$'")

(invalid-program '("write A$$") 1
               "Terminator immediately following id")

(valid-program '("write A+B"
                 "$$")
               "Simple addition without whitespace")

(valid-program '("write A + B"
                 "$$")
               "Simple addition") 

(invalid-program '("( A )"
                 "$$") 1
               "Invalid: expression is not a statement")

(invalid-program '("read A read B"
                 "$$") 1
               "Invalid: multiple statements on single line")

(invalid-program '("read"
                   "A"
                   "$$")
                 1
                 "read statement split by new line")
                 

               
(invalid-program '("readA"
                   "$$")
                 1
                 "Keyword without space")
                 
                 

; Instructor-provided examples

(valid-program '("read A"
                 "read B"
                 "write (A + 1) * ((B - 2) + A - (B * A))"
                 "$$")
               "Basic valid example")

(valid-program '("read A"
                 "read B"
                 "C := Q + R * W + 6 - Z"
                 "write D"
                 "$$")
               "Messy valid example")

(valid-program '("read A"
                 "read B"
                 "C := Fred + Wilma - reader + writer * 5"
                 "$$")
               "Messy valid example testing longest match")

(invalid-program '("read A"
                   "read B"
                   "write (A + 1) * + ((B - 2) + A - (B * A))"
                   "$$")
                 3
                 "Invalid: mult_op followed by add_op")

(invalid-program '("read A"
                   "read B"
                   "write (A + 1) * ((B - 2) + A - (B * A)))))))"
                   "$$")
                 3
                 "Invalid: too many closing parenthesis")

(invalid-program '("read A"
                   "read B"
                   "C := A ^ B"
                   "$$")
                 3
                 "Invalid: Unrecognized token '^'")