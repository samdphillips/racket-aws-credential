#lang info

(define name "aws-credential-lib")
(define collection "aws-credential")
(define version "0.1")
(define deps
  '("base"
    "keyring-lib"
    "https://github.com/samdphillips/racket-aws-credential.git?path=aws-credential-lib"))
(define pkg-authors '(samdphillips@gmail.com))

