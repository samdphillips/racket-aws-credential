#lang racket/base

(require "interface.rkt"
         inifile)

(provide load-credential-from-file)

(define (load-credential-from-file filename profile-name)
  (call-with-input-file filename
    (lambda (inp)
      (define cfg (read-inifile inp))
      (define profile (hash-ref cfg profile-name))
      (aws-memory-credential
        (hash-ref profile "aws_access_key_id")
        (hash-ref profile "aws_secret_access_key")
        (hash-ref profile "aws_session_token" #f)))))

