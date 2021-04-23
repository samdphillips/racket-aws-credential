#lang racket/base

(provide keyring-aws-credential)

(require keyring
         "interface.rkt")

(struct keyring-aws-credential (service)
  #:methods gen:aws-credential
  [(define (aws-credential-access-key cr)
     (define key
       (get-password (keyring-aws-credential-service cr) "ACCESS_KEY_ID"))
     (unless key
       (error 'aws-credential-access-key "No key found in keyring"))
     (bytes->string/utf-8 key))

   (define (aws-credential-secret-access-key cr)
     (define key
       (get-password (keyring-aws-credential-service cr) "SECRET_ACCESS_KEY"))
     (unless key
       (error 'aws-credential-secret-access-key
              "No key found in keyring"))
     (bytes->string/utf-8 key))

   (define (aws-credential-security-token cr) #f)])

