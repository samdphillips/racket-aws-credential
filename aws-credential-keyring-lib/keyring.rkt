#lang racket/base

(require racket/contract/base)

(provide
  (contract-out
    [make-keyring-aws-credential
      (->* (string?) (string? string?) aws-credential?)]))

(require keyring
         aws-credential/interface)

(struct keyring-aws-credential (service user-tag secret-tag)
  #:methods gen:aws-credential
  [(define (aws-credential-access-key cr)
     (define key
       (get-password (keyring-aws-credential-service cr)
                     (keyring-aws-credential-user-tag cr)))
     (unless key
       (error 'aws-credential-access-key "No key found in keyring"))
     (bytes->string/utf-8 key))

   (define (aws-credential-secret-access-key cr)
     (define key
       (get-password (keyring-aws-credential-service cr)
                     (keyring-aws-credential-secret-tag cr)))
     (unless key
       (error 'aws-credential-secret-access-key
              "No key found in keyring"))
     (bytes->string/utf-8 key))

   (define (aws-credential-security-token cr) #f)])

(define (make-keyring-aws-credential service-name
                                     [access-key-name "ACCESS_KEY_ID"]
                                     [secret-key-name "SECRET_ACCESS_KEY"])
  (keyring-aws-credential service-name
                          access-key-name
                          secret-key-name))

