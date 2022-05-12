#lang racket/base

(provide with-aws-credential)

(require (prefix-in aws: aws/keys)
         aws/ext/credential/interface)

;; with-aws-credential : [aws-credential] thunk
;; sets the parameters for authentication that the aws library uses
(define with-aws-credential
  (case-lambda
    [(thunk) (with-aws-credential (current-aws-credential) thunk)]
    [(cred thunk)
     (cond
       [(not cred) (thunk)]
       [(aws-credential? cred)
         (define-values (access-key secret-access-key security-token)
           (aws-credential-get cred))
         (parameterize ([aws:public-key     access-key]
                        [aws:private-key    secret-access-key]
                        [aws:security-token security-token])
           (thunk))]
        [else (raise-argument-error
                'with-aws-credential
                "(or/c aws-credential? #f)"
                cred)])]))

