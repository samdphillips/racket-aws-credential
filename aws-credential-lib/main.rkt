#lang racket/base

(provide (all-from-out aws-credential/interface)
         with-aws-credential)

(require (prefix-in aws: aws/keys)
         aws-credential/interface)

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
        [else (error 'with-aws-credential
                     "not a valid aws credential\n  credential: ~.a"
                     cred)])]))

