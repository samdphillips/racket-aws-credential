#lang racket/base

(require aws/ext/credential/key
         aws/ext/credential/private/refresh
         aws/sts)

(provide sts-credential?
         make-sts-credential)

(struct sts-credential refresh-credential ())

(define (make-sts-credential src-cred role-arn role-session-name
                             #:wiggle [wiggle 60]
                             #:duration [duration #f])
  (define (refresh)
    (define assume-role-rpy
      (with-aws-credential src-cred
        (lambda ()
          (sts-assume-role role-arn role-session-name duration))))
    (define cr (sts-assume-role-result-credential assume-role-rpy))
    (values (list (credential-access-key-id cr)
                  (credential-secret-access-key cr)
                  (credential-session-token cr))
            (credential-expiration cr)))
  (make-refresh-credential sts-credential refresh #:wiggle wiggle))

