#lang racket/base

(require "interface.rkt"
         racket/contract)

(provide
  refresh-credential
  (contract-out
    [make-refresh-credential
      (-> (-> any/c any/c any/c aws-credential?)
          (-> (values (list/c string? string? string?)
                      positive?))
          aws-credential?)]))

(struct refresh-credential
  (refresh
   [keys    #:mutable]
   [expires #:mutable])
  #:methods gen:aws-credential
  [(define (aws-credential-get cr)
     (when (needs-refresh? cr)
       (update-credentials! cr))
     (define keys (refresh-credential-keys cr))
     (apply values keys))])

(define (needs-refresh? cr)
  (or (not (refresh-credential-expires cr))
      (not (refresh-credential-keys cr))
      (>= (current-seconds) (refresh-credential-expires cr))))

(define (update-credentials! cr)
  (define refresh (refresh-credential-refresh cr))
  (define-values (keys expires) (refresh))
  (set-refresh-credential-keys! cr keys)
  (set-refresh-credential-expires! cr expires))

(define (make-refresh-credential maker refresh)
  (maker refresh #f #f))

