#lang racket/base

(require racket/contract
         "interface.rkt"
         "log.rkt")

(provide
  (struct-out refresh-credential)
  (contract-out
    [make-refresh-credential
      (->* ((-> any/c any/c any/c any/c aws-credential?)
            (-> (values (list/c string? string? string?)
                        positive?)))
           (#:wiggle (>=/c 0))
           aws-credential?)]))

(struct refresh-credential
  (refresh
   wiggle
   [keys    #:mutable]
   [expires #:mutable])
  #:methods gen:aws-credential
  [(define (aws-credential-get cr)
     (when (needs-refresh? cr)
       (log-aws-credential-info "~a needs refresh" cr)
       (update-credentials! cr))
     (define keys (refresh-credential-keys cr))
     (apply values keys))])

(define (needs-refresh? cr)
  (define-syntax-rule (log-expr expr)
    (let ([v expr])
      (log-aws-credential-debug "~s -> ~s" 'expr v)
      v))
  (log-aws-credential-debug "checking credential refresh")
  (log-expr
    (or (log-expr (not (refresh-credential-expires cr)))
        (log-expr (not (refresh-credential-keys cr)))
        (log-expr (>= (current-seconds) (refresh-credential-expires cr))))))

(define (update-credentials! cr)
  (define refresh (refresh-credential-refresh cr))
  (define wiggle (refresh-credential-wiggle cr))
  (define-values (keys expires) (refresh))
  (set-refresh-credential-keys! cr keys)
  (set-refresh-credential-expires! cr (- expires wiggle))
  (log-aws-credential-info "updated credential expires: ~a" expires)
  (log-aws-credential-info "updated credential expires with wiggle: ~a"
                           (- expires wiggle)))

(define (make-refresh-credential maker refresh #:wiggle [wiggle 60])
  (maker refresh wiggle #f #f))

