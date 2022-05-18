#lang racket/base

(require racket/contract
         racket/exn
         "interface.rkt"
         "log.rkt")

(provide
  refresh-credential
  (contract-out
    [make-refresh-credential
      (->* ((-> any/c any/c aws-credential?)
            (-> (values (list/c string? string? string?)
                        positive?)))
           (#:wiggle (>=/c 0))
           aws-credential?)]))

(struct refresh-credential (pr ch)
  #:methods gen:aws-credential
  [(define (aws-credential-get cr)
     (when (thread-dead? (refresh-credential-pr cr))
       (error 'aws-credential-get
              "refresh thread has died"))
     (define rpy-ch (make-channel))
     (channel-put (refresh-credential-ch cr) rpy-ch)
     (apply values (channel-get rpy-ch)))])

(define (refresh-process refresh wiggle req-ch)
  (define (expire-evt expires)
    (alarm-evt (* 1000 expires)))
  (define (update-keys)
    (define-values (keys expires) (refresh))
    (log-aws-credential-info "updated credential expires: ~a" expires)
    (log-aws-credential-info "updated credential expires with wiggle: ~a"
                             (- expires wiggle))
    (run keys (- expires wiggle)))
  (define (run keys expires)
    (sync (handle-evt (expire-evt expires)
                      (λ (e) (update-keys)))
          (handle-evt req-ch
                      (λ (rpy-ch)
                         (channel-put rpy-ch keys)
                         (run keys expires)))))
  (with-handlers ([exn:fail?
                    (λ (e) (log-aws-credential-error
                             "failure in refresh thread: ~a"
                             (exn->string e)))])
    (update-keys)))

(define (make-refresh-credential maker refresh #:wiggle [wiggle 60])
  (define ch (make-channel))
  (define pr
    (thread (λ () (refresh-process refresh wiggle ch))))
  (maker pr ch))

