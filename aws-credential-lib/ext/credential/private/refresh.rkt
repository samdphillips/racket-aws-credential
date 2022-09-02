#lang racket/base

(require racket/contract
         racket/exn
         racket/match
         "interface.rkt"
         "log.rkt")

(provide
  refresh-credential
  (contract-out
    [make-refresh-credential
      (->* ((-> any/c any/c any/c aws-credential?)
            (-> (values (list/c string? string? string?)
                        positive?)))
           (#:wiggle (>=/c 0))
           aws-credential?)]))

(struct refresh-credential (pr req-ch rpy-ch)
  #:methods gen:aws-credential
  [(define (aws-credential-get cr)
     (log-aws-credential-debug "getting credential from ~a" cr)
     (match-define (refresh-credential pr req-ch rpy-ch) cr)
     (define oops-dead-evt
       (handle-evt (thread-dead-evt pr)
                   (λ (t) (error 'aws-credential-get "refresh thread has died: ~a" cr))))
     (sync oops-dead-evt
           (channel-put-evt req-ch #t))
     (sync oops-dead-evt
           (handle-evt rpy-ch (lambda (v) (apply values v)))))])

(define (refresh-process refresh wiggle req-ch rpy-ch)
  (define (expire-evt expires)
    (alarm-evt (* 1000 expires)))
  (define (update-keys)
    (log-aws-credential-info "refreshing credential")
    (define-values (keys expires) (refresh))
    (log-aws-credential-info "updated credential expires: ~a" expires)
    (log-aws-credential-info "updated credential expires with wiggle: ~a"
                             (- expires wiggle))
    (run keys (- expires wiggle)))
  (define (run keys expires)
    (sync (handle-evt (expire-evt expires)
                      (λ (e) (update-keys)))
          (handle-evt req-ch
                      (λ (ignore)
                         (channel-put rpy-ch keys)
                         (run keys expires)))))
  (with-handlers ([exn:fail?
                    (λ (e) (log-aws-credential-error
                             "failure in refresh thread: ~a"
                             (exn->string e)))])
    (update-keys)))

(define (make-refresh-credential maker refresh #:wiggle [wiggle 60])
  (define req-ch (make-channel))
  (define rpy-ch (make-channel))
  (define pr
    (thread (λ () (refresh-process refresh wiggle req-ch rpy-ch))))
  (maker pr req-ch rpy-ch))

