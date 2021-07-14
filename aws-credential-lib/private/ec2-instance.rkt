#lang racket/base

(require aws-credential/private/refresh
         http/request
         net/http-easy
         net/url)

(provide make-ec2-instance-credential)

(struct ec2-instance-credential refresh-credential ())

(define (make-ec2-instance-credential)
  (make-refresh-credential ec2-instance-credential refresh-instance-credential))

(define base-instance-security-credential-url
  (string->url
   "http://169.254.169.254/latest/meta-data/iam/security-credentials"))

(define (get-instance-iam-name)
  (define rsp (get base-instance-security-credential-url))
  (dynamic-wind
   void
   (lambda ()
     (define name-b (response-body rsp))
     (bytes->string/utf-8 name-b))
   (lambda ()
     (response-close! rsp))))

(define (get-raw-credentials)
  (define iam-name (get-instance-iam-name))
  (define instance-security-credential-url
    (struct-copy url base-instance-security-credential-url
                 [path (append (url-path base-instance-security-credential-url)
                               (list (path/param iam-name null)))]))
  (define rsp
    (get instance-security-credential-url))
  (dynamic-wind
   void
   (lambda ()
     (response-json rsp))
   (lambda ()
     (response-close! rsp))))

(define (refresh-instance-credential)
  (define creds (get-raw-credentials))
  (values
   (list (hash-ref creds 'AccessKeyId)
         (hash-ref creds 'SecretAccessKey)
         (hash-ref creds 'Token))
   (- (gmt-8601-string->seconds
       (hash-ref creds 'Expiration))
      300)))


