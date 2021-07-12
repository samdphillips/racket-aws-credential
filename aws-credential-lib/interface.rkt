#lang racket/base

(provide aws-credential?

         gen:aws-credential
         aws-credential-access-key
         aws-credential-secret-access-key
         aws-credential-security-token

         current-aws-credential

         with-aws-credential
         with-current-aws-credential)

(require (prefix-in aws: aws/keys)
         racket/generic
         syntax/parse/define)

(define-generics aws-credential
  [aws-credential-get aws-credential]
  [aws-credential-access-key aws-credential]
  [aws-credential-secret-access-key aws-credential]
  [aws-credential-security-token aws-credential]
  #:fallbacks
  [(define/generic get-access-key     aws-credential-access-key)
   (define/generic get-secret-key     aws-credential-secret-access-key)
   (define/generic get-security-token aws-credential-security-token)
   (define/generic get-all            aws-credential-get)

   (define cycle-detect? (make-parameter #f))

   (define (aws-credential-get cr)
     (when (cycle-detect?)
       (error (cycle-detect?)
              (string-append
               "cycle finding credentials. "
               "aws-credential implementation is incomplete.\n"
               "  credential: ~.a")
              cr))
     (parameterize ([cycle-detect? 'aws-credential-get])
       (values
        (get-access-key cr)
        (get-secret-key cr)
        (get-security-token cr))))

   (define-syntax-rule (defget name (vars ...) ret)
     (define (name cr)
       (define-values (vars ...)
         (parameterize ([cycle-detect? 'name])
           (get-all cr)))
       ret))

   (defget aws-credential-access-key        (a b c) a)
   (defget aws-credential-secret-access-key (a b c) b)
   (defget aws-credential-security-token    (a b c) c)])

(define current-aws-credential (make-parameter #f))

(define-syntax-parse-rule (with-current-aws-credential body ...)
  (with-aws-credential (current-aws-credential) body ...))

(define-syntax-parse-rule (with-aws-credential cred-expr body ...)
  (let ()
    (define (do-body) body ...)
    (let ([cred cred-expr])
      (cond
        ;; parameter unset just use the raw parameters as is
        [(not cred) (do-body)]
        [(aws-credential? cred)
         (define-values (access-key secret-access-key security-token)
           (aws-credential-get cred))
         (parameterize ([aws:public-key     access-key]
                        [aws:private-key    secret-access-key]
                        [aws:security-token security-token])
           (do-body))]
        [else (error 'with-aws-credential
                     "not a valid aws credential\n  credential: ~.a"
                     cred)]))))


