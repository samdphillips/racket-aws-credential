#lang racket/base

(provide aws-credential?

         gen:aws-credential
         aws-credential-get
         aws-credential-access-key
         aws-credential-secret-access-key
         aws-credential-security-token

         current-aws-credential

         aws-memory-credential)

(require racket/generic)

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

(struct aws-memory-credential (access-key secret-access-key security-token)
  #:methods gen:aws-credential
  [(define-syntax-rule (def method accessor)
     (define (method a) (accessor a)))

   (def aws-credential-access-key aws-memory-credential-access-key)
   (def aws-credential-secret-access-key aws-memory-credential-secret-access-key)
   (def aws-credential-security-token aws-memory-credential-security-token)])

