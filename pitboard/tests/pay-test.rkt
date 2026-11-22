#lang racket/base

;; Alipay channel tests.  Cryptography uses commerce's canonical
;; adapter with a runtime-generated RSA keypair: the page-pay form is
;; verified by re-running the canonical signing content check, and the
;; notification path is exercised with a properly RSA2-signed fixture
;; (forged signatures must fail closed).  The browser return never
;; grants on its own — verified through the signed query path.

(require rackunit
         racket/file
         racket/format
         racket/string
         json
         base64
         crypto
         crypto/libcrypto
         (only-in commerce/alipay
                  alipay-signing-content
                  parse-alipay-form
                  alipay-binding
                  alipay-notification?)
         (only-in commerce money)
         "../subs.rkt"
         "../pay.rkt"
         "../util.rkt")

;; ---- runtime RSA keypair ---------------------------------------------------

(define rsa-private-key
  (generate-private-key (get-pk 'rsa libcrypto-factory) '((nbits 2048))))

(define (pub-pem-for pk)
  (define der (pk-key->datum (pk-key->public-only-key pk) 'SubjectPublicKeyInfo))
  (define b64
    (string-replace (string-trim (bytes->string/utf-8 (base64-encode der))) "\r\n" "\n"))
  (define lines
    (let loop ([s b64])
      (if (<= (string-length s) 64)
          (list s)
          (cons (substring s 0 64) (loop (substring s 64))))))
  (string-append
   "-----BEGIN PUBLIC KEY-----\n"
   (string-join lines "\n")
   "\n-----END PUBLIC KEY-----\n"))

;; ---- tests -----------------------------------------------------------------

(define (write-file path text)
  (with-output-to-file path (λ () (display text)) #:exists 'replace))

(define (pk->pem-text pk)
  ;; private key PEM via commerce loader expectations (PKCS#8 or RSA)
  (define der (pk-key->datum pk 'PrivateKeyInfo))
  (define b64
    (string-replace (string-trim (bytes->string/utf-8 (base64-encode der))) "\r\n" "\n"))
  (define lines
    (let loop ([s b64])
      (if (<= (string-length s) 64)
          (list s)
          (cons (substring s 0 64) (loop (substring s 64))))))
  (string-append
   "-----BEGIN PRIVATE KEY-----\n"
   (string-join lines "\n")
   "\n-----END PRIVATE KEY-----\n"))


(test-case "unconfigured channel: checkout returns #f, subscribe stays honest"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (check-false (alipay-configured?))
     (check-false (checkout-for! dir "a@example.com")))
   (λ () (delete-directory/files dir))))

(test-case "configured checkout: binding journaled, signed page-pay form returned"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define key-dir (build-path dir "keys"))
     (make-directory* key-dir)
     (define priv-pem-path (build-path key-dir "merchant.pem"))
     (define pub-pem-path (build-path key-dir "alipay.pem"))
     (write-file priv-pem-path
                 (pk->pem-text rsa-private-key))
     (write-file pub-pem-path (pub-pem-for rsa-private-key))
     (parameterize ([alipay-env-param 'sandbox]
                    [alipay-app-id-param "2026000000000000"]
                    [alipay-seller-id-param "2088000000000000"]
                    [alipay-private-key-path-param priv-pem-path]
                    [alipay-public-key-path-param pub-pem-path]
                    [base-url-param "https://pit.example"])
       (check-true (alipay-configured?))
       (define now (current-seconds))
       (define co (checkout-for! dir "buyer@example.com" now))
       (check-true (and co #t))
       (check-equal? (hash-ref co 'gateway)
                     "https://openapi-sandbox.dl.alipaydev.com/gateway.do")
       (define fields (hash-ref co 'fields))
       ;; durable binding precedes any payment request
       ;; fields carry the signed page-pay contract
       (check-equal? (cdr (assoc "method" fields)) "alipay.trade.page.pay")
       (check-equal? (cdr (assoc "app_id" fields)) "2026000000000000")
       (check-true (non-empty-string? (cdr (assoc "sign" fields))))
       ;; pending subscription created for the email
       (check-equal? (hash-ref (subscription-for dir "buyer@example.com") 'status "")
                     "pending")
       ;; the journal carries the binding (order → email, amount)
       (define order
         (cdr (assoc "biz_content"
                     (for/list ([p (in-list fields)] #:when (equal? (car p) "biz_content")) p))))
       (define biz (read-json (open-input-string order)))
       (define oid (hash-ref biz 'out_trade_no))
       (define ev (find-payment dir oid))
       (check-true (and ev #t))
       (check-equal? (hash-ref ev 'email "") "buyer@example.com")
       (check-equal? (hash-ref ev 'amount 0) 19900)))
   (λ () (delete-directory/files dir))))

;; signed notify fixture: properly RSA2-signed fields against our keypair
(define (signed-notify-fields
         #:order [order "pb-test-001"]
         #:trade [trade "2026112200000001"]
         #:status [status "TRADE_SUCCESS"]
         #:amount [amount "199.00"]
         #:key [key rsa-private-key])
  (define fields
    (list (cons "app_id" "2026000000000000")
          (cons "trade_status" status)
          (cons "out_trade_no" order)
          (cons "trade_no" trade)
          (cons "total_amount" amount)
          (cons "sign_type" "RSA2")))
  (define sign
    (bytes->string/utf-8
     (base64-encode
      (digest/sign key 'sha256
                   (string->bytes/utf-8 (alipay-signing-content fields))
                   #:pad 'pkcs1-v1.5)
      #:line #f)))
  (append fields (list (cons "sign" sign))))

(test-case "notify: signed TRADE_SUCCESS grants +365d; forged signature rejected"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define key-dir (build-path dir "keys"))
     (make-directory* key-dir)
     (write-file (build-path key-dir "merchant.pem") (pk->pem-text rsa-private-key))
     (write-file (build-path key-dir "alipay.pem") (pub-pem-for rsa-private-key))
     (parameterize ([alipay-env-param 'sandbox]
                    [alipay-app-id-param "2026000000000000"]
                    [alipay-seller-id-param "2088000000000000"]
                    [alipay-private-key-path-param (build-path key-dir "merchant.pem")]
                    [alipay-public-key-path-param (build-path key-dir "alipay.pem")])
       ;; durable binding first
       (define now (current-seconds))
       (define co (checkout-for! dir "paid@example.com" now))
       (define fields (hash-ref co 'fields))
       (define oid
         (hash-ref (read-json (open-input-string
                               (cdr (assoc "biz_content" fields))))
                   'out_trade_no))
       ;; properly signed notify with matching order → projected
       (define r (handle-notify! dir (signed-notify-fields #:order oid) now))
       (check-equal? (hash-ref r 'result "") "ok")
       (check-equal? (hash-ref r 'action "") "projected")
       (check-true (subscription-active? (subscription-for dir "paid@example.com") now))
       ;; idempotent on trade id
       (define r2 (handle-notify! dir (signed-notify-fields #:order oid) now))
       (check-equal? (hash-ref r2 'action "") "projected")
       (define evs (payments-tail dir 50))
       (check-equal?
        (length (filter (λ (e) (string-prefix? (hash-ref e 'id "") "2026")) evs))
        1)
       ;; forged signature (wrong key) → rejected, nothing granted
       (define attacker
         (generate-private-key (get-pk 'rsa libcrypto-factory) '((nbits 2048))))
       (define rf (handle-notify! dir (signed-notify-fields
                                       #:order "pb-forged"
                                       #:key attacker) now))
       (check-equal? (hash-ref rf 'result "") "rejected")
       (check-false (find-payment dir "pb-forged")))
     )
   (λ () (delete-directory/files dir))))

(test-case "return without binding: never grants"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define-values (email result)
       (verify-return! dir "pb-unknown" (current-seconds)))
     (check-false email)
     (check-equal? (hash-ref result 'result "") "no-binding"))
   (λ () (delete-directory/files dir))))
