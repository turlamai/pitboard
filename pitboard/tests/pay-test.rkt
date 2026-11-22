#lang racket/base

;; Webhook handling tests.  The parked/idempotency/tamper paths use
;; commerce's own Pancake fixtures (copied from commerce-test; Apache-2.0)
;; — the exact signed vector, no forged crypto.  The projected path
;; generates an RSA keypair at runtime and signs a fresh payload with
;; the same canonical algorithm (RSA PKCS1-v1.5 SHA256 over "ts.body").

(require rackunit
         racket/file
         racket/format
         racket/runtime-path
         racket/string
         json
         crypto
         crypto/libcrypto
         net/base64
         commerce/pancake
         "../subs.rkt"
         "../pay.rkt"
         "../util.rkt")

(define-runtime-path fixture-root "fixtures/pancake")

(define fixture-pem (string-trim (file->string (build-path fixture-root "test-public.pem"))))
(define fixture-prod-pem (string-trim (file->string (build-path fixture-root "production-public.pem"))))
(define fixture-body (file->bytes (build-path fixture-root "order-completed.json")))
(define fixture-header (string-trim (file->string (build-path fixture-root "valid-signature.txt"))))
;; fixture signed at this instant (see commerce-test/pancake-test.rkt)
(define signed-at-ms 1792108800000)

(define (fixture-verify)
  (λ (raw sig)
    (verify-pancake-webhook raw sig
                            #:environment 'test
                            #:keyring (pancake-webhook-keyring fixture-pem fixture-prod-pem)
                            #:now-ms signed-at-ms)))

;; ---- runtime-signed webhook (RSA, canonical algorithm) ---------------------

(define (pub-pem-for pk)
  (define der (pk-key->datum (pk-key->public-only-key pk) 'SubjectPublicKeyInfo))
  (define b64
    (string-replace (string-trim (bytes->string/utf-8 (base64-encode der))) "\r\n" "\n"))
  ;; wrap at 64 chars like standard PEM
  (define lines
    (let loop ([s b64])
      (if (<= (string-length s) 64)
          (list s)
          (cons (substring s 0 64) (loop (substring s 64))))))
  (string-append
   "-----BEGIN PUBLIC KEY-----\n"
   (string-join lines "\n")
   "\n-----END PUBLIC KEY-----\n"))

(struct test-channel (pk pem verify))

(define (make-test-channel)
  (define pk (generate-private-key (get-pk 'rsa libcrypto-factory) '((nbits 2048))))
  (define pk2 (generate-private-key (get-pk 'rsa libcrypto-factory) '((nbits 2048))))
  (define pem (pub-pem-for pk))
  (define pem2 (pub-pem-for pk2))
  (define verify
    (λ (raw sig)
      (verify-pancake-webhook raw sig
                              #:environment 'test
                              #:keyring (pancake-webhook-keyring pem pem2))))
  (test-channel pk pem verify))

(define (sign-webhook! ch body)
  (define ts (~a (inexact->exact (floor (current-inexact-milliseconds)))))
  (define signed-bytes (bytes-append (string->bytes/utf-8 ts) #"." body))
  (define sig (digest/sign (test-channel-pk ch) 'sha256 signed-bytes
                           #:pad 'pkcs1-v1.5))
  (format "t=~a,v1=~a" ts
          (string-trim (bytes->string/utf-8 (base64-encode sig)))))

(define (paid-body order-id email)
  (jsexpr->bytes
   (hasheq 'id "evt_self_001"
           'timestamp "2026-11-22T00:00:00.000Z"
           'eventType "order.completed"
           'eventId "PAY_self_001"
           'storeId "STO_x" 'storeName "T"
           'data (if email
                     (hasheq 'orderId order-id 'email email)
                     (hasheq 'orderId order-id))
           'mode "test")))

;; ---- tests -----------------------------------------------------------------

(test-case "fixture webhook: parked (no email in payload), tamper rejected"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define r (handle-webhook! dir fixture-body fixture-header
                                (current-seconds)
                                #:verify (fixture-verify)))
     (check-equal? (hash-ref r 'result "") "ok")
     (check-equal? (hash-ref r 'action "") "parked")
     (check-equal? (hash-ref r 'order_id "") "ORD_fixture_001")
     ;; the parked payment is in the journal as paid
     (define ev (find-payment dir "ORD_fixture_001"))
     (check-equal? (hash-ref ev 'status "") "paid")
     ;; idempotent replay: same event id → duplicate, journal unchanged
     (define r2 (handle-webhook! dir fixture-body fixture-header
                                 (current-seconds)
                                 #:verify (fixture-verify)))
     (check-equal? (hash-ref r2 'action "") "duplicate")
     ;; tampered body → bad-signature, fail-closed
     (define tampered (bytes-append fixture-body #" "))
     (define r3 (handle-webhook! dir tampered fixture-header
                                 (current-seconds)
                                 #:verify (fixture-verify)))
     (check-equal? (hash-ref r3 'result "") "bad-signature")
     ;; claim page binds the parked payment to an email
     (define email (claim-payment! dir "ORD_fixture_001" "claim@example.com"))
     (check-equal? email "claim@example.com")
     (check-true (subscription-active?
                  (subscription-for dir "claim@example.com")))
     ;; claiming again re-extends (same email, another +365d)
     (check-not-false (claim-payment! dir "ORD_fixture_001" "claim@example.com")))
   (λ () (delete-directory/files dir))))

(test-case "runtime-signed webhook with email projects +365d immediately"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define ch (make-test-channel))
     (define body (paid-body "ORD_self_001" "paid@example.com"))
     (define header (sign-webhook! ch body))
     (define now (current-seconds))
     (define r (handle-webhook! dir body header now #:verify (test-channel-verify ch)))
     (check-equal? (hash-ref r 'result "") "ok")
     (check-equal? (hash-ref r 'action "") "projected")
     (check-equal? (hash-ref r 'email "") "paid@example.com")
     (define rec (subscription-for dir "paid@example.com"))
     (check-true (subscription-active? rec now))
     ;; expires ≈ now + 365d
     (define exp (with-handlers ([exn:fail? (λ (_) 0)])
                   (- (parse-iso-utc (hash-ref rec 'expires "")) now)))
     (check-true (and (> exp (* 364 86400)) (< exp (* 366 86400)))))
   (λ () (delete-directory/files dir))))

(test-case "checkout url requires configuration; ref recorded against email"
  (define dir (make-temporary-file "pitboard-pay-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (check-false (pay-configured?))
     (check-false (checkout-url-for dir "a@example.com"))
     (parameterize ([pay-url-param "https://pay.example/product/journal"])
       (check-true (pay-configured?))
       (define url (checkout-url-for dir "a@example.com"))
       (check-true (string-contains? url "pb_ref=pb-"))
       ;; pending subscriber created for the email
       (check-equal? (hash-ref (subscription-for dir "a@example.com") 'status "")
                     "pending")))
   (λ () (delete-directory/files dir))))
