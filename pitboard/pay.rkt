#lang racket/base

;; Payment face of the paid journal — Alipay Vibe Pay (basic web
;; collection), the same rail publy uses.  Cryptography delegates to the
;; commerce package's canonical Alipay adapter: RSA2-signed page-pay
;; form, notification verification against the Alipay public key, and a
;; signed trade query for the browser return.  pitboard never
;; re-implements the trust chain.
;;
;; Orders live in an append-only JSONL journal: checkout binds
;; out_trade_no ↔ email + amount durably before any payment request;
;; projection is idempotent on trade id; a verified TRADE_SUCCESS
;; grants +365 days to the bound email.
;;
;; Fail-closed: without configured credentials the subscribe page says
;; the channel is not live, and the callback never grants anything it
;; cannot verify.

(require racket/string
         racket/list
         racket/file
         racket/port
         racket/format
         racket/match
         json
         crypto
         crypto/libcrypto
         crypto/pem
         (only-in commerce/alipay
                  alipay-binding
                  alipay-notification?
                  verify-alipay-trade-query-response
                  alipay-confirmed-order-status
                  alipay-confirmed-order-order-id
                  alipay-confirmed-order-trade-id
                  verify-alipay-notification
                  alipay-notification-order-id
                  alipay-notification-trade-id
                  alipay-notification-status
                  alipay-notification-amount
                  make-alipay-page-pay-parameters
                  alipay-cny->fen)
         (only-in commerce/alipay/client
                  make-alipay-query-client
                  fetch-alipay-trade-query-response)
         (only-in commerce money)
         "util.rkt"
         "subs.rkt")

(provide base-url-param
         alipay-env-param
         alipay-app-id-param
         alipay-seller-id-param
         alipay-private-key-path-param
         alipay-public-key-path-param
         alipay-configured?
         alipay-env
         checkout-for!
         handle-notify!
         verify-return!
         load-merchant-key)

;; ---- config ----------------------------------------------------------------

(define base-url-param
  (make-parameter
   (or (getenv "PITBOARD_BASE_URL") "https://pitboard.jrtx.site")))
(define alipay-env-param
  (make-parameter
   (or (and (getenv "ALIPAY_ENV") (string->symbol (getenv "ALIPAY_ENV")))
       'sandbox)))
(define alipay-app-id-param (make-parameter (getenv "ALIPAY_APP_ID")))
(define alipay-seller-id-param (make-parameter (getenv "ALIPAY_SELLER_ID")))
(define alipay-private-key-path-param
  (make-parameter (getenv "ALIPAY_PRIVATE_KEY_PATH")))
(define alipay-public-key-path-param
  (make-parameter (getenv "ALIPAY_PUBLIC_KEY_PATH")))

(define sandbox-gateway "https://openapi-sandbox.dl.alipaydev.com/gateway.do")
(define production-gateway "https://openapi.alipay.com/gateway.do")

(define (alipay-env) (alipay-env-param))

(define (alipay-configured?)
  (and (alipay-app-id-param) (alipay-seller-id-param)
       (alipay-private-key-path-param) (alipay-public-key-path-param)
       #t))

;; ---- credentials (path-based; values never logged) ------------------------

(define key-cache (box #f))

;; commerce's canonical loader: PEM → datum → pk-key (libcrypto).
(define (load-merchant-key)
  (or (unbox key-cache)
      (let ([pem (file->string (alipay-private-key-path-param))])
        (define entry
          (call-with-input-string
           (string-replace pem "\r\n" "\n")
           (λ (in)
             (read-pem in #:only (list #"PRIVATE KEY" #"RSA PRIVATE KEY")))))
        (unless (and (pair? entry)
                     (member (car entry)
                             (list #"PRIVATE KEY" #"RSA PRIVATE KEY")))
          (error 'alipay "merchant private key is not a supported RSA PEM"))
        (define format
          (if (bytes=? (car entry) #"PRIVATE KEY")
              'OneAsymmetricKey 'RSAPrivateKey))
        (define key
          (datum->pk-key (cdr entry) format (list libcrypto-factory)))
        (unless (private-key? key)
          (error 'alipay "merchant RSA key is invalid"))
        (set-box! key-cache key)
        key)))

(define (alipay-public-key-pem)
  (file->string (alipay-public-key-path-param)))

;; ---- checkout binding ------------------------------------------------------

;; Annual offer: CNY ¥199 = 19900 minor units.  out_trade_no is
;; generated here and journaled against the subscriber's email before
;; any payment request, so the async notification can bind without an
;; email from Alipay.
(define annual-minor-units 19900)

(define (annual-binding order-id email)
  (alipay-binding (alipay-app-id-param)
                  (alipay-seller-id-param)
                  order-id
                  "journal-annual"
                  email
                  (money "CNY" annual-minor-units)))

(define (alipay-timestamp [secs (current-seconds)])
  ;; Alipay's API expects UTC+08:00 wall time regardless of host zone.
  (define d (seconds->date (+ secs (* 8 3600)) #t))
  (define (p2 n) (~r n #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~a ~a:~a:~a"
          (date-year d) (p2 (date-month d)) (p2 (date-day d))
          (p2 (date-hour d)) (p2 (date-minute d)) (p2 (date-second d))))

(define (web-gateway-url)
  (if (eq? (alipay-env-param) 'production)
      production-gateway sandbox-gateway))

(define (notify-url-for)
  (string-append (base-url-param) "/pay/callback"))

;; ---- checkout ---------------------------------------------------------------

;; Creates the pending subscription + durable binding, returns
;; {gateway, fields} — site renders an auto-submitting form posting the
;; signed fields to the gateway.  Without credentials returns #f.
(define (checkout-for! data-dir email [now (current-seconds)])
  (unless (alipay-configured?) #f)
  (and (alipay-configured?)
       (let* ([k (string-downcase (string-trim email))]
              [order-id
               (string-append "pb-" (number->string now) "-"
                              (substring (new-token) 0 10))]
              [binding (annual-binding order-id k)]
              [fields
               (make-alipay-page-pay-parameters
                binding
                (load-merchant-key)
                #:title "PITBOARD 周刊 · 年订"
                #:notify-url (notify-url-for)
                #:return-url (string-append (base-url-param)
                                            "/pay/return?order=" order-id)
                #:timestamp (alipay-timestamp now))])
         (subscribe-pending! data-dir k now)
         (record-payment!
          data-dir
          (hasheq 'id (string-append "ref:" order-id)
                  'event "checkout.created"
                  'order_id order-id
                  'email k
                  'amount annual-minor-units
                  'status "pending")
          now)
         (hasheq 'gateway (web-gateway-url) 'fields fields))))

;; ---- async notification -----------------------------------------------------

;; fields: raw form bindings Alipay POSTs, as (cons "name" "value")
;; pairs with decoded spelling preserved.  Verifies the RSA2 signature
;; against the Alipay public key, matches out_trade_no to the journaled
;; binding and the amount to the offer, then grants.  Idempotent on
;; trade id.  Payment events whose binding is missing park in the
;; journal for the claim page.
(define (handle-notify! data-dir fields [now (current-seconds)])
  (with-handlers
      ([exn:fail? (λ (e)
                    (hasheq 'result "rejected"
                            'message (format "~a" (exn-message e))))])
    (define notice (verify-alipay-notification fields (alipay-public-key-pem)))
    (define out-trade-no (alipay-notification-order-id notice))
    (define trade-id (alipay-notification-trade-id notice))
    (define status (alipay-notification-status notice))
    (define ev-id (if (and (string? trade-id) (non-empty-string? trade-id))
                      trade-id
                      (string-append "notif:" out-trade-no)))
    (define binding-ev (find-payment data-dir out-trade-no))
    (define bound-email
      (and binding-ev
           (let ([v (hash-ref binding-ev 'email "")])
             (and (non-empty-string? v) v))))
    (define paid? (equal? status "TRADE_SUCCESS"))
    (record-payment!
     data-dir
     (hasheq 'id ev-id
             'event "alipay.notify"
             'order_id out-trade-no
             'email (or bound-email "")
             'status (if paid? "paid" "recorded"))
     now)
    (cond
      [(and paid? bound-email)
       (grant-subscription! data-dir bound-email 365 now)
       (hasheq 'result "ok" 'action "projected" 'email bound-email)]
      [(and paid? (not bound-email))
       (hasheq 'result "ok" 'action "parked" 'order_id out-trade-no)]
      [paid?
       (hasheq 'result "ok" 'action "unbound" 'order_id out-trade-no)]
      [else
       (hasheq 'result "ok" 'action "ignored" 'status status)])))

;; ---- sync return: signed trade query ----------------------------------------

;; The browser return is never trusted on its own: independently query
;; the gateway with a signed trade query, verify the response against
;; the Alipay public key and the durable binding, then grant.  Returns
;; (values email result) where result names the outcome.
(define (verify-return! data-dir order-id [now (current-seconds)]
                        #:query-client [client #f])
  (define binding-ev (find-payment data-dir order-id))
  (define bound-email
    (and binding-ev
         (let ([v (hash-ref binding-ev 'email "")])
           (and (non-empty-string? v) v))))
  (cond
    [(not (and binding-ev (alipay-configured?)))
     (values #f (hasheq 'result "no-binding"))]
    [(not bound-email)
     (values #f (hasheq 'result "parked" 'order_id order-id))]
    [else
     (with-handlers
         ([exn:fail? (λ (e)
                       (values #f (hasheq 'result "query-failed"
                                          'message (format "~a" (exn-message e)))))])
       (define binding (annual-binding order-id bound-email))
       (define client*
         (or client
             (make-alipay-query-client
              #:app-id (alipay-app-id-param)
              #:merchant-private-key (load-merchant-key)
              #:alipay-public-key (alipay-public-key-pem)
              #:environment (alipay-env-param))))
       (define raw (fetch-alipay-trade-query-response client* binding))
       (define confirmed
         (verify-alipay-trade-query-response
          raw (alipay-public-key-pem) binding))
       ;; canonical cross-checks, per commerce's own confirm contract
       (unless (equal? (alipay-confirmed-order-status confirmed)
                       "TRADE_SUCCESS")
         (error 'alipay "query status is not TRADE_SUCCESS"))
       (unless (equal? (alipay-confirmed-order-order-id confirmed) order-id)
         (error 'alipay "queried order does not match binding"))
       (record-payment!
        data-dir
        (hasheq 'id (string-append "query:" (alipay-confirmed-order-trade-id confirmed))
                'event "alipay.query"
                'order_id order-id
                'email bound-email
                'status "paid")
        now)
       (grant-subscription! data-dir bound-email 365 now)
       (values bound-email (hasheq 'result "ok" 'action "granted")))]))

;; ---- claim (unbound parked payments) ----------------------------------------

;; Bind a parked/paid payment to an email the subscriber supplies; the
;; grant happens only when the journal already carries a paid event for
;; that order (notify landed) or the signed query confirms it.
(define (claim-payment! data-dir order-id email [now (current-seconds)])
  (define ev (find-payment data-dir order-id))
  (and ev
       (equal? (hash-ref ev 'status "") "paid")
       (let ([k (string-downcase (string-trim email))])
         (bind-payment-email! data-dir order-id k)
         (grant-subscription! data-dir k 365 now)
         k)))
