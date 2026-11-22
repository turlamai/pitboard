#lang racket/base

;; Payment face of the paid journal.  Cryptographic webhook
;; verification delegates to the commerce package's canonical Pancake
;; implementation (RSA over "<timestamp>.<body>", official keyrings
;; mirrored from Pancake's SDK) — pitboard never re-implements the
;; trust chain.  Orders and their projection into subscriptions live
;; here: append-only JSONL journal, idempotent on event id, email-keyed
;; subscription extension of +365 days on a paid event.
;;
;; Checkout is a merchant-configured product URL (env PITBOARD_PAY_URL):
;; the subscriber's email is captured here, the pending order carries a
;; pb_ref reference, and correlation happens on the webhook's email
;; field or via the /pay/return claim page (order id + email).

(require racket/string
         racket/list
         racket/file
         racket/port
         racket/format
         racket/match
         json
         "util.rkt"
         "subs.rkt")

(provide pay-configured?
         pay-url-param
         pay-env-param
         checkout-url-for
         handle-webhook!
         webhook-email-from-data
         success-event?
         claim-payment!
         pay-env)

;; ---- config ----------------------------------------------------------------

(define pay-url-param (make-parameter (getenv "PITBOARD_PAY_URL")))
(define pay-env-param (make-parameter
                       (or (and (getenv "PITBOARD_PAY_ENV")
                                (string->symbol (getenv "PITBOARD_PAY_ENV")))
                           'test)))

(define (pay-configured?)
  (and (pay-url-param) (non-empty-string? (pay-url-param)) #t))

(define (pay-env) (pay-env-param))

;; ---- commerce bridge (optional dependency, degrade with a message) --------

(define (commerce-fn name)
  (with-handlers ([exn:fail:filesystem? (λ (_) #f)])
    (dynamic-require '(lib "commerce/pancake.rkt") name)))

;; event types that count as a paid order (canonical fixture:
;; "order.completed"; tolerate sibling spellings defensively)
(define (success-event? event-type)
  (or (equal? event-type "order.completed")
      (and (string? event-type)
           (or (string-contains? event-type "succeed")
               (string-contains? event-type "paid")))))

;; Best-effort email extraction from the webhook payload — Pancake
;; stores may nest it differently; the claim page is the fallback.
(define (webhook-email-from-data data)
  (and (hash? data)
       (or (let ([v (hash-ref data 'email #f)]) (and (string? v) v))
           (let ([c (hash-ref data 'customer #f)])
             (and (hash? c)
                  (let ([v (hash-ref c 'email #f)]) (and (string? v) v))))
           (let ([v (hash-ref data 'customerEmail #f)]) (and (string? v) v))
           (let ([m (hash-ref data 'metadata #f)])
             (and (hash? m)
                  (let ([v (hash-ref m 'email #f)]) (and (string? v) v)))))))

;; ---- webhook handling ------------------------------------------------------

;; Default verifier: commerce's canonical Pancake verification against
;; the official built-in keyrings, environment from PITBOARD_PAY_ENV.
(define (default-verify)
  (define verify (commerce-fn 'verify-pancake-webhook))
  (and verify
       (λ (raw-body signature-header)
         (verify raw-body signature-header
                 #:environment (pay-env-param)))))

;; Returns a response jsexpr: {result: ok|bad-signature|miss-commerce,
;; action: projected|parked|ignored, email: ...}.  Projection: a paid
;; event with a resolvable email extends that subscription by a year;
;; without an email it parks in the journal for claim/admin binding.
;; #:verify is injectable so tests can pin a fixture keyring.
(define (handle-webhook! data-dir raw-body signature-header
                         [now (current-seconds)]
                         #:verify [verify (default-verify)])
  (define et-fn (commerce-fn 'pancake-webhook-event-type))
  (define eid-fn (commerce-fn 'pancake-webhook-event-id))
  (define data-fn (commerce-fn 'pancake-webhook-data))
  (cond
    [(or (not verify) (not et-fn) (not eid-fn) (not data-fn))
     (hasheq 'result "miss-commerce"
             'message "commerce package not installed — payment channel disabled")]
    [else
     (with-handlers
         ([exn:fail? (λ (e)
                       (hasheq 'result "bad-signature"
                               'message (format "~a" (exn-message e))))])
       (define webhook (verify raw-body signature-header))
       (define event-type (format "~a" (et-fn webhook)))
       (define event-id (format "~a" (eid-fn webhook)))
       (define data (data-fn webhook))
       (define order-id
         (format "~a" (hash-ref data 'orderId
                                (hash-ref data 'order_id ""))))
       (define email (webhook-email-from-data data))
       (define was-seen (payment-seen? data-dir event-id))
       (record-payment!
        data-dir
        (hasheq 'id event-id
                'event event-type
                'order_id order-id
                'email (or email "")
                'status (if (success-event? event-type) "paid" "recorded"))
        now)
       (cond
         [(and was-seen (success-event? event-type))
          (hasheq 'result "ok" 'action "duplicate" 'email email)]
         [(and (success-event? event-type) email)
          (grant-subscription! data-dir email 365 now)
          (hasheq 'result "ok" 'action "projected" 'email email)]
         [(success-event? event-type)
          (hasheq 'result "ok" 'action "parked" 'order_id order-id)]
         [else
          (hasheq 'result "ok" 'action "ignored" 'event event-type)]))]))

;; ---- checkout ---------------------------------------------------------------

;; Pending order for the email, then the merchant-configured checkout
;; URL with a reference suffix.  Without PITBOARD_PAY_URL this returns
;; #f — the subscribe page explains the channel is not configured.
(define (checkout-url-for data-dir email [now (current-seconds)])
  (and (pay-configured?)
       (let* ([k (string-downcase (string-trim email))]
              [_ (subscribe-pending! data-dir k now)]
              [ref (string-append "pb-" (number->string now) "-"
                                  (substring (new-token) 0 8))]
              [base (pay-url-param)]
              [sep (if (string-contains? base "?") "&" "?")]
              [url (string-append base sep "pb_ref=" ref)])
         ;; the reference is recorded against the email for the claim page
         (record-payment!
          data-dir
          (hasheq 'id (string-append "ref:" ref)
                  'event "checkout.opened"
                  'order_id ref
                  'email k
                  'status "pending")
          now)
         url)))

;; ---- claim (pay/return page) ------------------------------------------------

;; Bind a parked/paid payment to an email the subscriber supplies.
;; The claim is honored only when the journal already carries a paid
;; event for that order id (webhook arrived) — the claim page never
;; fabricates a paid status.
(define (claim-payment! data-dir order-id email [now (current-seconds)])
  (define ev (find-payment data-dir order-id))
  (and ev
       (equal? (hash-ref ev 'status "") "paid")
       (let ([k (string-downcase (string-trim email))])
         (bind-payment-email! data-dir order-id k)
         (grant-subscription! data-dir k 365 now)
         k)))
