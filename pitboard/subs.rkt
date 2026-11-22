#lang racket/base

;; Subscriber and order stores for the paid journal.  File-backed with
;; atomic writes (tmp + rename), single process, one semaphore around
;; read-modify-write — same philosophy as the digest store: the data is
;; files, backup is a copy.
;;
;; Subscribers are stored as a JSON array of records (write-json only
;; accepts symbol hash keys, so records are keyed by an 'email field,
;; never by hash key).  Lifecycle: subscribe-pending! → (double opt-in,
;; 72h token) → active (expires = now+365d) → expiry / unsubscribe.
;; Payments append to a JSONL journal; projection is idempotent on
;; event id.  Tokens (confirm/magic/session/access) are single-use,
;; expiring, 32-byte random.

(require racket/file
         racket/string
         racket/list
         racket/format
         racket/port
         racket/random
         json
         net/base64
         "util.rkt")

(provide subs-file
         load-subs
         save-subs!
         new-token
         subscribe-pending!
         confirm-subscribe!
         unsubscribe-email!
         unsubscribe-by-token!
         set-status!
         delete-subscriber!
         subscription-for
         subscription-active?
         extend-subscription!
         grant-subscription!
         put-token!
         take-token!
         peek-token!
         put-session!
         session-email!
         end-sessions-for!
         record-payment!
         payment-seen?
         find-payment
         bind-payment-email!
         payments-tail
         subscriber-count)

;; ---- plumbing -------------------------------------------------------------

(define store-sema (make-semaphore 1))
(define pay-sema (make-semaphore 1))

(define (subs-file dir) (build-path dir "subs.json"))
(define (payments-file dir) (build-path dir "payments.jsonl"))

(define (empty-subs) (hasheq 'subscribers '() 'tokens '()))

(define (load-subs dir)
  (with-handlers ([exn:fail? (λ (_) (empty-subs))])
    (define p (subs-file dir))
    (if (file-exists? p)
        (with-input-from-file p read-json)
        (empty-subs))))

(define (save-subs! dir subs)
  (define tmp (build-path dir "subs.json.tmp"))
  (with-output-to-file tmp (λ () (write-json subs)) #:exists 'replace)
  (rename-file-or-directory tmp (subs-file dir) #t))

;; 32 random bytes, URL-safe base64, no padding.
(define (new-token)
  ;; 32 bytes → 44-char base64; strip padding and make it URL-safe
  (define s (bytes->string/utf-8 (base64-encode (crypto-random-bytes 32))))
  (define trimmed
    (string-trim (string-replace (string-replace s "\r" "") "\n" "") "="))
  (string-replace (string-replace trimmed "+" "-") "/" "_"))

(define (email-key email)
  (string-downcase (string-trim (or email ""))))

(define (days-later now days)
  (iso-datetime (+ now (* days 86400))))

(define (token-expired? entry now)
  (define e (iso->secs* (hash-ref entry 'expires 'null)))
  (and e (>= now e)))

(define (iso->secs* v)
  (and (not (eq? v 'null)) (string? v) (parse-iso-utc v)))

;; Tokens live in the same file as a list of {token kind email expires}
;; records — write-json rejects non-symbol hash keys, so both stores are
;; arrays keyed by fields.

(define (tokens-put tokens entry)
  (cons entry tokens))

(define (tokens-get tokens token)
  (for/first ([e (in-list tokens)]
              #:when (equal? (hash-ref e 'token "") token))
    e))

(define (tokens-remove tokens token)
  (for/list ([e (in-list tokens)]
             #:unless (equal? (hash-ref e 'token "") token))
    e))

(define (tokens-remove-session-email tokens email)
  (for/list ([e (in-list tokens)]
             #:unless (and (equal? (hash-ref e 'kind "") "session")
                           (equal? (hash-ref e 'email "") email)))
    e))

;; ---- subscriber records (list of hashes with 'email field) ---------------

(define (sub-find dir email)
  (define k (email-key email))
  (for/first ([rec (in-list (hash-ref (load-subs dir) 'subscribers '()))]
              #:when (equal? (hash-ref rec 'email "") k))
    rec))

;; Replace by email, or prepend.  Returns the updated subs value.
(define (sub-put subs rec)
  (define k (hash-ref rec 'email ""))
  (hasheq 'subscribers
          (cons rec
                (for/list ([r (in-list (hash-ref subs 'subscribers '()))]
                           #:unless (equal? (hash-ref r 'email "") k))
                  r))
          'tokens (hash-ref subs 'tokens)))

;; ---- subscriber lifecycle -------------------------------------------------

;; A subscribe attempt (re)creates a pending record and returns a fresh
;; 72h confirm token.  Existing expiry is preserved for renewals.
(define (subscribe-pending! dir email [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define k (email-key email))
     (unless (non-empty-string? k)
       (error 'subscribe-pending! "empty email"))
     (define prev (sub-find dir k))
     (define token (new-token))
     (define rec
       (hasheq 'email k
               'status "pending"
               'created (if prev (hash-ref prev 'created 'null) (iso-datetime now))
               'confirmed_at (if prev (hash-ref prev 'confirmed_at 'null) 'null)
               'expires (if prev (hash-ref prev 'expires 'null) 'null)))
     (define subs (sub-put (load-subs dir) rec))
     (save-subs! dir
                 (hash-set subs 'tokens
                           (tokens-put (hash-ref subs 'tokens)
                                       (hasheq 'token token
                                               'kind "confirm" 'email k
                                               'expires (days-later now 3)))))
     token)))

;; Double opt-in: the confirm token activates the subscription and
;; starts/extends the paid window (a renewal confirmation extends too).
(define (confirm-subscribe! dir token [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (define entry (tokens-get (hash-ref subs 'tokens) token))
     (cond
       [(or (not entry)
            (not (equal? (hash-ref entry 'kind "") "confirm"))
            (token-expired? entry now))
        #f]
       [else
        (define k (hash-ref entry 'email ""))
        (define prev (sub-find dir k))
        (define prev-expires (and prev (iso->secs* (hash-ref prev 'expires 'null))))
        (define base (if (and prev-expires (> prev-expires now)) prev-expires now))
        (define rec
          (hasheq 'email k
                  'status "active"
                  'created (if prev (hash-ref prev 'created 'null) (iso-datetime now))
                  'confirmed_at (iso-datetime now)
                  'expires (days-later base 365)
                  'unsub_token (new-token)))
        (save-subs! dir
                    (hash-set (sub-put subs rec) 'tokens
                              (tokens-remove (hash-ref subs 'tokens) token)))
        (hasheq 'email k 'expires (hash-ref rec 'expires))]))))

(define (unsubscribe-email! dir email [now (current-seconds)])
  (set-status! dir email "unsubscribed" now))

(define (unsubscribe-by-token! dir token [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (define hit
       (for/first ([rec (in-list (hash-ref subs 'subscribers '()))]
                   #:when (equal? (hash-ref rec 'unsub_token 'null) token))
         (hash-ref rec 'email "")))
     (and hit
          (begin
            (save-subs! dir (sub-put subs (hash-set (sub-find dir hit)
                                                    'status "unsubscribed")))
            hit)))))

(define (set-status! dir email status [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define rec (sub-find dir email))
     (and rec
          (begin
            (save-subs! dir (sub-put (load-subs dir)
                                     (hash-set rec 'status status)))
            (hash-ref rec 'email))))))

(define (delete-subscriber! dir email)
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (define k (email-key email))
     (define had (sub-find dir k))
     (define subs2
       (hasheq 'subscribers
               (for/list ([r (in-list (hash-ref subs 'subscribers '()))]
                          #:unless (equal? (hash-ref r 'email "") k))
                 r)
               'tokens
               (tokens-remove-session-email (hash-ref subs 'tokens) k)))
     (save-subs! dir subs2)
     (and had #t))))

(define (subscription-for dir email)
  (sub-find dir email))

(define (subscription-active? rec [now (current-seconds)])
  (and rec
       (equal? (hash-ref rec 'status "") "active")
       (let ([e (iso->secs* (hash-ref rec 'expires 'null))])
         (and e (< now e)))))

(define (extend-subscription! dir email days [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define prev (sub-find dir email))
     (unless prev
       (error 'extend-subscription! "no subscriber: ~a" (email-key email)))
     (define prev-expires (iso->secs* (hash-ref prev 'expires 'null)))
     (define base (if (and prev-expires (> prev-expires now)) prev-expires now))
     (define rec (hash-set prev 'expires (days-later base days)))
     (save-subs! dir (sub-put (load-subs dir) rec))
     (hash-ref rec 'expires))))

;; A paid grant: ensure the record exists (a webhook email may never
;; have subscribed), extend from the current expiry, and activate.
(define (grant-subscription! dir email days [now (current-seconds)])
  (unless (sub-find dir email)
    (subscribe-pending! dir email now))
  (extend-subscription! dir email days now)
  (set-status! dir email "active" now)
  (hash-ref (sub-find dir email) 'expires))

;; ---- tokens (confirm/magic/session/access share one index) ----------------

(define (put-token! dir kind email minutes [now (current-seconds)])
  (define t (new-token))
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (save-subs! dir
                 (hash-set subs 'tokens
                           (tokens-put (hash-ref subs 'tokens)
                                       (hasheq 'token t
                                               'kind kind
                                               'email (email-key email)
                                               'expires (iso-datetime (+ now (* minutes 60)))))))
     t)))

;; Single-use: popped on successful take; kind and expiry checked.
(define (take-token! dir kind token [now (current-seconds)])
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (define entry (tokens-get (hash-ref subs 'tokens) token))
     (cond
       [(and entry
             (equal? (hash-ref entry 'kind "") kind)
             (not (token-expired? entry now)))
        (save-subs! dir
                    (hash-set subs 'tokens
                              (tokens-remove (hash-ref subs 'tokens) token)))
        (hash-ref entry 'email)]
       [else #f]))))

;; Read-many token check (access tokens accompany every issue link of
;; their notify email and may be reused within the window) — unlike
;; take-token! which pops.
(define (peek-token! dir kind token [now (current-seconds)])
  (define subs (load-subs dir))
  (define entry (tokens-get (hash-ref subs 'tokens) token))
  (and entry
       (equal? (hash-ref entry 'kind "") kind)
       (not (token-expired? entry now))
       (hash-ref entry 'email #f)))

(define (put-session! dir email [now (current-seconds)])
  (put-token! dir "session" email (* 7 24 60) now))

(define (session-email! dir token [now (current-seconds)])
  ;; sessions are read-many: peek without popping, expiry checked
  (define entry (tokens-get (hash-ref (load-subs dir) 'tokens) token))
  (and entry
       (equal? (hash-ref entry 'kind "") "session")
       (not (token-expired? entry now))
       (hash-ref entry 'email #f)))

(define (end-sessions-for! dir email)
  (call-with-semaphore
   store-sema
   (λ ()
     (define subs (load-subs dir))
     (save-subs! dir
                 (hash-set subs 'tokens
                           (tokens-remove-session-email
                            (hash-ref subs 'tokens) (email-key email)))))))

(define (subscriber-count dir)
  (define subs (load-subs dir))
  (for/fold ([active 0] [pending 0] [unsub 0]
             #:result (hasheq 'active active 'pending pending 'unsubscribed unsub))
            ([rec (in-list (hash-ref subs 'subscribers '()))])
    (case (hash-ref rec 'status "")
      [("active") (values (add1 active) pending unsub)]
      [("pending") (values active (add1 pending) unsub)]
      [("unsubscribed") (values active pending (add1 unsub))]
      [else (values active pending unsub)])))

;; ---- payment journal (JSONL, append-only, replayable) ---------------------

(define (read-payments dir)
  (define f (payments-file dir))
  (if (not (file-exists? f))
      '()
      (with-input-from-file f
        (λ ()
          (for/list ([line (in-lines)]
                     #:when (non-empty-string? (string-trim line)))
            (with-handlers ([exn:fail? (λ (_) #f)])
              (read-json (open-input-string line))))))))

;; record: {id event email order_id status at}.  Idempotent on id.
;; Returns 'recorded or 'duplicate.
(define (record-payment! dir ev [now (current-seconds)])
  (call-with-semaphore
   pay-sema
   (λ ()
     (cond
       [(payment-seen? dir (hash-ref ev 'id "")) 'duplicate]
       [else
        (make-directory* dir)
        (define full (hash-set (hash-set ev 'at (iso-datetime now))
                               'status (hash-ref ev 'status "recorded")))
        (with-output-to-file (payments-file dir)
          (λ () (write-string (string-append (jsexpr->string full) "\n")))
          #:exists 'append)
        'recorded]))))

(define (payment-seen? dir id)
  (and (non-empty-string? id)
       (for/or ([ev (in-list (read-payments dir))]
                #:when ev)
         (equal? (hash-ref ev 'id "") id))))

(define (find-payment dir order-id)
  (for/first ([ev (in-list (reverse (read-payments dir)))]
              #:when ev
              #:when (equal? (hash-ref ev 'order_id "") order-id))
    ev))

;; Admin/manual binding: set the email on a parked payment.
(define (bind-payment-email! dir order-id email)
  (call-with-semaphore
   pay-sema
   (λ ()
     (define rows (read-payments dir))
     (define k (email-key email))
     (define hit? (box #f))
     (define out
       (for/list ([ev (in-list rows)])
         (if (and ev (equal? (hash-ref ev 'order_id "") order-id))
             (begin (set-box! hit? #t)
                    (hash-set ev 'email k))
             ev)))
     (when (unbox hit?)
       (with-output-to-file (payments-file dir)
         (λ ()
           (for ([ev (in-list out)])
             (when ev (write-string (string-append (jsexpr->string ev) "\n")))))
         #:exists 'replace))
     (unbox hit?))))

(define (payments-tail dir [n 20])
  (define rows (reverse (filter values (read-payments dir))))
  (take rows (min n (length rows))))
