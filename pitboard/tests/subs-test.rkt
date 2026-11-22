#lang racket/base

;; Subscriber store lifecycle: pending → confirm (double opt-in) →
;; active (+365d) → expiry / unsubscribe; token single-use semantics;
;; payment journal idempotency and binding.

(require rackunit
         racket/file
         racket/string
         "../subs.rkt"
         "../util.rkt")

(define now (current-seconds))

(test-case "subscribe → confirm → active lifecycle"
  (define dir (make-temporary-file "pitboard-subs-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (check-false (subscription-for dir "reader@example.com"))
     (define tok (subscribe-pending! dir "Reader@Example.com " now))
     (check-true (non-empty-string? tok))
     (define rec (subscription-for dir "reader@example.com"))
     (check-equal? (hash-ref rec 'status "") "pending")
     (check-false (subscription-active? rec now))
     ;; pending emails do not authenticate
     (check-exn exn:fail? (λ () (extend-subscription! dir "nobody@x.com" 365 now)))
     (define res (confirm-subscribe! dir tok (+ now 60)))
     (check-true (and res #t))
     (check-equal? (hash-ref res 'email) "reader@example.com")
     (define rec2 (subscription-for dir "reader@example.com"))
     (check-equal? (hash-ref rec2 'status "") "active")
     (check-true (subscription-active? rec2 (+ now 60)))
     ;; +365d window: expires − confirmed_at ≈ 365 days
     (define exp-secs (parse-iso-utc (hash-ref rec2 'expires "")))
     (check-true (>= exp-secs (+ now (* 364 86400))))
     (check-true (< exp-secs (+ now (* 366 86400))))
     ;; confirm token is single-use
     (check-false (confirm-subscribe! dir tok (+ now 120))))
   (λ () (delete-directory/files dir))))

(test-case "renewal extends from existing expiry, not from now"
  (define dir (make-temporary-file "pitboard-subs-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define tok (subscribe-pending! dir "r@example.com" now))
     (confirm-subscribe! dir tok now)
     ;; renewal 200 days in: extends from the existing expiry
     ;; (subscription convention) → expiry ≈ now+730d = t200+530d
     (define t200 (+ now (* 200 86400)))
     (extend-subscription! dir "r@example.com" 365 t200)
     (define rec (subscription-for dir "r@example.com"))
     (define exp-secs (parse-iso-utc (hash-ref rec 'expires "")))
     (check-true (and (>= exp-secs (+ t200 (* 529 86400)))
                      (< exp-secs (+ t200 (* 531 86400))))))
   (λ () (delete-directory/files dir))))

(test-case "tokens are single-use and expiring; sessions read-many"
  (define dir (make-temporary-file "pitboard-subs-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define t (put-token! dir "magic" "a@example.com" 15 now))
     (check-equal? (take-token! dir "magic" t now) "a@example.com")
     (check-false (take-token! dir "magic" t now) "second take must fail")
     ;; wrong kind
     (define t2 (put-token! dir "confirm" "b@example.com" 15 now))
     (check-false (take-token! dir "magic" t2 now))
     ;; expired
     (define t3 (put-token! dir "magic" "c@example.com" 1 now))
     (check-false (take-token! dir "magic" t3 (+ now 3600)))
     ;; session: read-many, expiry-checked
     (define s (put-session! dir "d@example.com" now))
     (check-equal? (session-email! dir s now) "d@example.com")
     (check-equal? (session-email! dir s (+ now 60)) "d@example.com")
     (end-sessions-for! dir "d@example.com")
     (check-false (session-email! dir s now)))
   (λ () (delete-directory/files dir))))

(test-case "unsubscribe by token and admin status changes"
  (define dir (make-temporary-file "pitboard-subs-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (define tok (subscribe-pending! dir "u@example.com" now))
     (confirm-subscribe! dir tok now)
     (define rec (subscription-for dir "u@example.com"))
     (define unsub (hash-ref rec 'unsub_token ""))
     (check-true (non-empty-string? unsub))
     (check-equal? (unsubscribe-by-token! dir unsub now) "u@example.com")
     (check-equal? (hash-ref (subscription-for dir "u@example.com") 'status "")
                   "unsubscribed")
     (check-false (subscription-active? (subscription-for dir "u@example.com") now))
     ;; re-subscribe puts them back to pending (paid again)
     (subscribe-pending! dir "u@example.com" now)
     (check-equal? (hash-ref (subscription-for dir "u@example.com") 'status "")
                   "pending")
     ;; admin delete removes the record entirely
     (check-true (delete-subscriber! dir "u@example.com"))
     (check-false (subscription-for dir "u@example.com")))
   (λ () (delete-directory/files dir))))

(test-case "payment journal: idempotency, lookup, binding"
  (define dir (make-temporary-file "pitboard-subs-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (check-equal?
      (record-payment! dir (hasheq 'id "evt_1" 'event "order.completed"
                                   'order_id "ORD_1" 'email ""
                                   'status "paid") now)
      'recorded)
     (check-equal?
      (record-payment! dir (hasheq 'id "evt_1" 'event "order.completed"
                                   'order_id "ORD_1" 'email ""
                                   'status "paid") now)
      'duplicate)
     (check-true (payment-seen? dir "evt_1"))
     (check-false (payment-seen? dir "evt_2"))
     (define ev (find-payment dir "ORD_1"))
     (check-equal? (hash-ref ev 'status "") "paid")
     (check-false (find-payment dir "ORD_missing"))
     ;; binding sets the email in place
     (check-true (bind-payment-email! dir "ORD_1" "claim@example.com"))
     (check-equal? (hash-ref (find-payment dir "ORD_1") 'email "")
                   "claim@example.com")
     (check-true (<= (length (payments-tail dir 10)) 10)))
   (λ () (delete-directory/files dir))))
