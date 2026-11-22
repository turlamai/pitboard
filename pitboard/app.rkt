#lang racket/base

;; The web application: one Racket process serving the paid journal
;; site — landing, journal archive and issues (paywalled), the
;; subscribe → pay → confirm flow, magic-link login, subscription
;; self-service, and the editorial console.  No free-site surface: the
;; daily rules engine is internal pipeline infrastructure, not a
;; product.

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         racket/format
         "journal.rkt"
         "site.rkt"
         "mail.rkt")

(provide start-server)

(define (start-server #:port [port 8090]
                      #:data-dir [data-dir "data"])
  (define site (make-site data-dir))
  ;; mail templates build absolute links from the site address
  (mail-base-param
   (or (getenv "PITBOARD_BASE_URL") "https://pitboard.jrtx.site"))
  (define-values (dispatch _)
    (dispatch-rules
     [("") #:method _ (hash-ref site 'landing)]
     [("journal") #:method _ (hash-ref site 'journal-archive)]
     [("journal" (string-arg)) #:method _ (hash-ref site 'journal-issue)]
     [("subscribe") #:method _ (hash-ref site 'subscribe)]
     [("pay" "callback") #:method _ (hash-ref site 'pay-callback)]
     [("pay" "return") #:method _ (hash-ref site 'pay-claim)]
     [("confirm" (string-arg)) #:method _ (hash-ref site 'confirm)]
     [("login") #:method _ (hash-ref site 'login)]
     [("magic" (string-arg)) #:method _ (hash-ref site 'magic)]
     [("me") #:method _ (hash-ref site 'me)]
     [("unsubscribe" (string-arg)) #:method _ (hash-ref site 'unsubscribe)]
     [("about") #:method _ (hash-ref site 'about)]
     [("console") #:method _ (hash-ref site 'admin)]
     [("healthz") (hash-ref site 'health)]))
  (printf "pitboard serving on http://0.0.0.0:~a (data: ~a)\n" port data-dir)
  (flush-output)
  (serve/servlet dispatch
                 #:port port
                 #:listen-ip #f
                 #:servlet-path "/"
                 #:servlet-regexp
                 #rx"^/($|journal(/.+)?|subscribe|pay/.+|confirm/.+|login|magic/.+|me|unsubscribe/.+|about|console|healthz)"
                 #:banner? #f))
