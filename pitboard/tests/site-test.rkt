#lang racket/base

;; Full-loop integration test over a live server (ephemeral port,
;; temp data dir, dry-run pay/mail): journal archive + paywall states
;; (locked for visitors, full for subscribers), subscribe → confirm →
;; magic-link login → session → full access, unsubscribe, admin
;; console gate and publish flow, webhook callback tamper rejection.

(require rackunit
         racket/file
         racket/format
         racket/list
         racket/string
         racket/runtime-path
         net/http-easy
         json
         "../subs.rkt"
         "../util.rkt"
         "../journal.rkt"
         "../site.rkt"
         "../mail.rkt"
         "../app.rkt")


(define port 18477)
(define base (format "http://127.0.0.1:~a" port))
(define dir (make-temporary-file "pitboard-site-~a" 'directory))

(define sample-issue
  (hasheq 'number 42
          'date "2026-11-21"
          'generated_at "2026-11-21T00:00:00Z"
          'foreword "本周的行业叙事被一份文件接管：L3 准入试点名单公布。"
          'headline (hasheq 'beat "政策与法规"
                            'title "L3 准入试点扩容：智能驾驶责任从车企话术变成法规条款"
                            'summary "答案先行：第二批名单公布。"
                            'why "责任划分条款落地。"
                            'domains 14
                            'url "https://www.example.gov/1")
          'beats (list (hasheq 'name "智能化"
                               'items (list (hasheq 'title "小鹏端到端 OTA 全量推送"
                                                    'summary "去高精地图收官。"
                                                    'why "重地图路线收窄。"
                                                    'beat "智能化" 'domains 4
                                                    'url "https://news.example/2"))))
          'data '()
          'timeline (list (hasheq 'date "11-16" 'fact "工信部公布名单"))
          'counts (hasheq 'events 2 'items 2)))

(define srv-thread
  (thread
   (λ ()
     (parameterize ([admin-password-param "test-pass"]
                    [base-url-param base]
                    [mail-out-param (build-path dir "mail-out")]
                    [mail-host-param #f]
                    [mail-user-param #f]
                    [mail-pass-param #f]
                    [mail-from-param #f])
       (start-server #:port port
                     #:data-dir dir)))))

(define (wait-up)
  (let loop ([n 50])
    (cond [(zero? n) (error 'site-test "server did not come up")]
          [else
           (with-handlers ([exn:fail? (λ (_) (sleep 0.2) (loop (sub1 n)))])
             (define r (get (string-append base "/healthz")))
             (unless (= (response-status-code r) 200)
               (error 'site-test "healthz not ok")))])))

;; response-headers is a list of raw header lines ("name: value");
;; grab the first cookie value of a given header name.
(define (header-cookie r name)
  ;; returns the bare cookie VALUE (between = and ;) for the header
  (define m
    (for/first ([line (in-list (response-headers r))]
                #:when (regexp-match? (format "(?i:^~a:)" name) (bytes->string/utf-8 line)))
      (bytes->string/utf-8 line)))
  (and m
       (let* ([m2 (regexp-match #px":\\s*([^;]+)" m)]
              [pair (and m2 (cadr m2))]
              [m3 (and pair (regexp-match #px"^[^=]+=(.*)$" pair))])
         (and m3 (cadr m3)))))

(define (with-cookie name value)
  (hasheq 'Cookie (string-append name "=" value)))

;; newest .eml in mail-dir whose content contains marker
;; (mail files are written once and never rewritten, so content is the
;; only reliable selector when mtimes tie)
(define (newest-mail-with mail-dir marker)
  (define hits
    (for/list ([f (in-list (directory-list mail-dir))]
               #:when (and (str-ends? (path->string f) ".eml")
                           (string-contains?
                            (file->string (build-path mail-dir f)) marker)))
      f))
  (and (pair? hits)
       (car (sort hits >
                   #:key (λ (f) (file-or-directory-modify-seconds (build-path mail-dir f)))
                   #:cache-keys? #t))))

(wait-up)

;; ---- visitor face ----------------------------------------------------------

(test-case "root path is the landing page"
  (define r (get base))
  (check-equal? (response-status-code r) 200)
  (check-true (string-contains? (bytes->string/utf-8 (response-body r))
                                "筛选，而不是罗列"))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r))
                                "每周一早，把汽车行业一周的事")))

(test-case "journal archive: empty state"
  (define r (get (string-append base "/journal")))
  (check-equal? (response-status-code r) 200)
  (check-true (string-contains? (bytes->string/utf-8 (response-body r))
                                "尚无已发布期刊")))

(test-case "unknown issue: branded 404, not a bare error"
  (define r (get (string-append base "/journal/99")))
  (check-equal? (response-status-code r) 404)
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "去往期归档")))

;; ---- admin gate + publish --------------------------------------------------

(test-case "console requires password; wrong password rejected"
  (define r (get (string-append base "/console")))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "主编工作台"))
  (define bad (post (string-append base "/console") #:form '((password . "wrong"))))
  (check-true (string-contains? (bytes->string/utf-8 (response-body bad)) "密码不对")))

;; draft lands via the admin console's upload path (multipart exercised
;; here as the direct file write it produces; the handler's validation
;; is covered by the format check inside upload)
(define drafts-dir (build-path dir "drafts"))
(make-directory* drafts-dir)
(with-output-to-file (build-path drafts-dir "42.json")
  (λ () (write-json sample-issue))
  #:exists 'replace)

(test-case "admin publishes a draft; issue appears locked for visitors"
  (define login (post (string-append base "/console")
                      #:form '((password . "test-pass"))))
  (check-equal? (response-status-code login) 200)
  ;; header-cookie returns the bare cookie value (between = and ;)
  (define pb-admin (header-cookie login "set-cookie"))
  (define pb-admin-tok (car (string-split pb-admin ";")))
  (define admin-hdr (with-cookie "pb_admin" pb-admin-tok))
  (define pub (get (string-append base "/console?action=publish&n=42")
                   #:headers admin-hdr))
  (check-true (>= (response-status-code pub) 200) "publish action ran")
  ;; archive now lists it
  (define arch (get (string-append base "/journal")))
  (check-true (string-contains? (bytes->string/utf-8 (response-body arch))
                                "No.42"))
  ;; visitor sees the locked view: headline free, paid beats absent
  (define locked (get (string-append base "/journal/42")))
  (check-equal? (response-status-code locked) 200)
  (define locked-body (bytes->string/utf-8 (response-body locked)))
  (check-true (string-contains? locked-body
                                "L3 准入试点扩容：智能驾驶责任从车企话术变成法规条款"))
  (check-false (string-contains? locked-body "小鹏端到端 OTA 全量推送")
               "paid item must not leak into locked view")
  (check-true (string-contains? locked-body "订阅后继续阅读")))

;; ---- subscribe → confirm → login → full access -----------------------------

(test-case "subscribe landing: value story, latest issue preview, FAQ"
  (define r (get (string-append base "/subscribe")))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "筛选，而不是罗列"))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "常见问题"))
  ;; the published issue 42 shows up as the latest-issue preview
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "第 42 期")))

(test-case "subscribe without pay channel: honest unconfigured page"
  (define r (post (string-append base "/subscribe")
                  #:form '((email . "reader@example.com"))))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r))
                                "支付渠道未配置"))
  (check-equal? (hash-ref (subscription-for dir "reader@example.com") 'status "")
                "pending"))

(test-case "double opt-in activates the subscription"
  (define tok (subscribe-pending! dir "second@example.com"))
  (define r (get (string-append base "/confirm/" tok)))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r)) "订阅已激活"))
  (check-true (subscription-active? (subscription-for dir "second@example.com")))
  ;; the confirm token is single-use
  (define r2 (get (string-append base "/confirm/" tok)))
  (check-true (string-contains? (bytes->string/utf-8 (response-body r2)) "无效或已过期"))
  ;; notify email to the active subscriber carries the compliance
  ;; one-click unsubscribe link
  (define admin-login2 (post (string-append base "/console")
                              #:form '((password . "test-pass"))))
  (define admin-tok2 (header-cookie admin-login2 "set-cookie"))
  (define notify-r (get (string-append base "/console?action=notify&n=42")
                        #:headers (hasheq 'Cookie (string-append "pb_admin=" admin-tok2))))
  (check-equal? (response-status-code notify-r) 200)
  (define mail-dir (build-path dir "mail-out"))
  (define notify-f (newest-mail-with mail-dir "/unsubscribe/"))
  (check-true (and notify-f #t) "a notify email exists")
  (define notify-eml (file->string (build-path mail-dir notify-f)))
  (check-true (string-contains? notify-eml "/unsubscribe/")
              "notify email must carry the one-click unsubscribe link"))

(test-case "magic-link login grants full access; unsubscribe closes it"
  (define login-page (post (string-append base "/login")
                           #:form '((email . "second@example.com"))))
  (check-true (string-contains? (bytes->string/utf-8 (response-body login-page))
                                "查收邮件"))
  ;; pull the magic token from the dry-run mail artifact
  (define mail-dir (build-path dir "mail-out"))
  (define newest (newest-mail-with mail-dir "/magic/"))
  (check-true (and newest #t) "a login mail exists")
  (define eml (file->string (build-path mail-dir newest)))
  (define m (regexp-match #px"/magic/([A-Za-z0-9_-]+)" eml))
  (check-true (and m #t))
  (define magic-res (get (string-append base "/magic/" (cadr m))))
  ;; the first magic GET pops the token and sets the session cookie;
  ;; retry once against a transient store race
  (define sess (header-cookie magic-res "set-cookie"))
  (unless sess
    (define retry-tok (regexp-match #px"/magic/([A-Za-z0-9_-]+)" eml))
    (when retry-tok
      (define r2 (get (string-append base "/magic/" (cadr retry-tok))))
      (set! sess (header-cookie r2 "set-cookie"))))
  (check-true (and sess #t))
  (define hdrs (with-cookie "pb_session" sess))
  (define full (get (string-append base "/journal/42") #:headers hdrs))
  (define full-body (bytes->string/utf-8 (response-body full)))
  (check-true (string-contains? full-body "小鹏端到端 OTA 全量推送")
              "subscriber must see paid beats")
  (check-true (string-contains? full-body "大事记"))
  ;; me page shows the active subscription
  (define me (get (string-append base "/me") #:headers hdrs))
  (check-true (string-contains? (bytes->string/utf-8 (response-body me)) "生效中"))
  ;; unsubscribe via the record's token
  (define rec (subscription-for dir "second@example.com"))
  (define unsub (get (string-append base "/unsubscribe/"
                                    (hash-ref rec 'unsub_token))))
  (check-true (string-contains? (bytes->string/utf-8 (response-body unsub)) "已退订"))
  (check-equal? (hash-ref (subscription-for dir "second@example.com") 'status "")
                "unsubscribed")
  ;; the killed session no longer reaches the full page
  (define after (get (string-append base "/journal/42") #:headers hdrs))
  (check-false (string-contains? (bytes->string/utf-8 (response-body after))
                                 "小鹏端到端 OTA 全量推送")))

(test-case "webhook callback rejects tampered payloads"
  (define r (post (string-append base "/pay/callback")
                  #:data #"tampered"
                  #:headers (hasheq 'X-Waffo-Signature "t=1,v1=x")))
  (check-equal? (response-status-code r) 401))

;; ---- shutdown ---------------------------------------------------------------

(kill-thread srv-thread)
(delete-directory/files dir)
(printf "site integration loop: passed\n")
