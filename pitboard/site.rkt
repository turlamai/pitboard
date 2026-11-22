#lang racket/base

;; The commercial face of the site: journal archive and issue pages
;; (locked for visitors, full for active subscribers), the subscribe →
;; pay → confirm flow, magic-link login, subscription self-service, and
;; the editorial console (draft upload / publish / notify / subscriber
;; management).  Rendering reuses the journal design tokens so the
;; dynamic pages match the design baseline exactly.

(require web-server/servlet
         web-server/http/request-structs
         web-server/http/cookie-parse
         web-server/http/redirect
         net/url
         racket/string
         racket/list
         racket/file
         racket/format
         racket/match
         racket/port
         json
         "util.rkt"
         "journal.rkt"
         "subs.rkt"
         "pay.rkt"
         "mail.rkt")

(provide make-site
         base-url-param
         admin-password-param)

;; ---- params ----------------------------------------------------------------

(define base-url-param (make-parameter
                        (or (getenv "PITBOARD_BASE_URL")
                            "https://pitboard.jrtx.site")))
(define admin-password-param (make-parameter (getenv "PITBOARD_ADMIN_PASSWORD")))

(define (v x) (if (bytes? x) (bytes->string/utf-8 x) (format "~a" x)))

(define (form-binding req key)
  (define b (bindings-assq (string->bytes/utf-8 key)
                           (request-bindings/raw req)))
  (and b (binding:form? b) (v (binding:form-value b))))

(define (cookie-value req name)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (for/first ([c (in-list (request-cookies req))]
                #:when (equal? (client-cookie-name c) name))
      (client-cookie-value c))))

(define (set-cookie-header name value max-age-seconds secure?)
  (header #"Set-Cookie"
          (string->bytes/utf-8
           (format "~a=~a; Path=/; Max-Age=~a; HttpOnly;~a SameSite=Lax"
                   name value max-age-seconds
                   (if secure? " Secure;" "")))))

(define (secure-cookie?) (str-starts? (base-url-param) "https://"))

(define (resp-html b)
  (response/full 200 #"OK" (current-seconds)
                 (bytes->immutable-bytes #"text/html; charset=utf-8")
                 '() (list (string->bytes/utf-8 b))))

(define (resp-html-headers b headers)
  (response/full 200 #"OK" (current-seconds)
                 (bytes->immutable-bytes #"text/html; charset=utf-8")
                 headers (list (string->bytes/utf-8 b))))

(define (resp-text-headers b headers)
  (response/full 200 #"OK" (current-seconds)
                 (bytes->immutable-bytes #"text/plain; charset=utf-8")
                 headers (list (string->bytes/utf-8 b))))

(define (resp-redirect loc)
  (redirect-to loc))

(define (resp-status code msg)
  (response/full code (string->bytes/utf-8 msg) (current-seconds)
                 (bytes->immutable-bytes #"text/plain; charset=utf-8")
                 '() (list (string->bytes/utf-8 (string-append msg "\n")))))

;; ---- issue storage ---------------------------------------------------------

(define (drafts-dir data-dir) (build-path data-dir "drafts"))
(define (issues-dir data-dir) (build-path data-dir "journal-issues"))

(define (valid-issue? j)
  (and (hash? j)
       (real? (hash-ref j 'number #f))
       (string? (hash-ref j 'date #f))
       (string? (hash-ref j 'foreword #f))
       (hash? (hash-ref j 'headline #f))
       (list? (hash-ref j 'beats #f))))

(define (published-issues data-dir)
  (with-handlers ([exn:fail? (λ (_) '())])
    (for/list ([p (in-list (directory-list (issues-dir data-dir)))]
               #:when (regexp-match? #px"^\\d+\\.json$" (path->string p)))
      (with-handlers ([exn:fail? (λ (_) #f)])
        (with-input-from-file (build-path (issues-dir data-dir) p) read-json)))))

(define (issue-by-number data-dir n)
  (define p (build-path (issues-dir data-dir) (format "~a.json" n)))
  (and (file-exists? p)
       (with-handlers ([exn:fail? (λ (_) #f)])
         (with-input-from-file p read-json))))

;; ---- auth helpers ----------------------------------------------------------

(define (access-email! dir token [now (current-seconds)])
  ;; peek without popping: access tokens accompany every issue link of
  ;; their notify email and may be reused within the window
  (define subs (load-subs dir))
  (define entry (hash-ref (hash-ref subs 'tokens) token #f))
  (and entry
       (equal? (hash-ref entry 'kind "") "access")
       (let ([e (and (not (eq? (hash-ref entry 'expires 'null) 'null))
                     (parse-date-any (hash-ref entry 'expires "")))])
         (and e (<= now e))
         (hash-ref entry 'email #f))))

(define (subscriber-authenticated? req data-dir)
  (or (let ([email (session-email-from-cookie req data-dir)])
        (and email (subscription-active? (subscription-for data-dir email))))
      (let* ([m (and (request-uri req)
                     (regexp-match #px"[?&]k=([^&]+)" (url->string (request-uri req))))]
             [email (and m (access-email! data-dir (cadr m)))])
        (and email (subscription-active? (subscription-for data-dir email))))))

(define (session-email-from-cookie req data-dir)
  (define tok (cookie-value req "pb_session"))
  (and tok (session-email! data-dir tok)))

;; ---- pages -----------------------------------------------------------------

(define (nav-footer)
  (string-append
   "<footer class=\"site-footer\">"
   "<p>PITBOARD 周刊 · 汽车行业的一周 · <a href=\"/subscribe\">订阅</a> · "
   "<a href=\"/journal\">往期归档</a> · <a href=\"/me\">我的订阅</a></p>"
   "<p>所有条目附原文链接；AI 汇编 + 主编复核，发现错误当期更正、下期刊登。</p></footer>"))

(define (issue-auth-email req data-dir)
  ;; email when authenticated as an active subscriber, else #f
  (or (let ([email (session-email-from-cookie req data-dir)])
        (and email (subscription-active? (subscription-for data-dir email)) email))
      (let* ([m (and (request-uri req)
                     (regexp-match #px"[?&]k=([^&]+)" (url->string (request-uri req))))]
             [email (and m (access-email! data-dir (cadr m)))])
        (and email (subscription-active? (subscription-for data-dir email)) email))))

(define (journal-archive-h site)
  (λ (req)
    (define issues (filter values (published-issues (site-data-dir site))))
    (define rows
      (string-join
       (for/list ([j (in-list (sort issues > #:key (λ (j) (hash-ref j 'number 0))))])
         (format
          "<div class=\"arc-row\"><span class=\"arc-no num\">No.~a</span><a class=\"arc-title\" href=\"/journal/~a\">~a</a><span class=\"arc-meta num\">~a · ~a 条 · 免费阅读卷首与头条</span></div>"
          (hash-ref j 'number 0)
          (hash-ref j 'number 0)
          (esc (hash-ref (hash-ref j 'headline (hasheq)) 'title ""))
          (esc (hash-ref j 'date ""))
          (hash-ref (hash-ref j 'counts (hasheq)) 'items 0)))
       "\n"))
    (resp-html
     (page-shell
      "往期归档 — PITBOARD 周刊"
      (string-append
       "<header class=\"masthead\"><div class=\"masthead-rule\"></div>"
       "<h1 class=\"masthead-title\"><span class=\"en\">PITBOARD</span> 周刊</h1>"
       "<div class=\"masthead-issue\"><span>往期归档</span>"
       "<span class=\"tag\">汽车行业的一周</span></div></header>"
       (if (null? issues)
           (string-append
            "<section class=\"section\"><div class=\"section-head\">"
            "<span class=\"section-title\">尚无已发布期刊</span>"
            "<span class=\"section-count\">0 期</span></div>"
            "<p class=\"item-meta\">创刊号正在路上——订阅后你会在每周六 08:00 收到出刊通知。</p></section>")
           (format "<section class=\"section\" style=\"margin-top:40px\"><div class=\"section-head\"><span class=\"section-title\">2026 年</span><span class=\"section-count\">~a 期 · 每周六出刊</span></div>~a</section>"
                   (length issues) rows))
       (nav-footer))))))

(define (journal-issue-h site)
  (λ (req n-str)
    (define n (string->number (string-trim n-str "/")))
    (define j (and n (issue-by-number (site-data-dir site) n)))
    (cond
      [(not j) (resp-status 404 "no such issue")]
      [else
       (define email (issue-auth-email req (site-data-dir site)))
       (if email
           (resp-html (render-full-html j))
           (resp-html (render-locked-html (locked-view j))))])))

(define (subscribe-h site)
  (λ (req)
    (cond
      [(equal? (request-method req) #"POST")
       (define email (form-binding req "email"))
       (cond
         [(or (not email)
              (not (regexp-match? #px"^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$" email)))
          (resp-html
           (page-shell "订阅 — PITBOARD 周刊"
                       (string-append
                        "<header class=\"masthead\"><div class=\"masthead-rule\"></div>"
                        "<h1 class=\"masthead-title\"><span class=\"en\">PITBOARD</span> 周刊</h1>"
                        "<div class=\"masthead-issue\"><span>订阅</span>"
                        "<span class=\"tag\">汽车行业的一周</span></div></header>"
                        "<div class=\"subscribe-card\"><h3>邮箱看起来不对</h3>"
                        "<p class=\"note\">请回到订阅页填写一个可收信的邮箱地址。</p>"
                        "<a class=\"btn-primary\" href=\"/subscribe\">返回订阅页</a></div>"
                        (nav-footer))))]
         [(not (pay-configured?))
          ;; channel not configured (dev/staging): honest state, no fake checkout
          (subscribe-pending! (site-data-dir site) email)
          (resp-html
           (page-shell "订阅 — PITBOARD 周刊"
                       (string-append
                        "<div class=\"subscribe-card\"><h3>支付渠道未配置</h3>"
                        "<p class=\"note\">你的邮箱已登记为待订阅（"
                        (esc email) "），但支付渠道尚未上线——这是部署环境未配置 PITBOARD_PAY_URL 的诚实提示。</p>"
                        "<a class=\"btn-primary\" href=\"/\">返回首页</a></div>")))]
         [else
          (define url (checkout-url-for (site-data-dir site) email))
          (resp-redirect (or url "/subscribe"))])]
      [else
       (resp-html
        (page-shell
         "订阅 — PITBOARD 周刊"
         (string-append
          "<header class=\"masthead\"><div class=\"masthead-rule\"></div>"
          "<h1 class=\"masthead-title\"><span class=\"en\">PITBOARD</span> 周刊</h1>"
          "<div class=\"masthead-issue\"><span>订阅</span>"
          "<span class=\"tag\">汽车行业的一周</span></div></header>"
          "<div class=\"subscribe-card\"><h3>年订</h3>"
          "<div class=\"price num\">¥199<span class=\"unit\"> / 年 · 52 期 ≈ ¥3.8 一期</span></div>"
          "<p class=\"note\">每周六 08:00 出刊通知直达 · 网页全文与往期归档 · 未满 30 天可全额退款</p>"
          "<form method=\"post\" action=\"/subscribe\" style=\"margin-top:12px\">"
          "<input type=\"email\" name=\"email\" required placeholder=\"你的邮箱\" "
          "style=\"width:260px;padding:10px 12px;border:1px solid var(--border);border-radius:8px;"
          "font-size:15px;background:var(--surface);color:var(--text)\"> "
          "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">去支付</button>"
          "</form>"
          "<div class=\"subscribe-flow\"><span class=\"step\">① 填邮箱</span>"
          "<span class=\"step\">② 支付</span><span class=\"step\">③ 邮件确认</span></div></div>"
          (nav-footer))))])))

(define (pay-callback-h site)
  (λ (req)
    (cond
      [(not (equal? (request-method req) #"POST"))
       (resp-status 405 "POST only")]
      [else
       (define sig
         (for/first ([h (in-list (request-headers/raw req))]
                     #:when (equal? (header-field h) #"x-waffo-signature"))
           (bytes->string/utf-8 (header-value h))))
       (define body (request-post-data/raw req))
       (define result
         (handle-webhook! (site-data-dir site)
                          (or body #"") (or sig "")
                          (current-seconds)))
       (if (equal? (hash-ref result 'result "") "ok")
           (resp-text-headers "ok\n" '())
           (resp-status 401 (hash-ref result 'result "rejected")))])))

(define (pay-return-h site)
  (λ (req)
    (define m (regexp-match #px"[?&](pb_ref|order)=([^&]+)"
                            (url->string (request-uri req))))
    (define order (and m (cadr m)))
    (resp-html
     (page-shell
      "支付回执 — PITBOARD 周刊"
      (string-append
       "<div class=\"subscribe-card\"><h3>支付完成？</h3>"
       "<p class=\"note\">如果你的支付已完成但还没收到确认邮件，用支付凭据上的订单号和你的邮箱完成绑定。</p>"
       (if order
           (string-append
            "<form method=\"get\" action=\"/pay/return\">"
            "<input type=\"hidden\" name=\"order\" value=\"" (esc order) "\">"
            "<input type=\"email\" name=\"email\" required placeholder=\"你的邮箱\" "
            "style=\"width:260px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
            "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">绑定订阅</button></form>")
           (string-append
            "<form method=\"get\" action=\"/pay/return\">"
            "<input type=\"text\" name=\"order\" required placeholder=\"订单号\" "
            "style=\"width:180px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
            "<input type=\"email\" name=\"email\" required placeholder=\"你的邮箱\" "
            "style=\"width:220px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
            "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">绑定订阅</button></form>"))
       "</div>")))))

;; GET with both params → attempt claim and show result
(define (pay-claim-h site)
  (λ (req)
    (define u (url->string (request-uri req)))
    (define order (let ([m (regexp-match #px"[?&]order=([^&]+)" u)]) (and m (cadr m))))
    (define email (let ([m (regexp-match #px"[?&]email=([^&]+)" u)]) (and m (cadr m))))
    (if (and order email)
        (let ([ok (claim-payment! (site-data-dir site) order email)])
          (resp-html
           (page-shell
            "支付回执 — PITBOARD 周刊"
            (string-append
             "<div class=\"subscribe-card\">"
             (if ok
                 (string-append "<h3>订阅已激活</h3><p class=\"note\">" (esc email)
                                " 的年订已生效，下期起每周六 08:00 你会收到出刊通知。</p>"
                                "<a class=\"btn-primary\" href=\"/login\">登录查看订阅</a>")
                 (string-append "<h3>暂时没能绑定</h3><p class=\"note\">订单 "
                                (esc order) " 还没有到账记录——支付回调可能有几分钟延迟，稍后重试，"
                                "或联系主编人工处理。</p>"
                                "<a class=\"btn-primary\" href=\"/pay/return\">重新绑定</a>"))
             "</div>"))))
        ((pay-return-h site) req))))

(define (confirm-h site)
  (λ (req token)
    (define res (confirm-subscribe! (site-data-dir site) token))
    (cond
      [(not res)
       (resp-html
        (page-shell "订阅确认 — PITBOARD 周刊"
                    (string-append
                     "<div class=\"subscribe-card\"><h3>确认链接无效或已过期</h3>"
                     "<p class=\"note\">确认链接 72 小时有效。如果支付已完成，"
                     "回到订阅页重新走一遍流程即可（不会重复扣款——支付以支付渠道为准）。</p>"
                     "<a class=\"btn-primary\" href=\"/subscribe\">返回订阅页</a></div>")))]
      [else
       (send-mail!
        (hash-ref res 'email)
        "欢迎来到 PITBOARD 周刊"
        (string-append
         "<div style=\"font-family:-apple-system,'PingFang SC',sans-serif\">"
         "<p>订阅已激活（每周六 08:00 出刊）。网页全文与往期归档：</p>"
         "<p><a href=\"" (esc (base-url-param)) "/login\">登录入口（邮箱魔法链接）</a></p></div>"))
       (resp-html
        (page-shell "订阅确认 — PITBOARD 周刊"
                    (string-append
                     "<div class=\"subscribe-card\"><h3>订阅已激活</h3>"
                     "<p class=\"note\">" (esc (hash-ref res 'email))
                     " · 有效期至 " (esc (hash-ref res 'expires))
                     "。每周六 08:00 收到出刊通知。</p>"
                     "<a class=\"btn-primary\" href=\"/login\">登录查看订阅</a></div>")))])))

(define (login-h site)
  (λ (req)
    (cond
      [(equal? (request-method req) #"POST")
       (define email (form-binding req "email"))
       (cond
         [(or (not email) (not (subscription-for (site-data-dir site) email)))
          (resp-html
           (page-shell "登录 — PITBOARD 周刊"
                       (string-append
                        "<div class=\"subscribe-card\"><h3>没有找到这个邮箱</h3>"
                        "<p class=\"note\">该邮箱没有订阅记录——先去订阅吧。</p>"
                        "<a class=\"btn-primary\" href=\"/subscribe\">订阅</a></div>")))]
         [else
          (define tok (put-token! (site-data-dir site) "magic" email 15))
          (define url (string-append (base-url-param) "/magic/" tok))
          (send-mail! email "登录 PITBOARD 周刊" (login-html url))
          (resp-html
           (page-shell "登录 — PITBOARD 周刊"
                       (string-append
                        "<div class=\"subscribe-card\"><h3>查收邮件</h3>"
                        "<p class=\"note\">15 分钟内有效的登录链接已发到 "
                        (esc email) "。没收到？检查垃圾箱，或重新发送。</p>"
                        "<a class=\"btn-primary\" href=\"/login\">重新发送</a></div>")))])]
      [else
       (resp-html
        (page-shell
         "登录 — PITBOARD 周刊"
         (string-append
          "<div class=\"subscribe-card\"><h3>登录</h3>"
          "<p class=\"note\">输入订阅邮箱，我们会发一封 15 分钟内有效的登录邮件——无密码。</p>"
          "<form method=\"post\" action=\"/login\">"
          "<input type=\"email\" name=\"email\" required placeholder=\"你的邮箱\" "
          "style=\"width:260px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
          "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">发送登录链接</button>"
          "</form></div>"
          (nav-footer))))])))

(define (magic-h site)
  (λ (req token)
    (define email (take-token! (site-data-dir site) "magic" token))
    (if email
        (let ([sess (put-session! (site-data-dir site) email)])
          (resp-html-headers
           "<html><head><meta charset=\"UTF-8\"></head><body>ok</body></html>"
           (list (set-cookie-header "pb_session" sess (* 7 86400) (secure-cookie?)))))
        (resp-status 403 "invalid or expired magic link"))))

(define (me-h site)
  (λ (req)
    (define email (session-email-from-cookie req (site-data-dir site)))
    (cond
      [(not email)
       (resp-html
        (page-shell "我的订阅 — PITBOARD 周刊"
                    (string-append
                     "<div class=\"subscribe-card\"><h3>尚未登录</h3>"
                     "<a class=\"btn-primary\" href=\"/login\">登录</a> "
                     "<a class=\"btn-primary\" href=\"/subscribe\" style=\"background:var(--accent-bg);color:var(--accent)\">订阅</a></div>")))]
      [else
       (define rec (subscription-for (site-data-dir site) email))
       (define active (subscription-active? rec))
       (resp-html
        (page-shell
         "我的订阅 — PITBOARD 周刊"
         (string-append
          "<div class=\"subscribe-card\"><h3>我的订阅</h3>"
          "<p class=\"note\">" (esc email)
          (cond [active (string-append " · 生效中 · 有效期至 " (esc (hash-ref rec 'expires)))]
                [(equal? (hash-ref rec 'status "") "pending") " · 待确认（查收确认邮件）"]
                [(equal? (hash-ref rec 'status "") "unsubscribed") " · 已退订"]
                [else " · 已过期"]) "</p>"
          (if active
              (string-append "<a class=\"btn-primary\" href=\"/journal\">阅读最新一期</a> ")
              (string-append "<a class=\"btn-primary\" href=\"/subscribe\">续订</a> "))
          "<a class=\"btn-primary\" style=\"background:var(--accent-bg);color:var(--accent)\" "
          "href=\"/unsubscribe/" (esc (hash-ref rec 'unsub_token "missing")) "\">一键退订</a>"
          "</div>"
          (nav-footer))))])))

(define (unsubscribe-h site)
  (λ (req token)
    (define email (unsubscribe-by-token! (site-data-dir site) token))
    (if email
        (begin
          (end-sessions-for! (site-data-dir site) email)
          (resp-html
           (page-shell "退订 — PITBOARD 周刊"
                       (string-append
                        "<div class=\"subscribe-card\"><h3>已退订</h3>"
                        "<p class=\"note\">" (esc email)
                        " 不再收到出刊通知。订阅有效期内的网页访问保留至到期。</p></div>"))))
        (resp-status 403 "invalid unsubscribe link"))))

;; ---- editorial console ------------------------------------------------------

(define (admin-token-box site) (site-admin-tokens site))

(define (admin-ok? req site)
  (define tok (cookie-value req "pb_admin"))
  (and tok
       (hash-has-key? (unbox (admin-token-box site)) tok)
       (let ([exp (hash-ref (unbox (admin-token-box site)) tok)])
         (>= exp (current-seconds)))))

(define (admin-login-page [msg ""])
  (page-shell
   "主编工作台 — PITBOARD 周刊"
   (string-append
    "<div class=\"subscribe-card\"><h3>主编工作台</h3>"
    (if (non-empty-string? msg)
        (string-append "<p class=\"note\">" (esc msg) "</p>")
        "")
    "<form method=\"post\" action=\"/admin\">"
    "<input type=\"password\" name=\"password\" required placeholder=\"工作台密码\" "
    "style=\"width:220px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
    "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">进入</button>"
    "</form></div>")))

(define (admin-page-h site)
  (λ (req)
    (cond
      [(equal? (request-method req) #"POST")
       (define password (form-binding req "password"))
       (cond
         [(and password
               (admin-password-param)
               (non-empty-string? (admin-password-param))
               (equal? password (admin-password-param)))
          (define tok (new-token))
          (set-box! (admin-token-box site)
                    (hash-set (unbox (admin-token-box site))
                              tok (+ (current-seconds) (* 4 3600))))
          (resp-html-headers
           (admin-console site)
           (list (set-cookie-header "pb_admin" tok (* 4 3600) (secure-cookie?))))]
         [else (resp-html (admin-login-page "密码不对（或服务端未配置 PITBOARD_ADMIN_PASSWORD）"))])]
      [(admin-ok? req site) (resp-html (admin-console site))]
      [else (resp-html (admin-login-page))])))

(define (admin-console site)
  (define data-dir (site-data-dir site))
  (define drafts
    (with-handlers ([exn:fail? (λ (_) '())])
      (for/list ([p (in-list (directory-list (drafts-dir data-dir)))]
                 #:when (regexp-match? #px"^\\d+\\.json$" (path->string p)))
        (string-append (path->string p)))))
  (define drafts-rows
    (if (null? drafts)
        "<p class=\"item-meta\">没有待审草稿。每周 workflow 生成完整期刊 artifact 后，主编下载 issue.json 从这里上传。</p>"
        (string-join
         (for/list ([d (in-list drafts)])
           (define n (regexp-replace #px"\\.json$" d ""))
           (format "<div class=\"toc-line\"><span class=\"name\">草稿 第 ~a 期</span><span><a href=\"/admin?action=publish&n=~a\">发布</a> · <a href=\"/admin?action=discard&n=~a\">丢弃</a></span></div>"
                   n n n))
         "\n")))
  (define published (filter values (published-issues data-dir)))
  (define published-rows
    (if (null? published)
        "<p class=\"item-meta\">还没有已发布期刊。</p>"
        (string-join
         (for/list ([j (in-list (sort published > #:key (λ (j) (hash-ref j 'number 0))))])
           (format "<div class=\"toc-line\"><span class=\"name\">No.~a · ~a · ~a</span>"
                   "<span><a href=\"/journal/~a\">查看</a> · "
                   "<a href=\"/admin?action=notify&n=~a\">发刊通知邮件</a></span></div>"
                   (hash-ref j 'number 0)
                   (esc (hash-ref j 'date ""))
                   (esc (hash-ref (hash-ref j 'headline (hasheq)) 'title ""))
                   (hash-ref j 'number 0)
                   (hash-ref j 'number 0)))
         "\n")))
  (define counts (subscriber-count data-dir))
  (define payments
    (string-join
     (for/list ([ev (in-list (payments-tail data-dir 10))])
       (format "<div class=\"toc-line\"><span class=\"name\">~a · ~a</span>"
               "<span class=\"count\">~a · ~a</span></div>"
               (esc (hash-ref ev 'event ""))
               (esc (hash-ref ev 'email "")) (esc (hash-ref ev 'status ""))
               (esc (hash-ref ev 'order_id ""))))
     "\n"))
  (page-shell
   "主编工作台 — PITBOARD 周刊"
   (format
    (string-append
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">订阅名单</span>"
     "<span class=\"section-count num\">active ~a · pending ~a · unsub ~a</span></div>"
     "<p class=\"item-meta\">名单明细含订阅者邮箱，只在此工作台查看，不进公开面。</p></section>"
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">草稿</span>"
     "<span class=\"section-count\">待复核</span></div>"
     "<form method=\"post\" action=\"/admin\" enctype=\"multipart/form-data\">"
     "<input type=\"file\" name=\"draft\" accept=\".json\" required> "
     "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">上传 issue.json</button>"
     "</form><div class=\"toc-locked\" style=\"margin-top:16px\">~a</div></section>"
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">已发布</span>"
     "<span class=\"section-count\">~a 期</span></div><div class=\"toc-locked\">~a</div></section>"
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">支付流水（最近）</span>"
     "<span class=\"section-count\">journal</span></div><div class=\"toc-locked\">~a</div></section>"
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">测试发信</span>"
     "<span class=\"section-count\">dry-run 时写 data/mail-out/</span></div>"
     "<form method=\"post\" action=\"/admin\" enctype=\"multipart/form-data\">"
     "<input type=\"email\" name=\"test_to\" placeholder=\"收件邮箱\" "
     "style=\"width:220px;padding:10px 12px;border:1px solid var(--border);border-radius:8px\"> "
     "<input type=\"hidden\" name=\"action\" value=\"test-mail\">"
     "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">发测试邮件</button>"
     "</form></section>"
     (nav-footer))
    (hash-ref counts 'active 0)
    (hash-ref counts 'pending 0)
    (hash-ref counts 'unsubscribed 0)
    drafts-rows
    (length published)
    published-rows
    payments)))

(define (admin-action-h site)
  (λ (req)
    (cond
      [(not (admin-ok? req site)) (resp-status 403 "admin only")]
      [else
       (define action (form-binding req "action"))
       (define n-str (form-binding req "n"))
       (define n (and n-str (string->number n-str)))
       (cond
         [(equal? action "publish")
          (define src (build-path (drafts-dir (site-data-dir site)) (format "~a.json" n)))
          (cond
            [(file-exists? src)
             (make-directory* (issues-dir (site-data-dir site)))
             (copy-file src (build-path (issues-dir (site-data-dir site))
                                        (format "~a.json" n)) #t)
             (delete-file src)
             (resp-redirect "/admin")]
            [else (resp-status 404 "no such draft")])]
         [(equal? action "discard")
          (define src (build-path (drafts-dir (site-data-dir site)) (format "~a.json" n)))
          (when (file-exists? src) (delete-file src))
          (resp-redirect "/admin")]
         [(equal? action "notify")
          (define j (issue-by-number (site-data-dir site) n))
          (if j
              (let* ([subs (load-subs (site-data-dir site))]
                     [actives
                      (for/list ([(k rec) (in-hash (hash-ref subs 'subscribers))]
                                 #:when (subscription-active? rec))
                        k)]
                     [sent
                      (for/list ([k (in-list actives)])
                        (define tok (put-token! (site-data-dir site) "access" k (* 35 24 60)))
                        (define url
                          (string-append (base-url-param) "/journal/" (~a n) "?k=" tok))
                        (send-mail! k
                                    (format "PITBOARD 周刊 · 第 ~a 期" (hash-ref j 'number))
                                    (notify-html j url))
                        k)])
                (resp-text-headers
                 (format "notify sent to ~a subscribers (~a)\n" (length sent) (length actives))
                 '()))
              (resp-status 404 "no such issue"))]
         [(equal? action "test-mail")
          (define to (form-binding req "test_to"))
          (if (and to (non-empty-string? to))
              (let ([r (send-mail! to "PITBOARD 周刊 测试邮件"
                                   "<p>这是一封测试邮件。dry-run 模式下它写在 data/mail-out/。</p>")])
                (resp-text-headers (format "~a\n" r) '()))
              (resp-status 400 "test_to required"))]
         [else (resp-status 400 "unknown action")])])))

;; admin POST handles BOTH login and multipart uploads; route through
;; the same handler and branch on the binding set
(define (admin-post-h site)
  (λ (req)
    (cond
      [(admin-ok? req site)
       ;; logged in: either a multipart draft upload or a test mail action
       (define bindings (request-bindings/raw req))
       (define draft
         (for/first ([b (in-list bindings)]
                     #:when (and (binding:file? b)
                                 (equal? (binding-id b) #"draft")))
           b))
       (cond
         [draft
          (define body (binding:file-content draft))
          (define j
            (with-handlers ([exn:fail? (λ (_) #f)])
              (with-input-from-bytes body read-json)))
          (cond
            [(valid-issue? j)
             (make-directory* (drafts-dir (site-data-dir site)))
             (with-output-to-file
                 (build-path (drafts-dir (site-data-dir site))
                             (format "~a.json" (hash-ref j 'number)))
               (λ () (write-json j)) #:exists 'replace)
             (resp-redirect "/admin")]
            [else (resp-status 400 "not a valid issue.json")])]
         [else
          (define action (form-binding req "action"))
          (cond
            [(equal? action "test-mail")
             (define to (form-binding req "test_to"))
             (if (and to (non-empty-string? to))
                 (let ([r (send-mail! to "PITBOARD 周刊 测试邮件"
                                      "<p>这是一封测试邮件。dry-run 模式下它写在 data/mail-out/。</p>")])
                   (resp-text-headers (format "~a\n" r) '()))
                 (resp-status 400 "test_to required"))]
            [else (resp-status 400 "unknown action")])])]
      [else ((admin-page-h site) req)])))

;; ---- wiring ----------------------------------------------------------------

(struct site (data-dir admin-tokens))

;; /admin dispatches on method and query: GET with ?action= runs the
;; console actions (publish/discard/notify); GET plain shows console or
;; login; POST handles login, multipart uploads and test mail.
(define (admin-router-h site)
  (λ (req)
    (cond
      [(equal? (request-method req) #"POST") ((admin-post-h site) req)]
      [(regexp-match? #px"[?&]action=" (url->string (request-uri req)))
       ((admin-action-h site) req)]
      [else ((admin-page-h site) req)])))

(define (make-site data-dir)
  (define admin-tokens (box (hash)))
  (define s (site data-dir admin-tokens))
  (hasheq
   'journal-archive (journal-archive-h s)
   'journal-issue (journal-issue-h s)
   'subscribe (subscribe-h s)
   'pay-callback (pay-callback-h s)
   'pay-claim (pay-claim-h s)
   'confirm (confirm-h s)
   'login (login-h s)
   'magic (magic-h s)
   'me (me-h s)
   'unsubscribe (unsubscribe-h s)
   'admin (admin-router-h s)
   'admin-action (admin-action-h s)))
