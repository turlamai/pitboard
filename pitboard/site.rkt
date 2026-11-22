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

;; Trust surface: sourcing and corrections policy — linked from every
;; page footer (更正声明).
(define (about-page)
  (page-shell
   "来源与更正 — PITBOARD 周刊"
   (string-append
    "<header class=\"masthead\"><div class=\"masthead-rule\"></div>"
    "<h1 class=\"masthead-title\"><a href=\"/\" style=\"color:inherit\"><span class=\"en\">PITBOARD</span> 周刊</a></h1>"
    "<div class=\"masthead-issue\"><span>来源与更正</span>"
    "<span class=\"tag\">汽车行业的一周</span></div></header>"
    "<section class=\"section\" style=\"margin-top:40px\">"
    "<div class=\"section-head\"><span class=\"section-title\">内容从哪里来</span><span class=\"section-count\">三类信源</span></div>"
    "<div class=\"item\"><h3 class=\"item-title\">官方一手</h3>"
    "<p class=\"item-summary\">部委与监管机构发布、车企官方渠道、标准组织公告——政策与定点类事件的最高优先级信源。</p></div>"
    "<div class=\"item\"><h3 class=\"item-title\">行业媒体</h3>"
    "<p class=\"item-summary\">覆盖国内主流汽车媒体与海外行业媒体，按条线配置权重；同一事件多家报道只计一次。</p></div>"
    "<div class=\"item\"><h3 class=\"item-title\">研究与数据</h3>"
    "<p class=\"item-summary\">论文预印本、销量与价格数据发布——数据版与大事记的原料。</p></div></section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">怎么做出来</span><span class=\"section-count\">AI 流水线 + 主编复核</span></div>"
    "<p class=\"item-summary\">每天，规则引擎把上百条原始报道聚簇去重、按独立来源数与时间衰减算出热度——这是零 token 的内部工序，读者永远看不到中间形态。"
    "每周，AI 从一周的事件里精选、写作、汇编成期刊初稿，主编逐期复核后才发布。所有条目附原文链接，原文没有提到的结论不会出现在标题里。</p></section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">更正政策</span><span class=\"section-count\">可追溯</span></div>"
    "<p class=\"item-summary\">发现错误当期更正，并在下一期刊登更正声明，注明更正内容与原因；永不静默修改已发布内容。"
    "发现错误请通过邮件联系主编，注明期号与条目。</p></section>"
    (nav-footer))))

(define (nav-footer)
  (string-append
   "<footer class=\"site-footer\">"
   "<p>PITBOARD 周刊 · 汽车行业的一周 · <a href=\"/subscribe\">订阅</a> · "
   "<a href=\"/journal\">往期归档</a> · <a href=\"/me\">我的订阅</a> · "
   "<a href=\"/about\">更正声明</a></p>"
   "<p>所有条目附原文链接；AI 汇编 + 主编复核，发现错误当期更正、下期刊登。</p></footer>"))

(define (issue-auth-email req data-dir)
  ;; email when authenticated as an active subscriber, else #f
  (or (let ([email (session-email-from-cookie req data-dir)])
        (and email (subscription-active? (subscription-for data-dir email)) email))
      (let* ([m (and (request-uri req)
                     (regexp-match #px"[?&]k=([^&]+)" (url->string (request-uri req))))]
             [email (and m (access-email! data-dir (cadr m)))])
        (and email (subscription-active? (subscription-for data-dir email)) email))))

(define (journal-404-response)
  (response/full 404 #"Not Found" (current-seconds)
                 (bytes->immutable-bytes #"text/html; charset=utf-8")
                 '()
                 (list
                  (string->bytes/utf-8
                   (page-shell
                    "页面不存在 — PITBOARD 周刊"
                    (string-append
                     "<div class=\"subscribe-card\"><h3>这一页不存在</h3>"
                     "<p class=\"note\">期刊可能已下架或链接有误。往期归档里有全部已出刊的期号。</p>"
                     "<a class=\"btn-primary\" href=\"/journal\">去往期归档</a> "
                     "<a class=\"btn-primary\" style=\"background:var(--accent-bg);color:var(--accent)\" href=\"/subscribe\">订阅本周刊</a>"
                     "</div>"
                     (nav-footer)))))))

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
           (format "<section class=\"section\" style=\"margin-top:40px\"><div class=\"section-head\"><span class=\"section-title\">~a 年</span><span class=\"section-count\">~a 期 · 每周六出刊</span></div>~a</section>"
                   (let ([ds (hash-ref (car issues) 'date "")])
                     (if (>= (string-length ds) 4) (substring ds 0 4) ""))
                   (length issues) rows))
       (nav-footer))))))

(define (journal-issue-h site)
  (λ (req n-str)
    (define n (string->number (string-trim n-str "/")))
    (define j (and n (issue-by-number (site-data-dir site) n)))
    (cond
      [(not j) (journal-404-response)]
      [else
       (define email (issue-auth-email req (site-data-dir site)))
       (if email
           (resp-html (render-full-html j))
           (resp-html (render-locked-html (locked-view j))))])))

(define (subscribe-landing-page data-dir)
  (define issues (filter values (published-issues data-dir)))
  (define latest (and (pair? issues)
                      (car (sort issues > #:key (λ (j) (hash-ref j 'number 0))))))
  (define latest-preview
    (if latest
        (string-append
         "<div class=\"prose\" style=\"margin-bottom:20px\">"
         "<p>" (esc (hash-ref latest 'foreword "")) "</p></div>"
         "<div class=\"toc-locked\">"
         "<div class=\"toc-line\"><span class=\"name\">本周头条 · "
         (esc (hash-ref (hash-ref latest 'headline (hasheq)) 'title ""))
         "</span><span class=\"count\">免费阅读</span></div>"
         (string-join
          (for/list ([b (in-list (hash-ref latest 'beats '()))])
            (format "<div class=\"toc-line\"><span class=\"name\">~a</span><span class=\"count\">订阅可见</span></div>"
                    (esc (hash-ref b 'name))))
          "\n")
         "</div>"
         "<p style=\"margin-top:20px;font-size:14px\">"
         "<a href=\"/journal/" (~a (hash-ref latest 'number 0)) "\">阅读本期卷首与头条全文 →</a></p>")
        "<p class=\"item-meta\">创刊号正在路上——现在订阅，创刊当天你的邮箱会准时响起。</p>"))
  (page-shell
   "订阅 — PITBOARD 周刊"
   (string-append
    "<header class=\"masthead\"><div class=\"masthead-rule\"></div>"
    "<h1 class=\"masthead-title\"><a href=\"/\" style=\"color:inherit\"><span class=\"en\">PITBOARD</span> 周刊</a></h1>"
    "<div class=\"masthead-issue\"><span>订阅</span>"
    "<span class=\"tag\">汽车行业的一周</span></div></header>"
    "<section class=\"foreword prose\" style=\"margin-top:48px\">"
    "<p>每周一早，把汽车行业一周的事——筛好、写好、装订好，送进你的邮箱。</p></section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">为什么值得订阅</span><span class=\"section-count\">3 件事</span></div>"
    "<div class=\"value-item\"><h3 class=\"item-title\">筛选，而不是罗列</h3>"
    "<p class=\"item-summary\">每个条线只保留真正重要的 2–3 件事：政策与法规、智能化、三电与供应链、整车与市场。"
    "每条都有一句「为什么入选」——那是主编的判断，不是算法的输出。</p></div>"
    "<div class=\"value-item\"><h3 class=\"item-title\">装订，而不是推送</h3>"
    "<p class=\"item-summary\">本周头条给足深度综述，数据版汇总关键数字，大事记留档时间线。"
    "读完一期约 15 分钟，覆盖的是一个行业的一周。</p></div>"
    "<div class=\"value-item\"><h3 class=\"item-title\">准时，而不是「大概有」</h3>"
    "<p class=\"item-summary\">每周六早 8 点，一封出刊通知准时送进邮箱，点开直达本期全文。"
    "节假日顺延，出刊失败下一期补齐并公告——确定性本身就是产品的一部分。</p></div></section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">最新一期</span>"
    "<span class=\"section-count\">"
    (if latest (format "第 ~a 期 · ~a" (hash-ref latest 'number 0) (esc (hash-ref latest 'date ""))) "创刊准备中")
    "</span></div>"
    latest-preview
    "</section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">订阅价格</span><span class=\"section-count\">单一方案</span></div>"
    "<div class=\"subscribe-card\" style=\"margin-top:8px\"><h3>年订</h3>"
    "<div class=\"price num\">¥199<span class=\"unit\"> / 年 · 52 期 ≈ ¥3.8 一期</span></div>"
    "<p class=\"note\">每周六 08:00 出刊通知直达 · 网页全文与往期归档 · 未满 30 天可全额退款</p>"
    "<form method=\"post\" action=\"/subscribe\" style=\"margin-top:12px\">"
    "<input type=\"email\" name=\"email\" required placeholder=\"你的邮箱\" "
    "style=\"width:260px;padding:10px 12px;border:1px solid var(--border);border-radius:8px;"
    "font-size:15px;background:var(--surface);color:var(--text)\"> "
    "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer\">去支付</button>"
    "</form>"
    "<div class=\"subscribe-flow\"><span class=\"step\">① 填邮箱</span>"
    "<span class=\"step\">② 支付</span><span class=\"step\">③ 邮件确认</span></div></div></section>"
    "<section class=\"section\">"
    "<div class=\"section-head\"><span class=\"section-title\">常见问题</span><span class=\"section-count\">4 条</span></div>"
    "<dl class=\"faq\">"
    "<dt>什么时候出刊？</dt><dd>每周六早 8 点。节假日顺延至下一个周六；连续两期未出刊会邮件公告并顺延订阅期。</dd>"
    "<dt>内容在哪看？</dt><dd>订阅后在网页阅读全文——不做 App，也不做 PDF，好的排版在网页上就够了；"
    "每周六的出刊通知邮件带你直达本期。未订阅的访客只能看到当期的卷首按语与本周头条全文。</dd>"
    "<dt>怎么退订？中途退订退钱吗？</dt><dd>每期邮件末尾有一键退订链接，点击即生效，不再发送；订阅未满 30 天可申请全额退款。</dd>"
    "<dt>内容怎么来的，可信吗？</dt><dd>信源覆盖官方发布、行业媒体与论文预印本，全部条目附原文链接；"
    "由 AI 流水线筛选汇编，主编逐期复核。发现错误当期更正，并在下一期「更正声明」刊登。</dd>"
    "</dl></section>"
    (nav-footer))))

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
       (resp-html (subscribe-landing-page (site-data-dir site)))])))

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
    (define latest-n
      (let ([issues (filter values (published-issues (site-data-dir site)))])
        (and (pair? issues)
             (hash-ref (car (sort issues > #:key (λ (j) (hash-ref j 'number 0))))
                       'number))))
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
              (string-append "<a class=\"btn-primary\" href=\"/journal/" (~a (or latest-n ""))
                             "\">阅读最新一期</a> ")
              (string-append "<a class=\"btn-primary\" href=\"/subscribe\">续订</a> "))
          "<a class=\"btn-primary\" style=\"background:var(--accent-bg);color:var(--accent)\" "
          "href=\"/unsubscribe/" (esc (hash-ref rec 'unsub_token "missing")) "\" target=\"_self\">一键退订</a>"
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
           (format "<div class=\"toc-line\"><span class=\"name\">No.~a · ~a · ~a</span><span><a href=\"/journal/~a\">查看</a> · <a href=\"/admin?action=notify&n=~a\">发刊通知邮件</a></span></div>"
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
  (define subscriber-rows
    (string-join
     (for/list ([rec (in-list (hash-ref (load-subs (site-data-dir site)) 'subscribers '()))])
       (format "<div class=\"toc-line\"><span class=\"name\">~a</span><span class=\"count\">~a · 至 ~a · <a href=\"/console?action=deactivate&email=~a\">停用</a> · <a href=\"/console?action=delete&email=~a\">删除</a></span></div>"
               (esc (hash-ref rec 'email ""))
               (esc (hash-ref rec 'status ""))
               (esc (hash-ref rec 'expires "—"))
               (esc (hash-ref rec 'email ""))
               (esc (hash-ref rec 'email ""))))
     "\n"))
  (page-shell
   "主编工作台 — PITBOARD 周刊"
   (format
    (string-append
     "<section class=\"section\"><div class=\"section-head\">"
     "<span class=\"section-title\">订阅名单</span>"
     "<span class=\"section-count num\">active ~a · pending ~a · unsub ~a</span></div>"
     "<div class=\"toc-locked\">~a</div>"
     "<p class=\"item-meta\">名单明细含订阅者邮箱，只在此工作台查看，不进公开面。到期 30 天内用下方按钮触达续订。</p>"
     "<form method=\"post\" action=\"/console\" enctype=\"multipart/form-data\">"
     "<input type=\"hidden\" name=\"action\" value=\"remind\">"
     "<button type=\"submit\" class=\"btn-primary\" style=\"border:none;cursor:pointer;background:var(--accent-bg);color:var(--accent)\">发到期提醒（剩 ≤30 天）</button>"
     "</form></section>"
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
    subscriber-rows
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
                      (for/list ([rec (in-list (hash-ref subs 'subscribers '()))]
                                 #:when (subscription-active? rec))
                        rec)]
                     [sent
                      (for/list ([rec (in-list actives)])
                        (define k (hash-ref rec 'email ""))
                        (define tok (put-token! (site-data-dir site) "access" k (* 35 24 60)))
                        (define url
                          (string-append (base-url-param) "/journal/" (~a n) "?k=" tok))
                        (define unsub
                          (string-append (base-url-param) "/unsubscribe/"
                                         (hash-ref rec 'unsub_token "")))
                        (send-mail! k
                                    (format "PITBOARD 周刊 · 第 ~a 期" (hash-ref j 'number))
                                    (notify-html j url unsub))
                        k)])
                (resp-text-headers
                 (format "notify sent to ~a subscribers (~a)\n" (length sent) (length actives))
                 '()))
              (resp-status 404 "no such issue"))]
         [(equal? action "remind")
          ;; renewal loop: email actives whose subscription expires
          ;; within 30 days
          (define subs (load-subs (site-data-dir site)))
          (define now (current-seconds))
          (define due
            (for/list ([rec (in-list (hash-ref subs 'subscribers '()))]
                       #:when (subscription-active? rec now))
              (define e (and (not (eq? (hash-ref rec 'expires 'null) 'null))
                             (parse-iso-utc (hash-ref rec 'expires ""))))
              (and e (<= (- e now) (* 30 86400)) rec)))
          (define sent
            (for/list ([rec (in-list (filter values due))])
              (define k (hash-ref rec 'email ""))
              (define days (quotient (- (parse-iso-utc (hash-ref rec 'expires "")) now) 86400))
              (send-mail! k
                          (format "PITBOARD 周刊 · 订阅即将到期（剩 ~a 天）" days)
                          (renewal-html (string-append (base-url-param) "/subscribe")
                                        (string-append (base-url-param) "/unsubscribe/"
                                                       (hash-ref rec 'unsub_token ""))
                                        days))
              k))
          (resp-text-headers (format "renewal reminders sent to ~a subscribers\n" (length sent)) '())]
         [(equal? action "deactivate")
          (define email (form-binding req "email"))
          (if (and email (unsubscribe-email! (site-data-dir site) email))
              (resp-redirect "/console")
              (resp-status 404 "no such subscriber"))]
         [(equal? action "delete")
          (define email (form-binding req "email"))
          (if (and email (delete-subscriber! (site-data-dir site) email))
              (resp-redirect "/console")
              (resp-status 404 "no such subscriber"))]
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
         [(form-binding req "action")
          ;; plain POST actions (renewal reminders etc.)
          ((admin-action-h site) req)]
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
            [(equal? action "remind")
          ;; renewal loop: email actives whose subscription expires
          ;; within 30 days
          (define subs (load-subs (site-data-dir site)))
          (define now (current-seconds))
          (define due
            (for/list ([rec (in-list (hash-ref subs 'subscribers '()))]
                       #:when (subscription-active? rec now))
              (define e (and (not (eq? (hash-ref rec 'expires 'null) 'null))
                             (parse-iso-utc (hash-ref rec 'expires ""))))
              (and e (<= (- e now) (* 30 86400)) rec)))
          (define sent
            (for/list ([rec (in-list (filter values due))])
              (define k (hash-ref rec 'email ""))
              (define days (quotient (- (parse-iso-utc (hash-ref rec 'expires "")) now) 86400))
              (send-mail! k
                          (format "PITBOARD 周刊 · 订阅即将到期（剩 ~a 天）" days)
                          (renewal-html (string-append (base-url-param) "/subscribe")
                                        (string-append (base-url-param) "/unsubscribe/"
                                                       (hash-ref rec 'unsub_token ""))
                                        days))
              k))
          (resp-text-headers (format "renewal reminders sent to ~a subscribers\n" (length sent)) '())]
         [(equal? action "deactivate")
          (define email (form-binding req "email"))
          (if (and email (unsubscribe-email! (site-data-dir site) email))
              (resp-redirect "/console")
              (resp-status 404 "no such subscriber"))]
         [(equal? action "delete")
          (define email (form-binding req "email"))
          (if (and email (delete-subscriber! (site-data-dir site) email))
              (resp-redirect "/console")
              (resp-status 404 "no such subscriber"))]
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
   'landing (λ (req) (resp-html (subscribe-landing-page data-dir)))
   'about (λ (req) (resp-html (about-page)))
   'health (λ (req) (resp-text-headers "ok\n" '()))
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
