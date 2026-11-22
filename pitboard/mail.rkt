#lang racket/base

;; Mail delivery for the paid journal.  A minimal SMTP-over-TLS client
;; (target: Aliyun DirectMail's smtpdm endpoint, port 465) plus a
;; dry-run mode: when SMTP is unconfigured, messages are written as
;; .eml files under data/mail-out/ — tests and staging never send,
;; and the operator can inspect exactly what would have gone out.

(require racket/file
         racket/string
         racket/format
         racket/port
         racket/match
         json
         openssl
         net/base64
         "util.rkt")

(provide mail-configured?
         mail-host-param
         mail-port-param
         mail-user-param
         mail-pass-param
         mail-from-param
         mail-out-param
         send-mail!
         notify-html
         confirm-html
         login-html)

;; ---- config (parameters so tests can override without touching env) -------

(define mail-host-param (make-parameter (getenv "PITBOARD_SMTP_HOST")))
(define mail-port-param (make-parameter
                         (or (and (getenv "PITBOARD_SMTP_PORT")
                                  (string->number (getenv "PITBOARD_SMTP_PORT")))
                             465)))
(define mail-user-param (make-parameter (getenv "PITBOARD_SMTP_USER")))
(define mail-pass-param (make-parameter (getenv "PITBOARD_SMTP_PASS")))
(define mail-from-param (make-parameter (getenv "PITBOARD_MAIL_FROM")))
(define mail-out-param (make-parameter "data/mail-out"))

(define (mail-configured?)
  (and (mail-host-param) (mail-user-param) (mail-pass-param) (mail-from-param)
       #t))

;; ---- minimal SMTP-over-TLS client -----------------------------------------

;; Reply loop: RFC 5321 multiline replies end at "NNN " (space); the
;; interim "NNN-" continuation lines keep reading.
(define (open-smtp host port)
  (define-values (in out) (ssl-connect host port))
  (define (read-reply)
    (let loop ()
      (define l (read-line in 'return-linefeed))
      (cond [(eof-object? l) (error 'smtp "connection closed by server")]
            [(regexp-match? #px"^\\d{3} " l) l]
            [(regexp-match? #px"^\\d{3}-" l) (loop)]
            [else (error 'smtp "malformed reply: ~a" l)])))
  (define (cmd s [expect 250])
    (fprintf out "~a\r\n" s)
    (flush-output out)
    (define r (read-reply))
    (unless (equal? (substring (string-trim r) 0 3) (~a expect))
      (error 'smtp "unexpected reply for ~a: ~a"
             (regexp-replace #px"^AUTH LOGIN.*" s "AUTH ...") r))
    r)
  (values in out cmd read-reply))

;; Dot-stuffing is unnecessary here: the body is base64 (no line can
;; start with "."), but headers still go through the normal writer.
(define (smtp-send! to subject html)
  (define host (mail-host-param))
  (define port (mail-port-param))
  (define user (mail-user-param))
  (define pass (mail-pass-param))
  (define from (mail-from-param))
  (define-values (in out cmd read-reply) (open-smtp host port))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (read-reply)  ; greeting
     (cmd (format "EHLO pitboard") 250)
     (cmd "AUTH LOGIN" 334)
     (fprintf out "~a\r\n" (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 user)))))
     (flush-output out)
     (define r1 (read-reply))
     (unless (equal? (substring (string-trim r1) 0 3) "334")
       (error 'smtp "AUTH LOGIN username rejected: ~a" r1))
     (fprintf out "~a\r\n" (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 pass)))))
     (flush-output out)
     (define r2 (read-reply))
     (unless (equal? (substring (string-trim r2) 0 3) "235")
       (error 'smtp "AUTH LOGIN rejected: ~a" r2))
     (cmd (format "MAIL FROM:<~a>" from) 250)
     (cmd (format "RCPT TO:<~a>" to) 250)
     (cmd "DATA" 354)
     ;; RFC 2047 for the subject; body is base64 (7-bit clean).
     (define b64-body
       (string-replace (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 html)))) "\n" "\r\n"))
     (fprintf out
              "From: PITBOARD 周刊 <~a>\r\nTo: <~a>\r\nSubject: =?UTF-8?B?~a?=\r\nMIME-Version: 1.0\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\n~a\r\n.\r\n"
              from to
              (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 subject))))
              b64-body)
     (flush-output out)
     (read-reply)
     (cmd "QUIT" 221)
     'sent)
   (λ ()
     (close-input-port in)
     (close-output-port out))))

;; ---- send -----------------------------------------------------------------

(define (send-mail! to subject html [now (current-seconds)])
  (if (mail-configured?)
      (smtp-send! to subject html)
      ;; dry-run: never send, leave an inspectable .eml instead
      (let* ([dir (mail-out-param)]
             [p (build-path dir (format "~a-~a.eml" now (random 99999)))])
        (make-directory* dir)
        (with-output-to-file p
          (λ ()
            (printf "From: PITBOARD 周刊 <~a>\r\nTo: <~a>\r\nSubject: =?UTF-8?B?~a?=\r\nMIME-Version: 1.0\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"
                    (or (mail-from-param) "dry-run@example.invalid")
                    to
                    (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 subject)))))
            (write-string html))
          #:exists 'replace)
        'dry-run)))

;; ---- templates (email-client-safe: inline styles, no web fonts) ----------

(define (esc s)
  (regexp-replace* #px"<"
    (regexp-replace* #px">"
      (regexp-replace* #px"&" s "&amp;")
      "&gt;")
    "&lt;"))

(define (notify-html issue read-url)
  (define number (hash-ref issue 'number "?"))
  (define date (hash-ref issue 'date ""))
  (define foreword (hash-ref issue 'foreword ""))
  (define headline (hash-ref issue 'headline (hasheq)))
  (define date-parts (string-split date "-"))
  (define (p i) (if (> (length date-parts) i) (list-ref date-parts i) ""))
  (string-append
   "<div style=\"max-width:600px;margin:0 auto;font-family:-apple-system,'PingFang SC','Microsoft YaHei',sans-serif;color:#1C1917\">"
   "<div style=\"border-top:2px solid #1C1917;height:0\"></div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:24px;font-weight:600;text-align:center;padding:20px 0 4px\">"
   "<span style=\"letter-spacing:.14em\">PITBOARD</span> 周刊</div>"
   "<div style=\"text-align:center;font-size:12px;color:#A8A29E;padding-bottom:14px;"
   "border-bottom:1px solid #D6D3D1\">第 " (esc (~a number)) " 期 · " (esc date)
   " · 汽车行业的一周</div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:15px;line-height:1.9;"
   "margin-top:24px;text-align:justify\">" (esc foreword) "</div>"
   "<div style=\"font-size:13px;letter-spacing:.12em;color:#57534E;margin-top:24px;"
   "padding-bottom:8px;border-bottom:1px solid #D6D3D1\">本周头条</div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:19px;font-weight:600;"
   "line-height:1.5;margin-top:16px\">" (esc (hash-ref headline 'title "")) "</div>"
   "<div style=\"text-align:center;margin-top:28px\">"
   "<a href=\"" (esc read-url) "\" style=\"display:inline-block;padding:10px 32px;"
   "background:#D97757;color:#ffffff;text-decoration:none;font-weight:600;"
   "font-size:15px;border-radius:8px\">阅读本期全文</a></div>"
   "<p style=\"text-align:center;font-size:12px;color:#A8A29E;margin-top:12px\">"
   "链接直达、免登录，仅发送给订阅者。</p>"
   "<div style=\"margin-top:28px;padding-top:16px;border-top:1px solid #E7E5E4;"
   "text-align:center;font-size:12px;color:#A8A29E\">"
   "<a href=\"" (esc read-url) "\" style=\"color:#57534E\">往期归档</a> · "
   "<a href=\"#\" style=\"color:#57534E\">更正声明</a> · 一键退订见邮件末尾链接</div>"
   "</div>"))

(define (login-html url)
  (string-append
   "<div style=\"max-width:600px;margin:0 auto;font-family:-apple-system,'PingFang SC','Microsoft YaHei',sans-serif;color:#1C1917\">"
   "<div style=\"border-top:2px solid #1C1917;height:0\"></div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:24px;font-weight:600;text-align:center;padding:20px 0 4px\">"
   "<span style=\"letter-spacing:.14em\">PITBOARD</span> 周刊</div>"
   "<div style=\"text-align:center;font-size:12px;color:#A8A29E;padding-bottom:14px;"
   "border-bottom:1px solid #D6D3D1\">登录链接</div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:15px;line-height:1.9;margin-top:24px\">"
   "有人刚刚请求登录这个邮箱的订阅。是你吗？</div>"
   "<div style=\"text-align:center;margin-top:24px\">"
   "<a href=\"" (esc url) "\" style=\"display:inline-block;padding:10px 32px;background:#D97757;"
   "color:#ffffff;text-decoration:none;font-weight:600;font-size:15px;border-radius:8px\">登录</a></div>"
   "<p style=\"text-align:center;font-size:12px;color:#A8A29E;margin-top:12px\">"
   "链接 15 分钟内有效。不是你操作的？忽略此邮件。</div>"))

(define (confirm-html url)
  (string-append
   "<div style=\"max-width:600px;margin:0 auto;font-family:-apple-system,'PingFang SC','Microsoft YaHei',sans-serif;color:#1C1917\">"
   "<div style=\"border-top:2px solid #1C1917;height:0\"></div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:24px;font-weight:600;text-align:center;padding:20px 0 4px\">"
   "<span style=\"letter-spacing:.14em\">PITBOARD</span> 周刊</div>"
   "<div style=\"text-align:center;font-size:12px;color:#A8A29E;padding-bottom:14px;"
   "border-bottom:1px solid #D6D3D1\">订阅确认</div>"
   "<div style=\"font-family:Georgia,'Songti SC',serif;font-size:15px;line-height:1.9;margin-top:24px\">"
   "有人刚刚为这个邮箱订阅了年订（¥199/年）。是你吗？</div>"
   "<div style=\"text-align:center;margin-top:24px\">"
   "<a href=\"" (esc url) "\" style=\"display:inline-block;padding:10px 32px;background:#D97757;"
   "color:#ffffff;text-decoration:none;font-weight:600;font-size:15px;border-radius:8px\">确认订阅</a></div>"
   "<p style=\"text-align:center;font-size:12px;color:#A8A29E;margin-top:12px\">"
   "确认链接免登录、72 小时内有效；点击即完成订阅并直达最新一期。</p>"
   "<div style=\"margin-top:28px;padding-top:16px;border-top:1px solid #E7E5E4;font-size:13px;"
   "line-height:1.8;color:#57534E\">确认后：每周六 08:00 收到出刊通知，网页全文与往期归档随时可读。"
   "不是你操作的？忽略这封邮件即可，不会产生任何订阅。</div></div>"))
