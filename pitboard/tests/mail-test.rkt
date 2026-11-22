#lang racket/base

;; Mail tests: with SMTP unconfigured, send-mail! is a dry-run — it
;; must never attempt a network call and must leave an inspectable
;; .eml carrying the recipient, subject and body content.

(require rackunit
         racket/file
         racket/string
         racket/path
         "../mail.rkt")

(define mail-out (make-parameter (make-temporary-file "pitboard-mail-~a" 'directory)))

(test-case "dry-run never sends and writes inspectable .eml"
  (parameterize ([mail-out-param (mail-out)]
                 [mail-host-param #f]
                 [mail-user-param #f]
                 [mail-pass-param #f]
                 [mail-from-param #f])
    (check-false (mail-configured?))
    (define r (send-mail! "reader@example.com"
                          "PITBOARD 周刊 · 第 42 期"
                          "<p>答案先行：本期是测试正文。</p>"))
    (check-equal? r 'dry-run)
    (define files (directory-list (mail-out)))
    (check-true (= (length files) 1))
    (define eml (file->string (build-path (mail-out) (car files))))
    (check-true (string-contains? eml "reader@example.com"))
    ;; subject is RFC 2047 base64-encoded UTF-8
    (check-true (string-contains? eml "=?UTF-8?B?"))
    (check-true (string-contains? eml "本期是测试正文"))))

(test-case "templates carry the design vocabulary and required links"
  (define issue
    (hasheq 'number 42
            'date "2026-11-21"
            'foreword "本周的行业叙事被一份文件接管。"
            'headline (hasheq 'title "L3 准入试点扩容"
                              'summary "答案先行。" 'why "为什么重要。")))
  (define n (notify-html issue "https://pitboard.example/journal/42"))
  (check-true (string-contains? n "PITBOARD"))
  (check-true (string-contains? n "L3 准入试点扩容"))
  (check-true (string-contains? n "https://pitboard.example/journal/42"))
  (define c (confirm-html "https://pitboard.example/confirm/tok"))
  (check-true (string-contains? c "确认订阅"))
  (check-true (string-contains? c "https://pitboard.example/confirm/tok"))
  (define l (login-html "https://pitboard.example/magic/tok"))
  (check-true (string-contains? l "登录"))
  (check-true (string-contains? l "https://pitboard.example/magic/tok"))
  ;; HTML escaping holds: a title with markup cannot inject
  (define esc-issue (hasheq 'number 1 'date "2026-01-01" 'foreword "x"
                            'headline (hasheq 'title "<script>alert(1)</script>")))
  (check-false (string-contains? (notify-html esc-issue "u") "<script>")))
