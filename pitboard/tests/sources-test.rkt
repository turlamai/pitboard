#lang racket/base

(require rackunit
         racket/date
         racket/list
         racket/string
         "../sources.rkt"
         "../item.rkt")

;; json-path navigation
(test-case "json-path"
  (define h (hash 'a (hash 'b (list 10 (hash 'c "x")))))
  (check-equal? (json-path h '(a b 1 c)) "x")
  (check-equal? (json-path h '(a b 0)) 10)
  (check-equal? (json-path h '(a missing)) 'null)
  (check-equal? (json-path h '(a b 5 c)) 'null)
  (check-equal? (json-path h '()) h))

;; Chinese slash-date format ("2026/10/13", sometimes with a trailing
;; slash) that parse-date-any (8601/822 only) cannot read
(test-case "parse-date-slash"
  (check-equal? (parse-date-slash "2026/10/13/")
                (find-seconds 0 0 12 13 10 2026 #t))
  (check-equal? (parse-date-slash "2026/10/13")
                (find-seconds 0 0 12 13 10 2026 #t))
  (check-equal? (parse-date-slash " 2026/1/2 ")
                (find-seconds 0 0 12 2 1 2026 #t))
  (check-false (parse-date-slash "2026/13/13"))
  (check-false (parse-date-slash "not a date"))
  (check-false (parse-date-slash #f)))

;; fxbaogao-shaped entries through the pure json_api item builder:
;; no url field (url_template instead), slash dates, keyword gates.
(define fx-entries
  (hash 'code 0
        'data (list (hash 'docId 1001 'title "中国整车出口白皮书"
                          'orgName "特易咨询" 'pubTimeStr "2026/10/13/")
                    (hash 'docId 1002 'title "开源晨会"
                          'orgName "某证券" 'pubTimeStr "2026/10/13/")
                    (hash 'docId 1003 'title "人形机器人减速器专题"
                          'orgName "某证券" 'pubTimeStr "2026/10/12/"))))

(define fx-cfg
  (hasheq 'name "发现报告 · 交运设备"
          'tier 2 'weight 0.95
          'path "data"
          'title_field "title"
          'date_field "pubTimeStr"
          'url_template "https://www.fxbaogao.com/detail/{docId}"
          'max 10
          'except_keywords (list "晨会" "盘前" "复盘")))

(test-case "json-api items: url_template + gates + slash dates"
  (define items (json-api-items fx-entries fx-cfg))
  ;; 开源晨会 gated out by except_keywords; the rest survive
  (check-equal? (length items) 2)
  (define first (car items))
  (check-equal? (item-title first) "中国整车出口白皮书")
  (check-equal? (item-url first) "https://www.fxbaogao.com/detail/1001")
  (check-equal? (item-source first) "发现报告 · 交运设备")
  (check-equal? (item-published first) (find-seconds 0 0 12 13 10 2026 #t))
  (check-equal? (hash-ref (item-extra first) 'weight) 0.95)
  ;; docId numbers must render into the URL as digits, not "1001.0"
  (check-false (string-contains? (item-url first) ".0")))

(test-case "json-api items: url_template missing field drops entry"
  (define broken (hash 'code 0 'data (list (hash 'title "无 docId 的条目"))))
  (check-equal? (json-api-items broken fx-cfg) '()))

(test-case "json-api items: only_keywords gate"
  (define cfg-elec
    (hasheq 'name "发现报告 · 电气设备"
            'tier 2 'weight 0.8
            'path "data"
            'title_field "title"
            'date_field "pubTimeStr"
            'url_template "https://www.fxbaogao.com/detail/{docId}"
            'max 10
            'only_keywords (list "固态" "电池" "锂电")))
  (define elec-entries
    (hash 'code 0
          'data (list (hash 'docId 2001 'title "固态电池带来新投资周期"
                            'pubTimeStr "2026/10/14/")
                      (hash 'docId 2002 'title "算力租赁价格跟踪"
                            'pubTimeStr "2026/10/14/"))))
  (define items (json-api-items elec-entries cfg-elec))
  (check-equal? (length items) 1)
  (check-equal? (item-title (car items)) "固态电池带来新投资周期"))

(test-case "json-api items: classic url_field still works"
  (define entries (hash 'items (list (hash 'title "外刊标题"
                                           'url "https://example.com/a"
                                           'published_at "2026-10-13T09:00:00Z"))))
  (define cfg (hasheq 'name "api" 'path "items" 'max 5))
  (define items (json-api-items entries cfg))
  (check-equal? (length items) 1)
  (check-equal? (item-url (car items)) "https://example.com/a")
  (check-equal? (item-published (car items)) (find-seconds 0 0 9 13 10 2026 #t)))

(test-case "json-api items: max caps entries"
  (define many (hash 'code 0
                     'data (for/list ([i (in-range 12)])
                             (hash 'docId i 'title (format "报告 ~a" i)
                                   'pubTimeStr "2026/10/13/"))))
  (check-equal? (length (json-api-items many fx-cfg)) 10))

;; ---- GBK page decoding (from the retired root test/sources-test.rkt) ---------

(define gbk-cvt (bytes-open-converter "UTF-8" "GBK"))
(define gbk-supported? (boolean? gbk-cvt))

(when gbk-supported?
  (define utf8-title "汽车之家：本周新车盘点与降价潮")
  ;; GBK-encoded page (meta charset declaration + anchor links with text)
  (define utf8-html
    (string-append
     "<html><head><meta charset=\"gb2312\"></head><body>"
     "<a href=\"/news/202607/1315940.html\">" utf8-title "</a>"
     "<a href=\"/news/202607/1315911.html\">第二条测试标题足够长</a>"
     "</body></html>"))
  (define gbk-bytes
    (let ([out (make-bytes (* 3 (string-length utf8-html)))]
          [cvt (bytes-open-converter "UTF-8" "GBK")])
      (define-values (used got status)
        (bytes-convert cvt (string->bytes/utf-8 utf8-html)
                       0 (string-length utf8-html) out))
      (bytes-close-converter cvt)
      (subbytes out 0 got)))

  (test-case "decode-html-bytes gbk"
    (define html (decode-html-bytes gbk-bytes))
    (check-true (string-contains? html utf8-title) "GBK should round-trip to UTF-8")
    (check-false (string-contains? html "\uFFFD") "no replacement chars"))

  (test-case "extract-web-items on gbk page"
    (define rows (extract-web-items (decode-html-bytes gbk-bytes)
                                    "https://www.autohome.com.cn"
                                    #px"/news/[0-9]{6}/[0-9]+\\.html"))
    (check-equal? (length rows) 2)
    (check-equal? (car (car rows)) utf8-title)
    (check-equal? (cadr (car rows))
                  "https://www.autohome.com.cn/news/202607/1315940.html")))

;; ---- protocol-relative hrefs -------------------------------------------------

(test-case "protocol-relative hrefs inherit https"
  (define html
    "<html><body><a href=\"//www.example.com/news/202607/99.html\">这条标题足够长了吧</a></body></html>")
  (define rows (extract-web-items html "https://www.example.com"
                                  #px"//www\\.example\\.com/news/"))
  (check-equal? (length rows) 1)
  (check-equal? (cadr (car rows)) "https://www.example.com/news/202607/99.html"))

;; ---- web_list keyword gate (exercised through fetch-all) ---------------------
;; The gate lives in fetch-web-list; a synthetic config verifies behavior.

(define gate-config
  (hasheq 'feeds
          (list (hasheq 'name "门控测试"
                        'type "web_list"
                        'url "data:text/html,<html></html>"
                        'base_url "https://x.example"
                        'link_regex "/news/"
                        'only_keywords (list "汽车")
                        'max 10))))

(test-case "web_list keyword gate config accepted"
  ;; fetch-all must not error on the gate fields (the network request
  ;; fails, the source is marked failed)
  (define-values (items statuses) (fetch-all gate-config))
  (check-true (list? items)))
