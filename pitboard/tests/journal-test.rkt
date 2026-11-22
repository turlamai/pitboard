#lang racket/base

;; Journal assembly tests: candidate merge, LLM passes with an injected
;; mock provider, budget 熔断, the locked/paywall boundary, and an
;; output-side leak scan of the rendered locked page.

(require rackunit
         racket/file
         racket/list
         racket/string
         json
         "../store.rkt"
         "../journal.rkt")

;; ---- fixtures --------------------------------------------------------------

(define (mk-item title url src)
  (hasheq 'id (format "x:~a" url)
          'kind "news"
          'source src
          'source_url src
          'title title
          'url url
          'summary (string-append title "。摘要。")
          'published_at "2026-11-20T00:00:00Z"
          'extra (hasheq)))

(define (mk-cluster title summary url src heat)
  (hasheq 'id (format "hot:~a" title)
          'heat heat
          'labels '()
          'domains 1
          'title title
          'summary summary
          'first_seen 'null
          'items (list (mk-item title url src))))

(define (mk-digest date clusters)
  (hasheq 'date date
          'generated_at (string-append date "T00:00:00Z")
          'mode "scored"
          'clusters clusters
          'sources '()))

;; ---- candidate merge -------------------------------------------------------

(test-case "week-candidates merges the same event across days"
  (define dir (make-temporary-file "pitboard-journal-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (save-digest! dir (mk-digest "2026-11-20"
                                  (list (mk-cluster "欧盟发布电池护照实施细则"
                                                    "欧盟本周发布电池护照实施细则。" "https://a.example/1" "a.example" "12.0"))))
     (save-digest! dir (mk-digest "2026-11-21"
                                  (list (mk-cluster "欧盟电池护照实施细则发布"
                                                    "电池护照实施细则发布。" "https://b.example/2" "b.example" "9.0")
                                        (mk-cluster "宁德时代神行 Pro 批量交付"
                                                    "神行 Pro 开始批量交付。" "https://c.example/3" "c.example" "8.0"))))
     (define cands (week-candidates dir "2026-11-20" "2026-11-21"))
     ;; the battery-passport pair merges into one event; the CATL story stands alone
     (check-equal? (length cands) 2)
     (define merged
       (findf (λ (c) (string-contains? (candidate-title c) "电池护照")) cands))
     (check-true (and merged #t))
     (check-equal? (candidate-dates merged) '("2026-11-20" "2026-11-21"))
     (check-equal? (candidate-domains merged) 2)
     ;; face of the merged event is the higher-heat day's title
     (check-equal? (candidate-heat merged) 12.0)
     ;; news-only: a paper-led cluster stays out
     (save-digest! dir (mk-digest "2026-11-19"
                                  (list (hasheq 'id "hot:paper" 'heat "20.0" 'labels '()
                                                'domains 1
                                                'title "某篇论文" 'summary "论文摘要。"
                                                'first_seen 'null
                                                'items (list (hasheq 'id "p1" 'kind "paper"
                                                                     'source "arxiv" 'source_url "arxiv.org"
                                                                     'title "某篇论文" 'url "https://arxiv.org/x"
                                                                     'summary "论文摘要。" 'published_at "2026-11-19T00:00:00Z"
                                                                     'extra (hasheq)))))))
     (check-equal? (length (week-candidates dir "2026-11-19" "2026-11-21")) 2))
   (λ () (delete-directory/files dir))))

;; ---- full assembly with the mock provider ----------------------------------

(test-case "build-issue end to end with mock provider"
  (define dir (make-temporary-file "pitboard-journal-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     ;; seven genuinely distinct events (titles that differ only by a
     ;; digit would tokenize identically and merge into one candidate)
     (define week-titles
       (list "工信部公布 L3 准入试点名单"
             "欧盟发布电池护照实施细则"
             "宁德时代神行 Pro 批量交付"
             "小鹏端到端 OTA 全量推送"
             "乾崑 ADS 5 首批车型上市"
             "碳酸锂现货价格反弹"
             "新能源渗透率首破六成"))
     (for ([d (in-list '("2026-11-15" "2026-11-16" "2026-11-17" "2026-11-18"
                         "2026-11-19" "2026-11-20" "2026-11-21"))]
           [t (in-list week-titles)]
           [i (in-naturals)])
       (save-digest! dir
                     (mk-digest d
                                (list (mk-cluster
                                       t
                                       (string-append t "。周内动态。")
                                       (format "https://s.example/~a" i)
                                       "s.example"
                                       (number->string (- 20 i)))))))
     (define cands (week-candidates dir "2026-11-15" "2026-11-21"))
     (check-equal? (length cands) 7)
     (define issue (build-issue cands
                                #:issue-number 42
                                #:date "2026-11-21"
                                #:provider mock-provider
                                #:budget 24))
     ;; structure: headline + capped selection (mock includes ≤5), fixed beat set
     (check-equal? (hash-ref issue 'number) 42)
     (check-equal? (hash-ref issue 'date) "2026-11-21")
     (check-true (non-empty-string? (hash-ref issue 'foreword "")))
     (check-true (string-prefix? (hash-ref (hash-ref issue 'headline) 'title) "【演示】"))
     (check-true (<= (hash-ref (hash-ref issue 'counts) 'events) 5))
     (for ([b (in-list (hash-ref issue 'beats))])
       (check-not-false (member (hash-ref b 'name) BEATS) "beat must be whitelisted"))
     ;; timeline sorted by first appearance
     (define tl (hash-ref issue 'timeline))
     (check-true (equal? tl (sort tl string<? #:key (λ (r) (hash-ref r 'date)))))
     ;; data table capped at 6 and label-deduped
     (check-true (<= (length (hash-ref issue 'data)) 6))

     ;; ---- paywall boundary ----
     (define locked (locked-view issue))
     (check-true (hash-has-key? locked 'toc))
     (check-false (hash-has-key? locked 'beats))
     (check-false (hash-has-key? locked 'data))
     (check-false (hash-has-key? locked 'timeline))
     ;; toc counts match the beats it stripped
     (define toc-n
       (for/sum ([t (in-list (hash-ref locked 'toc))]) (hash-ref t 'count)))
     (check-equal? toc-n
                   (- (hash-ref (hash-ref issue 'counts) 'items)
                      1))

     ;; ---- rendered pages: output-side leak scan ----
     (define paths (write-issue! dir issue))
     (define locked-html (file->string (build-path dir "journal-locked.html")))
     (define full-html (file->string (build-path dir "journal.html")))
     ;; free part must be present
     (check-true (string-contains? locked-html (hash-ref (hash-ref locked 'headline) 'title)))
     (check-true (string-contains? locked-html "订阅后继续阅读"))
     ;; paid titles must NOT appear anywhere in the locked page
     (define paid-titles
       (append-map (λ (b) (map (λ (it) (hash-ref it 'title)) (hash-ref b 'items)))
                   (hash-ref issue 'beats)))
     (for ([t (in-list paid-titles)])
       (check-false (string-contains? locked-html t)
                    (format "paid item leaked into locked page: ~a" t)))
     ;; the full page carries them all, plus the demo marking
     (for ([t (in-list paid-titles)])
       (check-true (string-contains? full-html t)))
     (check-true (string-contains? full-html "【演示】")))
   (λ () (delete-directory/files dir))))

;; ---- budget 熔断 -----------------------------------------------------------

(test-case "budget cap aborts runaway passes"
  (define dir (make-temporary-file "pitboard-journal-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     ;; titles that differ only by a digit tokenize identically and
     ;; would merge into one candidate — use genuinely distinct events
     (define week-titles
       (list "工信部公布 L3 准入试点名单"
             "欧盟发布电池护照实施细则"
             "宁德时代神行 Pro 批量交付"
             "小鹏端到端 OTA 全量推送"
             "乾崑 ADS 5 首批车型上市"
             "碳酸锂现货价格反弹"
             "新能源渗透率首破六成"))
     (for ([d (in-list '("2026-11-15" "2026-11-16" "2026-11-17" "2026-11-18"
                         "2026-11-19" "2026-11-20" "2026-11-21"))]
           [t (in-list week-titles)]
           [i (in-naturals)])
       (save-digest! dir
                     (mk-digest d
                                (list (mk-cluster t
                                                  (string-append t "。周内动态。")
                                                  (format "https://x.example/~a" i)
                                                  "x.example"
                                                  "5.0")))))
     ;; 7 independent events; mock includes 5 → 1 select + 5 writes + 1 foreword = 7 calls
     (define cands (week-candidates dir "2026-11-15" "2026-11-21"))
     (check-equal? (length cands) 7)
     (check-exn exn:fail?
                (λ () (build-issue cands
                                   #:issue-number 1
                                   #:date "2026-11-21"
                                   #:provider mock-provider
                                   #:budget 3))
                "budget exhaustion must raise"))
   (λ () (delete-directory/files dir))))

(test-case "fail-closed without a provider"
  ;; zero candidates → explicit error, not an empty issue
  (define dir (make-temporary-file "pitboard-journal-~a" 'directory))
  (dynamic-wind
   (λ () (void))
   (λ ()
     (check-exn exn:fail?
                (λ () (build-issue '()
                                   #:issue-number 1
                                   #:date "2026-11-21"
                                   #:provider mock-provider))
                "empty candidate range must raise"))
   (λ () (delete-directory/files dir))))
