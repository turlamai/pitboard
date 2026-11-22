#lang racket/base

;; Weekly journal assembly — the paid product.  Compresses the past
;; week's daily cluster archives into one issue: cross-day candidate
;; merge (pure rules, zero tokens), then three LLM passes (selection,
;; per-event writing, foreword).  Data table and timeline are compiled
;; by rules from the written events.
;;
;; Fail-closed: without a configured LLM there is no journal — the paid
;; product never ships rules-only content.  A provider may be injected
;; for tests; the default talks to any OpenAI-compatible endpoint using
;; the same env config as llm.rkt.  A hard call budget aborts runaway
;; passes.
;;
;; Content boundary: issue.json carries the full paid issue; locked-view
;; strips everything behind the paywall (beats/data/timeline), keeping
;; foreword + headline + table of contents — that and only that may go
;; to public surfaces.

(require racket/string
         racket/list
         racket/format
         racket/file
         racket/port
         racket/match
         json
         "util.rkt"
         "score.rkt"
         "store.rkt"
         "llm.rkt")

(provide BEATS
         (struct-out candidate)
         week-candidates
         build-issue
         locked-view
         write-issue!
         extract-json-any
         selection-prompt
         write-prompt
         foreword-prompt
         page-shell
         journal-css
         esc
         render-full-html
         render-locked-html
         locked-view
         default-provider
         mock-provider
         journal-budget)

;; ---- config ---------------------------------------------------------------

(define BEATS
  (list "政策与法规" "智能化" "三电与供应链" "整车与市场"))

;; Hard cap on LLM calls per issue (熔断): selection 1 + writing ≤14 +
;; foreword 1 fits well inside the default.
(define (journal-budget)
  (or (string->number (or (getenv "PITBOARD_JOURNAL_BUDGET") "")) 24))

;; ---- providers ------------------------------------------------------------

(define (default-provider prompt)
  (unless (llm-available?)
    (error 'journal
           "no LLM configured (PITBOARD_LLM_API_KEY) — journal is fail-closed, refusing rules-only issues"))
  (chat-completions prompt))

;; Deterministic demo provider: exercises the pipeline end to end with
;; no network.  Every string it produces is prefixed 【演示】 so demo
;; output can never be mistaken for editorial content.
(define (mock-provider prompt)
  (define tail
    (let ([m (regexp-match #px"输入：([\\s\\S]*)" prompt)])
      (if m (cadr m) "")))
  (define input (extract-json-any tail))
  (cond
    [(string-contains? prompt "热点候选事件")
     (define items (if (list? input) input '()))
     (jsexpr->string
      (hasheq 'headline 0
              'items (for/list ([e (in-list items)] [i (in-naturals)])
                       (hasheq 'i (hash-ref e 'i i)
                               'beat (list-ref BEATS (modulo i (length BEATS)))
                               'include (< i 5)))))]
    [(string-contains? prompt "热点事件的聚合信息")
     (define t (if (hash? input) (hash-ref input 'title "") ""))
     (jsexpr->string
      (hasheq 'beat (if (hash? input) (hash-ref input 'beat (last BEATS)) (last BEATS))
              'title (string-append "【演示】" t)
              'summary (string-append "（演示条目）" (truncate-s t 60) "。")
              'why "（演示）占位判断，用于流水线自检。"
              'numbers '()))]
    [(string-contains? prompt "卷首按语")
     (jsexpr->string
      (hasheq 'foreword "（演示卷首）本期条目为流水线自检内容，不代表真实编辑判断，不得发布。"))]
    [else (jsexpr->string (hasheq))]))

;; ---- llm plumbing ---------------------------------------------------------

(struct journal-ctx (provider remaining) #:mutable)

(define (ask! ctx prompt)
  (define r (journal-ctx-remaining ctx))
  (when (<= r 0)
    (error 'journal "LLM call budget exhausted (熔断)"))
  (set-journal-ctx-remaining! ctx (sub1 r))
  (define content ((journal-ctx-provider ctx) prompt))
  (unless (and (string? content) (non-empty-string? content))
    (error 'journal "LLM returned no content"))
  content)

;; LLM replies embed one JSON value in prose.  read-json tolerates
;; trailing garbage, so a greedy {...} match would happily "parse" the
;; first element of an array — dispatch on the first non-space char
;; instead, then fall through to the other shape if parsing fails.
(define (extract-json-any content)
  (define (try pat)
    (define m (regexp-match pat content))
    (and m
         (with-handlers ([exn:fail? (λ (_) #f)])
           (with-input-from-string (car m) read-json))))
  (define trimmed (string-trim (or content "")))
  (define object-first?
    (not (and (non-empty-string? trimmed)
              (equal? (string-ref trimmed 0) #\[))))
  (if object-first?
      (or (try #px"\\{[\\s\\S]*\\}") (try #px"\\[[\\s\\S]*\\]"))
      (or (try #px"\\[[\\s\\S]*\\]") (try #px"\\{[\\s\\S]*\\}"))))

;; ---- candidates: cross-day merge (rules only, zero tokens) ----------------

(struct candidate (title summary items domains dates heat) #:transparent #:mutable)

(define (dates-between dir from to)
  (define f (parse-date-any from))
  (define t (parse-date-any to))
  (unless (and f t) (error 'journal "bad date range: ~a .. ~a" from to))
  (reverse
   (for/list ([d (in-list (list-dates dir))]
              #:do [(define s (parse-date-any d))]
              #:when (and s (>= s f) (<= s t)))
     d)))

(define (cluster->candidate c date)
  (define items (hash-ref c 'items '()))
  (candidate (hash-ref c 'title "")
             (hash-ref c 'summary "")
             items
             (length (remove-duplicates
                      (map (λ (it) (hash-ref it 'source_url "")) items)))
             (list date)
             (let ([v (hash-ref c 'heat "0")])
               (cond [(real? v) v]
                     [(string? v) (or (string->number v) 0)]
                     [else 0]))))

(define (merge-into! base cand)
  (define seen
    (for/hash ([it (in-list (candidate-items base))])
      (values (hash-ref it 'url "") #t)))
  (set-candidate-items! base
                        (append (candidate-items base)
                                (for/list ([it (in-list (candidate-items cand))]
                                           #:unless (hash-has-key? seen (hash-ref it 'url "")))
                                  it)))
  (set-candidate-dates! base
                        (sort (remove-duplicates
                               (append (candidate-dates base) (candidate-dates cand)))
                              string<?))
  (set-candidate-domains! base
                          (length (remove-duplicates
                                   (map (λ (it) (hash-ref it 'source_url ""))
                                        (candidate-items base)))))
  (when (> (candidate-heat cand) (candidate-heat base))
    (set-candidate-title! base (candidate-title cand))
    (set-candidate-summary! base (candidate-summary cand))
    (set-candidate-heat! base (candidate-heat cand))))

;; Flatten the range's top daily clusters into deduped event candidates.
;; Same greedy join as cluster-items, at event granularity and the same
;; 0.55 threshold as annotate-progress!.  News clusters only — the
;; journal's beats are market/policy beats (papers/github stay on the
;; free digest).
(define (week-candidates dir from to
                         #:per-day [per-day 10]
                         #:cap [cap 40]
                         #:merge-threshold [th 0.55])
  (define raw
    (for*/list ([d (in-list (dates-between dir from to))]
                [dg (in-value (load-digest dir d))]
                #:when dg
                [c (in-list (hash-ref dg 'clusters '()))]
                #:when (let ([items (hash-ref c 'items '())])
                         (and (pair? items)
                              (equal? (hash-ref (car items) 'kind "news") "news"))))
      (cluster->candidate c d)))
  (define sorted (sort raw > #:key candidate-heat #:cache-keys? #t))
  (define groups '())  ; (cons token-set candidate)
  (for ([cand (in-list sorted)])
    (define toks (tokenize (string-append (candidate-title cand) " "
                                          (candidate-summary cand))))
    (define-values (best bs)
      (for/fold ([best #f] [bsim 0.0]) ([g (in-list groups)])
        (define s (jaccard toks (car g)))
        (if (> s bsim) (values g s) (values best bsim))))
    (if (and best (>= bs th))
        (merge-into! (cdr best) cand)
        (set! groups (cons (cons toks cand) groups))))
  (define merged (map cdr groups))
  (take (sort merged > #:key candidate-heat #:cache-keys? #t)
        (min cap (length merged))))

;; ---- LLM passes -----------------------------------------------------------

(define (selection-prompt input-json)
  (string-append
   "你是汽车行业周刊的主编。下面是过去一周的热点候选事件（JSON 数组，i 为编号）。"
   "从中选出真正值得进本周刊的事件：政策法规、定点与供应、智驾与技术路线、销量与市场格局优先；"
   "软文、促销、花边一律排除。最多选 14 条，宁缺毋滥；并选出一条本周头条（最重要、从业者必须知道的那条）。"
   "每条标注条线，只能是：政策与法规 / 智能化 / 三电与供应链 / 整车与市场。"
   "只输出一个 JSON 对象：{\"headline\": <头条编号>, \"items\": [{\"i\": <编号>, \"beat\": \"<条线>\", \"include\": true|false}]}，不要任何其他文字。\n"
   "输入：" input-json))

(define (write-prompt input-json)
  (string-append
   "你是汽车行业周刊的编辑。下面是一个热点事件的聚合信息（JSON）。为周刊写一条目，要求：\n"
   "- beat：条线，回显或按事件实际内容修正（只能是 政策与法规 / 智能化 / 三电与供应链 / 整车与市场）\n"
   "- title：重写为期刊条目标题，不超过 30 字，专有名词保留英文；原文没有提到的公司或结论不得出现\n"
   "- summary：答案先行，2-3 句，不超过 120 字，只依据给定信息\n"
   "- why：一句话（不超过 60 字）说明这条为什么值得从业者关注\n"
   "- numbers：若事件含具体数字（销量/价格/渗透率/份额等），抽 0-2 条 "
   "[{\"label\": \"指标\", \"value\": \"数值\", \"delta\": \"环比或同比，没有留空\", \"note\": \"口径备注\"}]，没有就空数组\n"
   "只输出一个 JSON 对象，不要任何其他文字。\n"
   "输入：" input-json))

(define (foreword-prompt input-json)
  (string-append
   "你是汽车行业周刊的主编。根据本周入选条目（JSON 数组，含 title 与 beat），写一段卷首按语："
   "2-3 句，有立场有判断，不超过 120 字，不要罗列条目名。"
   "只输出一个 JSON 对象：{\"foreword\": \"...\"}，不要任何其他文字。\n"
   "输入：" input-json))

(define (normalize-beat b)
  (if (and (string? b) (member b BEATS)) b (last BEATS)))

(define (select-events! ctx cands)
  (define input
    (for/list ([c (in-list cands)] [i (in-naturals)])
      (hasheq 'i i
              'title (truncate-s (candidate-title c) 80)
              'summary (truncate-s (candidate-summary c) 160)
              'domains (candidate-domains c)
              'dates (candidate-dates c))))
  (define parsed (extract-json-any (ask! ctx (selection-prompt (jsexpr->string input)))))
  (unless (hash? parsed) (error 'journal "selection pass returned unparseable output"))
  (define chosen
    (for/list ([e (in-list (hash-ref parsed 'items '()))]
               #:when (and (hash? e) (equal? (hash-ref e 'include #f) #t))
               #:do [(define i (hash-ref e 'i #f))]
               #:when (and (integer? i) (>= i 0) (< i (length cands))))
      (cons i (normalize-beat (hash-ref e 'beat #f)))))
  (define headline-i (hash-ref parsed 'headline #f))
  (define hi
    (if (and (integer? headline-i)
             (>= headline-i 0)
             (< headline-i (length cands))
             (assq headline-i chosen))
        headline-i
        (and (pair? chosen) (car (car chosen)))))
  (values hi chosen))

;; Mechanical backstop for 宁缺毋滥: dedupe, cap 14 total and 4 per beat.
(define (cap-selected chosen)
  (define seen (make-hash))
  (define per-beat (make-hash))
  (let loop ([rest chosen] [acc '()])
    (cond [(or (null? rest) (>= (length acc) 14)) (reverse acc)]
          [else
           (define i (car (car rest)))
           (define b (cdr (car rest)))
           (cond [(or (hash-has-key? seen i)
                      (>= (hash-ref per-beat b 0) 4))
                  (loop (cdr rest) acc)]
                 [else
                  (hash-set! seen i #t)
                  (hash-set! per-beat b (add1 (hash-ref per-beat b 0)))
                  (loop (cdr rest) (cons (car rest) acc))])])))

(define (write-event! ctx cand idx beat)
  (define items (candidate-items cand))
  (define members
    (for/list ([it (in-list (take items (min 6 (length items))))])
      (truncate-s (hash-ref it 'title "") 60)))
  (define input
    (hasheq 'i idx
            'beat beat
            'title (truncate-s (candidate-title cand) 80)
            'summary (truncate-s (candidate-summary cand) 200)
            'domains (candidate-domains cand)
            'dates (candidate-dates cand)
            'members members))
  (define parsed (extract-json-any (ask! ctx (write-prompt (jsexpr->string input)))))
  (unless (hash? parsed) (error 'journal "write pass returned unparseable output for: ~a" (candidate-title cand)))
  (define numbers
    (for/list ([nb (in-list (hash-ref parsed 'numbers '()))]
               #:when (and (hash? nb)
                           (string? (hash-ref nb 'label #f))
                           (string? (hash-ref nb 'value #f))))
      (hasheq 'label (hash-ref nb 'label)
              'value (hash-ref nb 'value)
              'delta (let ([d (hash-ref nb 'delta "")]) (if (string? d) d ""))
              'note (let ([n (hash-ref nb 'note "")]) (if (string? n) n "")))))
  (hasheq 'i idx
          'beat (normalize-beat (hash-ref parsed 'beat beat))
          'title (let ([t (hash-ref parsed 'title "")])
                   (if (and (string? t) (non-empty-string? t)) t (candidate-title cand)))
          'summary (let ([s (hash-ref parsed 'summary "")])
                     (if (and (string? s) (non-empty-string? s)) s (candidate-summary cand)))
          'why (let ([w (hash-ref parsed 'why "")]) (if (string? w) w ""))
          'numbers (take numbers (min 2 (length numbers)))
          'domains (candidate-domains cand)
          'url (if (pair? items) (hash-ref (car items) 'url "") "")
          'first-date (if (pair? (candidate-dates cand)) (car (candidate-dates cand)) "")))

(define (write-foreword! ctx written)
  (define input
    (for/list ([w (in-list written)])
      (hasheq 'title (hash-ref w 'title) 'beat (hash-ref w 'beat))))
  (define parsed (extract-json-any (ask! ctx (foreword-prompt (jsexpr->string input)))))
  (unless (and (hash? parsed) (string? (hash-ref parsed 'foreword #f)))
    (error 'journal "foreword pass returned unparseable output"))
  (hash-ref parsed 'foreword))

;; ---- assembly -------------------------------------------------------------

(define (event-face w)
  (hasheq 'title (hash-ref w 'title)
          'summary (hash-ref w 'summary)
          'why (hash-ref w 'why)
          'beat (hash-ref w 'beat)
          'domains (hash-ref w 'domains)
          'url (hash-ref w 'url)))

(define (assemble-issue n date-str foreword headline-idx written)
  (define headline-c
    (findf (λ (w) (equal? (hash-ref w 'i) headline-idx)) written))
  (unless headline-c (error 'journal "headline event missing from written set"))
  (define others (filter (λ (w) (not (equal? (hash-ref w 'i) headline-idx))) written))
  (define beats
    (filter values
            (for/list ([b (in-list BEATS)])
              (define items
                (for/list ([w (in-list others)]
                           #:when (equal? (hash-ref w 'beat) b))
                  (event-face w)))
              (and (pair? items) (hasheq 'name b 'items items)))))
  ;; data: dedupe by label, first (hottest) wins, cap 6
  (define data
    (let loop ([rest (append-map (λ (w) (hash-ref w 'numbers)) written)]
               [seen (hash)]
               [acc '()])
      (cond [(or (null? rest) (>= (length acc) 6))
             (reverse acc)]
            [else
             (define nb (car rest))
             (define key (string-downcase (hash-ref nb 'label "")))
             (if (hash-has-key? seen key)
                 (loop (cdr rest) seen acc)
                 (loop (cdr rest) (hash-set seen key #t) (cons nb acc)))])))
  (define timeline
    (for/list ([w (in-list (sort written string<? #:key (λ (w) (hash-ref w 'first-date)) #:cache-keys? #t))])
      (hasheq 'date (let ([d (hash-ref w 'first-date "")])
                      (if (>= (string-length d) 10) (substring d 5) d))
              'fact (hash-ref w 'title))))
  (hasheq 'number n
          'date date-str
          'generated_at (iso-datetime)
          'foreword foreword
          'headline (event-face headline-c)
          'beats beats
          'data data
          'timeline timeline
          'counts (hasheq 'events (length written)
                          'items (add1 (length others)))))

(define (build-issue cands
                     #:issue-number n
                     #:date date-str
                     #:provider [provider default-provider]
                     #:budget [budget (journal-budget)])
  (when (null? cands) (error 'journal "no candidates in range ~a" date-str))
  (define ctx (journal-ctx provider budget))
  (define-values (headline-idx selected) (select-events! ctx cands))
  (unless headline-idx (error 'journal "LLM selected nothing from ~a candidates" (length cands)))
  (define selected2 (cap-selected selected))
  (when (null? selected2) (error 'journal "LLM selection came back empty"))
  (define written
    (for/list ([p (in-list selected2)])
      (write-event! ctx (list-ref cands (car p)) (car p) (cdr p))))
  (define foreword (write-foreword! ctx written))
  (assemble-issue n date-str foreword headline-idx written))

;; ---- paywall boundary -----------------------------------------------------

;; The only part of an issue that may reach public surfaces: foreword +
;; headline (both free by design) + the table of contents with counts.
(define (locked-view issue)
  (hasheq 'number (hash-ref issue 'number)
          'date (hash-ref issue 'date)
          'generated_at (hash-ref issue 'generated_at)
          'foreword (hash-ref issue 'foreword)
          'headline (hash-ref issue 'headline)
          'toc (for/list ([b (in-list (hash-ref issue 'beats))])
                 (hasheq 'name (hash-ref b 'name)
                         'count (length (hash-ref b 'items))))
          'counts (hash-ref issue 'counts)))

;; ---- rendering (aligns with design/prototype/journal*.html) ---------------

(define (esc s)
  (regexp-replace* #px"<"
    (regexp-replace* #px">"
      (regexp-replace* #px"&" s "&amp;")
      "&gt;")
    "&lt;"))

(define journal-css
  (string-append
   ":root{--bg:#F9F7F4;--surface:#FFFFFF;--text:#1C1917;--text-2:#57534E;--text-3:#A8A29E;"
   "--accent:#D97757;--accent-hover:#C25F3D;--border:#E7E5E4;--border-strong:#D6D3D1;"
   "--serif:\"Songti SC\",\"Noto Serif SC\",\"SimSun\",Georgia,serif;"
   "--ui:-apple-system,BlinkMacSystemFont,\"Segoe UI\",\"PingFang SC\",\"Noto Sans SC\",sans-serif}"
   "@media(prefers-color-scheme:dark){:root{--bg:#171412;--surface:#231F1D;--text:#E7E5E4;"
   "--text-2:#A8A29E;--text-3:#78716C;--accent:#D4915C;--border:#3D3835;--border-strong:#4A443F}}"
   "*{margin:0;padding:0;box-sizing:border-box}"
   "body{font-family:var(--ui);font-size:15px;line-height:1.5;color:var(--text);"
   "background:var(--bg);-webkit-font-smoothing:antialiased}"
   "a{color:var(--accent);text-decoration:none}a:hover{color:var(--accent-hover)}"
   ".wrap{max-width:820px;margin:0 auto;padding:0 24px}"
   ".masthead-rule{border-top:2px solid var(--text);border-bottom:1px solid var(--text);height:6px;margin-top:40px}"
   ".masthead-title{font-family:var(--serif);font-weight:600;letter-spacing:.02em;font-size:34px;"
   "text-align:center;padding:28px 0 4px}"
   ".masthead-title .en{font-family:Georgia,\"Times New Roman\",serif;letter-spacing:.14em;font-size:.92em}"
   ".masthead-issue{display:flex;justify-content:space-between;align-items:baseline;font-size:13px;"
   "color:var(--text-2);padding:14px 0 12px;border-bottom:1px solid var(--border-strong);"
   "font-variant-numeric:tabular-nums}"
   ".masthead-issue .tag{color:var(--text-3);letter-spacing:.08em;font-size:12px}"
   ".prose{font-family:var(--serif);font-size:17px;line-height:1.85}"
   ".prose p{margin-bottom:1.1em;text-align:justify}"
   ".num{font-variant-numeric:tabular-nums}"
   ".foreword{margin-top:40px;padding-left:20px;border-left:2px solid var(--accent)}"
   ".foreword-by{font-size:12px;color:var(--text-3);margin-top:12px;letter-spacing:.06em}"
   ".headline{margin-top:40px}"
   ".headline-title{font-family:var(--serif);font-weight:600;font-size:26px;line-height:1.45;margin-bottom:14px}"
   ".headline-meta{font-size:12px;color:var(--text-3);padding-bottom:16px;border-bottom:1px solid var(--border);"
   "margin-bottom:24px;font-variant-numeric:tabular-nums}"
   ".headline-meta a{color:var(--text-2);border-bottom:1px solid var(--border)}"
   ".section{margin-top:48px}"
   ".section-head{display:flex;align-items:baseline;justify-content:space-between;padding-bottom:10px;"
   "border-bottom:1px solid var(--border-strong);margin-bottom:24px}"
   ".section-title{font-size:13px;font-weight:600;letter-spacing:.12em;color:var(--text-2)}"
   ".section-count{font-size:12px;color:var(--text-3);font-variant-numeric:tabular-nums}"
   ".item{margin-bottom:32px}.item:last-child{margin-bottom:0}"
   ".item-title{font-family:var(--serif);font-weight:600;font-size:20px;line-height:1.5;margin-bottom:10px}"
   ".item-title a{color:inherit}.item-title a:hover{color:var(--accent)}"
   ".item-summary{font-family:var(--serif);font-size:15px;line-height:1.85;margin-bottom:8px;text-align:justify}"
   ".item-why{font-size:13px;line-height:1.7;color:var(--text-2);margin-bottom:6px}"
   ".item-why .why-label{color:var(--text-3);letter-spacing:.06em;margin-right:6px}"
   ".item-meta{font-size:12px;color:var(--text-3)}"
   ".item-meta a{color:var(--text-2);border-bottom:1px solid var(--border)}"
   ".subscribe-card{margin:48px 0;padding:32px;text-align:center;background:var(--surface);"
   "border:1px solid var(--border);border-radius:12px}"
   ".subscribe-card h3{font-family:var(--serif);font-size:20px;font-weight:600;margin-bottom:8px}"
   ".price{font-size:30px;font-weight:600;font-variant-numeric:tabular-nums}"
   ".price .unit{font-size:14px;font-weight:400;color:var(--text-2)}"
   ".subscribe-card .note{font-size:13px;color:var(--text-2);margin:6px 0 20px}"
   ".btn-primary{display:inline-block;padding:10px 32px;border-radius:8px;background:var(--accent);"
   "color:#fff;font-weight:600;font-size:15px}"
   ".btn-primary:hover{background:var(--accent-hover);color:#fff}"
   ".subscribe-flow{display:flex;justify-content:center;gap:8px;margin-top:20px;font-size:12px;color:var(--text-3)}"
   ".subscribe-flow .step{padding:2px 10px;border:1px solid var(--border);border-radius:99px}"
   ".toc-line{display:flex;justify-content:space-between;align-items:baseline;padding:12px 0;"
   "border-bottom:1px solid var(--border);font-size:14px}"
   ".toc-line .name{font-family:var(--serif);font-size:15px;color:var(--text-2)}"
   ".toc-line .count{color:var(--text-3);font-size:12px;font-variant-numeric:tabular-nums}"
   ".data-table{width:100%;border-collapse:collapse;font-size:14px}"
   ".data-table th{text-align:left;font-weight:500;font-size:12px;color:var(--text-3);"
   "letter-spacing:.06em;padding:8px 12px 8px 0;border-bottom:1px solid var(--border-strong)}"
   ".data-table td{padding:10px 12px 10px 0;border-bottom:1px solid var(--border);"
   "font-variant-numeric:tabular-nums}"
   ".data-table td:first-child{font-family:var(--serif);font-size:15px}"
   ".timeline{list-style:none}"
   ".timeline li{display:flex;gap:16px;padding:10px 0;border-bottom:1px solid var(--border);font-size:14px}"
   ".timeline li:last-child{border-bottom:none}"
   ".timeline .date{color:var(--text-3);flex-shrink:0;font-variant-numeric:tabular-nums}"
   ".timeline .fact{font-family:var(--serif);font-size:15px}"
   ".issue-nav{display:flex;justify-content:space-between;align-items:baseline;margin-top:56px;"
   "padding-top:16px;border-top:1px solid var(--border);font-size:13px}"
   ".issue-nav a{color:var(--text-2);border-bottom:1px solid var(--border)}"
   ".site-footer{margin-top:64px;padding:24px 0 48px;border-top:1px solid var(--border);"
   "font-size:12px;color:var(--text-3);text-align:center}"
   ".site-footer a{color:var(--text-2);border-bottom:1px solid var(--border)}"
   ".site-footer p{margin-bottom:6px}"
   ".faq{margin-top:24px}"
   ".faq dt{font-family:var(--serif);font-weight:600;font-size:16px;margin-bottom:6px}"
   ".faq dd{font-size:14px;line-height:1.75;color:var(--text-2);margin-bottom:20px;padding-left:0}"
   ".value-item{margin-bottom:32px}"
   ".value-item:last-child{margin-bottom:0}"
   "@media print{body{background:#fff}.site-footer,.issue-nav,.subscribe-card,.btn-primary{display:none}"
   ".masthead-rule{border-color:#1C1917}.prose,.item-summary,.timeline .fact{font-size:12pt}}}"))

(define (page-shell title body)
  (format
   (string-append
    "<!DOCTYPE html>\n<html lang=\"zh-CN\">\n<head>\n<meta charset=\"UTF-8\">\n"
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
    "<title>~a</title>\n<style>~a</style>\n</head>\n<body>\n<div class=\"wrap\">\n~a\n</div>\n</body>\n</html>\n")
   title journal-css body))

(define (render-masthead number date-str)
  (define parts (string-split date-str "-"))
  (define (p i) (if (> (length parts) i) (list-ref parts i) ""))
  (format
   (string-append
    "<header>\n<div class=\"masthead-rule\"></div>\n"
    "<h1 class=\"masthead-title\"><a href=\"/\" style=\"color:inherit\"><span class=\"en\">PITBOARD</span> 周刊</a></h1>\n"
    "<div class=\"masthead-issue\"><span>第 ~a 期 · ~a 年 ~a 月 ~a 日</span>"
    "<span class=\"tag\">汽车行业的一周</span></div>\n</header>")
   number (p 0) (p 1) (p 2)))

(define (render-headline h)
  (format
   (string-append
    "<article class=\"headline\">\n<h2 class=\"headline-title\">~a</h2>\n"
    "<div class=\"headline-meta\">~a · 综合 ~a 家信源 · <a href=\"~a\">原文链接</a></div>\n"
    "<div class=\"prose\"><p><strong>答案先行</strong>：~a</p><p><strong>为什么重要</strong>：~a</p></div>\n</article>")
   (esc (hash-ref h 'title ""))
   (esc (or (hash-ref h 'beat #f) ""))
   (hash-ref h 'domains 0)
   (esc (hash-ref h 'url "#"))
   (esc (hash-ref h 'summary ""))
   (esc (hash-ref h 'why ""))))

(define (render-foreword issue)
  (define f (hash-ref issue 'foreword ""))
  (if (non-empty-string? f)
      (format "<section class=\"foreword prose\"><p>~a</p><div class=\"foreword-by\">主编 · ~a</div></section>"
              (esc f) (esc (hash-ref issue 'date "")))
      ""))

(define (render-beats issue)
  (string-join
   (for/list ([b (in-list (hash-ref issue 'beats '()))])
     (format
      (string-append
       "<section class=\"section\">\n<div class=\"section-head\">"
       "<span class=\"section-title\">~a</span><span class=\"section-count\">~a 条</span></div>\n~a\n</section>")
      (esc (hash-ref b 'name))
      (length (hash-ref b 'items))
      (string-join
       (for/list ([it (in-list (hash-ref b 'items))])
         (format
          (string-append
           "<div class=\"item\">\n<h3 class=\"item-title\"><a href=\"~a\">~a</a></h3>\n"
           "<p class=\"item-summary\">~a</p>\n"
           "<p class=\"item-why\"><span class=\"why-label\">为什么入选</span>~a</p>\n"
           "<div class=\"item-meta\"><a href=\"~a\">原文</a> · 综合 ~a 家信源</div>\n</div>")
          (esc (hash-ref it 'url "#"))
          (esc (hash-ref it 'title ""))
          (esc (hash-ref it 'summary ""))
          (esc (hash-ref it 'why ""))
          (esc (hash-ref it 'url "#"))
          (hash-ref it 'domains 0)))
       "\n")))
   "\n"))

(define (render-data issue)
  (define rows (hash-ref issue 'data '()))
  (if (null? rows)
      ""
      (format
       (string-append
        "<section class=\"section\">\n<div class=\"section-head\">"
        "<span class=\"section-title\">数据版</span><span class=\"section-count\">~a 项</span></div>\n"
        "<table class=\"data-table\"><thead><tr><th>指标</th><th>本期</th><th>变化</th><th>备注</th></tr></thead><tbody>~a</tbody></table>\n</section>")
       (length rows)
       (string-join
        (for/list ([nb (in-list rows)])
          (format "<tr><td>~a</td><td class=\"num\">~a</td><td class=\"num\">~a</td><td>~a</td></tr>"
                  (esc (hash-ref nb 'label ""))
                  (esc (hash-ref nb 'value ""))
                  (esc (hash-ref nb 'delta ""))
                  (esc (hash-ref nb 'note ""))))
        "\n"))))

(define (render-timeline issue)
  (define rows (hash-ref issue 'timeline '()))
  (if (null? rows)
      ""
      (format
       (string-append
        "<section class=\"section\">\n<div class=\"section-head\">"
        "<span class=\"section-title\">大事记</span><span class=\"section-count\">~a 条</span></div>\n"
        "<ul class=\"timeline\">~a</ul>\n</section>")
       (length rows)
       (string-join
        (for/list ([r (in-list rows)])
          (format "<li><span class=\"date num\">~a</span><span class=\"fact\">~a</span></li>"
                  (esc (hash-ref r 'date ""))
                  (esc (hash-ref r 'fact ""))))
        "\n"))))

(define subscribe-card-html
  (string-append
   "<div class=\"subscribe-card\">\n<h3>订阅后继续阅读</h3>\n"
   "<div class=\"price num\">¥199<span class=\"unit\"> / 年 · 52 期</span></div>\n"
   "<p class=\"note\">每周出刊通知直达 · 网页全文与往期归档 · 未满 30 天可全额退款</p>\n"
   "<a class=\"btn-primary\" href=\"#\">订阅本周刊</a>\n"
   "<div class=\"subscribe-flow\"><span class=\"step\">① 支付</span>"
   "<span class=\"step\">② 填写邮箱</span><span class=\"step\">③ 邮件确认</span></div>\n</div>"))

(define (render-footer)
  (string-append
   "<footer class=\"site-footer\">\n"
   "<p>PITBOARD 周刊 · 汽车行业的一周 · <a href=\"#\">订阅</a> · <a href=\"#\">往期归档</a> · <a href=\"#\">退订</a> · <a href=\"#\">更正声明</a></p>\n"
   "<p>所有条目附原文链接；AI 汇编 + 主编复核，发现错误当期更正、下期刊登。</p>\n</footer>"))

(define (render-locked-html issue)
  (define locked (if (hash-has-key? issue 'toc) issue (locked-view issue)))
  (page-shell
   (format "PITBOARD 周刊 · 第 ~a 期" (hash-ref locked 'number))
   (string-append
    (render-masthead (hash-ref locked 'number) (hash-ref locked 'date))
    (render-foreword locked)
    (format "<article class=\"headline\">~a</article>" (render-headline (hash-ref locked 'headline)))
    subscribe-card-html
    (format
     "<section class=\"section\">\n<div class=\"section-head\"><span class=\"section-title\">本期目录</span><span class=\"section-count\">订阅可见 ~a 条</span></div>\n~a\n</section>"
     (hash-ref (hash-ref locked 'counts) 'items 0)
     (string-join
      (for/list ([t (in-list (hash-ref locked 'toc '()))])
        (format
         "<div class=\"toc-line\"><span class=\"name\">~a</span><span class=\"count\">~a 条 · 订阅可见</span></div>"
         (esc (hash-ref t 'name))
         (hash-ref t 'count 0)))
      "\n"))
    (render-footer))))

(define (render-full-html issue)
  (page-shell
   (format "PITBOARD 周刊 · 第 ~a 期" (hash-ref issue 'number))
   (string-append
    (render-masthead (hash-ref issue 'number) (hash-ref issue 'date))
    (render-foreword issue)
    (format "<article class=\"headline\">~a</article>" (render-headline (hash-ref issue 'headline)))
    (render-beats issue)
    (render-data issue)
    (render-timeline issue)
    "<nav class=\"issue-nav\"><a href=\"#\">← 往期归档</a><a href=\"#\">订阅 →</a></nav>"
    (render-footer))))

;; ---- output ---------------------------------------------------------------

(define (write-json-file path j)
  (with-output-to-file path (λ () (write-json j)) #:exists 'replace))

;; Writes the full issue and its locked public view.  Returns the paths
;; written, so callers (CLI/CI) decide what goes where — the full issue
;; must never reach a public surface.
(define (write-issue! out-dir issue)
  (make-directory* out-dir)
  (define locked (locked-view issue))
  (define p-issue (build-path out-dir "issue.json"))
  (define p-locked-json (build-path out-dir "locked.json"))
  (define p-full (build-path out-dir "journal.html"))
  (define p-locked (build-path out-dir "journal-locked.html"))
  (write-json-file p-issue issue)
  (write-json-file p-locked-json locked)
  (with-output-to-file p-full (λ () (write-string (render-full-html issue))) #:exists 'replace)
  (with-output-to-file p-locked (λ () (write-string (render-locked-html locked))) #:exists 'replace)
  (list p-issue p-locked-json p-full p-locked))
