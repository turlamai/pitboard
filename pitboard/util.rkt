#lang racket/base

;; Shared helpers: strings, HTML cleanup, dates, URLs.

(require racket/string
         racket/list
         racket/format
         racket/match
         racket/date
         net/url
         (only-in feed parse-8601 parse-rfc822 parse-date-any))

(provide collapse-ws
         strip-html
         truncate-s
         iso-date
         iso-datetime
         parse-iso-utc
         parse-8601
         parse-rfc822
         parse-date-any
         slug-domain
         normalize-url
         str-starts?
         str-ends?
         jx)

;; ---- strings ------------------------------------------------------------

(define (collapse-ws s)
  (string-trim (string-normalize-spaces s)))

(define entity-map
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"")
    ("apos" . "'") ("nbsp" . " ") ("hellip" . "…") ("mdash" . "—")
    ("ndash" . "–") ("rsquo" . "’") ("lsquo" . "‘")
    ("ldquo" . "“") ("rdquo" . "”") ("copy" . "©") ("reg" . "®")))

(define (decode-entities s)
  (define (code->str n)
    (with-handlers ([exn:fail? (λ (_) "")])
      (string (integer->char n))))
  (collapse-ws
   (regexp-replace*
    #px"&[A-Za-z][A-Za-z0-9]*;"
    (regexp-replace*
     #px"&#([0-9]+);"
     (regexp-replace* #px"&#x([0-9A-Fa-f]+);" s (λ (_all hex) (code->str (string->number hex 16))))
     (λ (_all dec) (code->str (string->number dec 10))))
    (λ (all)
      (define name (substring all 1 (sub1 (string-length all))))
      (cond [(assoc name entity-map) => cdr] [else all])))))

(define (strip-html s)
  (unless (string? s) (set! s (or (and s (format "~a" s)) "")))
  (decode-entities
   (regexp-replace*
    #px"<[^>]*>"
    (regexp-replace*
     #px"(?i:<script[^>]*>[\\s\\S]*?</script>)"
     (regexp-replace* #px"(?i:<style[^>]*>[\\s\\S]*?</style>)" s " ")
     " ")
    " ")))

(define (truncate-s s [n 300])
  (cond [(<= (string-length s) n) s]
        [else (string-append (substring s 0 n) "…")]))

(define (str-starts? s p)
  (and (>= (string-length s) (string-length p))
       (equal? (substring s 0 (string-length p)) p)))

(define (str-ends? s p)
  (and (>= (string-length s) (string-length p))
       (equal? (substring s (- (string-length s) (string-length p))) p)))

;; #f and missing values must become 'null before write-json.
(define (jx v)
  (if (not v) 'null v))

;; ---- dates (UTC everywhere) ----------------------------------------------
;; Parsing lives in the feed package; util re-exports it (see require).

(define (pad2 n) (~r n #:min-width 2 #:pad-string "0"))

;; UTC civil date from epoch.  seconds->date's UTC flag proved
;; unreliable here (Racket 9.3 on darwin returns local time even with
;; #t), so civil time is computed from epoch arithmetic —
;; civil_from_days (proleptic Gregorian, Howard Hinnant).
(define (utc-civil secs)
  (define days0 (quotient secs 86400))
  (define rem (modulo secs 86400))
  (define hh (quotient rem 3600))
  (define mi (quotient (modulo rem 3600) 60))
  (define ss (modulo rem 60))
  (define z (+ days0 719468))
  (define era (quotient (- z (if (>= z 0) 0 146096)) 146097))
  (define doe (- z (* era 146097)))
  (define yoe (quotient (- doe (quotient doe 1460) (quotient doe 36524) (quotient doe 146096)) 365))
  (define y (+ yoe (* era 400)))
  (define doy (- doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100)))))
  (define mp (quotient (+ (* 5 doy) 2) 153))
  (define d (+ 1 (- doy (quotient (+ (* 153 mp) 2) 5))))
  (define mo (+ mp (if (< mp 10) 3 -9)))
  (define y2 (if (<= mo 2) (add1 y) y))
  (list y2 mo d hh mi ss))

(define (iso-date [secs (current-seconds)])
  (match-define (list y mo d _ _ _) (utc-civil secs))
  (format "~a-~a-~a" y (pad2 mo) (pad2 d)))

(define (iso-datetime [secs (current-seconds)])
  (match-define (list y mo d hh mi ss) (utc-civil secs))
  (format "~a-~a-~aT~a:~a:~aZ"
          y (pad2 mo) (pad2 d) (pad2 hh) (pad2 mi) (pad2 ss)))

;; Parse our own iso-datetime output ("YYYY-MM-DDTHH:MM:SSZ") as true
;; UTC — feed's 8601 parser shifts by the machine timezone, which broke
;; expiry math on any machine not in UTC.  days_from_civil (proleptic
;; Gregorian), then seconds.
(define (parse-iso-utc s)
  (define m
    (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z$"
                  (or s "")))
  (and m
       (let* ([y (string->number (list-ref m 1))]
              [mo (string->number (list-ref m 2))]
              [d (string->number (list-ref m 3))]
              [hh (string->number (list-ref m 4))]
              [mi (string->number (list-ref m 5))]
              [ss (string->number (list-ref m 6))]
              [yy (if (<= mo 2) (sub1 y) y)]
              [era (quotient yy 400)]
              [yoe (- yy (* 400 era))]
              [mp (+ mo (if (> mo 2) -3 9))]
              [doy (+ (quotient (+ (* 153 mp) 2) 5) (sub1 d))]
              [doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)]
              [days (- (+ (* era 146097) doe) 719468)])
         (+ (* days 86400) (* hh 3600) (* mi 60) ss))))

;; ---- urls -----------------------------------------------------------------

(define (slug-domain s)
  (with-handlers ([exn:fail? (λ (_) "unknown")])
    (define u (string->url s))
    (cond [(url-host u) => string-downcase]
          [else "unknown"])))

;; Drop tracking params and fragments so the same story from the same
;; outlet dedupes to one URL.  Query entries are (field . value) pairs.
(define (normalize-url s)
  (with-handlers ([exn:fail? (λ (_) s)])
    (define u (string->url s))
    (define (track? b)
      (define f (string-downcase (~a (car b))))
      (or (str-starts? f "utm_")
          (member f '("ref" "ref_src" "ref_url" "spm" "fbclid" "gclid"
                      "igshid" "guccounter" "cmpid" "sh"))))
    (define u2 (struct-copy url u
                            [query (for/list ([b (in-list (url-query u))]
                                              #:unless (track? b))
                                     b)]
                            [fragment #f]))
    (define s2 (url->string u2))
    (if (str-ends? s2 "/") (substring s2 0 (sub1 (string-length s2))) s2)))
