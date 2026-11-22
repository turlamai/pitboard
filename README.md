# pitboard

English | [中文](README.zh-CN.md)

![platform](https://img.shields.io/badge/platform-Web%20%7C%20Docker%20%7C%20Self--hosted-lightgrey) ![built with](https://img.shields.io/badge/built%20with-Racket-9f1d20) [![CI](https://github.com/turlamai/pitboard/actions/workflows/ci.yml/badge.svg)](https://github.com/turlamai/pitboard/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**PITBOARD Weekly**: a paid subscription journal for the automotive
industry. Every Saturday at 08:00, the week's events — curated,
written, and bound — land in subscribers' inboxes.

An AI pipeline does the heavy lifting; the editor-in-chief gates every
issue before it ships. What you pay for is not information (information
is free everywhere) but three things: **curation** (every item carries
a one-line "why this made the cut"), **craft** (newspaper-grade
typography, one issue reads in ~15 minutes), and **certainty** (every
Saturday 08:00 — the punctuality itself is part of the product).

## How it works

```
Daily (zero tokens, internal — never visible to readers):
  RSS/page-list/GitHub/arXiv sources → fetch → normalize → cluster
  (Jaccard) → heat (independent sources × time decay) → data/YYYY-MM-DD.json

Saturdays (the only LLM stage):
  one week of events → selection (headline + beats) → per-event writing
  (answer-first summary + why-this-cut + number extraction) → foreword
  → data table & timeline (rule-compiled) → full-issue artifact →
  editorial review (upload at /console → publish)

The commercial loop:
  landing page → Pancake checkout → email double opt-in → notify email
  (login-free direct link) → full text on the web → expiry reminder →
  renewal (extends from the existing expiry)
```

- **Cost structure**: the rules engine compresses raw material daily at
  zero token cost; the LLM runs only in the weekly assembly — a few RMB
  per issue. One annual subscription covers the year's LLM cost.
- **Fail-closed**: without an LLM key no issue ships — a paid product
  never publishes rules-only content. A per-issue call budget caps
  runaway passes.
- **The paywall boundary is enforced in code**: the public surface is
  foreword + headline + table of contents only; paid body text never
  enters the public repo (the full issue lives in an Actions artifact;
  publishing goes through the editor's upload).

## Reading

[Subscribe and read](https://pitboard.jrtx.site/) — the current issue's
foreword and headline are free on the web; everything else is for
subscribers.

## Quick start (self-hosting)

Requires Racket 8.14+ (development uses 9.3) and the
[commerce](https://github.com/turlamai/commerce) package (webhook
signature verification).

```sh
git clone https://github.com/turlamai/pitboard.git && cd pitboard
raco pkg install --auto --no-docs http-easy feed server-sent-events
raco pkg install --auto --no-docs https://github.com/turlamai/commerce.git
raco pkg install --auto --no-docs --link "$PWD"

# internal raw-material intake (daily, zero tokens)
raco pitboard fetch --data data --config config/sources.json

# weekly assembly (--mock self-checks the pipeline; real issues need an LLM key)
raco pitboard journal --issue 1 --mock

# serve the site (landing / journal / subscribe-pay flow / editorial console)
raco pitboard serve --port 8090 --data data

# curation quality eval (annotated samples)
raco pitboard eval --data data --samples eval/samples.json
```

Tests: `raco test -p pitboard`.

## Configuration

| Variable | Meaning |
| --- | --- |
| `PITBOARD_BASE_URL` | absolute site address (used in mail links) |
| `PITBOARD_ADMIN_PASSWORD` | editorial console password (`/console`; unset = login refused) |
| `PITBOARD_PAY_URL` | Pancake product checkout URL (unset = subscribe page says the channel is not live) |
| `PITBOARD_PAY_ENV` | `test` / `production` (webhook keyring) |
| `PITBOARD_LLM_API_KEY` | DeepSeek or any OpenAI-compatible key (unset = no issue, fail-closed) |
| `PITBOARD_LLM_BASE_URL` / `PITBOARD_LLM_MODEL` | default `https://api.deepseek.com` / `deepseek-chat` |
| `PITBOARD_JOURNAL_BUDGET` | hard cap on LLM calls per issue (default 24) |
| `PITBOARD_SMTP_HOST/PORT/USER/PASS` | Aliyun DirectMail smtpdm (unset = mail dry-run writing `data/mail-out/`) |
| `PITBOARD_MAIL_FROM` | sender address (must match the DirectMail sending domain) |

Webhook signature verification delegates to [commerce](https://github.com/turlamai/commerce)'s
canonical Pancake implementation (RSA, official keyrings).

## GitHub Actions

| Workflow | What it does |
| --- | --- |
| `ingest.yml` | daily 07:30 Beijing: internal raw-material intake, archives committed — no public surface |
| `journal.yml` | Saturdays 08:00 Beijing: full issue assembled — full version goes to an artifact for editorial review, the locked public view is committed under `data/journal/` |
| `ci.yml` | test gate |

## Self-hosting and deployment

One Racket process is the whole site: `raco pitboard serve` (pages +
subscribe-pay flow + editorial console); data is just the `data/`
directory, backup is a copy. Docker image in `Dockerfile`; the payment
callback needs public HTTPS (`PITBOARD_BASE_URL`).

## Layout

```
pitboard/            Racket package body (collection "pitboard")
  cli.rkt            raco pitboard entry point (fetch / journal / serve / eval)
  sources.rkt        source adapters (RSS / page lists / GitHub / arXiv) + hard timeouts
  score.rkt          tokenization, clustering, heat (internal intake, zero tokens)
  llm.rkt            OpenAI-compatible calls (chat-completions; optional daily polish)
  journal.rkt        weekly journal assembly (cross-day merge → LLM select/write/foreword → render)
  subs.rkt           subscriber/payment stores + token lifecycle
  pay.rkt            payment face (commerce Pancake verification + subscription projection)
  mail.rkt           SMTP-over-TLS delivery + dry-run + email templates
  site.rkt           journal site routes (landing / journal / subscribe flow / self-service / console)
  app.rkt            web-server application
  store.rkt          data/YYYY-MM-DD.json storage
  util.rkt           date/string/URL helpers (explicit UTC)
.github/workflows/   ingest (daily intake) / journal (weekly issue) / ci
config/              source configuration (your taste list goes here)
data/                daily event archive (Actions-committed) + locked issue views
design/              design contract and prototype (yinwang typography kernel × family HIG)
```

## License

MIT
