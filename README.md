# pitboard

**English** · [中文](README.zh-CN.md)

![platform](https://img.shields.io/badge/platform-Web%20%7C%20Docker%20%7C%20Self--hosted-lightgrey) ![built with](https://img.shields.io/badge/built%20with-Racket-9f1d20) [![CI](https://github.com/turlamai/pitboard/actions/workflows/ci.yml/badge.svg)](https://github.com/turlamai/pitboard/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

The daily hot-topics digest for the automotive industry: it pulls open-source
projects, industry news and cutting-edge papers together, clusters and
deduplicates them, scores hotness by independent source count with time decay,
and publishes one "pit board" every day. Full-stack Racket (web-server),
single-process self-hosting, no external database service.

Language: Racket

## Read first

[Read the automotive daily board](https://pitboard.jrtx.site/) · [Reading, privacy and corrections](docs/READING.md)

Reading is free and needs no account. Ranking/deduplication can merge unrelated stories or miss developments. Optional AI-generated headlines/summaries may be wrong; independent sources, not a score, establish a claim. Scheduled publication can fail; an old edition is not today’s news.

## How it works

```
RSS feeds    ─┐
GitHub topics ┼→ fetch → normalize → cluster (Jaccard / CJK+EN tokenization) → hotness (independent sources × time decay) → publish
arXiv papers ─┘                                                              ↓
                                            site / RSS / JSON API / daily digest .md / llms.txt / SSE
```

- **Runs without an LLM**: the hotness engine is pure rules (clustering +
  scoring), and every source is key-free.
- **Optional LLM enhancement**: point it at any OpenAI-compatible endpoint and
  top stories are automatically rewritten into Chinese headlines and summaries;
  on failure or without configuration it falls back to pure ranking and the
  site is unaffected.
- **Agent-friendly**: `/rss.xml`, `/api/v1/latest.json`, `/daily/<date>.md`,
  `/llms.txt`, `/api/events` (SSE push on publish) all work out of the box.

## Quick start

Requires Racket 8.14+ (minimal-racket counts; local development uses 9.3).

```sh
# Install as a Racket package (pulls in web-server and http-easy automatically)
raco pkg install --auto https://github.com/turlamai/pitboard.git

# Publish an edition (fetch + cluster + write data/YYYY-MM-DD.json)
raco pitboard fetch --config config/sources.json

# Assemble the weekly paid journal (--mock runs a pipeline self-check
# with a demo provider; without it an LLM key is required, fail-closed)
raco pitboard journal --issue 42 --mock

# Serve the site (default port 8090)
raco pitboard serve --port 8090

# Export a GitHub Pages static mirror into site/
raco pitboard export --out site
```

For development, clone and link locally with `raco pkg install --auto --link .`,
then run the tests with `raco test -p pitboard`.

## Configuration

### Sources (`config/sources.json`)

| Field | Meaning |
| --- | --- |
| `feeds` | RSS feed list — `name` / `url` / `weight` (the weight feeds into hotness) |
| `github.topics` | GitHub topic search terms (repos pushed in the last 30 days ranked by stars + fast-rising repos of the last 90 days) |
| `arxiv` | arXiv API query strings and per-query caps |

The default configuration is the repo's own `config/sources.json`: four media
feeds (Ars Technica Cars, TechCrunch Transportation, IEEE Spectrum
Transportation, Electrek) + 8 automotive topics + arXiv robotics /
systems-engineering papers. Swap in your own sources and it becomes your
industry's digest.

### LLM (optional, environment variables)

| Variable | Meaning |
| --- | --- |
| `PITBOARD_LLM_API_KEY` | key for an OpenAI-compatible endpoint (unset = pure ranking mode) |
| `PITBOARD_LLM_BASE_URL` | defaults to `https://api.deepseek.com` |
| `PITBOARD_LLM_MODEL` | defaults to `deepseek-chat` |

When `GITHUB_TOKEN` is present, GitHub search uses authenticated quota
(injected automatically in Actions).

### Commercial (paid journal, optional)

The paid face lives in the same `raco pitboard serve` process. Without the
variables below, commercial features degrade honestly (the subscribe page
says the channel is not live; mail runs in dry-run mode writing
`data/mail-out/`), and the free digest site is unaffected.

| Variable | Meaning |
| --- | --- |
| `PITBOARD_BASE_URL` | absolute site address (links in mail; default `https://pitboard.jrtx.site`) |
| `PITBOARD_ADMIN_PASSWORD` | editorial console password (`/console`; unset = login refused) |
| `PITBOARD_PAY_URL` | Pancake product checkout URL (unset = channel not configured) |
| `PITBOARD_PAY_ENV` | `test` / `production` (webhook keyring selection) |
| `PITBOARD_SMTP_HOST/PORT/USER/PASS` | Aliyun DirectMail smtpdm (unset = mail dry-run) |
| `PITBOARD_MAIL_FROM` | sender address (must match the DirectMail sending domain) |

Webhook signature verification delegates to the commerce package's canonical
Pancake implementation (RSA, official keyrings); install
`raco pkg install https://github.com/turlamai/commerce.git` (CI installs it).
Publishing flow: GitHub Actions assembles the full issue (artifact) → the
editor uploads issue.json at `/console` → previews → publishes (locked view
public, full text for subscribers only) → notify email with a login-free
per-subscriber link.

## Self-hosting and deployment

- **Process**: a single `raco pitboard serve` process does everything (pages +
  API + static assets + SSE); the data is just `data/*.json` files, so backup
  is a copy.
- **Static mirror**: the repo ships four GitHub Actions — `ci.yml`
  (three-platform tests), `digest.yml` (daily publish at 07:30 Beijing time
  with archive commits), `journal.yml` (weekly paid journal, Saturdays 08:00
  Beijing time; the full issue stays in a workflow artifact for editorial
  review and only the locked public view is committed) and `pages.yml`
  (rebuilds the mirror from committed editions/current templates and deploys it to GitHub Pages, without fetching new sources).
  Fork it, enable Pages (build source: "GitHub Actions"), and you get the same
  daily-updating site.

## Layout

```
pitboard/            Racket package body (collection "pitboard")
  cli.rkt            raco pitboard entry point (fetch / journal / serve / export)
  sources.rkt        source adapters (RSS / GitHub / arXiv) + hard timeouts
  feed.rkt           RSS 2.0 + Atom parsing (pure functions)
  score.rkt          tokenization, clustering, hotness, tags (new / rising)
  llm.rkt            OpenAI-compatible LLM enhancement (optional)
  journal.rkt        paid weekly journal assembly (weekly LLM passes:
                     select / write / foreword; fail-closed + call budget)
  store.rkt          data/YYYY-MM-DD.json storage
  render.rkt         page / RSS / Markdown / llms.txt rendering
  sse.rkt            SSE hub (publish push + heartbeat)
  app.rkt            web-server application
  export.rkt         GitHub Pages static export
assets/              site static assets (zero-dependency CSS/JS)
config/              source configuration
skill/pitboard/      agent skill (authoritative source, served at /skill/)
data/                daily edition archive (committed daily by Actions)
site/                static mirror (export artifact, Pages deploy source)
```

## Roadmap

See [docs/ROADMAP.md](docs/ROADMAP.md) for the full gap analysis and
milestones. Highlights:

- **M1 Content & tracking** — cross-day event tracking (progress markers),
  weekly/monthly reports, web-page list source upgrade, Chinese-language
  channel buildout (self-hosted RSSHub + WeChat Official Account bridge),
  JSON feed sources, OG share images.
- **M2 Operations & engineering** — /admin source console, MCP export,
  OpenAPI, Docker (fnOS NAS), LLM budget circuit breaker, curation review
  scripts, uptime alerts, on-site full-text search.
- **M3+ Expansion** — X / Official Account source production, entity topic
  pages, read-later favorites, feed/sse extracted into catalog packages,
  mobile (waiting on the rivet mobile host).
- **M4 Agent layering** — editor-side LLM channel (free models = three
  environment variables), reader-side BYOK (decision pending), consumer-side
  agent skill + MCP + llms.txt.

## Agent integration

pitboard treats agents as first-class citizens: every publish ships a complete
machine-readable dataset, and your agent can use it directly as an
automotive-industry information source.

- **Install the skill**: send the line below to your agent and it will take
  care of the rest:

  > Please install the pitboard skill: <https://pitboard.jrtx.site/skill/pitboard/SKILL.md>

  The SKILL.md carries its own self-install guidance; manual installation is a
  single curl (see the install section in that file).
- **Use the data with zero install**: `/api/v1/latest.json`,
  `/api/v1/digest/<date>.json`, `/openapi.json`, `/llms.txt` — all read-only,
  no auth.
- **Self-hosted, further**: `raco pitboard mcp` serves MCP tools
  (latest_digest / get_edition / get_topic); the `PITBOARD_LLM_*` environment
  variables hook up any OpenAI-compatible model for publish enhancement
  (optional, auto-degrades to the pure algorithmic mode on failure).

## License

MIT
