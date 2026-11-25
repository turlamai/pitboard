# Security Policy

Pitboard is a self-hosted, single-process Racket web application with no
external database: editions, sources and curation state live in plain files
on the server. It ingests third-party content (industry sources, RSS-like
feeds) and clusters it into a daily digest; an LLM key is optional (BYOK)
and is provided by the operator, not readers. No release has shipped yet.

## Reporting a vulnerability

Email **security@jrtx.site** (or open a GitHub security advisory on this
repository). Include reproduction steps, affected versions, and your
assessment of impact. You will get an acknowledgment within 72 hours.

Please do not open public issues for security problems.

## Scope

In scope:

- The web server surface: routes, templates and static assets served by the
  self-hosted site (the `pitboard/` Racket web-server application)
- External content ingestion: the raw-material intake, clustering and
  assembly pipeline, including malicious or adversarial source content
- Operator secrets: the optional LLM API key handling (BYOK) — where it is
  stored, where it is sent, and what it can reach

Out of scope:

- The content and opinions of ingested third-party sources; attribution and
  correctness of the digest are editorial concerns, not security ones
- Vulnerabilities in reader-owned infrastructure (reverse proxies, TLS
  termination, the OS the operator self-hosts on)

## Supported versions

The development preview on `main`. Security fixes land on `main` first;
once releases exist, fixes are backported to the latest release line.
