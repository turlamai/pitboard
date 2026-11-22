# pitboard

[English](README.md) · **中文**

![platform](https://img.shields.io/badge/platform-Web%20%7C%20Docker%20%7C%20Self--hosted-lightgrey) ![built with](https://img.shields.io/badge/built%20with-Racket-9f1d20) [![CI](https://github.com/turlamai/pitboard/actions/workflows/ci.yml/badge.svg)](https://github.com/turlamai/pitboard/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**PITBOARD 周刊**：付费订阅的汽车行业期刊。每周六早 8 点，一周的事——筛好、写好、装订好，送进订阅者的邮箱。

AI 流水线做粗活，主编做把关：筛选、写作、汇编由 AI 完成，发布前主编逐期复核。卖的不是信息（信息到处免费），是三样东西：**筛选**（每条都有「为什么入选」）、**装订**（报纸级排版，读完一期约 15 分钟）、**准时**（每周六 08:00，确定性本身是产品的一部分）。

## 它怎么工作

```
每日（零 token，内部工序，读者不可见）：
  RSS/网页列表/GitHub/arXiv → 抓取 → 归一化 → 聚簇(Jaccard) → 热度(独立来源×时间衰减) → data/YYYY-MM-DD.json

每周六（LLM 仅此一段介入）：
  一周事件压缩 → 精选(头条+条线) → 逐条写作(答案先行摘要+为什么入选+数字抽取) → 卷首按语
  → 大事记/数据版(规则编排) → 完整期刊 artifact → 主编复核(/console 上传→发布)

商业闭环：
  落地页 → Pancake 支付 → 邮箱 double opt-in → 通知邮件(免登录直达链接) → 网页全文
  → 到期提醒 → 续订（从现有到期日顺延）
```

- **成本结构**：纯规则引擎每日零 token 压缩原料；LLM 只在周度汇编介入，单期成本几元人民币——一个年订户覆盖全年成本。
- **fail-closed**：无 LLM key 不出刊，付费产品不出纯规则期刊；每期调用有预算熔断。
- **付费墙边界是代码强制的**：公开面只有卷首 + 头条全文 + 目录；付费正文不进公开仓库（完整期刊只存 Actions artifact，发布走主编上传）。

## 阅读入口

[订阅与阅读](https://pitboard.jrtx.site/) · 当期网页版可见卷首按语与本周头条全文，其余订阅可见。

## 快速开始（自托管）

需要 Racket 8.14+（本机开发用 9.3）与 [commerce 包](https://github.com/turlamai/commerce)（webhook 验签）。

```sh
git clone https://github.com/turlamai/pitboard.git && cd pitboard
raco pkg install --auto --no-docs http-easy feed server-sent-events
raco pkg install --auto --no-docs https://github.com/turlamai/commerce.git
raco pkg install --auto --no-docs --link "$PWD"

# 内部原料积累（每日定时跑，零 token）
raco pitboard fetch --data data --config config/sources.json

# 周刊汇编（--mock 流水线自检；真实出刊必须有 LLM key）
raco pitboard journal --issue 1 --mock

# 起站点（落地页 / 期刊 / 订阅支付 / 主编工作台）
raco pitboard serve --port 8090 --data data

# 精选质量评测（标注样本）
raco pitboard eval --data data --samples eval/samples.json
```

测试：`raco test -p pitboard`。

## 配置

| 变量 | 说明 |
| --- | --- |
| `PITBOARD_BASE_URL` | 站点绝对地址（邮件链接用） |
| `PITBOARD_ADMIN_PASSWORD` | 主编工作台密码（`/console`；不设则拒绝登录） |
| `PITBOARD_PAY_URL` | Pancake 商品结账链接（不设 = 订阅页诚实提示渠道未上线） |
| `PITBOARD_PAY_ENV` | `test` / `production`（webhook 公钥钥匙环） |
| `PITBOARD_LLM_API_KEY` | DeepSeek 等 OpenAI 兼容 key（不设 = 不出刊，fail-closed） |
| `PITBOARD_LLM_BASE_URL` / `PITBOARD_LLM_MODEL` | 默认 `https://api.deepseek.com` / `deepseek-chat` |
| `PITBOARD_JOURNAL_BUDGET` | 每期 LLM 调用硬顶（默认 24） |
| `PITBOARD_SMTP_HOST/PORT/USER/PASS` | 阿里云邮件推送 smtpdm（不设 = 邮件 dry-run 写 `data/mail-out/`） |
| `PITBOARD_MAIL_FROM` | 发件地址（与 DirectMail 发信域名一致） |

webhook 签名验证委托 [commerce](https://github.com/turlamai/commerce) 的 Pancake 正典实现（RSA，官方钥匙环）。

## GitHub Actions

| Workflow | 做什么 |
| --- | --- |
| `ingest.yml` | 每日 07:30（北京）内部原料积累并提交归档——无公开面 |
| `journal.yml` | 每周六 08:00（北京）汇编完整期刊：完整版进 artifact 供主编复核，锁定态公开视图提交 `data/journal/` |
| `ci.yml` | 测试门禁 |

## 自托管与部署

单一 Racket 进程即整站：`raco pitboard serve`（页面 + 订阅支付流 + 主编工作台），数据就是 `data/` 目录文件，备份即拷贝。Docker 镜像见 `Dockerfile`；支付回调需要公网 HTTPS（`PITBOARD_BASE_URL`）。

## 目录结构

```
pitboard/            Racket 包主体（collection "pitboard"）
  cli.rkt            raco pitboard 入口（fetch / journal / serve / eval）
  sources.rkt        信源适配器（RSS / 网页列表 / GitHub / arXiv）+ 硬超时
  score.rkt          分词、聚簇、热度（内部原料工序，零 token）
  llm.rkt            OpenAI 兼容调用（chat-completions + 每日增强，可选）
  journal.rkt        周刊汇编（跨日候选合并 → LLM 精选/写作/卷首 → 渲染）
  subs.rkt           订阅者/支付流水存储 + 令牌生命周期
  pay.rkt            支付面（commerce 包 Pancake 验签 + 订阅投影）
  mail.rkt           SMTP-over-TLS 投递 + dry-run + 邮件模板
  site.rkt           期刊站点路由（落地页/期刊/订阅流/自助/主编工作台）
  app.rkt            web-server 应用
  store.rkt          data/YYYY-MM-DD.json 存储
  util.rkt           日期/字符串/URL 工具（UTC 显式计算）
.github/workflows/   ingest（每日原料）/ journal（每周出刊）/ ci
config/              信源配置（你的品味清单进这里）
data/                每日事件归档（Actions 提交）+ 期刊锁定视图
design/              设计契约与原型（王垠排版内核 × 家族 HIG）
```

## License

MIT
