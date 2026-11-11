# pitboard DESIGN.md — 视觉与交互契约（单一真源）

原型（`design/prototype/`）与实现（`assets/` + `pitboard/render.rkt`）都遵循本文件；改视觉先改这里，再同步实现。
Tokens 逐值提取自 `assets/styles.css`（站点 tokens）与 `site/family.css`（site-kit 家族 tokens，accent 同值），
原型 → 实现零翻译成本。这是免费内容站：不出现订阅墙、账号、付费任何形态。

## Brief（2026-11-11 试点）

- **目标**：非从零重设计。在既有暖纸色编辑体系上做 HIG 级打磨；让「独立来源数 × 热度排名」这条可信度主线在界面上站出来；来源标注与勘误呈现成为产品级可信度设计；界面达到可直接用于宣传的质量。
- **原型范围**（5 屏 × 亮/暗）：① 今日版面（英雄屏：报头 + EDITION 号码盘 + TOP5 快讯 + 筛选工具栏 + 热点卡含来源展开）② 话题追踪（TOPIC RADAR 跨期时间线）③ 往期归档（期次列表 + 已更正标记）④ 关于与勘误（内容状态说明 + 勘误记录 + 报错路径）⑤ 空态与防错（404 / 未出刊 / 旧刊警示）。
- **Exit 条件**：5 屏关键状态齐（含空态/失败态/警示态）+ 交互逻辑定稿 + HIG 清单全过 + 用户评审通过。
- **不在范围**：新功能（书签同步、站内全文搜索重构、admin 控制台）、商业/付费 UI、`data/` schema 变更、新配色体系、移动端布局重做（响应式已有基线）。

## 设计原则（HIG 内核 × 本产品特有）

- **排版是产品资产**：版心 840px、行高 1.7、中西混排 cap-height 对齐、所有数字 tabular-nums——日报的可读性本身就是卖点，装饰不与标题和来源争注意力。
- **来源立信**：独立来源数永远与标题同屏可见（🔥 热度只是排序信号，不是事实背书）；原始来源入口与域名（`src-dom`）逐条可点，读者永远能回到一手信息。
- **AI 永远署名**：LLM 增强的标题/摘要必带「LLM 增强」标注；无 LLM 时站点照常工作——界面上不存在冒充编辑内容的机器内容。
- **时效不撒谎**：期号与出刊日期常驻首屏；旧刊、出刊失败、404 各有明确状态界面，绝不静默。「旧刊不是今日新闻」。
- **勘误可追溯**：更正带日期与原因，永不静默修改；报错路径（期号 + 条目/来源 URL + 期望更正）在关于页与页脚常驻。
- **层级克制**：bg < surface < board 三层封顶；accent 只给链接、可交互件与状态。

## Tokens（与实现 1:1，来源 `assets/styles.css`）

| Token | Light | Dark | 用途 |
|---|---|---|---|
| `--bg` | `#faf7f2` | `#161411` | 画布 |
| `--surface` | `#fffdf9` | `#1f1c18` | 卡片/浮层/搜索框 |
| `--ink` | `#211e1a` | `#ece7de` | 主文字 |
| `--muted` | `#6f6960` | `#a49d92` | 次级文字（摘要/meta） |
| `--faint` | `#a89f92` | `#6f6960` | 辅助文字（rank/时间/域名） |
| `--line` | `#e9e2d8` | `#2e2a25` | 分隔线 |
| `--line-strong` | `#d8cfbf` | `#413a31` | 悬浮描边 |
| `--accent` | `#D97757` | `#E08A6B` | 链接/交互/状态/finish-line 条纹 |
| `--accent-deep` | `#A94E31` | `#E89E82` | 标题 hover（暗态反转为浅） |
| `--on-accent` | `#FFFFFF` | `#1D120C` | accent 底上的文字（SSE 新刊横幅；取自 family.css `--kit-on-accent`） |
| `--accent-soft` | `#FBEDE7` | `#382920` | accent 弱底（active tab/LLM 徽标/安装盒） |
| `--chip-bg` / `--chip-line` | `#f1ece2` / `#e2dacb` | `#28241f` / `#38322a` | chip 与 code 底/描边 |
| `--new-bg` / `--new-ink` | `#e6f1e2` / `#2f6b36` | `#233020` / `#a5cf90` | 「新」状态徽标 |
| `--rising-bg` / `--rising-ink` | `#fdeed2` / `#92610c` | `#382e18` / `#e0b45c` | 「↑ 上升」状态徽标 |
| `--board-bg` / `--board-line` / `--board-ink` | `#17140f` / `#3a332a` / `#f5efe2` | `#0c0a08` / `#332c24` / `#f5efe2` | EDITION 号码盘（唯一深色品牌件，亮暗近乎不变） |

- **字体栈**：正文 `ui-sans-serif, -apple-system, "PingFang SC", "Segoe UI", "Microsoft YaHei", sans-serif` + `font-size-adjust: cap-height 0.7`（中西混排字面高对齐，浏览器不支持则忽略）；报头/大日期/栏目标题用衬线栈 `"Newsreader", "Songti SC", "Noto Serif SC", Georgia, serif`（Newsreader 由站点自托管 woff2 提供，原型用系统衬线回退）；mono `ui-monospace, "SF Mono", Menlo, Consolas, monospace` 只用于 kicker、rank、热度、时间、域名、期号。
- **字阶**：报头大日期 clamp(30–42px)/1.15 · 卡标题 20px/1.42 · 正文 16px/1.7 · 摘要 15px · nav/meta 14–13px · chip 12px · kicker 11px mono 600 大写宽字距 0.24em。
- **版心**：内容 840px，prose 720px。**圆角**：卡片 16px · 号码盘 14px · 快讯块 12px · 控件/搜索 999px pill · code 6px。**间距节奏**：4/8/12/16/24。**动效**：140ms ease，不过度。
- **品牌签名**：页面顶部 3px「终点线」条纹（accent 与 ink 交替 14px）；EDITION 号码盘是唯一的深色件，任何页面不得再造深色块。
- **语言**：界面文案跟随系统（zh-CN / en 双源，i18n 字典在 `render.rkt` 生成）；界面文案中文为主、kicker 保持英文大写（现状如此）；外链标题不翻译。

## 组件词汇

`KitNav`（家族导航：品牌 + 今日/话题/书签/归档/关于/RSS + 语言/主题切换）· `FinishLine`（顶部 3px 条纹）· `HeroMasthead`（kicker + 衬线大日期 + 统计 meta + 「LLM 增强」徽标）· `EditionBoard`（EDITION 号码盘）· `QuickStrip`（当前热点 TOP5）· `Toolbar`（类型 tabs 全部/开源/资讯/论文 + 搜索框，`/` 聚焦）· `StoryCard`（rank/类型/热度/状态 chips/热力条/标题/摘要/来源 chips/展开全部来源）· `SourceList`（展开后的来源清单，域名 mono 弱化）· `TopicTimeline`（话题跨期行：日期 + 标题 + 域名）· `ArchiveList`（期次行：期号 + 日期 + 头条 + 条数 + 已更正标记）· `ErrataLog`（勘误记录：日期 + 原况 + 更正 + 原因）· `StaleBanner`（旧刊/出刊失败警示）· `EmptyState`（404 / 未出刊）· `Footer`。

## 不变式

1. `accent` 只作小面积：链接与可交互件、状态徽标（新/上升/进展/热度/LLM 增强）、kicker 与期号等 mono 短语的文字色；accent 底只允许两处既有点——SSE 新刊横幅 pill 与 agent 安装盒（后者为 accent-soft 弱底）；深色底只有 EDITION 号码盘一处。
2. 层级 = `bg` → `surface` → `board` 三层封顶；分隔靠 `line` 与留白表达，不堆阴影、不重描边。
3. 亮暗主题成对设计，不存在只有暗态的界面；`--faint` 文字不得低于次级可读对比（仅用于非关键辅助信息）。
4. 每条热点必须可见原始来源入口与域名；AI 增强内容必带「LLM 增强」标注；热度数字不得呈现为事实结论。
5. 时效与勘误永不删除：期号/出刊日期常驻首屏，旧刊必须显示警示横幅；勘误记录带日期与原因可追溯，报错路径常驻页脚与关于页。
6. 数字（rank/热度/期号/时间/统计）一律 mono + `tabular-nums`；键盘焦点 `:focus-visible` 用 accent 2px 描边，搜索框支持 `/` 快捷聚焦。


## 平台映射与差异声明（预钉 · 宿主实现时升级为落点表）

统一层（三平台必须一致，`design/prototype/` 原型已锁定）：信息架构、核心流程、快捷键语义、
语义色与层级词汇。因地制宜层（按平台惯例落地，不预画像素）：

| 维度 | macOS | Windows | Linux |
|---|---|---|---|
| 修饰键 | ⌘ | Ctrl | Ctrl |
| 命令入口 | 原生菜单栏 | 菜单栏/命令行内 | 菜单栏 |
| 字体 | 系统栈 + PingFang SC | 系统栈 + Microsoft YaHei | 系统栈 + Noto Sans CJK |
| 原生控件 | SwiftUI 控件 | WinUI 3 控件 | GTK4 控件 |

差异声明纪律：宿主特例必须先写进本节才能实现；未声明的宿主差异按违约处理
（案例：taskly 漂移台账 D8）。
