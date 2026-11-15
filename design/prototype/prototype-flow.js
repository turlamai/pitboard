/* Local sample filtering, article inspection and accessible source placeholders. */
(() => {
 const P=Prototype,{ $, $$, text, link, modal, download, status }=P;
 const cards=$$('.cards > .card'),tabs=$$('.tabs [role=tab]'),search=$('#search');let category='全部';
 const quick=$('.quick-list');if(quick&&cards.length){quick.replaceChildren();cards.forEach((card,i)=>{const item=document.createElement('li');item.className='quick-item';const anchor=document.createElement('a');anchor.className='quick-link';anchor.href='#';const rank=document.createElement('span');rank.className='quick-rank';rank.textContent=String(i+1).padStart(2,'0');const name=document.createElement('span');name.className='quick-name';name.textContent=text($('.card-title',card));anchor.append(rank,name);item.append(anchor);quick.append(item)});if($('.quick-count'))$('.quick-count').textContent='TOP '+cards.length;const kicker=$('.hero .kicker');if(kicker)kicker.textContent+=' · 原型示例'}
 const count=document.createElement('p');count.className='proto-result';count.setAttribute('role','status');$('.toolbar')?.after(count);
 function filter(){const q=(search?.value||'').toLowerCase().trim();let n=0;cards.forEach(c=>{c.hidden=!!(category!=='全部'&&text($('.card-kind',c))!==category||q&&!text(c).toLowerCase().includes(q));if(!c.hidden)n++});if(cards.length)count.textContent=n?`展示 ${n} 条示例内容`:'没有匹配内容，请更换关键词或类型。';}
 tabs.forEach(tab=>{const name=tab.firstChild.textContent.trim();const n=$('.tab-n',tab);if(n)n.textContent=cards.filter(c=>name==='全部'||text($('.card-kind',c))===name).length;link(tab,()=>{category=name;tabs.forEach(t=>{t.classList.toggle('active',t===tab);t.setAttribute('aria-selected',t===tab);t.tabIndex=t===tab?0:-1});filter()});tab.addEventListener('keydown',e=>{if(['ArrowLeft','ArrowRight','Home','End'].includes(e.key)){e.preventDefault();let i=tabs.indexOf(tab);i=e.key==='Home'?0:e.key==='End'?tabs.length-1:(i+(e.key==='ArrowRight'?1:-1)+tabs.length)%tabs.length;tabs[i].click();tabs[i].focus()}})});
 search?.addEventListener('input',filter);filter();
 function article(card){modal(text($('.card-title',card)),text($('.card-summary',card))+'\n\n这是用于验证阅读流程的示例条目，未获取实时新闻或原文。')}
 cards.forEach(card=>{const a=$('.card-title a',card);if(a)link(a,()=>article(card))});
 $$('.quick-link').forEach(a=>link(a,()=>{const q=text($('.quick-name',a));const card=cards.find(c=>text($('.card-title',c))===q);if(card)article(card);else modal(q,'热点阅读入口示例；此原型未包含该条目的正文。')}));
 $$('a[href="#"]').forEach(a=>{
   if(a.matches('.card-title a,.quick-link'))return;
   if(/RSS/.test(text(a))){a.removeAttribute('aria-disabled');link(a,()=>modal('RSS 订阅示例','原型没有线上订阅地址。可下载一个本地 RSS 样例查看结构。',[],()=>download('pitboard-sample.xml','<?xml version="1.0"?><rss version="2.0"><channel><title>Pitboard prototype</title><description>Local sample, not a live feed</description><link>https://example.invalid/</link></channel></rss>','application/rss+xml'),'下载样例'));}
   else if(a.matches('.arch-link'))link(a,()=>modal('归档期次',text(a)+'\n示例旧刊，仅用于验证归档阅读入口。',[],()=>location.href='index.html','查看版面样例'));
   else link(a,()=>modal(text(a)||'来源信息','此处为设计样例入口，尚未配置真实来源地址。'));
 });
 const copy=$('#copy-install');if(copy){copy.onclick=null;link(copy,()=>P.copy(text($('.install-line'))));}
 $$('button').filter(b=>text(b)==='刷新').forEach(b=>link(b,()=>status('已重试演示加载 · 当前页面保留失败状态样本')));
})();
