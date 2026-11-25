"use strict";
const fs = require('node:fs');
const assert = require('node:assert/strict');
const path = require('node:path');
let chromium;
try { ({chromium} = require('playwright')); }
catch { console.error('Playwright is required. Install it or set NODE_PATH to its node_modules directory.'); process.exit(1); }
const args = process.argv.slice(2);
if (args.length && (args.length !== 2 || args[0] !== '--output-dir')) {
  console.error('Usage: node tools/doc/check-rendering.cjs [--output-dir <directory>]'); process.exit(1);
}
const output = args.length ? path.resolve(args[1]) : null;
if (output) fs.mkdirSync(output, {recursive:true});
const root = path.resolve(__dirname, '../..');
const source = fs.readFileSync(`${root}/inst/mascot/doc.js`, 'utf8');
const css = fs.readFileSync(`${root}/inst/mascot/doc.css`, 'utf8');

async function checkPanel(browser) {
  const results = [];
  for (const [width, height, theme] of [[320,568,'light'], [375,667,'dark'], [1280,800,'light'], [1280,800,'dark']]) {
    const context = await browser.newContext({viewport:{width,height}, reducedMotion:'reduce', colorScheme:theme});
    try {
      const page = await context.newPage();
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('http://updoc.test/**', route => route.fulfill({
        contentType:'text/html', body:'<!doctype html><html lang="en"><title>Preview check</title><body><button id="outside">Documentation</button></body></html>'
      }));
      let requests = 0;
      await page.route('**/__updoc__/rebuild', route => {
        assert.equal(route.request().method(), 'POST');
        assert.equal(route.request().headers()['x-updoc-request'], 'rebuild');
        requests++;
        return route.fulfill({status:requests === 1 ? 503 : 202, contentType:'application/json', body:JSON.stringify({
          phase:'building', progress:{operation:'Building documentation'}
        })});
      });
      await page.goto('http://updoc.test/');
      await page.evaluate(({theme,css}) => {
        document.documentElement.dataset.bsTheme = theme;
        document.body.style.background = theme === 'dark' ? '#181818' : '#fff';
        window.__updocPreviewBootstrap = {css};
      }, {theme,css:css + '\n' + fs.readFileSync(`${root}/inst/preview/panel.css`, 'utf8')});
      for (const file of ['inst/mascot/doc.js','inst/preview/panel-primitives.js','inst/preview/panel.js']) {
        await page.addScriptTag({content:fs.readFileSync(path.join(root,file),'utf8')});
      }
      await page.locator('#outside').focus();
      await page.evaluate(() => window.__updocPreviewPanel.initialize({phase:'error', warnings:[], error:{
        summary:'Could not render the article', location:'vignettes/' + 'long-path-'.repeat(20) + '.Rmd:12',
        cause:'A required input is missing.', details:'Detailed diagnostic information.\n'.repeat(30),
        hint:'Correct the input and save again.'
      }}));
      const shell = page.locator('.updoc-shell');
      await page.waitForFunction(() => document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-shell').dataset.expanded === 'true');
      assert.equal(await page.locator('#outside').evaluate(node => node === document.activeElement), true);
      const box = await shell.boundingBox();
      assert.ok(box.x >= -1 && box.y >= -1 && box.x + box.width <= width + 1 && box.y + box.height <= height + 1,
        `Panel exceeds ${width}x${height} viewport: ${JSON.stringify(box)}`);
      const overflow = await page.locator('.ui-scroll-area-viewport').evaluate(node => ({
        vertical:node.scrollHeight > node.clientHeight, horizontal:node.scrollWidth > node.clientWidth + 1
      }));
      assert.equal(overflow.vertical, true, 'Long diagnostics should scroll');
      assert.equal(overflow.horizontal, false, 'Long diagnostics should wrap');
      await page.getByRole('button', {name:'Minimize updoc message'}).press('Enter');
      await page.waitForFunction(() => document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-shell').dataset.updocState === 'error-compact');
      await shell.press('Enter');
      await page.waitForFunction(() => document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-shell').dataset.updocState === 'error-full');
      const rebuild = page.getByRole('button', {name:'Rebuild',exact:true});
      await rebuild.press('Enter');
      await page.getByText('Could not start rebuild. Try again.', {exact:true}).waitFor();
      assert.equal(await rebuild.isEnabled(), true);
      await rebuild.press('Enter');
      await page.waitForFunction(() => document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-shell').dataset.updocState === 'building');
      assert.equal(requests, 2);
      const announcements = await page.evaluate(async () => {
        const region = document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-announcement');
        let changes = 0;
        const observer = new MutationObserver(records => { changes += records.length; });
        observer.observe(region, {childList:true,subtree:true,characterData:true});
        for (let current = 1; current <= 3; current++) {
          window.__updocPreviewPanel.render({phase:'building', progress:{
            operation:'Building reference', target:'topic-' + current, current,total:3
          }});
        }
        await Promise.resolve();
        observer.disconnect();
        return {changes,text:region.textContent};
      });
      assert.deepEqual(announcements, {changes:0,text:'Building documentation.'});
      await page.evaluate(() => window.__updocPreviewPanel.render({phase:'ready', warnings:[{
        summary:'An article has no title',location:'vignettes/article.Rmd',cause:'Add a title to its YAML front matter.'
      }]}));
      await page.waitForFunction(() => document.getElementById('__updoc_status').shadowRoot.querySelector('.updoc-shell').dataset.updocState === 'warning-full');
      if (output) await page.screenshot({path:path.join(output,`panel-${width}-${theme}.png`)});
      assert.deepEqual(errors, []);
      results.push({width,height,theme,requests,overflow});
      console.log(`PASS preview layout, keyboard controls, HTTP retry and announcements at ${width}px (${theme})`);
    } finally {
      await context.close();
    }
  }
  return results;
}

(async () => {
  const browser = await chromium.launch(process.env.CHROME_PATH ? {executablePath:process.env.CHROME_PATH, headless:true} : {channel:'chrome', headless:true});
  const report = {errors:[]};
  const passed = name => console.log('PASS ' + name);
  try {
    const page = await browser.newPage({viewport:{width:900,height:620},deviceScaleFactor:2});
    page.on('pageerror', error=>report.errors.push(error.message));
    await page.emulateMedia({reducedMotion:'no-preference'});
    await page.setContent('<style>body{font:16px system-ui;margin:48px;background:#fff;color:#111}#fixtures{display:flex;gap:40px;align-items:center}.mount{width:96px;height:96px;display:flex;align-items:center;justify-content:center}</style><h1>Mascot motion QA</h1><div id="fixtures" class="doc-intro"></div>');
    await page.addStyleTag({content:css});
    await page.addStyleTag({content:fs.readFileSync(path.join(root, 'pkgdown/extra.css'), 'utf8')});
    await page.addScriptTag({content:source});
    await page.evaluate(() => {
      // Test actual checked-in CSS; do not inject poseCSS() into the fixture.
      const raf=requestAnimationFrame.bind(window), caf=cancelAnimationFrame.bind(window);
      window.pendingFrames=new Set();
      window.requestAnimationFrame=fn=>{let id=raf(time=>{pendingFrames.delete(id);fn(time);});pendingFrames.add(id);return id;};
      window.cancelAnimationFrame=id=>{pendingFrames.delete(id);caf(id);};
      const nativeAdd=EventTarget.prototype.addEventListener, nativeRemove=EventTarget.prototype.removeEventListener;
      window.listeners=new Map();
      EventTarget.prototype.addEventListener=function(name,fn,options){
        if(this===window||this===document){const key=(this===window?'window:':'document:')+name;let set=listeners.get(key)||new Set();set.add(fn);listeners.set(key,set);}
        return nativeAdd.call(this,name,fn,options);
      };
      EventTarget.prototype.removeEventListener=function(name,fn,options){
        if(this===window||this===document){const key=(this===window?'window:':'document:')+name;listeners.get(key)?.delete(fn);}
        return nativeRemove.call(this,name,fn,options);
      };
      window.make=(seed='qa')=>{const controller=UpdocDoc.create({seed});const mount=document.createElement('div');mount.className='mount';mount.append(controller.element);document.getElementById('fixtures').append(mount);return controller;};
      window.first=make('first');window.second=make('second');window.third=make('third');
      window.controllers=[first,second,third];
      const matrix=element=>{const m=element.getCTM();return [m.a,m.b,m.c,m.d,m.e,m.f];};
      window.snapshot=c=>({
        state:c.element.dataset.state, celebrating:c.element.dataset.celebrating||false,
        body:matrix(c.element.querySelector('.doc-body')),
        celebration:matrix(c.element.querySelector('.doc-celebration')),
        eyes:matrix(c.element.querySelector('.doc-eyes')),
        cross:getComputedStyle(c.element.querySelector('.doc-eye-cross')).opacity,
        open:getComputedStyle(c.element.querySelector('.doc-eye-open')).opacity,
        happy:getComputedStyle(c.element.querySelector('.doc-eye-happy')).opacity,
        bridges:c.element.getAnimations({subtree:true}).filter(a=>!a.animationName&&!a.transitionProperty).length
      });
      window.distance=(a,b)=>Math.max(...[[4,4],[44,4],[4,44],[44,44],[24,24]].map(([x,y])=>Math.hypot((a[0]-b[0])*x+(a[2]-b[2])*y+a[4]-b[4],(a[1]-b[1])*x+(a[3]-b[3])*y+a[5]-b[5])));
      window.delay=ms=>new Promise(resolve=>setTimeout(resolve,ms));
    });
    report.transitions=await page.evaluate(async()=>{
      const result=[];
      for(const state of ['building','error','warning','ready','disconnected','building','ready']){
        const from=first.element.dataset.state;
        const before=snapshot(first);first.setState(state);const after=snapshot(first);
        const start=performance.now();let previous=after;const frames=[];
        await new Promise(resolve=>{
          function tick(){const now=snapshot(first);const elapsed=performance.now()-start;frames.push({elapsed,delta:distance(previous.body,now.body),bridges:now.bridges,cross:now.cross,open:now.open});previous=now;if(elapsed<420)requestAnimationFrame(tick);else resolve();}requestAnimationFrame(tick);
        });
        result.push({from,to:state,startDelta:distance(before.body,after.body),startBridges:after.bridges,maxDelta:Math.max(...frames.map(f=>f.delta)),boundary:frames.filter(f=>f.elapsed>280&&f.elapsed<365),midpoint:frames.filter(f=>f.elapsed>145&&f.elapsed<185)});
      }
      return result;
    });
  report.endpoints=await page.evaluate(async()=>{
   const delay=ms=>new Promise(resolve=>setTimeout(resolve,ms));
   function distance(a,b){const x=new DOMMatrix(a==='none'?undefined:a),y=new DOMMatrix(b==='none'?undefined:b);return Math.max(...[[4,4],[44,4],[4,44],[44,44]].map(([px,py])=>Math.hypot((x.a-y.a)*px+(x.c-y.c)*py+x.e-y.e,(x.b-y.b)*px+(x.d-y.d)*py+x.f-y.f)));}
   const results=[];
   for(const [from,to] of [['ready','building'],['building','error'],['error','warning'],['warning','ready'],['ready','disconnected'],['disconnected','building'],['building','ready'],['error','celebrate']]){
    const c=UpdocDoc.create({state:from,gaze:'none',seed:'first'});document.getElementById('fixtures').append(c.element);await delay(60);
    if(to==='celebrate'){c.setState('ready');c.celebrate();}else c.setState(to);
    const effects=c.element.getAnimations({subtree:true});
    const bridges=effects.filter(a=>!a.animationName&&!a.transitionProperty);
    const css=effects.filter(a=>a.animationName||a.transitionProperty);
    const duration=bridges[0].effect.getTiming().duration;
    const remaining=duration-bridges[0].currentTime;
    const advance=css.map(a=>({a,time:a.currentTime,running:a.playState==='running'}));
    bridges.forEach(a=>a.pause());css.forEach(a=>a.pause());
    advance.forEach(({a,time,running})=>{if(running&&typeof time==='number')a.currentTime=time+remaining;});
    bridges.forEach(a=>a.currentTime=duration-.0001);
    const end=bridges.map(a=>{const property=Object.hasOwn(a.effect.getKeyframes()[0],'opacity')?'opacity':'transform';return {target:a.effect.target,property,expected:getComputedStyle(a.effect.target)[property]};});
    bridges.forEach(a=>a.cancel());
    const residuals=end.map(({target,property,expected})=>({target:target.getAttribute('class'),property,expected,actual:getComputedStyle(target)[property],delta:property==='opacity'?Math.abs(+expected-+getComputedStyle(target)[property]):distance(expected,getComputedStyle(target)[property])}));
    results.push({from,to,remaining,maxDelta:Math.max(...residuals.map(r=>r.delta)),nonzero:residuals.filter(r=>r.delta>0.001)});
    c.destroy();
   }
   return results;
  });
    report.interrupted=await page.evaluate(async()=>{
      const result=[];
      for(const state of ['building','error','warning','ready','error','ready']){
        const before=snapshot(first);first.setState(state);const after=snapshot(first);
        result.push({to:state,delta:distance(before.body,after.body),bridges:after.bridges});await delay(65);
      }
      const before=snapshot(first);const accepted=first.celebrate();const after=snapshot(first);
      result.push({to:'celebrate',accepted,delta:distance(before.body,after.body),repeat:first.celebrate()});
      await delay(350);const airborne=snapshot(first);first.setState('warning');const interrupted=snapshot(first);
      result.push({to:'warning-during-hop',delta:distance(airborne.body,interrupted.body),before:airborne,after:interrupted});
      await delay(450);first.setState('ready');await delay(400);first.celebrate();await delay(350);
      const beforeStop=snapshot(first);first.stopCelebration();const afterStop=snapshot(first);
      result.push({to:'stopCelebration-during-hop',delta:distance(beforeStop.body,afterStop.body)});
      return result;
    });
    report.expression = await page.evaluate(() => {
      const c=make('expression-midpoint');
      c.setState('error');
      const bridges=c.element.getAnimations({subtree:true}).filter(a=>!a.animationName&&!a.transitionProperty);
      bridges.forEach(a=>{a.pause();a.currentTime=a.effect.getTiming().duration/2;});
      const eyes=c.element.querySelector('.doc-eyes');
      const matrix=new DOMMatrix(getComputedStyle(eyes).transform);
      const result={closure:matrix.d,open:+getComputedStyle(c.element.querySelector('.doc-eye-open')).opacity,
        cross:+getComputedStyle(c.element.querySelector('.doc-eye-cross')).opacity};
      c.destroy();
      return result;
    });
    await page.waitForTimeout(500);
    // A second script copy models the homepage plus injected preview panel.
    await page.addScriptTag({content:source});
    await page.evaluate(()=>controllers.push(make('another-script')));
    report.pointer=await page.evaluate(async()=>{
      async function settleFrames() {
        const deadline=performance.now()+5000;
        while(pendingFrames.size && performance.now()<deadline) await delay(25);
      }
      const listenersCount=Object.fromEntries([...listeners].map(([key,values])=>[key,values.size]));
      window.dispatchEvent(new PointerEvent('pointermove',{clientX:850,clientY:550,pointerType:'mouse'}));
      const scheduled=pendingFrames.size;await settleFrames();
      const settled=pendingFrames.size;
      const before=first.element.style.getPropertyValue('--doc-look-x');
      window.dispatchEvent(new PointerEvent('pointerout',{relatedTarget:null,pointerType:'mouse'}));
      const immediate=first.element.style.getPropertyValue('--doc-look-x');
      await new Promise(resolve=>requestAnimationFrame(resolve));
      const handoff=first.element.style.getPropertyValue('--doc-look-x');
      await settleFrames();const rest=first.element.style.getPropertyValue('--doc-look-x');
      return {listenersCount,scheduled,settled,before,immediate,handoff,rest,restFrames:pendingFrames.size};
    });
    report.reduced=await page.evaluate(async()=>{
      first.setState('building');first.setState('error');first.setMotion('reduce');
      window.dispatchEvent(new PointerEvent('pointermove',{clientX:10,clientY:10,pointerType:'mouse'}));
      await delay(50);
      return {animations:first.element.getAnimations({subtree:true}).length,tracking:first.element.dataset.tracking||null,gaze:first.element.style.getPropertyValue('--doc-look-x'),pose:snapshot(first)};
    });
    report.colors=await page.evaluate(async()=>{
      const colors=[];
      for(const theme of ['light','dark']){
        document.documentElement.dataset.bsTheme=theme;
        for(const state of ['ready','warning','error']){first.setState(state);await delay(10);colors.push({theme,state,fill:getComputedStyle(first.element.querySelector('.doc-eye-open')).fill,stroke:getComputedStyle(first.element.querySelector('.doc-eye-cross')).stroke});}
      }
      return colors;
    });
    if (output) {
      for (const theme of ['light','dark']) {
        await page.evaluate(theme=>{document.documentElement.dataset.bsTheme=theme; document.body.style.background=theme==='dark'?'#181818':'#fff';document.body.style.color=theme==='dark'?'#eee':'#111';},theme);
        await page.screenshot({path:path.join(output,theme+'.png')});
      }
    }
    report.cleanup=await page.evaluate(async()=>{
      controllers.forEach(c=>c.destroy());await delay(30);
      return {pending:pendingFrames.size,listeners:Object.fromEntries([...listeners].filter(([,values])=>values.size).map(([key,values])=>[key,values.size])),mascots:document.querySelectorAll('.doc-mascot').length};
    });
    report.fallback=await page.evaluate(async()=>{
      const oldAnimations=Element.prototype.getAnimations,oldAnimate=Element.prototype.animate;
      Element.prototype.getAnimations=undefined;Element.prototype.animate=undefined;
      const c=make('fallback');c.setState('warning');c.setState('ready');const accepted=c.celebrate();
      window.dispatchEvent(new PointerEvent('pointermove',{clientX:50,clientY:40,pointerType:'mouse'}));await delay(200);
      const result={accepted,state:c.element.dataset.state,tracking:c.element.dataset.tracking};
      c.destroy();Element.prototype.getAnimations=oldAnimations;Element.prototype.animate=oldAnimate;return result;
    });
    report.transitions.forEach(step=>{
      assert.ok(step.startBridges>0, `${step.from} → ${step.to}: Web Animation bridge did not execute`);
      assert.ok(step.startDelta<.025, `${step.from} → ${step.to}: initial pose jumped ${step.startDelta}px`);
    });
    passed('state changes start at the displayed pose');
    report.endpoints.forEach(step=>assert.ok(step.maxDelta<.025, `${step.from} → ${step.to}: animation endpoint differs by ${step.maxDelta}`));
    passed('bridge endpoints meet the continuing CSS animation');
    report.interrupted.forEach(step=>assert.ok(step.delta<.025, `${step.to}: interrupted pose jumped ${step.delta}px`));
    const celebration=report.interrupted.find(step=>step.to==='celebrate');
    assert.equal(celebration.accepted,true); assert.equal(celebration.repeat,false);
    assert.ok(report.expression.closure<=.1,'eye-shape swap should happen inside a blink');
    assert.ok(report.expression.open>0&&report.expression.open<1);
    assert.ok(report.expression.cross>0&&report.expression.cross<1);
    passed('rapid state changes and airborne celebration interruptions remain continuous');
    assert.equal(report.pointer.listenersCount['window:pointermove'],1);
    assert.equal(report.pointer.scheduled,1);assert.equal(report.pointer.settled,0);
    assert.equal(report.pointer.before,report.pointer.immediate);
    assert.ok(Math.abs(parseFloat(report.pointer.handoff))<Math.abs(parseFloat(report.pointer.before)));
    assert.ok(Math.abs(parseFloat(report.pointer.handoff))>0);
    assert.equal(report.pointer.rest,'');assert.equal(report.pointer.restFrames,0);
    passed('separate script copies share pointer work and ease back to rest');
    assert.equal(report.reduced.animations,0);assert.equal(report.reduced.tracking,null);assert.equal(report.reduced.gaze,'');
    passed('reduced motion clears animations and gaze');
    report.colors.forEach(color=>{const expected=color.theme==='light'?'oklch(1 0 0)':'oklch(0.205 0 0)';assert.equal(color.fill,expected);assert.equal(color.stroke,expected);});
    passed('all state eye colors match their light or dark theme');
    assert.deepEqual(report.cleanup,{pending:0,listeners:{},mascots:0});
    passed('destroy removes global listeners and scheduled frames');
    assert.deepEqual(report.fallback,{accepted:true,state:'ready',tracking:'true'});
    assert.deepEqual(report.errors,[]);
    passed('state changes and pointer following work without Web Animations');
    report.panel = await checkPanel(browser);
    console.log(`All 8 motion checks and 4 preview scenarios passed in Chrome ${browser.version()}.`);
  } finally {
    if (output) fs.writeFileSync(path.join(output,'report.json'),JSON.stringify(report,null,2)+'\n');
    await browser.close();
  }
})().catch(error=>{console.error(error);process.exitCode=1;});
