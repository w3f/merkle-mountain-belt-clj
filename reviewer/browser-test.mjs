// Optional browser regression: install Playwright in your preferred test environment.
// MMB_PLAYWRIGHT_MODULE and MMB_BROWSER_EXECUTABLE allow an existing installation.
import assert from 'node:assert/strict';
import {readFileSync, mkdirSync} from 'node:fs';
const {chromium}=await import(process.env.MMB_PLAYWRIGHT_MODULE || 'playwright');
const browser=await chromium.launch({headless:true,
  ...(process.env.MMB_BROWSER_EXECUTABLE?{executablePath:process.env.MMB_BROWSER_EXECUTABLE}:{}),
  args:process.env.MMB_BROWSER_ARGS?JSON.parse(process.env.MMB_BROWSER_ARGS):[]});
const html=readFileSync(new URL('../docs/index.html',import.meta.url),'utf8');
const errors=[];
const screenshotDir=process.env.MMB_SCREENSHOTS;
if(screenshotDir)mkdirSync(screenshotDir,{recursive:true});
async function exercise(page,extended=false){
  assert.equal(await page.locator('#state-n').textContent(),'11');
  assert.match(await page.locator('#state-reference').textContent(),/root and append hash count match at n = 11/);
  assert.equal(await page.locator('#show-membership').isChecked(),false);
  const initialStages=await page.locator('.transition-stage').evaluateAll(nodes=>nodes.map(node=>({x:node.getBoundingClientRect().x,y:node.getBoundingClientRect().y})));
  assert.equal(initialStages[0].y,initialStages[1].y,'Small collapsed MMBs fit side by side');
  assert.ok(await page.locator('#proof-panel').isHidden());
  assert.equal(await page.locator('#graph [data-leaf]').count(),0);
  assert.deepEqual(await page.locator('#graph .peak-height').allTextContents(),['3','1','0']);
  assert.deepEqual(await page.locator('#graph-before .peak-height').allTextContents(),['2','2','1']);
  assert.equal(await page.locator('#before-n').textContent(),'10');
  assert.equal(await page.locator('#after-n').textContent(),'11');
  assert.equal(await page.locator('#graph [data-kind="range"] polygon').count(),3);
  assert.equal(await page.locator('#graph [data-kind="belt"] circle.node-shape').count(),2);
  assert.equal(await page.locator('#graph .absent-input').count(),3);
  assert.equal(await page.locator('#graph .on-path, #graph .proof-sibling').count(),0);
  await page.locator('#append').click();assert.equal(await page.locator('#state-n').textContent(),'12');
  await page.locator('#back').click();assert.equal(await page.locator('#state-n').textContent(),'11');
  await page.locator('#show-membership').check();
  assert.ok(await page.locator('#proof-panel').isVisible());
  assert.equal(await page.locator('#graph [data-leaf]').count(),1);
  assert.equal(await page.locator('#graph-before [data-leaf="6"]').count(),1);
  assert.equal(await page.locator('#graph [data-leaf="6"]').count(),1);
  assert.equal(await page.locator('#graph [data-kind="peak"] polygon').count(),3);
  assert.equal(await page.locator('#graph-before [data-kind="peak"] polygon').count(),3);
  assert.ok(await page.locator('#graph .on-path').count()>0);
  await page.locator('#leaf-select').selectOption('1');
  await page.locator('#verify').click();assert.match(await page.locator('#proof-result').textContent(),/accepted/);
  await page.locator('#tamper').check();await page.locator('#verify').click();assert.match(await page.locator('#proof-result').textContent(),/rejected/);
  await page.locator('#graph [data-span="5,8"][data-collapsed]').press('Enter');assert.equal(await page.locator('#leaf-select').inputValue(),'5');
  assert.equal(await page.locator('#tamper').isChecked(),false);
  assert.equal(await page.locator('#graph .proof-sibling').count(),4);
  assert.equal(await page.locator('#graph [data-kind="internal"][data-collapsed] polygon').count(),0);
  assert.equal(await page.locator('#graph [data-span="7,8"] rect.node-shape').count(),1);
  assert.equal(await page.locator('#graph [data-span="7,8"] .node-height').textContent(),'1');
  assert.equal(await page.locator('#graph [data-span="7,8"] .subtree-marker').count(),1);
  assert.equal(await page.locator('#graph [data-span="6,6"] .subtree-marker').count(),0,'A leaf has no hidden descendants');
  for(const leaf of ['5','6']){
    assert.equal(await page.locator(`#graph [data-span="${leaf},${leaf}"] .node-height`).textContent(),'0');
    assert.equal(await page.locator(`#graph [data-span="${leaf},${leaf}"] .leaf-value-label`).textContent(),`leaf ${leaf}`);
  }
  if(screenshotDir)await page.screenshot({path:`${screenshotDir}/membership.png`,fullPage:true});
  await page.locator('#show-membership').uncheck();
  assert.equal(await page.locator('#graph [data-leaf], #graph .on-path, #graph .proof-sibling').count(),0);
  assert.ok(await page.locator('#proof-panel').isHidden());
  await page.locator('#show-membership').check();
  assert.equal(await page.locator('#leaf-select').inputValue(),'5');
  await page.locator('#leaf-select').selectOption('11');
  assert.equal(await page.locator('#graph-before .on-path, #graph-before .proof-sibling, #graph-before [data-leaf]').count(),0);
  assert.match(await page.locator('#before-proof-note').textContent(),/introduced by this append/);
  assert.equal(await page.locator('#graph [data-leaf="11"] polygon').count(),1,'Selected height-zero peak stays triangular');
  await page.locator('#reset').click();assert.equal(await page.locator('#state-n').textContent(),'0');assert.ok(await page.locator('#back').isDisabled());
  assert.equal(await page.locator('#state-reference').textContent(),'No commitment to compare.');
  assert.ok(await page.locator('#verify').isDisabled());
  await page.locator('#append').click();await page.locator('#verify').click();assert.match(await page.locator('#proof-result').textContent(),/accepted/);
  assert.ok(await page.locator('#tamper').isDisabled());
  // Check every transition in both modes, alternating old and newly appended leaves.
  const states=JSON.parse(await page.locator('#artifact-data').textContent()).states;
  for(let n=0;n<=64;n++){
    await page.locator('#position').evaluate((el,n)=>{el.value=n;el.dispatchEvent(new Event('input',{bubbles:true}));},n);
    const leaf=n%2?1:n;
    if(n)await page.locator('#leaf-select').selectOption(String(leaf));
    assert.equal(await page.locator('#before-n').textContent(),String(Math.max(0,n-1)));
    assert.equal(await page.locator('#after-n').textContent(),String(n));
    assert.equal(await page.locator('#graph [data-leaf]').count(),n?1:0);
    assert.equal(await page.locator('#graph-before [data-leaf]').count(),n>1&&leaf<n?1:0);
    assert.equal(await page.locator('#graph .proof-sibling').count(),n?states[n-1].proofs[leaf-1].siblings.length:0);
    assert.equal(await page.locator('#graph-before .proof-sibling').count(),n>1&&leaf<n?states[n-2].proofs[leaf-1].siblings.length:0);
    assert.equal(await page.locator('#graph').getAttribute('height'),await page.locator('#graph-before').getAttribute('height'));
    assert.equal(await page.locator('#graph').getAttribute('viewBox'),await page.locator('#graph-before').getAttribute('viewBox'),'Before and after use the same scale');
    assert.equal(await page.locator('#graph [data-kind="peak"] polygon').count(),n?states[n-1].peaks.length:0);
    for(const graph of ['#graph','#graph-before']){
      assert.equal(await page.locator(`${graph} [data-collapsed]:not([data-kind="peak"]) polygon`).count(),0,'Only peaks use triangles');
      const collapsed=await page.locator(`${graph} [data-collapsed]:not([data-kind="peak"])`).evaluateAll(nodes=>nodes.map(node=>({
        span:node.dataset.span.split(',').map(Number),square:!!node.querySelector('rect.node-shape'),marker:!!node.querySelector('.subtree-marker')
      })));
      for(const node of collapsed){assert.ok(node.square);assert.equal(node.marker,node.span[0]<node.span[1]);}
      const mountainNodes=await page.locator(`${graph} [data-node-height]`).evaluateAll(nodes=>nodes.map(node=>({
        kind:node.dataset.kind,span:node.dataset.span.split(',').map(Number),
        height:node.querySelector('.node-height').textContent,
        square:!!node.querySelector('rect.node-shape'),circle:!!node.querySelector('circle.node-shape'),
        value:node.querySelector('.leaf-value-label')?.textContent||null
      })));
      for(const node of mountainNodes){
        assert.equal(node.height,String(Math.log2(node.span[1]-node.span[0]+1)),'Node text is its height');
        if(node.kind!=='peak'){assert.ok(node.square);assert.ok(!node.circle);}
        assert.equal(node.value,node.span[0]===node.span[1]?`leaf ${node.span[0]}`:null,'Leaf value is separate from node height');
      }
    }
    await page.locator('#show-membership').uncheck();
    assert.deepEqual(await page.locator('#graph .peak-height').allTextContents(),n?states[n-1].peaks.map(String):[]);
    assert.equal(await page.locator('#graph [data-kind="range"]').count(),n?states[n-1].peaks.length:0);
    assert.equal(await page.locator('#graph [data-kind="belt"]').count(),n?states[n-1].ranges.length:0);
    assert.deepEqual(await page.locator('#graph-before .peak-height').allTextContents(),n>1?states[n-2].peaks.map(String):[]);
    assert.equal(await page.locator('#graph [data-leaf], #graph .on-path, #graph .proof-sibling').count(),0);
    await page.locator('#show-membership').check();
  }
  assert.ok(await page.locator('#append').isEnabled(),'Appending is not limited by the 64 reference states');
  await page.locator('#append').click();assert.equal(await page.locator('#state-n').textContent(),'65');
  const reference=JSON.parse(await page.locator('#artifact-data').textContent());
  assert.equal(await page.locator('#root-hash').textContent(),reference.liveReference.checkpoints.find(s=>s.n===65).root);
  await page.locator('#target-n').fill('1337');await page.locator('#compute').click();
  await page.waitForFunction(()=>document.getElementById('state-n').textContent==='1337');
  assert.equal(await page.locator('#root-hash').textContent(),reference.liveReference.checkpoints.find(s=>s.n===1337).root);
  assert.equal(await page.locator('#history [data-position]').count(),64,'History remains readable beyond the reference range');
  assert.match(await page.locator('#history-caption').textContent(),/Appends 1274–1337 of 1337/);
  await page.locator('#leaf-number').fill('1337');await page.locator('#verify').click();
  assert.match(await page.locator('#proof-result').textContent(),/accepted: Keccak-256/);
  assert.ok(await page.locator('#transition-grid').evaluate(el=>el.classList.contains('stacked')),'Wide diagrams stack on desktop');
  const wideStages=await page.locator('.transition-stage').evaluateAll(nodes=>nodes.map(node=>({x:node.getBoundingClientRect().x,y:node.getBoundingClientRect().y})));
  assert.equal(wideStages[0].x,wideStages[1].x);assert.ok(wideStages[1].y>wideStages[0].y);
  await page.locator('#history-range').selectOption('all');
  assert.match(await page.locator('#history-caption').textContent(),/Appends 1–1337 of 1337/);
  const groups=await page.locator('#history [data-first]').evaluateAll(nodes=>nodes.map(node=>({first:+node.dataset.first,last:+node.dataset.last,peak:+node.dataset.position})));
  let covered=0;for(const group of groups){assert.equal(group.first,covered+1);assert.ok(group.peak>=group.first&&group.peak<=group.last);covered=group.last;}
  assert.equal(covered,1337,'All-appends plot covers the entire computed history');
  const firstMaximum=groups[0].peak;
  assert.equal(reference.states[firstMaximum-1].hashes,Math.max(...reference.states.slice(groups[0].first-1,groups[0].last).map(s=>s.hashes)));
  await page.locator('#history [data-first]').first().press('Enter');
  assert.equal(await page.locator('#state-n').textContent(),String(firstMaximum),'Grouped history selects its maximum');
  await page.locator('#compute').click();await page.waitForFunction(()=>document.getElementById('state-n').textContent==='1337');
  await page.locator('#history-range').selectOption('64');
  await page.locator('#tamper').check();await page.locator('#verify').click();
  assert.match(await page.locator('#proof-result').textContent(),/rejected/);
  if(extended){
    await page.locator('#target-n').fill('100000');await page.locator('#compute').click();
    await page.locator('#cancel-compute').click();
    await page.waitForFunction(()=>document.getElementById('compute-status').textContent.startsWith('Stopped'));
    assert.ok(Number(await page.locator('#state-n').textContent())<100000,'Long computation can be cancelled');
    await page.locator('#compute').click();
    await page.waitForFunction(()=>document.getElementById('state-n').textContent==='100000',null,{timeout:180000});
    assert.equal(await page.locator('#root-hash').textContent(),reference.liveReference.rootsHex.slice(99999*64,100000*64));
    assert.equal(await page.locator('#hash-now').textContent(),String(reference.liveReference.hashCounts[99999]));
    assert.match(await page.locator('#state-reference').textContent(),/root and append hash count match at n = 100000/);
    assert.ok(await page.locator('#append').isDisabled(),'Interactive resource limit enforced');
    assert.equal(await page.locator('#leaf-select option').count(),0,'Large states do not create one option per leaf');
    for(const leaf of ['1','50000','100000']){
      await page.locator('#leaf-number').fill(leaf);await page.locator('#verify').click();
      assert.match(await page.locator('#proof-result').textContent(),/accepted: Keccak-256/);
      assert.equal(await page.locator('#graph [data-leaf]').getAttribute('data-leaf'),leaf);
      assert.ok(await page.locator('#graph .graph-node').count()<150,'Large proof renders only visible nodes');
    }
    await page.locator('#leaf-number').fill('100001');await page.locator('#verify').click();
    assert.equal(await page.locator('#leaf-number').inputValue(),'100000','Out-of-range leaf input restores the selection');
    await page.locator('#history [data-position="99999"]').press('Enter');
    assert.equal(await page.locator('#state-n').textContent(),'99999','History uses actual append indices');
    await page.locator('#append').click();assert.equal(await page.locator('#state-n').textContent(),'100000');
    await page.locator('#history-range').selectOption('all');
    assert.match(await page.locator('#history-caption').textContent(),/Appends 1–100000 of 100000/);
    assert.ok(await page.locator('#history [data-first]').count()<=256,'Overview remains bounded in display elements, not covered appends');
    await page.locator('#show-membership').uncheck();
    assert.ok(await page.locator('#transition-grid').evaluate(el=>el.classList.contains('stacked')),'Large collapsed MMBs stack too');
    if(screenshotDir)await page.screenshot({path:`${screenshotDir}/large.png`,fullPage:true});
    await page.setViewportSize({width:390,height:844});
    assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth),'Large state has no mobile page overflow');
    if(screenshotDir)await page.screenshot({path:`${screenshotDir}/large-mobile.png`,fullPage:true});
    await page.setViewportSize({width:1440,height:1050});
    await page.locator('#history-range').selectOption('64');
  }
  await page.locator('#checks-tab').click();
  for(const limit of ['8','16','32','64','128']){
    await page.locator('#check-limit').selectOption(limit);await page.locator('#run-checks').click();
    await page.waitForFunction(()=>!document.getElementById('run-checks').disabled);
    assert.match(await page.locator('#check-status').textContent(),/7 \/ 7 checks passed/);
    assert.equal(await page.locator('.check-card').count(),7);
    assert.equal(await page.locator('.check-card').last().locator('.check-value').textContent(),`✓ ${limit} counts; ${limit} roots`);
  }
  for(const k of [...Array.from({length:16},(_,i)=>i+1),32,64,128,256]){
    await page.locator('#recency').selectOption(String(k));
    assert.ok(!(await page.locator('#amortized-values').textContent()).includes('✕'));
  }
  if(screenshotDir)await page.screenshot({path:`${screenshotDir}/checks.png`,fullPage:true});
  await page.locator('#explorer-tab').click();
  await page.locator('#show-membership').uncheck();
  await page.locator('#position').evaluate(el=>{el.value=11;el.dispatchEvent(new Event('input',{bubbles:true}));});
}
try{
  const context=await browser.newContext({viewport:{width:1440,height:1050},offline:true});
  const page=await context.newPage();page.on('pageerror',e=>errors.push(e.message));
  const requests=[];page.on('request',r=>requests.push(r.url()));
  await page.goto(new URL('../docs/index.html',import.meta.url).href);
  await exercise(page,true);
  assert.equal(requests.filter(url=>/^https?:/.test(url)).length,0,'No network requests from the artifact');
  if(screenshotDir)await page.screenshot({path:`${screenshotDir}/explorer.png`,fullPage:true});
  await page.setViewportSize({width:390,height:844});
  assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth),'No mobile page overflow');
  assert.ok(await page.locator('#graph-viewport').evaluate(el=>el.scrollWidth<=el.clientWidth),'Three collapsed mountains fit without mobile scrolling');
  if(screenshotDir)await page.screenshot({path:`${screenshotDir}/mobile.png`,fullPage:true});
  await page.locator('#show-membership').check();
  assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=window.innerWidth),'No mobile page overflow with membership paths');
  // Fulfill locally: no server/network required. Use the exact anonymous Pages sandbox policy.
  await context.setOffline(false);
  let servedHTML=html;
  await context.route('**/*',async route=>{
    assert.equal(route.request().url(),'https://mmb-artifact.invalid/');
    await route.fulfill({status:200,headers:{'content-type':'text/html','content-security-policy':'sandbox allow-scripts allow-popups allow-forms allow-modals'},body:servedHTML});
  });
  const sandboxPage=await context.newPage();sandboxPage.on('pageerror',e=>errors.push(e.message));
  await sandboxPage.setViewportSize({width:1280,height:1000});
  await sandboxPage.goto('https://mmb-artifact.invalid/');await exercise(sandboxPage);
  // A corrupted reference must be reported, not used to supply the live root.
  for(const [n,field] of [[11,'root'],[4097,'root'],[4097,'count']]){
    const fixture=JSON.parse(html.match(/<script id="artifact-data" type="application\/json">([\s\S]*?)<\/script>/)[1]);
    const refs=fixture.liveReference,offset=(n-1)*64,expectedRoot=refs.rootsHex.slice(offset,offset+64);
    if(field==='root')refs.rootsHex=refs.rootsHex.slice(0,offset)+'0'.repeat(64)+refs.rootsHex.slice(offset+64);
    else refs.hashCounts[n-1]=(refs.hashCounts[n-1]+1)%6;
    servedHTML=html.replace(/(<script id="artifact-data" type="application\/json">)[\s\S]*?(<\/script>)/,(_,a,b)=>a+JSON.stringify(fixture)+b);
    await sandboxPage.reload();
    if(n!==11){
      await sandboxPage.locator('#target-n').fill(String(n));await sandboxPage.locator('#compute').click();
      await sandboxPage.waitForFunction(n=>document.getElementById('state-n').textContent===String(n),n);
    }
    assert.equal(await sandboxPage.locator('#root-hash').textContent(),expectedRoot,'Displayed root is independently computed');
    assert.match(await sandboxPage.locator('#state-reference').textContent(),new RegExp(`mismatch at n = ${n}`),`Corrupt ${field} detected in reference comparison`);
    await sandboxPage.locator('#checks-tab').click();await sandboxPage.locator('#run-checks').click();
    await sandboxPage.waitForFunction(()=>!document.getElementById('run-checks').disabled);
    assert.match(await sandboxPage.locator('#check-status').textContent(),n===11?/6 \/ 7 checks passed/:/7 \/ 7 checks passed/,'Prefix check reports only mismatches within its selected range');
  }
  await context.close();
  assert.deepEqual(errors,[],'No browser exceptions');
  console.log('Browser checks passed: live computation through 100000 leaves, stacked large diagrams, sparse proof rendering, full-history coverage, cancellation, Keccak verification and tampering, recency through 256, reference mismatch detection, offline/mobile, and anonymous Pages CSP.');
}finally{await browser.close();}
