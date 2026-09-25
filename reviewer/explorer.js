(() => {
  'use strict';
  const reference = JSON.parse(document.getElementById('artifact-data').textContent);
  const engine = new MMBLive.Engine();
  const $ = id => document.getElementById(id);
  const esc = value => String(value).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const span = pair => pair[0] === pair[1] ? String(pair[0]) : `${pair[0]}–${pair[1]}`;
  const mean = values => values.length ? values.reduce((a,b)=>a+b,0)/values.length : 0;
  const matchesReference = state => state.n<=reference.liveReference.maxN
    &&state.root===reference.liveReference.rootsHex.slice((state.n-1)*64,state.n*64)
    &&state.hashes===reference.liveReference.hashCounts[state.n-1];
  let position = 11;
  let selectedLeaf = 6;
  let busy=false,cancelled=false;
  let diagramWidth=360;
  const current = () => engine.ensure(position);
  const yieldUI = () => new Promise(resolve=>setTimeout(resolve,0));

  function proofForSelection() {
    return engine.proof(position,selectedLeaf);
  }

  function displayedProof() {
    const proof=proofForSelection();
    if(proof&&$('tamper').checked&&proof.siblingDigests.length){
      const digest=proof.siblingDigests[0];
      proof.siblingDigests[0]=(digest[0]==='0'?'1':'0')+digest.slice(1);
    }
    return proof;
  }

  function updateTransitionLayout(){
    const available=$('transition-grid').clientWidth;
    if(available)$('transition-grid').classList.toggle('stacked',2*(diagramWidth+16)>available);
  }

  function renderGraph() {
    const membership=$('show-membership').checked;
    const focus=membership?selectedLeaf:null;
    const state=engine.snapshot(position,focus), before=engine.snapshot(Math.max(0,position-1),focus);
    const previous=buildPaperStructure(before), next=buildPaperStructure(state);
    const views=[focusPaperStructure(previous,membership?selectedLeaf:null),focusPaperStructure(next,membership?selectedLeaf:null)];
    const depth=Math.max(0,...views.map(view=>view.mountains.find(peak=>view.selected!==null&&peak.lo<=view.selected&&peak.hi>=view.selected)?.height||0));
    const height=275+depth*62;
    const slots=Math.max(1,...views.map(view=>view.nodes.filter(node=>!['range','belt'].includes(node.kind)&&(node.collapsed||node.lo===node.hi)).length));
    const width=Math.max(360,slots*84+80),minWidth=Math.max(260,slots*54+50);
    $('proof-panel').hidden=!membership;
    $('explorer-grid').classList.toggle('with-proof',membership);
    diagramWidth=width;updateTransitionLayout();
    $('sibling-legend').hidden=!membership;
    $('collapsed-legend').hidden=!membership;
    $('mountain-node-legend').hidden=!membership;
    $('graph-heading').textContent=position?`Append ${position-1} → ${position}`:'Empty MMB';
    $('before-n').textContent=Math.max(0,position-1);$('after-n').textContent=position;
    $('before-unit').textContent=position===2?'leaf':'leaves';$('after-unit').textContent=position===1?'leaf':'leaves';
    $('graph-mode-description').textContent=membership?'Only the selected leaf’s route is expanded.':'Mountains are collapsed to their peaks.';
    $('graph-note').textContent=membership
      ? 'Triangles mark peaks; squares mark nodes within mountains. Their numbers are heights. Leaf values appear below height-zero nodes; ellipses mark collapsed subtrees.'
      : 'Triangle numbers are mountain heights. Gray peaks show the merge inputs and result. Black dots mark absent inputs (no hash).';
    const sequence=s=>`S(${s?.n||0}) = [${s?.peaks.join(', ')||''}] · ${s?.ranges.length||0} ranges`;
    $('before-sequence').textContent=sequence(before);$('after-sequence').textContent=sequence(state);
    for(const [name,s] of [['before',before],['after',state]]){
      const note=$(`${name}-proof-note`),proof=s?engine.proof(s.n,selectedLeaf):null;
      note.hidden=!membership;
      note.textContent=!state?'No leaf selected.':proof?`Leaf ${selectedLeaf} · ${proof.siblings.length} sibling hashes`
        :`Leaf ${selectedLeaf} is introduced by this append.`;
    }
    const previousValues=new Set(previous.nodes.map(node=>`${node.kind}:${node.digest}`));
    const fresh=new Set(state?.events.map(event=>event.result)||[]);
    const mergePeak=state?.mergedDigest?next.mountains.find(peak=>peak.digest===state.mergedDigest):null;
    drawStructure($('graph-before'),before,views[0],{width,minWidth,height,membership,after:false,previousValues,fresh,mergePeak});
    drawStructure($('graph'),state,views[1],{width,minWidth,height,membership,after:true,previousValues,fresh,mergePeak});
  }

  function drawStructure(svg,state,graph,{width,minWidth,height,membership,after,previousValues,fresh,mergePeak}) {
    if(!state){
      svg.setAttribute('viewBox',`0 0 ${width} ${height}`);svg.setAttribute('width',String(width));svg.setAttribute('height',String(height));
      svg.style.width='100%';svg.style.minWidth='0';
      svg.innerHTML=`<text x="${width/2}" y="125" text-anchor="middle" fill="#444" font-size="18">Empty MMB</text><text x="${width/2}" y="150" text-anchor="middle" fill="#666" font-size="12">No leaves have been appended.</text>`;
      return;
    }
    const nodes=[...graph.nodes],byId=new Map(nodes.map(node=>[node.id,node]));
    const isBag=node=>['range','belt'].includes(node.kind);
    const terminals=nodes.filter(node=>!isBag(node)&&(node.collapsed||node.lo===node.hi)).sort((a,b)=>a.lo-b.lo);
    const coords=new Map(),children=new Map();
    for(const [parent,child] of graph.edges)children.set(parent,[...(children.get(parent)||[]),child]);
    const terminalX=new Map(terminals.map((node,i)=>[node.id,width/2+(i-(terminals.length-1)/2)*84]));
    function point(node){
      if(coords.has(node.id))return coords.get(node.id);
      let location;
      if(isBag(node))location={x:point(graph.mountains[node.column]).x,y:node.kind==='belt'?45:115};
      else{
        const peak=graph.mountains.find(peak=>peak.lo<=node.lo&&peak.hi>=node.hi);
        const cs=children.get(node.id)||[];
        const x=terminalX.has(node.id)?terminalX.get(node.id):mean(cs.map(id=>point(byId.get(id)).x));
        location={x,y:195+(peak.height-Math.log2(node.hi-node.lo+1))*62};
      }
      coords.set(node.id,location);return location;
    }
    nodes.forEach(point);
    nodes.sort((a,b)=>point(a).y-point(b).y||point(a).x-point(b).x);
    svg.setAttribute('viewBox',`0 0 ${width} ${height}`);svg.setAttribute('width',String(width));svg.setAttribute('height',String(height));
    svg.style.width='100%';svg.style.minWidth=`${minWidth}px`;
    const proof=membership?engine.proof(state.n,selectedLeaf):null;
    const siblings=new Set(proof?.siblings.map(s=>s.join(','))||[]);
    const onPath=node=>graph.selected!==null&&node.lo<=selectedLeaf&&node.hi>=selectedLeaf;
    let html=`<title>${after?'After':'Before'} append: MMB at ${state.n} leaves. ${proof?'Only the selected membership route is expanded; peaks remain triangular.':'Mountains collapsed to height-numbered peak triangles.'}</title>`;
    for(const [parent,child] of graph.edges){
      const a=coords.get(parent),b=coords.get(child),active=onPath(byId.get(parent))&&onPath(byId.get(child));
      html+=`<path class="graph-edge${active?' on-path':''}" d="M${a.x},${a.y} L${b.x},${b.y}"/>`;
    }
    for(const node of nodes){
      const {x,y}=point(node),peak=node.kind==='peak',bag=isBag(node);
      const triangle=peak;
      const hiddenSubtree=node.collapsed&&!peak&&node.lo<node.hi;
      const leaf=!bag&&node.lo===node.hi;
      const leafLabelWidth=Math.max(52,String(node.lo).length*6+42);
      const selected=!bag&&graph.selected!==null&&node.lo===selectedLeaf&&node.hi===selectedLeaf;
      const sibling=!node.identity&&siblings.has([node.lo,node.hi].join(','));
      const updated=after&&!previousValues.has(`${node.kind}:${node.digest}`);
      const mergeInput=!after&&peak&&mergePeak&&node.lo>=mergePeak.lo&&node.hi<=mergePeak.hi;
      let fill='#fff';
      if(bag&&(updated||node.id===graph.root))fill='#cda34e';
      else if(after&&!bag&&node.lo===position&&node.hi===position)fill='#80ab83';
      else if(!bag&&((after&&(fresh.has(node.digest)||node.digest===state.mergedDigest))||mergeInput))fill='#c6c6c6';
      if(sibling)fill='#f4d993';if(selected)fill='#397b91';
      const attrs=`class="node-shape" fill="${fill}" stroke="${selected?'#286781':'#272d29'}" stroke-width="1.7"`;
      const shape=triangle?`<polygon ${attrs} points="0,-22 -27,22 27,22"/>`
        :node.kind==='range'?`<polygon ${attrs} points="0,-16 16,0 0,16 -16,0"/>`
        :node.kind==='belt'?`<circle ${attrs} r="16"/>`
        :`<rect ${attrs} x="-14" y="-14" width="28" height="28"/>`;
      const nodeHeight=!bag?Math.log2(node.hi-node.lo+1):null;
      const label=!bag?String(nodeHeight):'';
      const target=membership&&!bag?(selected?selectedLeaf:node.collapsed?node.lo:null):null;
      const action=target!==null?` data-select-leaf="${target}" tabindex="0" role="button" aria-label="Prove leaf ${target}${node.collapsed?' in subtree '+span([node.lo,node.hi]):''}"`:'';
      const explanation=node.identity?'Identity bagging node: absent left input, reuses right child (0 hashes).'
        :after&&fresh.has(node.digest)?'Hashed during this append.'
        :after&&!bag&&node.digest===state.mergedDigest?'Merged peak: adopts the range node that already hashed this pair (0 hashes).'
        :mergeInput?'Input mountain to this append’s merge.':'';
      const collapsedMarker=hiddenSubtree?'<g class="subtree-marker" aria-hidden="true"><path d="M0,17 L0,25" stroke="#67736d" stroke-width="1.2" stroke-dasharray="2 2"/><text y="38" text-anchor="middle" font-size="19" fill="#67736d">…</text></g>':'';
      const leafValue=membership&&leaf?`<g class="leaf-value" data-leaf-value="${node.lo}"><path d="M0,${peak?23:15} L0,33" stroke="#9ca99e" stroke-width="1"/><rect x="${-leafLabelWidth/2}" y="33" width="${leafLabelWidth}" height="21" rx="4" fill="#f8f9f3" stroke="#dce1d6"/><text class="leaf-value-label" y="47" text-anchor="middle" font-size="11" fill="${selected?'#286781':'#596a60'}">leaf ${node.lo}</text></g>`:'';
      html+=`<g class="graph-node${sibling?' proof-sibling':''}${onPath(node)?' path-node':''}" data-kind="${node.kind}" data-node="${esc(node.id)}" data-span="${node.lo},${node.hi}"${!bag?` data-node-height="${nodeHeight}"`:''}${peak?` data-peak-height="${nodeHeight}"`:''}${node.collapsed?' data-collapsed="true"':''}${node.identity?' data-identity="true"':''}${selected?` data-leaf="${selectedLeaf}"`:''}${action} transform="translate(${x},${y})"><title>${esc(node.kind)}${!bag?' height '+nodeHeight:''}, leaves ${span([node.lo,node.hi])}\nKeccak-256: ${node.digest}\n${explanation}${hiddenSubtree?'\nCollapsed subtree. Select to follow leaf '+node.lo+'.':''}${node.id===graph.root?'\nMMB root.':''}</title>${shape}${collapsedMarker}${label?`<text class="node-height${peak?' peak-height':''}" text-anchor="middle" y="${triangle?12:6}" fill="${selected?'#fff':'#202924'}" font-size="${triangle?19:17}">${label}</text>`:''}${leafValue}${node.identity?'<circle class="absent-input" cx="-16" r="4" fill="#202924"/>':''}${node.id===graph.root?'<text x="0" y="-26" text-anchor="middle" fill="#6e5930" font-size="10">root</text>':''}</g>`;
    }
    svg.innerHTML=html;
  }

  function renderProof() {
    const proof=displayedProof();
    $('proof-length').textContent=proof ? `${proof.siblings.length} hashes` : '—';
    $('proof-prediction').textContent=proof ? `Formula: ${proof.expectedSize}` : '';
    let acc=[selectedLeaf,selectedLeaf];
    $('proof-steps').innerHTML=(proof?.siblings||[]).map((s,i)=>{
      const side=s[1]<acc[0]?'left':'right';
      const row=`<li title="Keccak-256: ${proof.siblingDigests[i]}"><span class="step-number">${i+1}</span><strong>[${span(s)}]</strong><span>${side} sibling</span></li>`;
      acc=[Math.min(acc[0],s[0]),Math.max(acc[1],s[1])];return row;
    }).join('') || (proof?'<li>The leaf is already the root.</li>':'<li>No leaves yet.</li>');
    $('verify').disabled=!proof;
    $('tamper').disabled=!proof?.siblings.length;
    $('proof-result').className='result muted';
    $('proof-result').textContent=proof ? 'Ready to recompute the root and check interval coverage.' : 'Append a leaf to inspect its path.';
  }

  function renderHistory() {
    const range=$('history-range').value;
    const width=700,height=150,base=119,count=Math.min(range==='all'?Infinity:Number(range),Math.max(1,engine.n)),step=650/count;
    const first=Math.max(1,Math.min(position-Math.floor((count-1)/2),engine.n-count+1)),last=first+count-1;
    const groupSize=Math.ceil(count/256);
    let html='<title>Structural hashes per append, excluding the leaf hash</title><line x1="30" y1="24" x2="683" y2="24" stroke="#999" stroke-dasharray="4 4"/><text x="10" y="28" font-size="9" fill="#666">5</text><text x="10" y="123" font-size="9" fill="#666">0</text>';
    if(groupSize===1)engine.states.slice(first-1,last).forEach((state,i)=>{
      const x=30+i*step, h=state.hashes*19;
      html+=`<g data-position="${state.n}" role="button" tabindex="0" aria-label="Inspect append ${state.n}, ${state.hashes} structural hashes"><title>n = ${state.n}: ${state.hashes} structural hashes + 1 leaf hash</title><rect x="${x}" y="${base-Math.max(3,h)}" width="${Math.max(.05,step*.7)}" height="${Math.max(3,h)}" fill="${state.n===position?'#397b91':state.n<position?'#707981':'#d7dade'}"/></g>`;
    });
    else for(let start=first;start<=last;start+=groupSize){
      const end=Math.min(last,start+groupSize-1);let low=Infinity,high=-1,peak=start;
      for(let n=start;n<=end;n++){const value=engine.states[n-1].hashes;low=Math.min(low,value);if(value>high){high=value;peak=n;}}
      const average=(engine.states[end-1].totalHashes-(engine.states[start-2]?.totalHashes||0))/(end-start+1);
      const x=30+(start-first)*step,barWidth=(end-start+1)*step*.8,active=start<=position&&position<=end;
      html+=`<g data-position="${peak}" data-first="${start}" data-last="${end}" role="button" tabindex="0" aria-label="Appends ${start} through ${end}: ${low} to ${high} structural hashes, mean ${average.toFixed(3)}. Inspect maximum at append ${peak}."><title>Appends ${start}–${end}: min ${low}, max ${high}, mean ${average.toFixed(3)} structural hashes. Select to inspect append ${peak}.</title><rect x="${x}" y="${base-high*19}" width="${barWidth}" height="${Math.max(2,(high-low)*19)}" fill="${active?'#b8d1da':'#d7dade'}"/><path d="M${x},${base-average*19} h${barWidth}" stroke="${active?'#397b91':'#596b77'}" stroke-width="2"/></g>`;
    }
    [...new Set([0,1,2,3,4].map(i=>first+Math.round((count-1)*i/4)))].forEach(n=>{html+=`<text x="${30+(n-first+.35)*step}" y="140" text-anchor="middle" font-size="9" fill="#666">${n}</text>`;});
    $('history').setAttribute('viewBox',`0 0 ${width} ${height}`);$('history').innerHTML=html;
    $('history-caption').textContent=(engine.n?`Appends ${first}–${last} of ${engine.n} computed. `+(groupSize===1?'Click a bar to inspect that append.':`Groups of up to ${groupSize} appends: shaded min–max range, dark mean. Select a group to inspect its maximum.`):'No appends computed yet.')+' Structural hashes only; the leaf hash is additional. Dashed line: the five-hash bound.';
  }

  function renderLedger() {
    const state=engine.snapshot(position,null);
    const comparison=$('state-reference'),matches=state&&matchesReference(state);
    comparison.className=`small ${state?(matches?'pass':'fail'):'muted'}`;
    comparison.textContent=!state?'No commitment to compare.':matches
      ?`Clojure reference: root and append hash count match at n = ${position}.`
      :`Clojure reference mismatch at n = ${position}: root or append hash count differs.`;
    if (!state) {$('ledger').innerHTML='<p class="small muted">No hashes have been performed.</p>';$('root-hash').textContent='No commitment yet.';return;}
    const nodes=new Map(state.nodes.map(n=>[n.digest,n]));
    $('ledger').innerHTML=state.events.map((event,i)=>{
      const node=nodes.get(event.result);
      const kind=node ? ({internal:'Mountain merge',peak:'Mountain merge',range:'Range bagging',belt:'Belt bagging'}[node.type] || 'Combine') : 'Intermediate combine';
      return `<div class="ledger-row"><span>${i+1}</span><strong>${kind}${node?' · '+span([node.lo,node.hi]):''}</strong><code title="${event.result}">${event.result.slice(0,8)}…</code></div>`;
    }).join('') || '<p class="small muted">No structural hash: the leaf itself is the commitment.</p>';
    $('ledger').innerHTML+=`<div class="ledger-row leaf-row"><span>+1</span>Leaf ${position} hash<code>${state.leafHash.slice(0,8)}…</code></div>`;
    $('root-hash').textContent=state.root;
  }

  function render() {
    const state=current();
    $('state-n').textContent=position;
    $('position').max=Math.max(64,engine.n);$('position').value=position;$('position-label').textContent=`${position} / ${Math.max(64,engine.n)}`;
    $('back').disabled=busy||position===0;$('reset').disabled=busy||position===0;$('append').disabled=busy||position===MMBLive.MAX_LEAVES;
    $('hash-now').textContent=state?.hashes ?? 0;$('leaf-cost').textContent=state?'+ 1 leaf hash':'No leaf hash';
    $('hash-max').textContent=state?.maxHashes||0;$('hash-mean').textContent=(position?state.totalHashes/position:0).toFixed(2);
    $('hash-total').textContent=`${state?.totalHashes||0} structural hashes in total`;
    $('append-case').textContent=state?({'no-merge':'No merge',fresh:'Fresh merge',delayed:'Delayed merge'}[state.case]):'Empty state';
    $('state-description').textContent=!state?'The first append creates a single leaf.':state.case==='no-merge'?'No mountain merge is scheduled on this append.':state.case==='fresh'?'The new leaf participates in the mountain merge.':'A pair of older mountains merges while the new leaf is appended.';
    const numericLeaf=position>256;
    $('leaf-select').hidden=numericLeaf;$('leaf-number').hidden=!numericLeaf;$('leaf-range').hidden=!numericLeaf;
    $('leaf-label').htmlFor=numericLeaf?'leaf-number':'leaf-select';
    $('leaf-number').max=position;$('leaf-number').value=selectedLeaf;
    $('leaf-range').textContent=`Leaf index: 1–${position}.`;
    $('leaf-select').innerHTML=numericLeaf?'':state?Array.from({length:state.n},(_,i)=>{const leaf=i+1;return `<option value="${leaf}"${leaf===selectedLeaf?' selected':''}>Leaf ${leaf}${leaf===position?' · newest':''}</option>`;}).join(''):'<option>No leaves</option>';
    $('leaf-select').disabled=!state;
    renderGraph();renderProof();renderHistory();renderLedger();
  }

  function setPosition(value) {
    if(busy)return;
    const target=Number(value);
    if(!Number.isSafeInteger(target)||target<0||target>MMBLive.MAX_LEAVES)return;
    engine.ensure(target);position=target;
    selectedLeaf=Math.max(1,Math.min(selectedLeaf,position));$('tamper').checked=false;render();
  }

  function setBusy(value){
    busy=value;
    for(const id of ['position','target-n','compute'])$(id).disabled=value;
    $('cancel-compute').hidden=!value;
    $('append').disabled=value||position===MMBLive.MAX_LEAVES;
    $('back').disabled=value||position===0;$('reset').disabled=value||position===0;
  }
  async function computeTo(target){
    if(busy)return;
    if(!Number.isSafeInteger(target)||target<0||target>MMBLive.MAX_LEAVES){$('compute-status').textContent=`Enter an integer from 0 to ${MMBLive.MAX_LEAVES.toLocaleString('en-US')}.`;return;}
    cancelled=false;setBusy(true);
    try{
      while(engine.n<target&&!cancelled){
        const end=Math.min(target,engine.n+32);engine.ensure(end);
        $('compute-status').textContent=`Computed ${engine.n} / ${target} leaves…`;await yieldUI();
      }
      const reached=Math.min(target,engine.n);
      $('compute-status').textContent=cancelled?`Stopped at ${reached} leaves.`:`Computed ${target} leaves in this browser.`;
      setBusy(false);setPosition(reached);
    }catch(error){$('compute-status').textContent=`Computation failed: ${error.message}`;}
    finally{setBusy(false);}
  }

  function selectLeaf(value) {
    const leaf=Number(value);
    if(!Number.isSafeInteger(leaf)||leaf<1||leaf>position)return;
    selectedLeaf=leaf;$('tamper').checked=false;$('leaf-select').value=leaf;$('leaf-number').value=leaf;renderGraph();renderProof();
  }

  $('position').addEventListener('input',event=>setPosition(event.target.value));
  $('compute-form').addEventListener('submit',event=>{event.preventDefault();computeTo(Number($('target-n').value));});
  $('cancel-compute').addEventListener('click',()=>{cancelled=true;});
  $('append').addEventListener('click',()=>setPosition(position+1));$('back').addEventListener('click',()=>setPosition(position-1));$('reset').addEventListener('click',()=>setPosition(0));
  $('leaf-select').addEventListener('change',event=>selectLeaf(event.target.value));$('tamper').addEventListener('change',renderProof);
  $('leaf-number').addEventListener('input',event=>selectLeaf(event.target.value));
  $('leaf-number').addEventListener('change',()=>{$('leaf-number').value=selectedLeaf;});
  $('show-membership').addEventListener('change',()=>{
    $('tamper').checked=false;
    $('graph-viewport').scrollLeft=0;
    $('graph-before-viewport').scrollLeft=0;
    renderGraph();renderProof();
  });
  function graphAction(event) {const target=event.target.closest('[data-select-leaf]');if(target)selectLeaf(target.dataset.selectLeaf);}
  for(const id of ['graph','graph-before']){
    $(id).addEventListener('click',graphAction);
    $(id).addEventListener('keydown',event=>{if(event.key==='Enter'||event.key===' '){event.preventDefault();graphAction(event);}});
  }
  function historyAction(event) {const target=event.target.closest('[data-position]');if(target)setPosition(target.dataset.position);}
  $('history').addEventListener('click',historyAction);$('history').addEventListener('keydown',event=>{if(event.key==='Enter'||event.key===' '){event.preventDefault();historyAction(event);}});
  $('history-range').addEventListener('change',renderHistory);
  $('verify').addEventListener('click',()=>{const ok=MMBLive.verifyProof(position,displayedProof(),current()?.root);$('proof-result').className=`result ${ok?'pass':'fail'}`;$('proof-result').textContent=ok?`Path accepted: Keccak-256 recomputation matches the root and the intervals cover [1, ${position}].`:'Path rejected: the sibling digests or intervals do not reconstruct the expected root.';});
  new ResizeObserver(updateTransitionLayout).observe($('transition-grid'));
  render();
})();
