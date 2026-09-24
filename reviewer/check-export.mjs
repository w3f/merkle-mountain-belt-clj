import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {Script, runInNewContext} from 'node:vm';

const html=readFileSync(new URL('../docs/index.html',import.meta.url),'utf8');
const match=html.match(/<script id="artifact-data" type="application\/json">([\s\S]*?)<\/script>/);
assert.ok(match,'Embedded data is present');
const data=JSON.parse(match[1]);
assert.equal(data.formatVersion,3);
const {MAX_LEAVES}=runInNewContext(readFileSync(new URL('./live.js',import.meta.url),'utf8')+';MMBLive');
assert.equal(data.liveReference.maxN,MAX_LEAVES,'Reference covers the browser limit');
assert.equal(data.liveReference.hashCounts.length,MAX_LEAVES);
assert.ok(data.liveReference.hashCounts.every(n=>Number.isInteger(n)&&n>=0&&n<=5));
assert.equal(data.liveReference.rootsHex.length,64*MAX_LEAVES);
assert.match(data.liveReference.rootsHex,/^[0-9a-f]+$/);
for(const state of [...data.states,...data.liveReference.checkpoints]){
  assert.equal(state.root,data.liveReference.rootsHex.slice((state.n-1)*64,state.n*64),'Detailed and full-range references agree');
  assert.equal(state.hashes,data.liveReference.hashCounts[state.n-1]);
}
assert.equal(data.states.length,data.maxN);
assert.ok(Buffer.byteLength(html)<8*1024*1024,'Standalone artifact stays within the 8 MiB size budget');
assert.ok(!html.includes('/*__'),'All template slots are filled');
assert.ok(!/<(?:script|link|img|iframe)[^>]+(?:src|href)\s*=\s*["']https?:/i.test(html),'No external assets');
assert.ok(!/\/home\/|\/Users\/|mailto:|github\.com\//i.test(html),'No local home paths, email links, or source repository links');
new Script(readFileSync(new URL('./explorer.js',import.meta.url),'utf8'),{filename:fileURLToPath(new URL('./explorer.js',import.meta.url))});
const {buildPaperStructure,focusPaperStructure}=runInNewContext(readFileSync(new URL('./structure.js',import.meta.url),'utf8')+'; ({buildPaperStructure,focusPaperStructure})');

function verify(n,leaf,siblings){
  let [lo,hi]=[leaf,leaf];
  for(const [a,b] of siblings){
    if(a>b)return false;
    if(hi+1===a)hi=b;else if(b+1===lo)lo=a;else return false;
  }
  return lo===1&&hi===n;
}
let proofs=0;
for(const [i,state] of data.states.entries()){
  assert.equal(state.n,i+1);
  assert.equal(state.nodes.length,2*state.n-1);
  assert.equal(state.edges.length,state.nodes.length-1);
  const nodes=new Map(state.nodes.map(node=>[node.id,node]));
  assert.equal(nodes.size,state.nodes.length);
  const children=new Map();
  const parented=new Set();
  for(const [parent,child] of state.edges){
    assert.ok(nodes.has(parent)&&nodes.has(child));
    assert.ok(!parented.has(child),'No node has two parents');parented.add(child);
    children.set(parent,[...(children.get(parent)||[]),nodes.get(child)]);
  }
  const roots=state.nodes.filter(node=>!parented.has(node.id));
  assert.equal(roots.length,1);assert.equal(roots[0].digest,state.root);
  assert.deepEqual([roots[0].lo,roots[0].hi],[1,state.n]);
  for(const node of state.nodes){
    assert.match(node.digest,/^[a-f0-9]{64}$/);
    if(node.type==='leaf')assert.equal(node.lo,node.hi);
    else{
      const cs=children.get(node.id).sort((a,b)=>a.lo-b.lo);
      assert.equal(cs.length,2);assert.equal(cs[0].hi+1,cs[1].lo);
      assert.deepEqual([cs[0].lo,cs[1].hi],[node.lo,node.hi]);
    }
  }
  assert.equal(state.events.length,state.hashes);assert.ok(state.hashes<=5);
  assert.deepEqual(state.peaks,state.expectedPeaks);
  assert.deepEqual(state.ranges.flat(),state.peaks);
  // The paper view restores identity nodes, but must preserve exactly the recorded
  // Merkle tree when those zero-hash nodes are contracted, including n=1 and n=2.
  const paper=buildPaperStructure(state);
  const paperNodes=new Map(paper.nodes.map(node=>[node.id,node]));
  const paperChildren=new Map();
  for(const [parent,child] of paper.edges)paperChildren.set(parent,[...(paperChildren.get(parent)||[]),child]);
  const resolve=id=>{
    const node=paperNodes.get(id);
    return node.identity?resolve(paperChildren.get(id)[0]):node.sourceId;
  };
  assert.equal(paper.mountains.length,state.peaks.length);
  assert.equal(paper.nodes.filter(n=>n.kind==='range').length,state.peaks.length);
  assert.equal(paper.nodes.filter(n=>n.kind==='belt').length,state.ranges.length);
  assert.equal(paper.nodes.filter(n=>n.identity).length,state.ranges.length+1);
  assert.equal(resolve(paper.root),roots[0].id);
  const contracted=[];
  for(const node of paper.nodes){
    assert.equal(node.digest,nodes.get(node.sourceId).digest);
    if(node.identity){
      assert.equal(paperChildren.get(node.id).length,1);
      assert.equal(node.digest,paperNodes.get(paperChildren.get(node.id)[0]).digest);
    }else for(const child of paperChildren.get(node.id)||[]) contracted.push([node.sourceId,resolve(child)]);
  }
  assert.equal(JSON.stringify(contracted.sort()),JSON.stringify([...state.edges].sort()),'Paper view preserves the exact hashing topology');
  assert.equal(state.proofs.length,state.n);
  for(const proof of state.proofs){
    assert.equal(proof.siblings.length,proof.expectedSize);
    assert.ok(verify(state.n,proof.leaf,proof.siblings));proofs++;
    const focus=focusPaperStructure(paper,proof.leaf);
    const focused=new Map(focus.nodes.map(node=>[node.id,node]));
    const frontier=focus.nodes.filter(node=>!['range','belt'].includes(node.kind)&&(node.collapsed||node.lo===node.hi)).sort((a,b)=>a.lo-b.lo);
    let nextLeaf=1;
    for(const node of frontier){assert.equal(node.lo,nextLeaf);nextLeaf=node.hi+1;}
    assert.equal(nextLeaf,state.n+1,'Collapsed subtrees still partition all leaves');
    const leaf=frontier.find(node=>!node.collapsed);
    assert.ok(leaf);assert.equal(leaf.lo,proof.leaf);assert.equal(leaf.hi,proof.leaf);
    for(const node of focus.nodes){
      if(['range','belt'].includes(node.kind))continue;
      if(!node.collapsed)assert.ok(node.lo<=proof.leaf&&node.hi>=proof.leaf,'Only selected path expands');
      else assert.ok(node.hi<proof.leaf||node.lo>proof.leaf,'Every collapsed subtree is off the path');
    }
    for(const sibling of proof.siblings){
      assert.equal(focus.nodes.filter(node=>!node.identity&&node.lo===sibling[0]&&node.hi===sibling[1]).length,1,'Each proof sibling remains visible exactly once');
    }
    for(const [parent,child] of focus.edges){
      assert.ok(focused.has(parent)&&focused.has(child));
      assert.ok(!focused.get(parent).collapsed,'Nothing is drawn below collapsed subtrees');
    }
    if(proof.siblings.length){
      const bad=proof.siblings.map(s=>[...s]);bad[0][1]++;
      assert.ok(!verify(state.n,proof.leaf,bad),'Tampered path must fail');
    }
  }
  const absent=focusPaperStructure(paper,state.n+1);
  assert.equal(absent.selected,null);
  assert.equal(absent.nodes.filter(node=>node.collapsed).length,state.peaks.length);
}
for(const sample of data.amortized){
  assert.equal(sample.sizes.length,sample.period);
  assert.equal(sample.sizes.reduce((a,b)=>a+b,0)/sample.period,sample.mmbObserved);
  assert.equal(sample.ummbObserved,sample.ummbFormula);
  assert.ok(sample.restricted<=sample.ummbFormula);
  assert.equal(sample.mmbObserved,sample.mmbStructural);
  assert.ok(sample.mmbObserved<=sample.mmbBound);
}
assert.ok(data.sourceTests.every(t=>t.passed&&t.assertions>0));
console.log(`Export verified: ${MAX_LEAVES} reference roots and counts; ${data.maxN} detailed states, ${proofs} focused membership paths, ${data.amortized.length} recency samples; topology, bounds, and asset checks passed (${Buffer.byteLength(html)} bytes).`);
