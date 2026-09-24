import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {runInNewContext} from 'node:vm';
const source=readFileSync(new URL('./live.js',import.meta.url),'utf8');
const L=runInNewContext(source+';MMBLive');
const {buildPaperStructure,focusPaperStructure}=runInNewContext(readFileSync(new URL('./structure.js',import.meta.url),'utf8')+';({buildPaperStructure,focusPaperStructure})');
const data=JSON.parse(readFileSync(new URL('../docs/index.html',import.meta.url),'utf8')
  .match(/<script id="artifact-data" type="application\/json">([\s\S]*?)<\/script>/)[1]);
const plain=value=>JSON.parse(JSON.stringify(value));
const bytes=hex=>Uint8Array.from(hex.match(/../g)||[],x=>parseInt(x,16));
assert.equal(data.formatVersion,3);
assert.equal(data.liveReference.maxN,L.MAX_LEAVES,'Clojure reference covers the entire interactive range');
assert.equal(data.liveReference.hashCounts.length,L.MAX_LEAVES);
assert.equal(data.liveReference.rootsHex.length,64*L.MAX_LEAVES);
assert.match(data.liveReference.rootsHex,/^[0-9a-f]+$/);
const referenceRoot=n=>data.liveReference.rootsHex.slice((n-1)*64,n*64);
assert.equal(L.keccak256(new Uint8Array()),'c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470');
assert.equal(L.keccak256(new TextEncoder().encode('abc')),'4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45');
for(const vector of data.liveReference.hashVectors)assert.equal(L.keccak256(bytes(vector.input)),vector.digest,'Bouncy Castle hash vector');
const engine=new L.Engine();
const events=list=>list.map(e=>`${e.left}:${e.right}:${e.result}`).sort().join(',');
const nodes=list=>list.map(({id,lo,hi,type,digest})=>({id,lo,hi,type,digest})).sort((a,b)=>a.id.localeCompare(b.id));
let proofCount=0;
let totalHashes=0,maxHashes=0;
for(const expected of data.states){
  const s=engine.append(),view=engine.snapshot(s.n);
  assert.equal(s.root,expected.root,`root at ${s.n}`);
  assert.equal(s.leafHash,expected.leafHash);
  assert.equal(s.hashes,expected.hashes,`hash count at ${s.n}`);
  assert.equal(s.case,expected.case);
  assert.deepEqual(plain(s.peaks),expected.peaks);
  assert.deepEqual(plain(s.ranges),expected.ranges);
  assert.equal(events(s.events),events(expected.events),'Every counted pair matches, irrespective of evaluation order');
  assert.deepEqual(plain(nodes(view.nodes)),nodes(expected.nodes),'Exact Clojure hashing topology and digests');
  assert.deepEqual(plain(view.edges).sort(),[...expected.edges].sort());
  const full=buildPaperStructure(view);
  assert.deepEqual(plain(focusPaperStructure(buildPaperStructure(engine.snapshot(s.n,null)))),plain(focusPaperStructure(full)),'Sparse collapsed view equals full topology projection');
  for(const ref of expected.proofs){
    const proof=engine.proof(s.n,ref.leaf);proofCount++;
    assert.deepEqual(plain(proof.siblings),ref.siblings);
    assert.equal(proof.expectedSize,ref.expectedSize);
    assert.deepEqual(plain(focusPaperStructure(buildPaperStructure(engine.snapshot(s.n,ref.leaf)),ref.leaf)),plain(focusPaperStructure(full,ref.leaf)),'Sparse membership view equals full topology projection');
    assert.ok(L.verifyProof(s.n,proof,s.root));
    assert.ok(!L.verifyProof(s.n,proof,'0'.repeat(64)),'Wrong root rejected');
    if(proof.siblings.length){
      const changed=plain(proof);changed.siblingDigests[0]='0'.repeat(64);
      assert.ok(!L.verifyProof(s.n,changed,s.root),'Changed digest rejected');
      changed.siblingDigests=proof.siblingDigests;changed.siblings[0][1]++;
      assert.ok(!L.verifyProof(s.n,changed,s.root),'Changed interval rejected');
    }
  }
}
engine.ensure(data.liveReference.maxN);
for(let i=0;i<data.liveReference.maxN;i++){
  assert.equal(engine.states[i].hashes,data.liveReference.hashCounts[i],`Clojure hash count at ${i+1}`);
  assert.equal(engine.states[i].root,referenceRoot(i+1),`Clojure root at ${i+1}`);
}
for(const ref of data.liveReference.checkpoints){
  const state=engine.states[ref.n-1];
  assert.equal(state.root,ref.root,`Clojure root at ${ref.n}`);
  assert.equal(events(state.events),events(ref.events),`Hash events at ${ref.n}`);
  for(const leaf of [1,Math.ceil(ref.n/2),ref.n])assert.ok(L.verifyProof(ref.n,engine.proof(ref.n,leaf),ref.root));
}
for(const ref of data.amortized)assert.deepEqual(plain(engine.sample(ref.k)),ref,'Clojure recency sample');
const largestSample=engine.sample(256);
assert.equal(largestSample.mmbObserved,largestSample.mmbStructural);
assert.ok(largestSample.mmbObserved<=largestSample.mmbBound);
const saved=engine.states[10].root;
engine.ensure(L.MAX_LEAVES);
assert.equal(engine.snapshot(11).root,saved,'Later appends preserve earlier states');
for(const state of engine.states){
  totalHashes+=state.hashes;maxHashes=Math.max(maxHashes,state.hashes);
  assert.equal(state.totalHashes,totalHashes);assert.equal(state.maxHashes,maxHashes);
  assert.deepEqual(plain(state.peaks),plain(L.expectedPeaks(state.n)));
  assert.ok(state.hashes<=5,`Hash bound at ${state.n}`);
}
for(const leaf of [1,5000,9999,10000])assert.ok(L.verifyProof(10000,engine.proof(10000,leaf),engine.states[9999].root));
for(const leaf of [1,Math.floor(L.MAX_LEAVES/2),L.MAX_LEAVES]){
  assert.ok(L.verifyProof(L.MAX_LEAVES,engine.proof(L.MAX_LEAVES,leaf),engine.states.at(-1).root));
  const view=engine.snapshot(L.MAX_LEAVES,leaf);
  assert.ok(view.nodes.length<150,'Large render snapshots contain only the visible route, siblings, and bags');
  const graph=focusPaperStructure(buildPaperStructure(view),leaf);
  assert.equal(graph.selected,leaf);
  assert.ok(graph.nodes.some(node=>node.lo===leaf&&node.hi===leaf),'Large-state selected leaf remains visible');
}
assert.ok(engine.snapshot(L.MAX_LEAVES,null).nodes.length<75,'Collapsed view does not traverse all leaves');
for(const n of [-1,NaN,1.5,Infinity,L.MAX_LEAVES+1])assert.throws(()=>engine.ensure(n),/leaf count/);
assert.throws(()=>engine.append(),/Interactive limit/);
assert.equal(engine.proof(0,1),null);
assert.equal(engine.proof(5,6),null);
console.log(`Live computation verified: every Clojure root and hash count through ${L.MAX_LEAVES} appends; Keccak vectors, ${proofCount} paths and sparse projections, checkpoint hash events, recency samples, altered proofs, and cumulative counts.`);
