// Browser implementation of the append schedule and bagging in linked_peaks.clj.
// All displayed digests are computed here; the exported trace is only a test oracle.
const MMBLive = (() => {
  'use strict';
  const MAX_LEAVES=100000;
  const MASK=(1n<<64n)-1n;
  const ROT=[0,1,62,28,27,36,44,6,55,20,3,10,43,25,39,41,45,15,21,8,18,2,61,56,14];
  const RC=[0x1n,0x8082n,0x800000000000808an,0x8000000080008000n,0x808bn,0x80000001n,
    0x8000000080008081n,0x8000000000008009n,0x8an,0x88n,0x80008009n,0x8000000an,
    0x8000808bn,0x800000000000008bn,0x8000000000008089n,0x8000000000008003n,
    0x8000000000008002n,0x8000000000000080n,0x800an,0x800000008000000an,
    0x8000000080008081n,0x8000000000008080n,0x80000001n,0x8000000080008008n];
  const rotate=(word,bits)=>bits?((word<<BigInt(bits))|(word>>BigInt(64-bits)))&MASK:word;
  function permutation(a){
    const c=Array(5),d=Array(5),b=Array(25);
    for(const rc of RC){
      for(let x=0;x<5;x++)c[x]=a[x]^a[x+5]^a[x+10]^a[x+15]^a[x+20];
      for(let x=0;x<5;x++)d[x]=c[(x+4)%5]^rotate(c[(x+1)%5],1);
      for(let y=0;y<5;y++)for(let x=0;x<5;x++){
        b[y+5*((2*x+3*y)%5)]=rotate(a[x+5*y]^d[x],ROT[x+5*y]);
      }
      for(let y=0;y<5;y++)for(let x=0;x<5;x++)a[x+5*y]=b[x+5*y]^((~b[(x+1)%5+5*y])&b[(x+2)%5+5*y]);
      a[0]^=rc;
    }
  }
  // Keccak-f[1600], rate 1088, capacity 512, original Keccak suffix 0x01.
  // This deliberately does not use the SHA3-256 suffix (0x06).
  function keccak256(bytes){
    const padded=new Uint8Array(Math.ceil((bytes.length+1)/136)*136);
    padded.set(bytes);padded[bytes.length]=1;padded[padded.length-1]^=128;
    const a=Array(25).fill(0n);
    for(let offset=0;offset<padded.length;offset+=136){
      for(let j=0;j<136;j++)a[Math.floor(j/8)]^=BigInt(padded[offset+j])<<BigInt(8*(j%8));
      permutation(a);
    }
    return Array.from({length:32},(_,i)=>Number((a[Math.floor(i/8)]>>BigInt(8*(i%8)))&255n).toString(16).padStart(2,'0')).join('');
  }
  function leafHash(index){
    if(!Number.isSafeInteger(index)||index<1)throw new RangeError('Leaf index must be a positive safe integer');
    let value=BigInt(index);const bytes=new Uint8Array(8);
    for(let i=0;i<8;i++){bytes[i]=Number(value&255n);value>>=8n;}
    return keccak256(bytes);
  }
  const validHash=hash=>typeof hash==='string'&&/^[0-9a-f]{64}$/.test(hash);
  function nodeHash(left,right){
    if(!validHash(left)||!validHash(right))throw new TypeError('Expected two Keccak-256 digests');
    const hex=left+right;
    return keccak256(Uint8Array.from({length:64},(_,i)=>parseInt(hex.slice(2*i,2*i+2),16)));
  }
  function expectedPeaks(n){
    const bits=(n+1).toString(2).slice(1);
    return [...bits].map((bit,i)=>bits.length-1-i+Number(bit));
  }
  function rangeSplits(heights){
    const ranges=[];let last=0,penultimate=-1;
    for(const h of heights){
      if(!ranges.length||last===penultimate||last-h===2)ranges.push([]);
      ranges[ranges.length-1].push(h);penultimate=last;last=h;
    }
    return ranges;
  }
  function proofSize(n,k){
    if(!Number.isSafeInteger(k)||k<1||k>n)throw new RangeError('Recency must lie within the MMB');
    const ranges=rangeSplits(expectedPeaks(n));let start=1;const leaf=n-k+1;
    for(let r=0;r<ranges.length;r++)for(let p=0;p<ranges[r].length;p++){
      const h=ranges[r][p],end=start+2**h-1;
      if(leaf<=end)return h+(ranges[r].length-1-p)+(p>0?1:0)+(ranges.length-1-r)+(r>0?1:0);
      start=end+1;
    }
    throw new Error('Leaf missing from peak schedule');
  }
  function verifyProof(n,proof,root){
    if(!proof||!Number.isSafeInteger(proof.leaf)||proof.leaf<1||proof.leaf>n||!validHash(root))return false;
    if(!Array.isArray(proof.siblings)||!Array.isArray(proof.siblingDigests)||proof.siblings.length!==proof.siblingDigests.length)return false;
    let lo=proof.leaf,hi=lo,digest=leafHash(lo);
    for(let i=0;i<proof.siblings.length;i++){
      const pair=proof.siblings[i],hash=proof.siblingDigests[i];
      if(!Array.isArray(pair)||pair.length!==2||!pair.every(Number.isSafeInteger)||!validHash(hash))return false;
      const [a,b]=pair;if(a<1||b>n||a>b)return false;
      if(hi+1===a){hi=b;digest=nodeHash(digest,hash);}
      else if(b+1===lo){lo=a;digest=nodeHash(hash,digest);}
      else return false;
    }
    return lo===1&&hi===n&&digest===root;
  }
  class Engine {
    constructor(){this.states=[];this.peaks=[];this.pairs=[];this.bags=new Map();this.views=new Map();}
    get n(){return this.states.length;}
    append(){
      if(this.n>=MAX_LEAVES)throw new RangeError(`Interactive limit: ${MAX_LEAVES} leaves`);
      const n=this.n+1,events=[],leaf={lo:n,hi:n,height:0,kind:'mountain',digest:leafHash(n)};
      // `cached` supplies a digest already computed for exactly these operands: no event.
      const combine=(left,right,kind,cached=null)=>{
        if(left.hi+1!==right.lo)throw new Error('Nonadjacent children');
        const digest=cached?cached.digest:nodeHash(left.digest,right.digest);
        if(!cached)events.push({left:left.digest,right:right.digest,result:digest});
        return {lo:left.lo,hi:right.hi,kind,digest,left,right};
      };
      const peaks=[...this.peaks,leaf];
      if(this.peaks.at(-1)?.height===0)this.pairs.push(leaf);
      let merged=false,mergedDigest=null;
      if(this.pairs.length){
        const right=this.pairs.pop(),i=peaks.indexOf(right),left=peaks[i-1];
        if(!left||left.height!==right.height)throw new Error('Invalid merge pair');
        // Algorithm 2's f = Begins(L), read before the merge: L's predecessor A is absent,
        // two heights taller, or the right half of a pending pair. A fresh merge
        // (right === leaf) never qualifies, since the leaf was never bagged. When f holds,
        // the previous append's range node over (L, R) already hashed this pair, so the
        // merged peak adopts that digest and no hash is counted (linked_peaks.clj
        // reuse-merge-hash). The memo key is the operand pair, so its presence is the proof.
        const a=peaks[i-2];
        const begins=!a||a.height-left.height===2||(!!peaks[i-3]&&peaks[i-3].height===a.height);
        const cached=right!==leaf&&begins?this.bags.get(`range:${left.digest}:${right.digest}`):null;
        if(right!==leaf&&begins&&!cached)throw new Error('Missing range node for a merge whose left peak begins its range');
        const peak={...combine(left,right,'mountain',cached),height:left.height+1};
        peaks.splice(i-1,2,peak);merged=true;mergedDigest=peak.digest;
        if(peaks[i-2]?.height===peak.height)this.pairs.push(peak);
      }
      const heights=peaks.map(p=>p.height),ranges=rangeSplits(heights),bags=new Map();
      // Retain only the previous state's range/belt operands. The merge above reuses one
      // of them in exactly the case Algorithm 2 licenses; nothing else crosses layers.
      const bag=(left,right,kind)=>{
        if(!left)return right;
        const key=`${kind}:${left.digest}:${right.digest}`,old=this.bags.get(key);
        const node=old?(old.left===left&&old.right===right?old:{...old,left,right}):combine(left,right,kind);
        bags.set(key,node);return node;
      };
      let offset=0,root=null;
      for(const range of ranges){
        let rangeRoot=null;
        for(let j=0;j<range.length;j++)rangeRoot=bag(rangeRoot,peaks[offset++],'range');
        root=bag(root,rangeRoot,'belt');
      }
      const state={n,root:root.digest,rootNode:root,leafHash:leaf.digest,mergedDigest,hashes:events.length,events,
        totalHashes:(this.states.at(-1)?.totalHashes||0)+events.length,
        maxHashes:Math.max(this.states.at(-1)?.maxHashes||0,events.length),
        peaks:heights,peakNodes:peaks,ranges,expectedPeaks:expectedPeaks(n),case:!merged?'no-merge':n%2?'delayed':'fresh'};
      this.states.push(state);this.peaks=peaks;this.bags=bags;
      return state;
    }
    ensure(n){
      if(!Number.isSafeInteger(n)||n<0||n>MAX_LEAVES)throw new RangeError(`Choose a leaf count from 0 to ${MAX_LEAVES}`);
      while(this.n<n)this.append();
      return n?this.states[n-1]:null;
    }
    // Omitted leaf requests the full test topology. null requests only peaks;
    // a leaf index expands just that path and its sibling roots for rendering.
    snapshot(n,leaf=undefined){
      if(!n)return null;
      const state=this.ensure(n);
      const key=`${n}:${leaf===undefined?'full':leaf===null?'peaks':leaf}`;
      if(this.views.has(key))return this.views.get(key);
      const peaks=new Set(state.peakNodes),nodes=[],edges=[];
      const visit=node=>{
        const kind=node.kind==='mountain'?(peaks.has(node)?'peak':'internal'):node.kind;
        const id=`[${node.lo} ${node.hi}]:${kind}`;
        nodes.push({id,lo:node.lo,hi:node.hi,type:node.lo===node.hi?'leaf':kind,digest:node.digest});
        if(node.left&&(node.kind!=='mountain'||leaf===undefined||(leaf!==null&&node.lo<=leaf&&leaf<=node.hi))){
          edges.push([id,visit(node.left)]);edges.push([id,visit(node.right)]);
        }
        return id;
      };
      visit(state.rootNode);
      const view={...state,nodes,edges};
      this.views.set(key,view);if(this.views.size>4)this.views.delete(this.views.keys().next().value);
      return view;
    }
    proof(n,leaf){
      if(!Number.isSafeInteger(leaf)||leaf<1||leaf>n)return null;
      const state=this.ensure(n),siblings=[],siblingDigests=[];
      let node=state.rootNode;
      while(node.left){
        const left=leaf<=node.left.hi,sibling=left?node.right:node.left;
        siblings.push([sibling.lo,sibling.hi]);siblingDigests.push(sibling.digest);
        node=left?node.left:node.right;
      }
      return {leaf,siblings:siblings.reverse(),siblingDigests:siblingDigests.reverse(),expectedSize:proofSize(n,n-leaf+1)};
    }
    sample(k){
      if(!Number.isSafeInteger(k)||k<1||k>256)throw new RangeError('Recency must be between 1 and 256');
      const d=Math.floor(Math.log2(k+1)),period=2**(d+1),sizes=[],depths=[],structural=[];
      this.ensure(k+period-1);
      for(let n=k;n<k+period;n++){
        const leaf=n-k+1;sizes.push(this.proof(n,leaf).siblings.length);structural.push(proofSize(n,k));
        depths.push(this.states[n-1].peakNodes.find(p=>p.lo<=leaf&&leaf<=p.hi).height);
      }
      const mean=a=>a.reduce((sum,x)=>sum+x,0)/a.length,firstCase=k+1<1.5*2**d;
      return {k,period,sizes,ummbObserved:mean(depths),restricted:mean(depths.slice(0,period/2)),
        ummbFormula:firstCase?d+3*(k+1)/2**(d+1)-2:d+(k+1)/2**(d+1)-.5,
        mmbObserved:mean(sizes),mmbStructural:mean(structural),
        mmbBound:firstCase?11/8*d+(4*(k+1)-5)/2**(d+1)+1/16:11/8*d+(3*(k+1)-6)/2**(d+2)+2};
    }
  }
  return {Engine,MAX_LEAVES,keccak256,leafHash,nodeHash,expectedPeaks,rangeSplits,proofSize,verifyProof};
})();
