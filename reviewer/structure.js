// Restore the paper's explicit identity bagging nodes around the recorded tree.
// Every value comes from the exported, independently rehashed topology.
const buildPaperStructure = state => {
  if (!state) return {nodes:[], edges:[], mountains:[], root:null};
  const bySpan = new Map(state.nodes.map(node=>[`${node.lo},${node.hi}`,node]));
  const mountains = state.nodes.filter(node=>node.id.endsWith(':peak'))
    .sort((a,b)=>a.lo-b.lo).map((node,column)=>({
      ...node, kind:'peak', height:Math.log2(node.hi-node.lo+1), column,
      sourceId:node.id, identity:false
    }));
  if (JSON.stringify(mountains.map(node=>node.height)) !== JSON.stringify(state.peaks)) {
    throw new Error(`Peak topology disagrees with S(${state.n})`);
  }
  const peakById = new Map(mountains.map(node=>[node.id,node]));
  const nodes = state.nodes.filter(node=>!['range','belt'].includes(node.type))
    .map(node=>peakById.get(node.id) || {...node, kind:node.type, sourceId:node.id, identity:false});
  const mountainIds = new Set(nodes.map(node=>node.id));
  const edges = state.edges.filter(([parent])=>mountainIds.has(parent)).map(edge=>[...edge]);
  function bag(id,kind,lo,hi,column,identity) {
    const source=bySpan.get(`${lo},${hi}`);
    if (!source) throw new Error(`Missing recorded ${kind} span [${lo}, ${hi}]`);
    const node={...source,id,kind,column,identity,sourceId:source.id};
    nodes.push(node);
    return node;
  }
  let column=0, previousBelt=null;
  state.ranges.forEach((heights,rangeIndex)=>{
    const start=mountains[column].lo;
    let previousRange=null;
    heights.forEach(height=>{
      const peak=mountains[column];
      if (peak.height!==height) throw new Error('Range partition disagrees with the recorded mountains');
      const range=bag(`paper-range-${column}`,'range',start,peak.hi,column,!previousRange);
      if (previousRange) edges.push([range.id,previousRange.id]);
      edges.push([range.id,peak.id]);
      previousRange=range;
      column++;
    });
    const belt=bag(`paper-belt-${rangeIndex}`,'belt',1,previousRange.hi,column-1,!previousBelt);
    if (previousBelt) edges.push([belt.id,previousBelt.id]);
    edges.push([belt.id,previousRange.id]);
    previousBelt=belt;
  });
  if (column!==mountains.length) throw new Error('Range partition leaves mountains unassigned');
  return {nodes,edges,mountains,root:previousBelt.id};
};

// Expand only the route to the selected leaf. Off-path subtrees stay collapsed;
// the renderer reserves triangles for peaks and marks hidden descendants with ellipses.
const focusPaperStructure = (graph,leaf=null) => {
  const byId=new Map(graph.nodes.map(node=>[node.id,node]));
  const children=new Map();
  for(const [parent,child] of graph.edges)children.set(parent,[...(children.get(parent)||[]),child]);
  const selected=Number.isInteger(leaf)&&leaf>=1&&leaf<=(byId.get(graph.root)?.hi||0)?leaf:null;
  const nodes=graph.nodes.filter(node=>['range','belt'].includes(node.kind));
  function visit(id){
    const node=byId.get(id),onPath=selected!==null&&node.lo<=selected&&node.hi>=selected;
    nodes.push({...node,collapsed:!onPath});
    if(onPath)for(const child of children.get(id)||[])visit(child);
  }
  for(const peak of graph.mountains)visit(peak.id);
  const visible=new Set(nodes.map(node=>node.id));
  return {...graph,nodes,selected,edges:graph.edges.filter(([a,b])=>visible.has(a)&&visible.has(b))};
};
