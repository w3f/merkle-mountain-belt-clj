// Produce a file-only snapshot from an explicit list, optionally redacting
// private literal terms supplied outside the distributed source tree.
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync,mkdirSync,lstatSync,realpathSync,existsSync} from 'node:fs';
import {resolve,dirname,relative,sep,isAbsolute} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';

const root=realpathSync(fileURLToPath(new URL('..',import.meta.url)));
const args=process.argv.slice(2),outArg=args.shift();
assert.ok(outArg&&!outArg.startsWith('--'),'Usage: node reviewer/package.mjs OUTPUT_DIRECTORY [--terms PRIVATE_LITERAL_TERMS_FILE]');
assert.ok(args.length===0||(args.length===2&&args[0]==='--terms'),'Unexpected arguments');
const out=resolve(outArg);
assert.ok(!existsSync(out),'Output directory already exists; choose a fresh directory');
const terms=args.length?readFileSync(args[1],'utf8').split(/\r?\n/).map(s=>s.trim()).filter(Boolean):[];
assert.ok(terms.every(term=>term.length>=3),'Redaction terms must be at least three characters');
const patterns=terms.map(term=>new RegExp(term.replace(/[.*+?^${}()|[\]\\]/g,'\\$&'),'gi'));
const manifest=JSON.parse(readFileSync(resolve(root,'reviewer/release-files.json'),'utf8'));
assert.equal(manifest.version,1);assert.ok(Array.isArray(manifest.files));
assert.equal(new Set(manifest.files).size,manifest.files.length,'Duplicate release paths');
const contents=new Map(),originals=new Map(),redacted=[];
const contained=(base,file)=>{const rel=relative(base,file);return rel!==''&&!rel.startsWith('..'+sep)&&rel!=='..'&&!isAbsolute(rel);};
for(const file of [...manifest.files].sort()){
  assert.ok(typeof file==='string'&&/^[A-Za-z0-9_.\/-]+$/.test(file),'Unexpected release path');
  assert.ok(!file.split('/').some(part=>['','..','.git','.beads','.agents','.claude','.codex','dist','backup','AGENTS.md','CLAUDE.md'].includes(part)),'Unsafe release path');
  const source=resolve(root,file);
  assert.ok(contained(root,source)&&lstatSync(source).isFile(),'Expected an ordinary file in the repository');
  assert.equal(realpathSync(source),source,'Release files must not traverse symlinks');
  const original=readFileSync(source),text=original.toString('utf8');
  assert.ok(Buffer.from(text).equals(original),`Non-text file requires a separate metadata audit: ${file}`);
  let clean=text;
  for(const pattern of patterns){
    pattern.lastIndex=0;assert.ok(!pattern.test(file),`Identifying term in filename: ${file}`);
    clean=clean.replace(pattern,'[redacted]');
  }
  assert.ok(!/\/(?:home|Users)\/[A-Za-z0-9_.-]+\//.test(clean),`Home directory in ${file}`);
  assert.ok(!/[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}/i.test(clean),`Email address in ${file}`);
  assert.ok(!/-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----/.test(clean),`Private key in ${file}`);
  if(clean!==text)redacted.push(file);
  contents.set(file,Buffer.from(clean));originals.set(source,original);
}
// If a collaborator edits a source during collection, retry with a coherent copy.
for(const [source,original] of originals)assert.ok(readFileSync(source).equals(original),'Source changed during packaging; rerun');
for(const file of contents.keys())assert.ok(resolve(out,file)!==resolve(root,file),'Output must not replace source files');
mkdirSync(out,{recursive:true});
const checksums=[];
for(const [file,bytes] of contents){
  const destination=resolve(out,file);mkdirSync(dirname(destination),{recursive:true});
  writeFileSync(destination,bytes,{flag:'wx'});
  checksums.push(`${createHash('sha256').update(bytes).digest('hex')}  ${file}`);
}
writeFileSync(resolve(out,'MANIFEST.sha256'),checksums.join('\n')+'\n',{flag:'wx'});
console.log(`Prepared ${contents.size} files (${[...contents.values()].reduce((n,b)=>n+b.length,0)} bytes), plus MANIFEST.sha256.`);
console.log(redacted.length?`Redacted literal terms in: ${redacted.join(', ')}`:'No supplied terms occurred in the snapshot.');
console.log('No Git metadata, private term list, or unlisted local files are included. Review and test this snapshot before publishing.');
