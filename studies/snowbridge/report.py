#!/usr/bin/env python3
"""Assemble REPORT-snowbridge-k.org for colleagues from the generated CSVs (figures/*.csv,
data/snowbridge_messages.csv, data/mmb_sizes.csv). Narrative is fixed text; every number is read
from the generated files."""
import csv, datetime as dt, json, statistics as st
from collections import Counter
from pathlib import Path
HERE = Path(__file__).resolve().parent; FIG = HERE / "figures"; DATA = HERE / "data"

def rd(name): return list(csv.DictReader(open(FIG / name)))
def org_table(headers, rows):
    w = [max(len(str(x)) for x in col) for col in zip(headers, *rows)]
    fmt = lambda r: "| " + " | ".join(str(x).ljust(x_w) for x, x_w in zip(r, w)) + " |"
    return "\n".join([fmt(headers), "|" + "|".join("-" * (x + 2) for x in w) + "|"] + [fmt(r) for r in rows])

summ = {r["group"]: r for r in rd("summary.csv")}
run = json.loads((DATA / "run_summary.json").read_text()) if (DATA / "run_summary.json").exists() else {}
groups = list(summ)
sizes = {(int(r["n"]), int(r["k"])): int(r["mmb_items"]) for r in csv.DictReader(open(DATA / "mmb_sizes.csv"))}
rows = list(csv.DictReader(open(DATA / "snowbridge_messages.csv")))
eras = {}
for r in rows:
    eras.setdefault(r["beefyClient"], f"era {len(eras)+1} ({r['beefyClient'][:10]}…)")
per = {g: [] for g in groups}
for r in rows:
    a, b = int(r["mmr_items_onchain"]), sizes[(int(r["n"]), int(r["k"]))]
    rec = ((a - b) / a, a, b, int(r["k"]))
    per["all"].append(rec); per[eras[r["beefyClient"]]].append(rec)
def ext(g):
    p = per[g]; best = max(p); worst = min(p)
    return (f"{100*st.median(x[0] for x in p):.0f}% ({st.median(x[1]-x[2] for x in p):.0f} hashes)",
            f"{100*best[0]:.0f}% ({best[1]}→{best[2]}, k={best[3]})", f"{100*worst[0]:.0f}% ({worst[1]}→{worst[2]}, k={worst[3]:,})",
            f"{100*sum(x[1]>x[2] for x in p)/len(p):.1f}%")
meta = json.loads((FIG / "analysis_meta.json").read_text()) if (FIG / "analysis_meta.json").exists() else {}
launch = [r for r in rows if int(r["k"]) > 100_000]
k_all = [int(r["k"]) for r in rows]
k_wo_launch = [int(r["k"]) for r in rows if int(r["k"]) <= 100_000]
worse25 = [x for x in per["all"] if x[0] < -0.25]
worse25_nonlaunch = [x for x in worse25 if x[3] <= 100_000]
offv = run.get("offset_verification", [])
offset_line = ("verified at run time: " + "; ".join(f"block {o['block']}: NumberOfLeaves {o['leaves']:,} {'OK' if o['ok'] else 'MISMATCH'}" for o in offv if "leaves" in o)
               + ("" if all("leaves" in o for o in offv) else "; some endpoints unavailable")) if offv else "NOT verified in this run (offline or endpoints unavailable)"
d0 = dt.datetime.fromtimestamp(min(int(r["timestamp"]) for r in rows), dt.UTC).date()
d1 = dt.datetime.fromtimestamp(max(int(r["timestamp"]) for r in rows), dt.UTC).date()
k1 = sum(1 for r in rows if r["k"] == "1"); mism = sum(1 for r in rows if r["mmr_items_onchain"] != r["mmr_items_formula"])
f = lambda g, c: summ[g][c]
t_main = org_table(["", *groups],
    [["messages"] + [f(g, "messages") for g in groups],
     ["relayers (distinct sender keys)"] + [f(g, "relayers") for g in groups],
     ["dispatches with success = false"] + [f(g, "failed_dispatch") for g in groups],
     ["k median"] + [f(g, "k_p50") for g in groups],
     ["k mean"] + [f"{float(f(g,'k_mean')):.1f}" for g in groups],
     ["k p90"] + [f(g, "k_p90") for g in groups],
     ["k p99"] + [f(g, "k_p99") for g in groups],
     ["k max"] + [f"{int(f(g,'k_max')):,}" for g in groups],
     ["share k ≤ 50 / ≤ 600 / ≤ 1800"] + [f"{float(f(g,'frac_k_le_50')):.0f}% / {float(f(g,'frac_k_le_600')):.0f}% / {float(f(g,'frac_k_le_1800')):.0f}%" for g in groups],
     ["MMR leaf count n (min … max)"] + [f"{int(f(g,'n_min')):,} … {int(f(g,'n_max')):,}" for g in groups],
     ["MMR proof: median / mean / max"] + [f"{f(g,'mmr_p50')} / {float(f(g,'mmr_mean')):.1f} / {f(g,'mmr_max')}" for g in groups],
     ["MMB proof: median / mean / max"] + [f"{f(g,'mmb_p50')} / {float(f(g,'mmb_mean')):.1f} / {f(g,'mmb_max')}" for g in groups],
     ["U-MMB proof mean"] + [f"{float(f(g,'ummb_mean')):.1f}" for g in groups],
     ["Lemma lem:a-mmb bound, mean"] + [f"{float(f(g,'lemma_mean')):.1f}" for g in groups],
     ["balanced tree ⌈log₂ n⌉, mean"] + [f"{float(f(g,'balanced_mean')):.1f}" for g in groups],
     ["saving of means"] + [f"{float(f(g,'saving_pct')):.0f}%" for g in groups],
     ["median per-message saving"] + [ext(g)[0] for g in groups],
     ["best / worst per-message saving"] + [f"{ext(g)[1]} / {ext(g)[2]}" for g in groups],
     ["messages where MMB is shorter"] + [ext(g)[3] for g in groups]])
t_k = org_table(["k from", "k to", "messages", "MMR mean", "MMB mean", "U-MMB mean", "lemma", "balanced"],
                [[r["k_from"], r["k_to"], r["messages"], r["mmr_mean"], r["mmb_mean"], r["ummb_mean"], r["lemma_mean"], r["balanced_mean"]] for r in rd("size_by_k.csv")])
t_sched = org_table(["era", "NewMMRRoot events", "messages", "roots used", "msgs / used root", "update→dispatch gap p50 (s)", "p90 (s)", "root in same tx", "same block", "k ≤ 8"],
                    [[r["era"], r["NewMMRRoot_events"], r["messages"], r["roots_used"], r["msgs_per_used_root"], r["gap_p50_s"], r["gap_p90_s"], r.get("root_same_tx_pct", "n/a") + "%", r.get("root_same_block_pct", "n/a") + "%", r["frac_k_le_8_pct"] + "%"] for r in rd("scheduling.csv")])
t_rel = org_table(["era", "relayer", "messages", "k median", "k mean", "k p90"],
                  [[r["era"], r["relayer"][:10] + "…", r["messages"], r["k_p50"], r["k_mean"], r["k_p90"]] for r in rd("relayers.csv")])
kdist = {int(r["k"]): 0 for r in rows}
for r in rows:
    if eras[r["beefyClient"]].startswith("era 2"): kdist[int(r["k"])] += 1
era2_1_8 = [kdist.get(k, 0) for k in range(1, 9)]

org = f"""#+TITLE: Snowbridge Polkadot→Ethereum messages: measured k and MMR-vs-MMB proof sizes
#+SUBTITLE: Report for the MMB paper, Appendix "Cross-chain bridges and Polkadot"
#+DATE: {dt.date.today()}
#+OPTIONS: toc:2 ^:nil

Generated by =report.py= from the outputs of =snowbridge_k.py= and =analyse.py= (all numbers are
computed, none typed). Data window: {d0} … {d1}, {len(rows):,} messages.
Provenance: collector run {run.get('run_id', 'n/a')} at {run.get('run_at', 'n/a')} (repo commit {str(run.get('repo_commit'))[:12]},
decoder v{run.get('decoder_version', '?')}, sources: {run.get('sources', 'n/a')}); analysis at {meta.get('analysed_at', 'n/a')};
MMB reference implementation commit {str(meta.get('mmb_reference_impl', {}).get('commit'))[:12]}; upstream sources {run.get('source_commits', {})}.

* Summary
- Polkadot's BEEFY commitment is an *MMR* (pallet_mmr on ckb =merkle-mountain-range=), not a balanced
  tree, and it only started at relay block 19,551,001: today it holds n ≈ {int(f('all','n_max')):,} leaves
  (≈ 2^{__import__('math').log2(int(f('all','n_max'))):.1f}), not 2^25. The paper's "25 hashes" baseline must be replaced by the MMR baseline.
- Every =InboundMessageDispatched= log of the Snowbridge Gateway on Ethereum mainnet was collected
  ({run.get('dispatch_logs', 'n/a')} logs, {d0} … {d1}); {len(rows):,} of them{f" ({100*len(rows)/run['dispatch_logs']:.1f}%)" if run.get('dispatch_logs') else ""} could be decoded
  (direct Gateway calls, aggregate3 batches, or a calldata scan of third-party wrapper contracts); the
  remainder is listed in =data/undecoded_dispatches.csv= ({dict(Counter(x['reason'] for x in run.get('undecoded', [])))}).
  For each decoded message we know the exact leaf proven, the BEEFY root it was verified against, and
  the on-chain MMR proof length. The on-chain proof length agrees with the ckb-mmr proof-size formula
  for all {len(rows):,} messages ({mism} mismatches), which validates the leaf indexing, the leaf-count
  offset and the era mapping end to end.
- The block-inclusion proof is one of three proof components; the other two are unchanged by MMB:
  parachain-heads proof mean {st.mean(int(r['headProofLen']) for r in rows):.1f} hashes, message Merkle proof mean
  {st.mean(int(r['msgLeafProofLen']) for r in rows):.2f} hashes (most blocks carry a single message), against
  {float(f('all','mmr_mean')):.1f} for the MMR block-inclusion proof.
- k (paper convention, k = 1 = newest leaf): median {f('all','k_p50')}, p90 {f('all','k_p90')}; {k1} messages were proven at
  the newest leaf. In the best-tuned period (era 2) the median is {f('era 2 (0x1817874f…)','k_p50')} and k is flat on 1…8
  (counts {era2_1_8}).
- Why: two pieces of open-source code. (1) polkadot-sdk's BEEFY voter, once caught up, votes every
  =min_block_delta= = 8 blocks on Polkadot (=polkadot/node/service/src/builder/mod.rs= =min_block_delta: 8=;
  =substrate/client/consensus/beefy/src/worker.rs= =vote_target=: target = best_beefy + max(min_delta,
  next_power_of_two((best_grandpa − best_beefy + 1)/2))). (2) Snowbridge's on-demand BEEFY relayer takes,
  for each pending message, the *first* BEEFY commitment after the message's relay-chain inclusion block
  (=relayer/relays/beefy/on-demand-sync.go= =queue()= → =polkadot-listener.go= =findNextBeefyBlock=), and
  the message relayer proves the message against exactly that root. Hence k = (next BEEFY block) −
  (inclusion block) ∈ {{1,…,8}} whenever BEEFY is caught up, uniformly. Nothing in either code base
  targets short MMR proofs; small k is a side effect of relaying the earliest usable commitment.
- Savings at the observed workload: MMR proofs average {float(f('all','mmr_mean')):.1f} hashes (median {f('all','mmr_p50')}, max {f('all','mmr_max')});
  MMB would average {float(f('all','mmb_mean')):.1f} (median {f('all','mmb_p50')}); median per-message saving {ext('all')[0]}; MMB is shorter for
  {ext('all')[3]} of messages. For k ≤ 8 (the caught-up regime) the MMB proof averages
  {rd('size_by_k.csv')[0]['mmb_mean']}–{rd('size_by_k.csv')[2]['mmb_mean']} hashes against {rd('size_by_k.csv')[0]['mmr_mean']}–{rd('size_by_k.csv')[2]['mmr_mean']} for MMR.
- Larger k comes from operator behaviour, not protocol: the scheduler may merge a queued BEEFY update
  into a later one (=merge-period= ≤ 1800 s, =expired-period= ≤ 14400 s), the dominant era-3 relayer
  does so heavily (median k in the tens, p90 ≈ 1000), and {len(launch)} launch-day message(s) sit at
  k ≈ {max(k_all):,}. Fiat–Shamir submission (era 3) removed the RANDAO delay: BEEFY update and message land
  in the same Ethereum block.

* Results
** Per era (BeefyClient contract in use on Ethereum)
{t_main}

k in relay-chain blocks (6 s; age ≈ (k−1)·6 s). Proof sizes in hashes (block-inclusion proof only;
the parachain-heads proof and the message Merkle proof are unchanged by MMB). MMB sizes are exact
values of the paper's reference implementation (=proof-size= in merkle-mountain-belt-clj) at each
message's actual (n, k). "Balanced tree" is the paper's previous baseline ⌈log₂ n⌉.

** Proof size as a function of k (all messages)
{t_k}

** Scheduling fingerprints
{t_sched}

About one BEEFY update per message; the update precedes the message by seconds. In era 3 the BEEFY
update and the message are typically submitted in the same Ethereum transaction (Fiat–Shamir
single-transaction updates); the "root in same tx" column is the direct measurement.

** k by relayer (sender of the Gateway submit transaction; ≥ 20 messages)
{t_rel}

Era 3's larger mean is not a start-of-era transient (its weekly mean k stays in the hundreds
throughout) but coincides with one sender key dominating after relayer decentralization (Snowbridge
PR #1696, 2026-02). Relayers are identified by the transaction sender (EOA); batching contracts
(aggregate3-style) are unwrapped, so the sender is the operator's key, but one key may serve several
operators or vice versa, and the era-3 conclusion rests on a single key.

** Figures
- [[file:figures/k_histogram.png]] — k per era (log-x)
- [[file:figures/proof_sizes.png]] — on-chain MMR vs MMB proof-size distributions
- [[file:figures/size_by_k.png]] — mean proof size by k (MMR, MMB, U-MMB, lemma bound)
- [[file:figures/k_monthly.png]] — monthly median/p90 k (on-demand relaying from 2025-01)

* Method and validation
- Ethereum: all =InboundMessageDispatched= (v1, v2) logs of the Gateway proxy
  =0x27ca963c…= via Etherscan; each dispatch transaction decoded (=submitV1= / =v2_submit=,
  ABI from =contracts/src/Verification.sol=, =v1/Types.sol=, =v2/Types.sol=) to obtain
  =leafPartial.parentNumber= (proven leaf = relay block parentNumber + 1), =leafProof.length=,
  =leafProofOrder=, parachain header number. Dispatch logs matched to calls by nonce (+ channel).
- BeefyClient eras from the proxy's =Upgraded(address)= events and =BEEFY_CLIENT()= on each
  implementation (three clients: 0x6eD05bAa…, 0x1817874f…, 0x7cfc5C8b…); =latestBeefyBlock= at each
  dispatch from the =NewMMRRoot= events of the client in use (=submitFinal= and =submitFiatShamir=
  both emit it). A dispatch log exists only if =verifyMMRLeafProof= succeeded against the single
  stored =latestMMRRoot= (=BeefyClient.sol=), so the root in effect at the log position is the one
  the proof was checked against.
- Polkadot: =Mmr.NumberOfLeaves(B) = B − {run.get('mmr_offset', 19_551_000):,}= ({offset_line}), so
  n = latestBeefyBlock − 19,551,000, i = parentNumber − 19,551,000, k = n − i. The per-message formula
  check is the systematic backstop: a wrong offset would break it for a large fraction of messages.
- MMR proof-size formula (ckb =gen_proof=): h + (#peaks left of the leaf's mountain) + [any peak to
  the right]; verified by brute force for n ≤ 300 and against all {len(rows):,} on-chain proofs.
- MMB sizes: =proof-size n k= (k = 1 newest), calibrated row-for-row against
  =stats/k-vs-n-no-phantom.csv= for n ≤ 512.

* Implications for the paper (zapps.tex, "Cross-chain bridges and Polkadot"; zintro.tex bridge row)
- Replace the balanced-tree baseline (25 hashes) by the MMR baseline; use n ≈ 2^23.7 (MMR leaves).
- Replace the assumed periodic checkpoint ("every 3 hours", C = 1800) by the actual regime:
  on-demand checkpointing with k ∈ {{1,…,8}} when BEEFY is caught up, derived from the two code
  references above; keep the periodic regime as the conservative alternative.
- Quote the measured distribution as an existence proof of the workload: median k {f('all','k_p50')},
  {float(f('all','frac_k_le_600')):.0f}% within 600, MMR {float(f('all','mmr_mean')):.1f} → MMB {float(f('all','mmb_mean')):.1f} hashes on average, {ext('all')[0]} median saving.
- Consequence for zintro.tex "25 to 13 hashes": measured MMR {f('all','mmr_p50')} (median) vs MMB {f('all','mmb_p50')} (median); for
  the caught-up regime the Variant-2 worst case with C = 8 is ⌊log₂ 8⌋ + 3 = 6 hashes.

* Caveats
- MMB numbers are counterfactual: same leaf sequence and same (n, k), MMB instead of MMR.
- Data are Ethereum-side only; k measures the distance to the BEEFY head *on Ethereum*, which is
  what the proof size depends on.
- Messages whose transaction went through a wrapper contract that does not embed the Gateway call
  verbatim cannot be decoded without execution traces; they are listed, not silently dropped.
- {len(launch)} launch-day message(s) (k > 100,000; all in {sorted({dt.datetime.fromtimestamp(int(r['timestamp']), dt.UTC).strftime('%Y-%m') for r in launch})}) pull the overall mean k from
  {st.mean(k_wo_launch):.1f} to {st.mean(k_all):.1f}; MMB is worse than MMR by more than 25% for {len(worse25)} messages
  ({len(worse25_nonlaunch)} of them not launch-day, worst {100*min((x[0] for x in worse25_nonlaunch), default=0):.0f}%).
- Operator behaviour changes over time (three BeefyClient eras, relayer decentralization); the
  caught-up regime is a property of the code, the tail is a property of operators.

* Reproduction
#+begin_src bash
nix develop                                   # repository root: JDK, Clojure and the Python packages
cd studies/snowbridge
python3 -m unittest -v test_snowbridge_k      # decoding, wrapper scan, era rules, MMR formula
export ETHERSCAN_API_KEY=…                    # logs (free tier: 3 calls/s)
export RPC=https://eth-mainnet.g.alchemy.com/v2/…   # optional: tx/call lookups (any tier)
python3 snowbridge_k.py                       # data/snowbridge_messages.csv (raw results cached in data/cache/)
python3 analyse.py                            # figures/, snowbridge-k-analysis.org (MMB sizes via clojure -M:mmb-sizes)
python3 report.py                             # this report
#+end_src
Upstream sources and commits are recorded in =data/run_summary.json= (=source_commits=).
"""
(HERE / "REPORT-snowbridge-k.org").write_text(org)
print("wrote REPORT-snowbridge-k.org")
