#!/usr/bin/env python3
"""
Analyse data/snowbridge_messages.csv (from snowbridge_k.py):
  * histogram of k (leaves appended after the proven leaf = "distance from best"),
  * on-chain MMR proof sizes vs MMB / U-MMB proof sizes for the same (n, k),
  * per-era and monthly summaries,
and write figures/*.png, figures/*.csv and snowbridge-k-analysis.org.

MMB sizes come from the paper's reference implementation (src/mmb_sizes.clj -> proof-size),
invoked as `clojure -M:mmb-sizes` from the repository root (run inside its devShell) unless
data/mmb_sizes.csv already covers all (n, k) pairs.

Convention (paper): k = 1 is the newest leaf; the k-th newest leaf has k-1 leaves appended
after it, i.e. it is about (k-1)*6 s old.  Clojure proof-size and Lemma lem:a-mmb use the same k.
"""
import argparse
import csv
import datetime as dt
import json
import math
import os
import subprocess
import sys
from collections import Counter, defaultdict
collections_counter = Counter
from pathlib import Path

import numpy as np

from mmr_proof_size import balanced_items, mmr_items

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
FIG = HERE / "figures"
REPO = HERE.parents[1]            # merkle-mountain-belt-clj root (this file lives in studies/snowbridge/)
BLOCK_SECONDS = 6
PAPER_K = [50, 600, 1800, 7200]   # k values quoted in zapps.tex (5 min, 1 h, 3 h, 12 h)

# dataviz reference palette (light mode)
BLUE, ORANGE, AQUA, YELLOW = "#2a78d6", "#eb6834", "#1baf7a", "#eda100"
INK, INK2, GRID = "#0b0b0b", "#52514e", "#e6e5e1"


def lemma_mmb_bound(k_paper: int) -> float:
    """Upper bound on the amortized MMB proof size, Lemma lem:a-mmb (zamortized.tex)."""
    k = k_paper
    if k == 1:
        return 17 / 8
    if k == 2:
        return 55 / 16
    d = int(math.floor(math.log2(k + 1)))
    if k + 1 < 1.5 * 2 ** d:
        return 11 / 8 * d + (4 * k + 1) / 2 ** (d + 1) + 1 / 8
    return 11 / 8 * d + (3 * k - 3) / 2 ** (d + 2) + 2


def load_rows(path):
    rows = []
    with open(path) as f:
        for r in csv.DictReader(f):
            for c in ("ethBlock", "logIndex", "latestBeefyBlock", "parentNumber", "leafBlock", "paraBlock",
                      "k", "n", "i", "mmr_items_onchain", "mmr_items_formula", "success", "nonce",
                      "headProofLen", "msgLeafProofLen"):
                r[c] = int(r[c])
            r["timestamp"] = int(r["timestamp"]) if r["timestamp"] else None
            r["rootSameTx"] = int(r["rootSameTx"]) if r.get("rootSameTx") not in (None, "") else None
            r["rootEthBlock"] = int(r["rootEthBlock"]) if r.get("rootEthBlock") not in (None, "") else None
            rows.append(r)
    return rows


def mmb_sizes(rows, force=False):
    """Return dict (n, k_paper) -> (mmb, ummb), computing missing pairs via mmb_sizes.clj."""
    out_path = DATA / "mmb_sizes.csv"
    have = {}
    if out_path.exists() and not force:
        with open(out_path) as f:
            for r in csv.DictReader(f):
                have[(int(r["n"]), int(r["k"]))] = (int(r["mmb_items"]), int(r["ummb_items"]))
    DATA.mkdir(exist_ok=True)
    need = sorted({(r["n"], r["k"]) for r in rows} - set(have))
    if need:
        pairs = DATA / "mmb_pairs.csv"
        with open(pairs, "w") as f:
            f.write("n,k\n")
            f.writelines(f"{n},{k}\n" for n, k in need)
        new = DATA / "mmb_sizes_new.csv"
        cmd = ["clojure", "-M:mmb-sizes", str(pairs), str(new)]   # src/mmb_sizes.clj; run inside the repo devShell
        print(f"computing {len(need)} MMB sizes via clojure -M:mmb-sizes ...", file=sys.stderr)
        subprocess.run(cmd, check=True, cwd=REPO)
        with open(new) as f:
            for r in csv.DictReader(f):
                have[(int(r["n"]), int(r["k"]))] = (int(r["mmb_items"]), int(r["ummb_items"]))
        with open(out_path, "w") as f:
            f.write("n,k,mmb_items,ummb_items\n")
            f.writelines(f"{n},{k},{a},{b}\n" for (n, k), (a, b) in sorted(have.items()))
        pairs.unlink(); new.unlink()
    return have


def q(xs, p):
    xs = sorted(xs)
    return xs[min(len(xs) - 1, int(len(xs) * p))] if xs else float("nan")


def fmt_minutes(k):
    """Age of the k-th newest leaf (k-1 leaves appended after it)."""
    m = (k - 1) * BLOCK_SECONDS / 60
    return f"{m:.0f} min" if m < 120 else f"{m/60:.1f} h"


def summarize(rows):
    ks = [r["k"] for r in rows]
    s = {"messages": len(rows),
         "k_p50": q(ks, .5), "k_p90": q(ks, .9), "k_p99": q(ks, .99), "k_max": max(ks), "k_mean": np.mean(ks),
         "n_min": min(r["n"] for r in rows), "n_max": max(r["n"] for r in rows),
         "mmr_mean": np.mean([r["mmr_items_onchain"] for r in rows]),
         "mmr_p50": q([r["mmr_items_onchain"] for r in rows], .5),
         "mmr_max": max(r["mmr_items_onchain"] for r in rows),
         "mmb_mean": np.mean([r["mmb"] for r in rows]),
         "mmb_p50": q([r["mmb"] for r in rows], .5),
         "mmb_max": max(r["mmb"] for r in rows),
         "ummb_mean": np.mean([r["ummb"] for r in rows]),
         "lemma_mean": np.mean([r["lemma"] for r in rows]),
         "balanced_mean": np.mean([r["balanced"] for r in rows]),
         "relayers": len({r["relayer"] for r in rows}),
         "failed_dispatch": sum(1 for r in rows if not r["success"]),
         "launch_k": sum(1 for r in rows if r["k"] > 100_000)}
    s["saving_hashes"] = s["mmr_mean"] - s["mmb_mean"]
    s["saving_pct"] = 100 * s["saving_hashes"] / s["mmr_mean"]
    for K in PAPER_K:
        s[f"frac_k_le_{K}"] = 100 * sum(1 for k in ks if k <= K) / len(ks)
    return s


def scheduling_stats(rows, era_name):
    """Per era: how the BEEFY update a message was verified against relates to the message.
    gap = dispatch timestamp − timestamp of that NewMMRRoot (seconds)."""
    import bisect, json
    cache = DATA / "cache" / "roots.json"
    if not cache.exists():
        return None
    roots = defaultdict(list)
    for x in json.load(open(cache))["items"]:
        roots[x["address"]].append((x["blockNumber"], x["logIndex"], x["ts"]))
    for c in roots:
        roots[c].sort()
    out = {}
    for c, name in era_name.items():
        g = [r for r in rows if r["beefyClient"] == c]
        keys = [(b, i) for b, i, _ in roots[c]]
        gaps, used = [], set()
        for r in g:
            j = bisect.bisect_left(keys, (r["ethBlock"], r["logIndex"])) - 1
            b, i, ts = roots[c][j]
            used.add((b, i))
            if r["timestamp"] and ts:
                gaps.append(r["timestamp"] - ts)
        same_tx = [r["rootSameTx"] for r in g if r["rootSameTx"] is not None]
        same_block = [int(r["rootEthBlock"] == r["ethBlock"]) for r in g if r["rootEthBlock"] is not None]
        out[name] = {"roots": len(roots[c]), "messages": len(g), "roots_used": len(used),
                     "msgs_per_used_root": len(g) / max(1, len(used)),
                     "gap_p50": q(gaps, .5), "gap_p90": q(gaps, .9),
                     "same_tx_pct": 100 * sum(same_tx) / len(same_tx) if same_tx else float("nan"),
                     "same_block_pct": 100 * sum(same_block) / len(same_block) if same_block else float("nan"),
                     "frac_k_le_8": 100 * sum(1 for r in g if r["k"] <= 8) / len(g)}
    return out


def org_table(headers, rows_):
    w = [max(len(str(x)) for x in col) for col in zip(headers, *rows_)]
    line = "|" + "|".join("-" * (x + 2) for x in w) + "|"
    fmt = lambda r: "| " + " | ".join(str(x).ljust(x_w) for x, x_w in zip(r, w)) + " |"
    return "\n".join([fmt(headers), line] + [fmt(r) for r in rows_])


def style(ax):
    ax.set_facecolor("#fcfcfb")
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_color(GRID)
    ax.tick_params(colors=INK2, labelsize=9)
    ax.yaxis.grid(True, color=GRID, linewidth=0.8)
    ax.set_axisbelow(True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default=str(DATA / "snowbridge_messages.csv"))
    ap.add_argument("--force-mmb", action="store_true")
    args = ap.parse_args()
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    plt.rcParams.update({"font.size": 10, "text.color": INK, "axes.labelcolor": INK2,
                         "axes.titlecolor": INK, "figure.facecolor": "#fcfcfb"})

    rows = load_rows(args.csv)
    if not rows:
        sys.exit("no rows")
    summary_path = DATA / "run_summary.json"
    run = json.loads(summary_path.read_text()) if summary_path.exists() else {}
    mism = sum(1 for r in rows if r["mmr_items_onchain"] != r["mmr_items_formula"])
    sizes = mmb_sizes(rows, args.force_mmb)
    for r in rows:
        r["mmb"], r["ummb"] = sizes[(r["n"], r["k"])]
        r["lemma"] = lemma_mmb_bound(r["k"])
        r["balanced"] = balanced_items(r["n"])
        r["date"] = dt.datetime.fromtimestamp(r["timestamp"], dt.timezone.utc) if r["timestamp"] else None
    lemma_viol = sum(1 for r in rows if r["mmb"] > r["lemma"] + 1e-9)  # informational: lemma bounds the *amortized* size

    # eras in order of first appearance
    first = {}
    for r in sorted(rows, key=lambda r: (r["ethBlock"], r["logIndex"])):
        first.setdefault(r["beefyClient"], r)
    eras = list(first)
    era_name = {c: f"era {i+1} ({c[:10]}…)" for i, c in enumerate(eras)}
    for r in rows:
        r["era"] = era_name[r["beefyClient"]]

    FIG.mkdir(exist_ok=True)
    try:
        clj_commit = subprocess.run(["git", "-C", str(REPO), "rev-parse", "HEAD"], capture_output=True, text=True, timeout=10).stdout.strip() or None
    except Exception:
        clj_commit = None
    (FIG / "analysis_meta.json").write_text(json.dumps({
        "analysed_at": dt.datetime.now(dt.UTC).isoformat(timespec="seconds"), "csv": os.path.relpath(args.csv, HERE), "rows": len(rows),
        "run_id": run.get("run_id"), "run_at": run.get("run_at"), "collector_commit": run.get("repo_commit"),
        "mmb_reference_impl": {"repo": "merkle-mountain-belt-clj", "commit": clj_commit}}, indent=1))
    groups = [("all", rows)] + [(era_name[c], [r for r in rows if r["beefyClient"] == c]) for c in eras]
    summaries = {name: summarize(g) for name, g in groups}

    # ---- tables --------------------------------------------------------------------
    cols = ["messages", "relayers", "failed_dispatch", "launch_k", "k_p50", "k_p90", "k_p99", "k_max", "k_mean", "n_min", "n_max",
            "mmr_mean", "mmr_p50", "mmr_max", "mmb_mean", "mmb_p50", "mmb_max", "ummb_mean", "lemma_mean",
            "balanced_mean", "saving_hashes", "saving_pct"] + [f"frac_k_le_{K}" for K in PAPER_K]
    with open(FIG / "summary.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["group"] + cols)
        for name, s in summaries.items():
            w.writerow([name] + [f"{s[c]:.2f}" if isinstance(s[c], float) else s[c] for c in cols])

    sched = scheduling_stats(rows, era_name)
    if sched:
        with open(FIG / "scheduling.csv", "w", newline="") as f:
            w = csv.writer(f); w.writerow(["era", "NewMMRRoot_events", "messages", "roots_used", "msgs_per_used_root", "gap_p50_s", "gap_p90_s", "root_same_tx_pct", "root_same_block_pct", "frac_k_le_8_pct"])
            for name, t in sched.items():
                w.writerow([name, t["roots"], t["messages"], t["roots_used"], f"{t['msgs_per_used_root']:.2f}", t["gap_p50"], t["gap_p90"], f"{t['same_tx_pct']:.1f}", f"{t['same_block_pct']:.1f}", f"{t['frac_k_le_8']:.1f}"])

    # per relayer (sender of the Gateway submit tx), per era
    byrel = defaultdict(list)
    for r in rows:
        byrel[(r["era"], r["relayer"])].append(r["k"])
    relayer_rows = [[era, rel, len(ks), q(ks, .5), f"{np.mean(ks):.0f}", q(ks, .9)]
                    for (era, rel), ks in sorted(byrel.items(), key=lambda x: (x[0][0], -len(x[1]))) if len(ks) >= 20]
    with open(FIG / "relayers.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["era", "relayer", "messages", "k_p50", "k_mean", "k_p90"]); w.writerows(relayer_rows)

    # rows for Table tab:polkadot in the paper: cumulative shares and means at the table's k columns
    TABLE_K = [2400, 600, 100, 15, 8]
    with open(FIG / "table_polkadot.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["k", "share_k_le_pct", "mmr_mean_k_le", "mmb_mean_k_le", "messages_k_le"])
        for K in TABLE_K:
            g = [r for r in rows if r["k"] <= K]
            w.writerow([K, f"{100*len(g)/len(rows):.1f}", f"{np.mean([r['mmr_items_onchain'] for r in g]):.2f}",
                        f"{np.mean([r['mmb'] for r in g]):.2f}", len(g)])

    # monthly
    monthly = defaultdict(list)
    for r in rows:
        if r["date"]:
            monthly[r["date"].strftime("%Y-%m")].append(r)
    months = sorted(monthly)
    with open(FIG / "monthly.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["month", "messages", "k_p50", "k_p90", "mmr_mean", "mmb_mean", "eras"])
        for m in months:
            g = monthly[m]
            w.writerow([m, len(g), q([r["k"] for r in g], .5), q([r["k"] for r in g], .9),
                        f"{np.mean([r['mmr_items_onchain'] for r in g]):.2f}", f"{np.mean([r['mmb'] for r in g]):.2f}",
                        "/".join(sorted({r["era"][:5] for r in g}))])

    # proof size as a function of k (log bins)
    kmax_ = max(r["k"] for r in rows)
    edges = [e for e in (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288) if e <= kmax_] + [kmax_ + 1]
    bink = defaultdict(list)
    for r in rows:
        b = next(j for j in range(len(edges) - 1) if edges[j] <= r["k"] < edges[j + 1])
        bink[b].append(r)
    with open(FIG / "size_by_k.csv", "w", newline="") as f:
        w = csv.writer(f); w.writerow(["k_from", "k_to", "messages", "mmr_mean", "mmb_mean", "ummb_mean", "lemma_mean", "balanced_mean"])
        for b in sorted(bink):
            g = bink[b]
            w.writerow([edges[b], edges[b + 1] - 1, len(g)] + [f"{np.mean([r[c] for r in g]):.2f}"
                       for c in ("mmr_items_onchain", "mmb", "ummb", "lemma", "balanced")])

    # ---- figures --------------------------------------------------------------------
    # 1. histogram of k, overall + per era (small multiples, shared log-x)
    kmax = max(r["k"] for r in rows)
    bins = np.logspace(0, math.log10(kmax + 1), 60)
    fig, axes = plt.subplots(len(groups), 1, figsize=(8, 2.2 * len(groups)), sharex=True)
    axes = np.atleast_1d(axes)
    for ax, (name, g) in zip(axes, groups):
        ks = np.array([r["k"] for r in g])
        ax.hist(ks, bins=bins, color=BLUE, edgecolor="#fcfcfb", linewidth=0.6)
        style(ax)
        ax.set_xscale("log")
        s = summaries[name]
        ax.set_title(f"{name}: {s['messages']} messages, median k = {s['k_p50']} ({fmt_minutes(s['k_p50'])}), "
                     f"p90 = {s['k_p90']} ({fmt_minutes(s['k_p90'])})", fontsize=9, loc="left")
        ax.set_ylabel("messages")
    ticks = [k for k in (1, 10, 100, 1000, 10000, 100000) if k <= kmax]
    axes[-1].set_xticks(ticks)
    axes[-1].set_xticklabels([f"{k}\n({fmt_minutes(k)})" for k in ticks])
    axes[-1].set_xlabel("k = position of the proven leaf from the newest MMR leaf (k = 1 newest; age ≈ (k−1)·6 s); log scale")
    fig.suptitle("Snowbridge Polkadot→Ethereum: distance k of the proven leaf below the BeefyClient's BEEFY head", fontsize=10, x=0.01, ha="left")
    fig.tight_layout()
    fig.savefig(FIG / "k_histogram.png", dpi=150); plt.close(fig)

    # 2. proof-size distributions: on-chain MMR vs MMB (same messages)
    cm, cb = Counter(r["mmr_items_onchain"] for r in rows), Counter(r["mmb"] for r in rows)
    xs = list(range(min(min(cm), min(cb)), max(max(cm), max(cb)) + 1))
    fig, ax = plt.subplots(figsize=(8, 3.6))
    w = 0.4
    ax.bar([x - w / 2 for x in xs], [cm.get(x, 0) for x in xs], width=w, color=BLUE, label="MMR (on-chain leafProof length)")
    ax.bar([x + w / 2 for x in xs], [cb.get(x, 0) for x in xs], width=w, color=ORANGE, label="MMB (same n, k)")
    style(ax)
    ax.set_xlabel("block-inclusion proof size (hashes)"); ax.set_ylabel("messages")
    ax.legend(frameon=False, fontsize=9)
    s = summaries["all"]
    ax.set_title(f"Proof sizes for {s['messages']} messages: mean MMR {s['mmr_mean']:.1f}, mean MMB {s['mmb_mean']:.1f} "
                 f"(−{s['saving_pct']:.0f}%)", fontsize=9, loc="left")
    fig.tight_layout(); fig.savefig(FIG / "proof_sizes.png", dpi=150); plt.close(fig)

    # 3. proof size vs k (binned means)
    fig, ax = plt.subplots(figsize=(8, 3.6))
    bx = [math.sqrt(edges[b] * edges[b + 1]) for b in sorted(bink)]
    for col, name, color in (("mmr_items_onchain", "MMR (on-chain)", BLUE), ("mmb", "MMB", ORANGE),
                             ("ummb", "U-MMB", AQUA), ("lemma", "Lemma bound (MMB, amortized)", YELLOW)):
        ax.plot(bx, [np.mean([r[col] for r in bink[b]]) for b in sorted(bink)], marker="o", ms=4, lw=2, color=color, label=name)
    style(ax); ax.set_xscale("log")
    ax.set_xlabel("k (log scale, bin geometric centre)"); ax.set_ylabel("mean proof size (hashes)")
    ax.legend(frameon=False, fontsize=9); ax.set_title("Mean block-inclusion proof size by k", fontsize=9, loc="left")
    fig.tight_layout(); fig.savefig(FIG / "size_by_k.png", dpi=150); plt.close(fig)

    # 4. monthly median / p90 of k
    if months:
        fig, ax = plt.subplots(figsize=(8, 3.2))
        xs_ = [dt.datetime.strptime(m, "%Y-%m") for m in months]
        ax.plot(xs_, [q([r["k"] for r in monthly[m]], .5) for m in months], color=BLUE, lw=2, marker="o", ms=4, label="median k")
        ax.plot(xs_, [q([r["k"] for r in monthly[m]], .9) for m in months], color=ORANGE, lw=2, marker="o", ms=4, label="p90 k")
        style(ax); ax.set_yscale("log"); ax.set_ylabel("k (relay-chain blocks)")
        ax.legend(frameon=False, fontsize=9); ax.set_title("Monthly k (BeefyClient eras change relayer behaviour)", fontsize=9, loc="left")
        fig.autofmt_xdate(); fig.tight_layout(); fig.savefig(FIG / "k_monthly.png", dpi=150); plt.close(fig)

    # ---- org notes -------------------------------------------------------------------
    s = summaries["all"]
    d0 = min(r["date"] for r in rows if r["date"]); d1 = max(r["date"] for r in rows if r["date"])
    hdr = ["group"] + cols
    tbl = [[name] + [f"{sm[c]:.2f}" if isinstance(sm[c], float) else sm[c] for c in cols] for name, sm in summaries.items()]
    era_rows = [[era_name[c], first[c]["ethBlock"], first[c]["date"].date() if first[c]["date"] else "", c,
                 summaries[era_name[c]]["messages"]] for c in eras]
    kbin_rows = [[edges[b], edges[b + 1] - 1, len(bink[b])] + [f"{np.mean([r[c] for r in bink[b]]):.2f}"
                 for c in ("mmr_items_onchain", "mmb", "ummb", "lemma", "balanced")] for b in sorted(bink)]
    org = f"""#+TITLE: Snowbridge Polkadot→Ethereum: distance k of proven leaves and MMR vs MMB proof sizes
#+DATE: {dt.date.today()}
#+OPTIONS: toc:2

Generated by =analyse.py= from =data/snowbridge_messages.csv= (=snowbridge_k.py=). Do not edit numbers by hand.

* Question
For the paragraph "Cross-chain bridges and Polkadot" in =../paper-wip/zapps.tex= (and =zintro.tex:214=):
what is the empirical distribution of k, the number of MMR leaves appended after the leaf a Snowbridge
message proof is anchored to, relative to the BEEFY root held by the Ethereum BeefyClient at verification
time, and how do the actual MMR proof sizes compare with MMB proof sizes for the same (n, k)?

* Method (verified against source, see plan and script docstrings)
- Every =InboundMessageDispatched= log on the Gateway (=0x27ca963c…=) is a message whose
  =Verification.Proof= was verified by =BeefyClient.verifyMMRLeafProof= against the single stored
  =latestMMRRoot=. The proven leaf is the MMR leaf of relay block =leafPartial.parentNumber + 1=.
- =latestBeefyBlock= at the log position is reconstructed from =NewMMRRoot= events of the BeefyClient the
  Gateway implementation used at that time (eras from =Upgraded(address)= + =BEEFY_CLIENT()=).
- Polkadot MMR leaf count at relay block B is n = B − 19,551,000 (checked via =Mmr.NumberOfLeaves=);
  leaf index i = parentNumber − 19,551,000; k = n − i (paper convention: k = 1 is the newest leaf,
  so a message at k has k − 1 leaves appended after it, i.e. is about (k − 1)·6 s old).
- MMR proof size = on-chain =leafProof.length=, cross-checked against the ckb-mmr formula
  h + (#peaks left) + [any peak right] for every message.
- MMB and U-MMB sizes from the paper's reference implementation (=proof-size= in
  merkle-mountain-belt-clj; its k=1..n matches =stats/k-vs-n-no-phantom.csv= row-for-row, n ≤ 512).
- Lemma lem:a-mmb bound evaluated at the same k (bounds the *amortized* size, so single
  messages may exceed it).

* Validation
- Messages: {s['messages']} ({d0.date()} … {d1.date()}), Ethereum blocks {min(r['ethBlock'] for r in rows)}…{max(r['ethBlock'] for r in rows)}.
- Dispatch logs in range: {run.get('dispatch_logs', 'n/a')}; decoded and attributed: {len(rows)}{f" ({100*len(rows)/run['dispatch_logs']:.1f}%)" if run.get('dispatch_logs') else ""}; not decoded: {len(run.get('undecoded', []))} (=data/undecoded_dispatches.csv=; reasons {dict(collections_counter(x['reason'] for x in run.get('undecoded', [])))}).
- Decode path of the decoded messages: {run.get('rows_by_decode_method', 'n/a')} (direct Gateway call / aggregate3 batch / calldata scan of a wrapper contract).
- MMR formula vs on-chain leafProof length mismatches: {mism}.
- Other proof components, unchanged by MMB (hashes, mean): parachain-heads proof {np.mean([r['headProofLen'] for r in rows]):.2f}, message Merkle proof {np.mean([r['msgLeafProofLen'] for r in rows]):.2f}; MMR block-inclusion proof {np.mean([r['mmr_items_onchain'] for r in rows]):.2f}.
- Messages whose exact MMB size exceeds the lemma's amortized bound: {lemma_viol} (informational).
- MMR leaf count n ranged {s['n_min']:,} … {s['n_max']:,} (≈ 2^{math.log2(s['n_min']):.2f} … 2^{math.log2(s['n_max']):.2f}).

* BeefyClient eras
{org_table(["era", "first eth block", "first date", "BeefyClient", "messages"], era_rows)}

* Summary per era
k: paper convention (1 = newest leaf; age ≈ (k−1)·6 s); proof sizes in hashes; frac_k_le_K in percent.
{org_table(hdr, tbl)}

* Proof size by k (all messages)
{org_table(["k from", "k to", "messages", "MMR mean", "MMB mean", "U-MMB mean", "lemma", "balanced"], kbin_rows)}

* Why k is small: relayer scheduling (Snowfork/snowbridge @ e65a6a9)
{org_table(["era", "NewMMRRoot events", "messages", "roots used by a message", "msgs per used root", "gap p50 (s)", "gap p90 (s)", "root in same tx (%)", "same block (%)", "k ≤ 8 (%)"],
           [[name, t["roots"], t["messages"], t["roots_used"], f"{t['msgs_per_used_root']:.2f}", t["gap_p50"], t["gap_p90"], f"{t['same_tx_pct']:.1f}", f"{t['same_block_pct']:.1f}", f"{t['frac_k_le_8']:.1f}"] for name, t in sched.items()]) if sched else "(no data/cache/roots.json)"}

gap = seconds between the =NewMMRRoot= a message was verified against and the message's dispatch
(block granularity, 12 s); "root in same tx" = the BEEFY update and the message were submitted in
the same Ethereum transaction, the stronger statement.

- The BEEFY commitment is chosen *per pending message*, not on a schedule. The on-demand relayer
  (=relayer/relays/beefy/on-demand-sync.go=, =queue()=) finds the relay-chain block in which the
  parachain block carrying the message was included (=fetchRelaychainInclusionBlock=, scanning
  =RelayParentNumber+1 …+8=), then =polkadotListener.generateBeefyUpdate(relayBlock)= →
  =findNextBeefyBlock= waits until the BEEFY finalized head is past that block and takes the
  commitment of the *current BEEFY finalized head* (=beefy_getFinalizedHead=). So the update lands
  a few blocks after inclusion, and k = (BEEFY head at that moment) − (inclusion block + 1) + 1, which
  is the BEEFY finality lag: the flat k = 1…8 block seen in eras 2 and 3. Nothing in the relayer
  targets a small MMR proof; the small k is a side effect of relaying the earliest usable commitment.
- The message relayer (=relayer/relays/parachain*/beefy-listener.go=) reacts to every =NewMMRRoot=
  event, reads =latestBeefyBlock= from the BeefyClient and calls =mmr_generateProof(inclusion+1,
  latestBeefyBlockHash)=, so the proof is always against the freshest root on Ethereum.
- Larger k arises from (a) tasks merged by the scheduler: a queued update is marked skippable when a
  later one exists within =merge-period= (≤ 1800 s) so earlier messages ride a later commitment
  (=task-map.go= =Pop()=; PR #1530, 2025-07); (b) the mandatory session-change relayer
  (=scanner.go=, one commitment per session) occasionally serving as the anchor; (c) pre-on-demand
  history: PR #1342 "Sync beefy commitment on demand" merged 2024-12-05, which matches the drop of
  the monthly median k from ~1000 to ~13 in 2025-01.
- Era 3 (BeefyClient 0x7cfc, 2026-04): =submitFiatShamir= replaces the RANDAO two-step
  (=submitInitial= → wait → =submitFinal=), so the BEEFY update and the message land in the same
  Ethereum block (gap 0 s); relayer PR #1669 (2026-01), "Relayer Decentralization" PR #1696 (2026-02).

* k by relayer (sender of the Gateway submit tx; relayers with ≥ 20 messages)
{org_table(["era", "relayer", "messages", "k p50", "k mean", "k p90"], relayer_rows)}

Era 3's larger mean k is not a start-of-era transient (weekly mean k stays in the hundreds throughout)
but a per-operator effect: the dominant era-3 relayer submits with median k in the tens and p90 around a thousand, while
the other era-3 relayers and the era-2 relayers sit at single-digit median k (see table). Plausible cause: that operator's
on-demand BEEFY relayer batches messages behind a later commitment (=merge-period= ≤ 1800 s,
=expired-period= ≤ 14400 s), which the code permits; not verifiable from on-chain data alone.

* Figures
- [[file:figures/k_histogram.png]]
- [[file:figures/proof_sizes.png]]
- [[file:figures/size_by_k.png]]
- [[file:figures/k_monthly.png]]

* Implications for the paper text (to be applied in the rewrite, not here)
- =zapps.tex= "As of 2026, Polkadot has n≈2^25 blocks … balanced tree … 25 hashes": the BEEFY commitment is an
  MMR whose leaf count is n ≈ {s['n_max']:,} ≈ 2^{math.log2(s['n_max']):.1f} (MMR pallet active since relay block 19,551,001), and
  the baseline must be the MMR proof size, measured here at mean {s['mmr_mean']:.1f} (max {s['mmr_max']}) hashes,
  not a balanced tree ({s['balanced_mean']:.0f} hashes).
- "the target network updates its BEEFY checkpoint every 3 hours … 1800 latest blocks": measured median k is
  {s['k_p50']} blocks ({fmt_minutes(s['k_p50'])}), p90 {s['k_p90']} ({fmt_minutes(s['k_p90'])}); {s['frac_k_le_1800']:.0f}% of messages have k ≤ 1800 and
  {s['frac_k_le_600']:.0f}% have k ≤ 600. Snowbridge's on-demand BEEFY relayer syncs a fresh commitment whenever messages are pending
  (=relayer/relays/beefy/on-demand-sync.go=), so most proofs are anchored close to the head.
- Expected savings with MMB at the measured k distribution: mean proof {s['mmb_mean']:.1f} vs {s['mmr_mean']:.1f} hashes
  ({s['saving_pct']:.0f}% fewer); U-MMB mean {s['ummb_mean']:.1f}.
- =zintro.tex:214= "from approximately 25 to 13 hashes": replace by the measured MMR mean/max and MMB mean/max above.
"""
    (HERE / "snowbridge-k-analysis.org").write_text(org)
    print(org_table(hdr, tbl))
    print(f"\nwrote {FIG}/ and snowbridge-k-analysis.org; MMR mismatches={mism}")


if __name__ == "__main__":
    main()
