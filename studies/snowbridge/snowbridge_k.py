#!/usr/bin/env python3
"""
Snowbridge Polkadot -> Ethereum messages: for every message dispatched by the Gateway on
Ethereum mainnet, recover

    k = latestBeefyBlock - leafBlock + 1    (leafBlock = leafPartial.parentNumber + 1)

i.e. the position of the proven leaf counted from the newest leaf (k = 1: newest; the
paper's convention), under the BEEFY root the BeefyClient held when the proof was
verified; plus the actual on-chain MMR proof length (`len(headerProof.leafProof)`) and
the MMR leaf count n, so MMR proof sizes can be validated against the ckb-mmr formula
and compared with MMB proof sizes.

Method (verified against Snowfork/snowbridge @ e65a6a9, polkadot-sdk, ckb-mmr):
 1. Eras: the Gateway's BeefyClient is an `immutable` of each implementation. Read the
    proxy's `Upgraded(address indexed implementation)` events and call `BEEFY_CLIENT()`
    on each implementation to get [(position, beefyClient)], where position is
    (ethBlock, logIndex) of the Upgraded event, so that a dispatch earlier in the same
    block is attributed to the previous implementation. The proxy constructor emits no
    event, so the initial client is `--initial-beefy-client`.
 2. Roots: `NewMMRRoot(bytes32 mmrRoot, uint64 blockNumber)` from every BeefyClient,
    ordered by (ethBlock, logIndex): latestBeefyBlock as a step function per client.
    (submitFinal and submitFiatShamir both emit it; the root set at construction does
    not, so dispatches verified against a client's *initial* root are counted as
    `no_root_yet` and dropped.)
 3. Dispatches: `InboundMessageDispatched` v1/v2 on the Gateway. A dispatch log exists
    iff verifyMMRLeafProof succeeded against the root in effect at that log position.
 4. Decode each dispatch tx: a direct Gateway call (`submitV1` / `v2_submit`), a
    Multicall3-style `aggregate3` batch, or -- for arbitrary wrapper contracts -- a scan
    of the calldata for an embedded, verbatim Gateway call (the wrapper's `bytes`
    argument). Records are matched to dispatch logs by (channelID, nonce, message id) for v1
    and (nonce, XCM topic) for v2, which also filters false positives of the scan; a
    completeness gate then checks that nonces are contiguous per channel over decoded plus
    undecoded dispatches (v1 nonces are sequential per channel, v2 nonces sequential). Transactions that still cannot be decoded
    are listed in data/undecoded_dispatches.csv with their raw input cached.
 5. k, n, i:  n = latestBeefyBlock - MMR_OFFSET,  i = parentNumber - MMR_OFFSET,
    k = n - i (k = 1 is the newest leaf).  MMR_OFFSET = 19_551_000 (Polkadot
    Mmr.NumberOfLeaves at block B is B - 19_551_000; re-verified at run time via
    state_getStorage unless --skip-dot).
 6. Validate mmr_items(i, n) == len(leafProof) for every message (exit code 1 on any
    mismatch), and write data/run_summary.json with every counter of the run.

Data sources (either or both):
  ETHERSCAN_API_KEY  event logs via Etherscan v2 (module=logs, free tier: 3 calls/s), and,
                     if RPC is unset, transactions / eth_call via its proxy module.
  RPC                JSON-RPC endpoint (e.g. Alchemy, any tier) for transactions, eth_call
                     and block timestamps; also used for logs if ETHERSCAN_API_KEY is unset
                     (needs wide-range eth_getLogs, i.e. a paid plan).
  --offline          no network: rebuild the CSV from data/cache/ only.

Usage:  ETHERSCAN_API_KEY=... [RPC=https://...] python3 snowbridge_k.py [--from-block 19000000]
Caches raw results under data/cache/ so reruns only fetch new blocks; the log caches are
keyed by (addresses, topics) and refetched if that key changes.
"""
import argparse
import bisect
import collections
import csv
import datetime as dt
import json
import os
import subprocess
import sys
import threading
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import requests
from eth_abi import decode
from eth_utils import keccak, to_checksum_address

from mmr_proof_size import mmr_items

VERSION = "2026-09-25"
DECODER_VERSION = 2                # bump whenever decode_submit / ABI types / match keys change: cached tx records are re-decoded
SOURCE_COMMITS = {                 # upstream sources the method was verified against (see docstring)
    "Snowfork/snowbridge": "e65a6a9451a86b027e9d17706ac3917bdbdff697",
    "paritytech/polkadot-sdk": "cccea0c9393792a97a7814b88ca73a7678c43fc7",
    "nervosnetwork/merkle-mountain-range": "master 2026-09-24",
}
GATEWAY = to_checksum_address("0x27ca963c279c93801941e1eb8799c23f407d68e7")
INITIAL_BEEFY_CLIENT = to_checksum_address("0x6ed05baa904df3de117ecfa638d4cb84e1b8a00c")
KNOWN_BEEFY_CLIENTS = {
    to_checksum_address(a) for a in (
        "0x6ed05baa904df3de117ecfa638d4cb84e1b8a00c",
        "0x1817874feab3ce053d0f40abc23870db35c2affc",
        "0x7cfc5C8b341991993080Af67D940B6aD19a010E1",
    )
}
MMR_OFFSET = 19_551_000          # Polkadot: NumberOfLeaves(B) = B - MMR_OFFSET (first leaf at block 19_551_001)
DEFAULT_FROM_BLOCK = 19_000_000  # Ethereum, Jan 2024; before the Gateway existed
DEFAULT_CONFIRMATIONS = 64       # stay this far behind the head (reorg safety)
DOT_RPCS = ["https://rpc.polkadot.io", "https://polkadot-rpc.dwellir.com", "https://polkadot.api.onfinality.io/public"]
HERE = Path(__file__).resolve().parent
CACHE = HERE / "data" / "cache"
OUT = HERE / "data" / "snowbridge_messages.csv"
SUMMARY = HERE / "data" / "run_summary.json"
UNDECODED = HERE / "data" / "undecoded_dispatches.csv"
ETHERSCAN_URL = "https://api.etherscan.io/v2/api"

def topic(sig): return "0x" + keccak(text=sig).hex()
def selector(sig): return keccak(text=sig)[:4]

TOPIC_UPGRADED     = topic("Upgraded(address)")
TOPIC_NEW_MMR_ROOT = topic("NewMMRRoot(bytes32,uint64)")
TOPIC_DISPATCH_V1  = topic("InboundMessageDispatched(bytes32,uint64,bytes32,bool)")
TOPIC_DISPATCH_V2  = topic("InboundMessageDispatched(uint64,bytes32,bool,bytes32)")

PROOF_T = ("((bytes32,uint256,bytes32,bytes32,(uint256,bytes4,bytes)[]),"   # ParachainHeader
           "(uint256,uint256,bytes32[]),"                                  # HeadProof
           "(uint8,uint32,bytes32,uint64,uint32,bytes32),"                 # MMRLeafPartial
           "bytes32[],uint256)")                                           # leafProof, leafProofOrder
MSG_V1_T = "(bytes32,uint64,uint8,bytes,uint64,uint256,uint256,bytes32)"
MSG_V2_T = "(bytes32,uint64,bytes32,(uint8,uint64,bytes)[])"
SUBMIT_V1_TYPES = [MSG_V1_T, "bytes32[]", PROOF_T]
SUBMIT_V2_TYPES = [MSG_V2_T, "bytes32[]", PROOF_T, "bytes32"]
SEL_SUBMIT_V1  = selector(f"submitV1({MSG_V1_T},bytes32[],{PROOF_T})")          # df4ed829
SEL_SUBMIT_V2  = selector(f"v2_submit({MSG_V2_T},bytes32[],{PROOF_T},bytes32)")  # de469bc7
SEL_AGGREGATE3 = selector("aggregate3((address,bool,bytes)[])")                  # 82ad56cb
SEL_BEEFY_CLIENT = selector("BEEFY_CLIENT()")
assert SEL_SUBMIT_V1.hex() == "df4ed829" and SEL_SUBMIT_V2.hex() == "de469bc7"
MAX_PROOF_ITEMS = 256            # MMRProof.sol MAXIMUM_PROOF_SIZE


def log(*a):
    print(*a, file=sys.stderr, flush=True)


def h2i(h):
    """Hex or int to int; Etherscan encodes zero as "0x"."""
    if isinstance(h, str):
        return int(h, 16) if h not in ("", "0x") else 0
    return int(h)


def hexbytes(h): return bytes.fromhex(h[2:] if h.startswith("0x") else h)


# ----------------------------------------------------------------------------- caching
def cache_path(name): return CACHE / f"{name}.json"

def load_cache(name):
    p = cache_path(name)
    return json.loads(p.read_text()) if p.exists() else None

def save_cache(name, obj):
    CACHE.mkdir(parents=True, exist_ok=True)
    tmp = cache_path(name).with_suffix(".tmp")
    tmp.write_text(json.dumps(obj))
    tmp.replace(cache_path(name))


# ----------------------------------------------------------------------------- data sources
class Etherscan:
    """Etherscan v2 API: logs (module=logs) and JSON-RPC proxy (module=proxy)."""
    def __init__(self, key, rate=3.0):
        self.key, self.min_gap, self.last, self.lock = key, 1.0 / rate, 0.0, threading.Lock()
        self.session = requests.Session()

    def _get(self, params):
        for attempt in range(6):
            with self.lock:
                wait = self.last + self.min_gap - time.time()
                if wait > 0:
                    time.sleep(wait)
                self.last = time.time()
            try:
                r = self.session.get(ETHERSCAN_URL, params={"chainid": 1, "apikey": self.key, **params}, timeout=120).json()
            except Exception as e:
                log(f"  etherscan error {e}; retry"); time.sleep(2 + attempt); continue
            if "result" in r and r.get("status") == "1":
                return r["result"]
            if "No records" in str(r.get("message", "")) or "No records" in str(r.get("result", "")):
                return []
            if "jsonrpc" in r:                      # proxy module
                if "error" in r:
                    raise RuntimeError(r["error"])
                return r["result"]
            msg = str(r.get("result", r))
            if "rate limit" in msg.lower() or "Max calls" in msg:
                time.sleep(1 + attempt); continue
            raise RuntimeError(f"etherscan: {r}")
        raise RuntimeError("etherscan: too many retries")

    def get_logs(self, address, topic0, start, end, page_size=1000):
        """All logs for one address + topic0 in [start, end]; splits ranges that overflow a page."""
        res = self._get({"module": "logs", "action": "getLogs", "address": address, "topic0": topic0,
                         "fromBlock": start, "toBlock": end, "page": 1, "offset": page_size})
        if len(res) < page_size:
            return [self._norm(x) for x in res]
        if start == end:  # more than page_size logs in one block: page through
            out, page = list(res), 2
            while len(res) == page_size:
                res = self._get({"module": "logs", "action": "getLogs", "address": address, "topic0": topic0,
                                 "fromBlock": start, "toBlock": end, "page": page, "offset": page_size})
                out += res; page += 1
            return [self._norm(x) for x in out]
        mid = (start + end) // 2
        return self.get_logs(address, topic0, start, mid, page_size) + self.get_logs(address, topic0, mid + 1, end, page_size)

    @staticmethod
    def _norm(x):
        return {"blockNumber": h2i(x["blockNumber"]), "logIndex": h2i(x["logIndex"] or "0x0"),
                "address": to_checksum_address(x["address"]), "topics": x["topics"], "data": x["data"],
                "tx": x["transactionHash"], "ts": h2i(x["timeStamp"])}

    def rpc(self, method, params):
        p = {"module": "proxy", "action": method}
        if method == "eth_getTransactionByHash":
            p["txhash"] = params[0]
        elif method == "eth_call":
            p.update({"to": params[0]["to"], "data": params[0]["data"], "tag": "latest"})
        elif method == "eth_getBlockByNumber":
            p.update({"tag": hex(params[0]), "boolean": "false"})
        elif method == "eth_blockNumber":
            pass
        else:
            raise ValueError(method)
        return self._get(p)


class Rpc:
    """Plain JSON-RPC over HTTP (Alchemy etc.)."""
    def __init__(self, url):
        self.url, self.session = url, requests.Session()

    def rpc(self, method, params):
        for attempt in range(6):
            try:
                r = self.session.post(self.url, json={"jsonrpc": "2.0", "id": 1, "method": method, "params": params}, timeout=120)
                j = r.json()
            except Exception as e:
                log(f"  rpc error {e}; retry"); time.sleep(2 + attempt); continue
            if "error" in j:
                if j["error"].get("code") == 429 or "rate" in str(j["error"]).lower():
                    time.sleep(1 + attempt); continue
                raise RuntimeError(j["error"])
            return j["result"]
        raise RuntimeError("rpc: too many retries")

    def get_logs(self, address, topic0, start, end, step=10_000):
        out, a = [], start
        while a <= end:
            b = min(a + step - 1, end)
            try:
                chunk = self.rpc("eth_getLogs", [{"address": address, "topics": [topic0], "fromBlock": hex(a), "toBlock": hex(b)}])
            except RuntimeError as e:
                if step <= 100:
                    raise
                step = max(100, step // 2); log(f"  eth_getLogs {a}-{b}: {str(e)[:80]} -> step={step}"); continue
            out += [{"blockNumber": h2i(x["blockNumber"]), "logIndex": h2i(x["logIndex"]), "address": to_checksum_address(x["address"]),
                     "topics": x["topics"], "data": x["data"], "tx": x["transactionHash"], "ts": None} for x in chunk]
            a = b + 1
        return out


class Sources:
    def __init__(self, etherscan, rpc):
        if not etherscan and not rpc:
            sys.exit("set ETHERSCAN_API_KEY and/or RPC, or use --offline")
        self.es, self.rpc_ = etherscan, rpc
        self.logs_src = etherscan or rpc          # prefer Etherscan for logs (no range limits)
        self.rpc_src = rpc or etherscan           # prefer JSON-RPC for tx / call / block

    def describe(self): return f"logs via {type(self.logs_src).__name__}, tx/call via {type(self.rpc_src).__name__}"

    def get_logs(self, addresses, topics, start, end):
        out = []
        for a in ([addresses] if isinstance(addresses, str) else addresses):
            for t in topics:
                out += self.logs_src.get_logs(a, t, start, end)
        return out

    def block_number(self): return h2i(self.rpc_src.rpc("eth_blockNumber", []))
    def call(self, to, data): return self.rpc_src.rpc("eth_call", [{"to": to, "data": data}])
    def get_transaction(self, h): return self.rpc_src.rpc("eth_getTransactionByHash", [h])
    def block_timestamp(self, b): return h2i(self.rpc_src.rpc("eth_getBlockByNumber", [b])["timestamp"])


def cache_key(addresses, topics):
    return [sorted(a.lower() for a in ([addresses] if isinstance(addresses, str) else addresses)), sorted(topics)]


def cached_logs(src, name, addresses, topics, start, end):
    """Incrementally cached log scan: extend the cached range when possible; refetch if the
    (addresses, topics) key changed; in offline mode the cache must cover the range."""
    c = load_cache(name)
    key = cache_key(addresses, topics)
    usable = c and c.get("key", key) == key and c["from"] == start   # caches written before keys were added are accepted
    if usable and c["to"] >= end:
        if "key" not in c:
            save_cache(name, {**c, "key": key})
        return [x for x in c["items"] if x["blockNumber"] <= end]
    if src is None:
        sys.exit(f"--offline: cache '{name}' does not cover {start}..{end} with the requested addresses/topics")
    if usable:
        items = c["items"] + src.get_logs(addresses, topics, c["to"] + 1, end)
    else:
        if c:
            log(f"  cache '{name}': key/range changed, refetching")
        items = src.get_logs(addresses, topics, start, end)
    save_cache(name, {"from": start, "to": end, "key": key, "items": items})
    return items


# ----------------------------------------------------------------------------- decoding
def gateway_calls(to_addr, data):
    """Yield calldata blobs addressed to the Gateway: a direct call, or calls inside a
    Multicall3-style aggregate3 batch (recursively), whatever the batching contract is."""
    if data[:4] == SEL_AGGREGATE3:
        try:
            (calls,) = decode(["(address,bool,bytes)[]"], data[4:])
        except Exception:
            return
        for target, _allow_fail, inner in calls:
            yield from gateway_calls(target, inner)
    elif to_addr and to_addr.lower() == GATEWAY.lower():
        yield data


def plausible(rec):
    """Filter garbage decodes produced by scanning arbitrary calldata."""
    return (MMR_OFFSET < rec["parentNumber"] < 10**9 and 0 < rec["leafProofLen"] <= MAX_PROOF_ITEMS
            and rec["headProofLen"] <= 64 and rec["msgLeafProofLen"] <= 64 and 0 < rec["headProofWidth"] <= 10**4
            and rec["paraBlock"] < 10**9 and rec["nonce"] < 10**9)


def decode_submit(blob):
    sel, body = blob[:4], blob[4:]
    if sel == SEL_SUBMIT_V1:
        msg, leaf_proof, proof = decode(SUBMIT_V1_TYPES, body)
        key = ("v1", "0x" + msg[0].hex(), msg[1], "0x" + msg[7].hex())     # channelID, nonce, message id
    elif sel == SEL_SUBMIT_V2:
        msg, leaf_proof, proof, _reward = decode(SUBMIT_V2_TYPES, body)
        key = ("v2", None, msg[1], "0x" + msg[2].hex())                     # nonce, XCM topic
    else:
        return None
    header, head_proof, leaf_partial, mmr_proof, order = proof
    return {"key": key, "version": key[0], "channelID": key[1], "nonce": key[2],
            "paraBlock": header[1], "headProofWidth": head_proof[1], "headProofLen": len(head_proof[2]),
            "parentNumber": leaf_partial[1], "parentHash": "0x" + leaf_partial[2].hex(),
            "nextAuthoritySetID": leaf_partial[3],
            "leafProofLen": len(mmr_proof), "leafProofOrder": order,
            "msgLeafProofLen": len(leaf_proof)}


def scan_for_submits(data):
    """Find Gateway submit calls embedded verbatim anywhere in calldata (wrapper contracts,
    Safe execTransaction, custom batchers). Trailing bytes after an ABI tuple are ignored
    by the decoder, so decoding from each selector occurrence works if the call is intact."""
    out, seen = [], set()
    for sel in (SEL_SUBMIT_V1, SEL_SUBMIT_V2):
        pos = data.find(sel)
        while pos != -1:
            try:
                r = decode_submit(data[pos:])
                if r and plausible(r) and tuple(r["key"]) not in seen:
                    seen.add(tuple(r["key"])); out.append(r)
            except Exception:
                pass
            pos = data.find(sel, pos + 1)
    return out


def extract_submits(to_addr, data):
    """Return (records, method)."""
    recs = []
    for blob in gateway_calls(to_addr, data):
        r = decode_submit(blob)
        if r:
            recs.append(r)
    if recs:
        return recs, ("direct" if to_addr and to_addr.lower() == GATEWAY.lower() else "aggregate3")
    recs = scan_for_submits(data)
    return recs, ("scan" if recs else "none")


def key_matches(rec_key, log_key):
    """Records decoded by an older decoder carry 3-element keys (no id/topic)."""
    rec_key = tuple(rec_key)
    return rec_key == log_key if len(rec_key) == 4 else rec_key == log_key[:3]


def decode_tx(src, txh):
    tx = src.get_transaction(txh)
    data = hexbytes(tx["input"])
    recs, method = extract_submits(tx["to"], data)
    rec = {"to": tx["to"], "from": tx["from"], "recs": recs, "method": method, "decoder": DECODER_VERSION}
    if not recs:
        rec["input"] = tx["input"]          # keep raw calldata so later decoders can retry offline
    return rec


# ----------------------------------------------------------------------------- eras
def era_lookup(eras, block, log_index):
    """eras: sorted list of ((block, logIndex), client, impl); returns the era in force at
    the given log position (an Upgraded event later in the same block does not apply)."""
    positions = [e[0] for e in eras]
    return eras[bisect.bisect_right(positions, (block, log_index)) - 1]


def upgrade_carriers(upgraded_logs, dispatch_logs):
    """Gateway upgrades are executed while dispatching the message that carries the Upgrade
    command (v1/v2 Handlers -> Upgrade.upgrade), so the first dispatch log after an Upgraded
    event *in the same transaction* belongs to a message whose proof was verified by the
    previous implementation, i.e. against the previous BeefyClient. Returns
    {(block, logIndex) of that dispatch log: position of the Upgraded event}."""
    by_tx = collections.defaultdict(list)
    for lg in dispatch_logs:
        by_tx[lg["tx"]].append(lg["logIndex"])
    out = {}
    for u in upgraded_logs:
        later = [i for i in by_tx.get(u["tx"], []) if i > u["logIndex"]]
        if later:
            out[(u["blockNumber"], min(later))] = (u["blockNumber"], u["logIndex"])
    return out


def era_for_dispatch(eras, carriers, block, log_index):
    """Era whose BeefyClient verified the dispatch at (block, log_index)."""
    if (block, log_index) in carriers:
        ub, ui = carriers[(block, log_index)]
        return era_lookup(eras, ub, ui - 1)      # just before the Upgraded event
    return era_lookup(eras, block, log_index)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--from-block", type=int, default=DEFAULT_FROM_BLOCK)
    ap.add_argument("--to-block", type=int, default=None)
    ap.add_argument("--confirmations", type=int, default=DEFAULT_CONFIRMATIONS, help="blocks to stay behind the head")
    ap.add_argument("--initial-beefy-client", default=INITIAL_BEEFY_CLIENT,
                    help="BeefyClient of the Gateway implementation before the first Upgraded event")
    ap.add_argument("--workers", type=int, default=3)
    ap.add_argument("--etherscan-rate", type=float, default=3.0, help="calls/s (free tier: 3)")
    ap.add_argument("--dot-rpc", default=",".join(DOT_RPCS), help="comma-separated Polkadot RPC endpoints, tried in order")
    ap.add_argument("--skip-dot", action="store_true", help="skip Polkadot offset re-verification")
    ap.add_argument("--offline", action="store_true", help="no network; rebuild from data/cache/ only")
    ap.add_argument("--out", default=str(OUT))
    args = ap.parse_args()
    try:
        repo_commit = subprocess.run(["git", "-C", str(HERE), "rev-parse", "HEAD"], capture_output=True, text=True, timeout=10).stdout.strip() or None
    except Exception:
        repo_commit = None
    summary = {"version": VERSION, "decoder_version": DECODER_VERSION, "run_id": str(uuid.uuid4()),
               "run_at": dt.datetime.now(dt.UTC).isoformat(timespec="seconds"), "repo_commit": repo_commit,
               "source_commits": SOURCE_COMMITS,
               "args": {**vars(args), "out": os.path.relpath(args.out, HERE)}}

    if args.offline:
        src = None
        c = load_cache("dispatches")
        if not c:
            sys.exit("--offline: no cache")
        end = args.to_block or c["to"]
        summary["sources"] = "offline (data/cache)"
    else:
        src = Sources(Etherscan(os.environ["ETHERSCAN_API_KEY"], args.etherscan_rate) if os.environ.get("ETHERSCAN_API_KEY") else None,
                      Rpc(os.environ["RPC"]) if os.environ.get("RPC") else None)
        end = args.to_block or (src.block_number() - args.confirmations)
        summary["sources"] = src.describe()
        summary["endpoints"] = {"etherscan": ETHERSCAN_URL if src.es else None,
                                "rpc": (os.environ.get("RPC", "").split("/v2/")[0].split("?")[0] if src.rpc_ else None)}
        log(summary["sources"])
    start = args.from_block
    summary["range"] = [start, end]
    log(f"Ethereum range {start}..{end}")

    # 1. eras -------------------------------------------------------------------------
    ups = cached_logs(src, "upgraded", GATEWAY, [TOPIC_UPGRADED], start, end)
    ups.sort(key=lambda x: (x["blockNumber"], x["logIndex"]))
    eras = [((start, -1), to_checksum_address(args.initial_beefy_client), None)]
    impl_client = load_cache("impl_client") or {}
    for u in ups:
        impl = to_checksum_address("0x" + u["topics"][1][-40:])
        if impl not in impl_client:
            if src is None:
                sys.exit(f"--offline: BEEFY_CLIENT() of implementation {impl} not cached")
            raw = src.call(impl, "0x" + SEL_BEEFY_CLIENT.hex())
            impl_client[impl] = to_checksum_address("0x" + raw[-40:])
        eras.append(((u["blockNumber"], u["logIndex"]), impl_client[impl], impl))
    save_cache("impl_client", impl_client)
    log("Eras ((ethBlock, logIndex), beefyClient, implementation):")
    for e in eras:
        log(f"  {str(e[0]):>18}  {e[1]}  {e[2]}")
    summary["implementation_eras"] = [{"block": e[0][0], "logIndex": e[0][1], "beefyClient": e[1], "implementation": e[2]} for e in eras]
    client_eras, seen_clients = [], set()
    for e in eras:
        if e[1] not in seen_clients:
            seen_clients.add(e[1]); client_eras.append({"block": e[0][0], "logIndex": e[0][1], "beefyClient": e[1]})
    summary["beefy_client_eras"] = client_eras
    clients = sorted({e[1] for e in eras})
    unknown = set(clients) - KNOWN_BEEFY_CLIENTS
    if unknown:
        log(f"WARNING: BeefyClient(s) not in the known set: {unknown}")
    summary["unknown_beefy_clients"] = sorted(unknown)

    # 2. roots ------------------------------------------------------------------------
    roots_raw = cached_logs(src, "roots", clients, [TOPIC_NEW_MMR_ROOT], start, end)
    roots = collections.defaultdict(list)   # client -> sorted [(ethBlock, logIndex, beefyBlock)]
    for lg in roots_raw:
        _root, beefy_block = decode(["bytes32", "uint64"], hexbytes(lg["data"]))
        roots[lg["address"]].append((lg["blockNumber"], lg["logIndex"], beefy_block, lg["tx"]))
    for c in roots:
        roots[c].sort()
    log("NewMMRRoot events per BeefyClient:")
    summary["roots"] = {}
    for c in clients:
        r = roots.get(c, [])
        summary["roots"][c] = {"events": len(r), "eth_first": r[0][0] if r else None, "eth_last": r[-1][0] if r else None,
                               "beefy_first": r[0][2] if r else None, "beefy_last": r[-1][2] if r else None}
        log(f"  {c}: {len(r):>6}  eth {r[0][0]}..{r[-1][0]}  beefy {r[0][2]}..{r[-1][2]}" if r else f"  {c}: none")
    root_keys = {c: [(b, i) for b, i, _, _ in r] for c, r in roots.items()}

    # 3. dispatches -------------------------------------------------------------------
    disp = cached_logs(src, "dispatches", GATEWAY, [TOPIC_DISPATCH_V1, TOPIC_DISPATCH_V2], start, end)
    disp.sort(key=lambda x: (x["blockNumber"], x["logIndex"]))
    log(f"{len(disp)} InboundMessageDispatched logs")
    summary["dispatch_logs"] = len(disp)

    # 4. decode txs (cached per hash) --------------------------------------------------
    txcache = load_cache("txs") or {}
    for t in txcache.values():                    # entries written before decode methods were recorded
        if "method" not in t and "error" not in t:
            t["method"] = ("direct" if (t.get("to") or "").lower() == GATEWAY.lower() else "aggregate3") if t.get("recs") else "none"
    # re-decode from cached raw input where the decoder changed or nothing was decoded
    for h, t in txcache.items():
        if t.get("input") and (not t.get("recs") or t.get("decoder") != DECODER_VERSION):
            recs, method = extract_submits(t.get("to"), hexbytes(t["input"]))
            t.update({"recs": recs, "method": method, "decoder": DECODER_VERSION})
            if recs:
                t.pop("input", None)
    hashes = {lg["tx"] for lg in disp}
    def needs_fetch(h):
        t = txcache.get(h)
        return (t is None or "error" in t or t.get("decoder") != DECODER_VERSION
                or (not t.get("recs") and "input" not in t))
    todo = sorted(h for h in hashes if needs_fetch(h))
    stale = sorted(h for h in hashes if h in txcache and "error" not in txcache[h] and txcache[h].get("decoder") != DECODER_VERSION)
    if src is None and todo:
        log(f"--offline: {len(todo)} txs need (re)fetching ({len(stale)} decoded by an older decoder, kept as is); rerun online to refresh")
        todo = []
    summary["stale_decoder_entries"] = len(stale) if src is None else 0
    log(f"decoding {len(todo)} txs ({len(txcache)} cached)")
    summary["unique_txs"] = len(hashes)
    def work(h):
        err = None
        for attempt in range(5):
            try:
                return h, decode_tx(src, h)
            except Exception as e:
                err = e; time.sleep(1 + attempt)
        return h, {"error": str(err)}
    with ThreadPoolExecutor(args.workers) as ex:
        for n, (h, r) in enumerate(ex.map(work, todo), 1):
            txcache[h] = r
            if n % 200 == 0:
                log(f"  {n}/{len(todo)} txs"); save_cache("txs", txcache)
    save_cache("txs", txcache)
    summary["txs_by_method"] = dict(collections.Counter(txcache[h].get("method", "error" if "error" in txcache[h] else "none") for h in hashes if h in txcache))

    # 5. match logs to submit calls, compute k ------------------------------------------
    carriers = upgrade_carriers(ups, disp)
    summary["upgrade_carriers"] = [{"block": b, "logIndex": i} for (b, i) in sorted(carriers)]
    rows, stats, undecoded = [], collections.Counter(), []
    for lg in disp:
        t0 = lg["topics"][0]
        data = hexbytes(lg["data"])
        if t0 == TOPIC_DISPATCH_V1:
            nonce, success = decode(["uint64", "bool"], data)
            key = ("v1", lg["topics"][1], nonce, lg["topics"][2])             # channelID, nonce, messageID
        else:
            nonce = h2i(lg["topics"][1])
            xcm_topic, success, _reward = decode(["bytes32", "bool", "bytes32"], data)
            key = ("v2", None, nonce, "0x" + xcm_topic.hex())
        tx = txcache.get(lg["tx"], {})
        if not tx or "error" in tx:
            stats["tx_error" if tx else "tx_missing"] += 1
            undecoded.append({"txHash": lg["tx"], "ethBlock": lg["blockNumber"], "logIndex": lg["logIndex"], "version": key[0],
                              "channelID": key[1] or "", "nonce": nonce, "to": tx.get("to", ""), "from": tx.get("from", ""), "reason": "tx_error" if tx else "tx_missing"})
            continue
        recs = [r for r in tx.get("recs", []) if key_matches(r["key"], key)]
        if recs and len(recs[0]["key"]) == 3:
            stats["legacy_key_match"] += 1
        if not recs:
            reason = "unmatched_nonce" if tx.get("recs") else "undecodable_wrapper"
            stats[reason] += 1
            undecoded.append({"txHash": lg["tx"], "ethBlock": lg["blockNumber"], "logIndex": lg["logIndex"], "version": key[0],
                              "channelID": key[1] or "", "nonce": nonce, "to": tx.get("to", ""), "from": tx.get("from", ""), "reason": reason})
            continue
        if len(recs) > 1:
            stats["duplicate_key"] += 1
        r = recs[0]
        pos = (lg["blockNumber"], lg["logIndex"])
        era = era_for_dispatch(eras, carriers, *pos)
        if pos in carriers:
            stats["upgrade_carrier"] += 1
        client = era[1]
        keys = root_keys.get(client, [])
        j = bisect.bisect_left(keys, pos) - 1
        if j < 0:
            stats["no_root_yet"] += 1
            undecoded.append({"txHash": lg["tx"], "ethBlock": lg["blockNumber"], "logIndex": lg["logIndex"], "version": key[0],
                              "channelID": key[1] or "", "nonce": nonce, "to": tx.get("to", ""), "from": tx.get("from", ""), "reason": "no_root_yet"})
            continue
        latest_beefy = roots[client][j][2]
        root_same_tx = int(roots[client][j][3] == lg["tx"])
        leaf_block = r["parentNumber"] + 1
        n = latest_beefy - MMR_OFFSET
        i = r["parentNumber"] - MMR_OFFSET
        k = n - i                      # paper convention: k = 1 is the newest leaf
        if k < 1 or i < 0:
            stats["negative_k"] += 1
            log(f"  negative k: tx {lg['tx']} latestBeefy={latest_beefy} leafBlock={leaf_block} client={client}")
            continue
        formula = mmr_items(i, n)
        if formula != r["leafProofLen"]:
            stats["mmr_mismatch"] += 1
            if stats["mmr_mismatch"] <= 20:
                log(f"  MMR mismatch tx {lg['tx']}: onchain={r['leafProofLen']} formula={formula} n={n} i={i} k={k}")
        rows.append({
            "ethBlock": lg["blockNumber"], "logIndex": lg["logIndex"], "timestamp": lg.get("ts"), "txHash": lg["tx"],
            "version": r["version"], "nonce": r["nonce"], "channelID": r["channelID"] or "",
            "beefyClient": client, "latestBeefyBlock": latest_beefy,
            "parentNumber": r["parentNumber"], "leafBlock": leaf_block, "paraBlock": r["paraBlock"],
            "k": k, "n": n, "i": i,
            "mmr_items_onchain": r["leafProofLen"], "mmr_items_formula": formula,
            "leafProofOrder": r["leafProofOrder"], "headProofWidth": r["headProofWidth"],
            "headProofLen": r["headProofLen"], "msgLeafProofLen": r["msgLeafProofLen"],
            "success": int(bool(success)), "relayer": tx.get("from", ""), "decode": tx.get("method", ""),
            "rootSameTx": root_same_tx, "rootEthBlock": roots[client][j][0],
        })
    log(f"{len(rows)} messages with k; anomalies: {dict(stats)}")
    summary["rows"] = len(rows)
    summary["anomalies"] = dict(stats)
    summary["undecoded"] = undecoded
    summary["rows_by_decode_method"] = dict(collections.Counter(r["decode"] for r in rows))

    # 5b. completeness gate: v1 nonces are sequential per channel, v2 nonces sequential;
    #     every nonce between the first and last seen must appear as a decoded or undecoded dispatch.
    seen = collections.defaultdict(set)
    for r in rows:
        seen[(r["version"], r["channelID"])].add(r["nonce"])
    for u in undecoded:
        seen[(u["version"], u["channelID"])].add(u["nonce"])
    gaps = {}
    for (ver, ch), ns in seen.items():
        missing = sorted(set(range(min(ns), max(ns) + 1)) - ns)
        if missing:
            gaps[f"{ver}:{ch}"] = missing[:50]
    summary["nonce_gaps"] = gaps
    if gaps:
        stats["nonce_gap"] += sum(len(v) for v in gaps.values())
        log(f"  NONCE GAPS (dispatch logs missing from the collection): {gaps}")

    # 6. timestamps (only for logs that came without one) -------------------------------
    blocks = load_cache("blocks") or {}
    need = sorted({r["ethBlock"] for r in rows if r["timestamp"] is None} - {int(b) for b in blocks})
    if need and src is not None:
        log(f"fetching {len(need)} block timestamps ({len(blocks)} cached)")
        def ts(b):
            for attempt in range(5):
                try:
                    return b, src.block_timestamp(b)
                except Exception:
                    time.sleep(1 + attempt)
            return b, None
        with ThreadPoolExecutor(args.workers) as ex:
            for n_, (b, t) in enumerate(ex.map(ts, need), 1):
                blocks[str(b)] = t
                if n_ % 500 == 0:
                    log(f"  {n_}/{len(need)} blocks"); save_cache("blocks", blocks)
        save_cache("blocks", blocks)
    for r in rows:
        if r["timestamp"] is None:
            r["timestamp"] = blocks.get(str(r["ethBlock"]))

    # 7. Polkadot offset re-verification ------------------------------------------------
    summary["offset_verification"] = []
    summary["mmr_offset"] = MMR_OFFSET
    if not args.skip_dot and rows:
        import xxhash
        def twox128(s):
            b = s.encode()
            return xxhash.xxh64(b, seed=0).digest()[::-1] + xxhash.xxh64(b, seed=1).digest()[::-1]
        key = "0x" + (twox128("Mmr") + twox128("NumberOfLeaves")).hex()
        def dot(m, p):
            last = None
            for url in args.dot_rpc.split(","):
                try:
                    j = requests.post(url.strip(), json={"jsonrpc": "2.0", "id": 1, "method": m, "params": p}, timeout=60).json()
                    if "result" in j and j["result"] is not None:
                        return j["result"]
                    last = j.get("error")
                except Exception as e:
                    last = str(e)
            raise RuntimeError(f"polkadot rpc failed: {last}")
        sample = sorted({rows[0]["latestBeefyBlock"], rows[len(rows)//2]["latestBeefyBlock"], rows[-1]["latestBeefyBlock"]})
        for b in sample:
            try:
                v = dot("state_getStorage", [key, dot("chain_getBlockHash", [b])])
                leaves = int.from_bytes(hexbytes(v), "little")
                ok = leaves == b - MMR_OFFSET
                log(f"  Polkadot block {b}: NumberOfLeaves={leaves} expected={b - MMR_OFFSET} {'OK' if ok else 'MISMATCH'}")
                summary["offset_verification"].append({"block": b, "leaves": leaves, "expected": b - MMR_OFFSET, "ok": ok})
                if not ok:
                    stats["offset_mismatch"] += 1
            except Exception as e:
                log(f"  Polkadot verification unavailable for block {b}: {e}")
                summary["offset_verification"].append({"block": b, "error": str(e)})

    # 8. write ---------------------------------------------------------------------------
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    cols = ["ethBlock", "logIndex", "timestamp", "txHash", "version", "nonce", "channelID", "beefyClient",
            "latestBeefyBlock", "parentNumber", "leafBlock", "paraBlock", "k", "n", "i",
            "mmr_items_onchain", "mmr_items_formula", "leafProofOrder", "headProofWidth", "headProofLen",
            "msgLeafProofLen", "success", "relayer", "decode", "rootSameTx", "rootEthBlock"]
    with open(args.out, "w", newline="") as f:
        wr = csv.DictWriter(f, fieldnames=cols)
        wr.writeheader()
        wr.writerows(rows)
    with open(UNDECODED, "w", newline="") as f:
        wr = csv.DictWriter(f, fieldnames=["txHash", "ethBlock", "logIndex", "version", "channelID", "nonce", "to", "from", "reason"])
        wr.writeheader(); wr.writerows(undecoded)
    ks = sorted(r["k"] for r in rows)
    if ks:
        q = lambda p: ks[min(len(ks) - 1, int(len(ks) * p))]
        summary["k"] = {"min": ks[0], "p50": q(.5), "p90": q(.9), "p99": q(.99), "max": ks[-1]}
        log(f"wrote {args.out}: {len(rows)} rows; k min {ks[0]} p50 {q(.5)} p90 {q(.9)} p99 {q(.99)} max {ks[-1]}")
    summary["mmr_mismatches"] = stats["mmr_mismatch"]
    SUMMARY.write_text(json.dumps(summary, indent=1))
    log(f"MMR formula mismatches: {stats['mmr_mismatch']}; undecoded dispatches: {len(undecoded)} (see {UNDECODED.name}); summary: {SUMMARY.name}")
    return 1 if stats["mmr_mismatch"] or stats.get("offset_mismatch") or stats.get("nonce_gap") else 0


if __name__ == "__main__":
    sys.exit(main())
