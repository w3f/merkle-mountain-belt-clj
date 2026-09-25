#!/usr/bin/env python3
"""
Single-leaf membership proof size in a Merkle Mountain Range, as produced by
nervosnetwork/merkle-mountain-range `MMR::gen_proof` (used by polkadot-sdk pallet_mmr)
and passed through unchanged in count by Snowbridge's `ConvertToSimplifiedMMRProof`
(relayer/crypto/merkle/simplified_mmr_proof.go): the contract's `leafProof` array has
exactly `mmr_items(i, n)` entries.

Derivation (ckb-mmr src/mmr.rs, gen_proof / bag_rhs_peaks):
  * sibling hashes on the path from the leaf to the peak of its own mountain: h items,
    where the mountain has 2**h leaves;
  * every peak strictly to the LEFT of that mountain: one item each;
  * all peaks strictly to the RIGHT: bagged into ONE item (a lone right peak is also one item);
  * n == 1: empty proof.

Peaks of an MMR with n leaves are the set bits of n, from the most significant (leftmost,
largest mountain) to the least significant (rightmost, newest).

Run this file directly for a brute-force self-test against an explicit position-based
MMR builder for all (i, n) with n <= 300.
"""
import math
import sys


def mountains(n: int):
    """Heights of the mountains left-to-right, i.e. exponents of the set bits of n, descending."""
    return [b for b in range(n.bit_length() - 1, -1, -1) if (n >> b) & 1]


def mmr_items(i: int, n: int) -> int:
    """Number of proof items for leaf index i (0-based) in an MMR with n leaves."""
    if not (0 <= i < n):
        raise ValueError(f"leaf index {i} out of range for n={n}")
    if n == 1:
        return 0
    hs = mountains(n)
    start = 0
    for j, h in enumerate(hs):
        size = 1 << h
        if i < start + size:
            left = j
            right = len(hs) - j - 1
            return h + left + (1 if right > 0 else 0)
        start += size
    raise AssertionError("unreachable")


def balanced_items(n: int) -> int:
    """Proof size in a single balanced binary Merkle tree over n leaves (paper's old baseline)."""
    return 0 if n <= 1 else math.ceil(math.log2(n))


# ---------------------------------------------------------------- brute-force reference
def _pos_height(pos: int) -> int:
    """Height of the node at 0-based MMR position `pos` (ckb helper::pos_height_in_tree)."""
    pos += 1
    while pos & (pos + 1):  # not all ones
        pos -= (1 << (pos.bit_length() - 1)) - 1
    return pos.bit_length() - 1


def _leaf_pos(i: int) -> int:
    """ckb leaf_index_to_pos."""
    return 2 * i - bin(i).count("1")


def _peaks(mmr_size: int):
    peaks = []
    left = 0
    # largest perfect tree that fits, repeatedly (ckb get_peaks)
    while left < mmr_size:
        h = 0
        while (1 << (h + 2)) - 1 <= mmr_size - left:
            h += 1
        peaks.append(left + (1 << (h + 1)) - 2)
        left += (1 << (h + 1)) - 1
    return peaks


def _mmr_size(n: int) -> int:
    return 2 * n - bin(n).count("1")


def _brute_items(i: int, n: int) -> int:
    """Simulate gen_proof for a single leaf: walk siblings to the peak, then peaks."""
    if n == 1:
        return 0
    size = _mmr_size(n)
    peaks = _peaks(size)
    pos = _leaf_pos(i)
    peak_idx = next(j for j, p in enumerate(peaks) if pos <= p)
    items = 0
    height = 0
    while pos != peaks[peak_idx]:
        # ckb gen_proof_for_peak: sibling then parent
        if _pos_height(pos + 1) > height:   # pos is a right child
            pos = pos + 1
        else:                                # pos is a left child
            pos = pos + (1 << (height + 1))  # parent = pos + sibling_offset + 1 ... see below
        items += 1
        height += 1
    # The parent arithmetic above is only used to stop at the peak; verify via height:
    left = peak_idx
    right = len(peaks) - peak_idx - 1
    return items + left + (1 if right > 0 else 0)


def _brute_items_by_height(i: int, n: int) -> int:
    """Independent check: mountain height via positions only."""
    if n == 1:
        return 0
    size = _mmr_size(n)
    peaks = _peaks(size)
    pos = _leaf_pos(i)
    peak_idx = next(j for j, p in enumerate(peaks) if pos <= p)
    h = _pos_height(peaks[peak_idx])
    left = peak_idx
    right = len(peaks) - peak_idx - 1
    return h + left + (1 if right > 0 else 0)


def selftest(nmax: int = 300) -> None:
    for n in range(1, nmax + 1):
        for i in range(n):
            a, b, c = mmr_items(i, n), _brute_items(i, n), _brute_items_by_height(i, n)
            if not (a == b == c):
                raise SystemExit(f"MISMATCH n={n} i={i}: formula={a} walk={b} height={c}")
    # spot values documented in the paper's setting
    assert mmr_items(0, 1) == 0
    assert mmr_items(0, 2) == 1 and mmr_items(1, 2) == 1
    assert mmr_items(2, 3) == 1          # lone right leaf: its proof is the single left peak
    assert mmr_items(0, 3) == 2          # path 1 + one right peak
    assert mmr_items((1 << 25) - 1, 1 << 25) == 25
    assert mmr_items((1 << 25) - 2, (1 << 25) - 1) == 24  # newest leaf, 25 peaks: 0 + 24 left + 0
    print(f"mmr_proof_size self-test OK (all i, n <= {nmax})")


if __name__ == "__main__":
    selftest(int(sys.argv[1]) if len(sys.argv) > 1 else 300)
