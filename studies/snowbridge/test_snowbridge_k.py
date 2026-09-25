#!/usr/bin/env python3
"""Unit tests for the decoding and attribution logic of snowbridge_k.py (no network).
Run: python3 -m unittest -v test_snowbridge_k"""
import os
import unittest

from eth_abi import encode
from eth_utils import keccak

import snowbridge_k as sk
from mmr_proof_size import mmr_items, selftest

PROOF = ((b"\x11" * 32, 1234, b"\x22" * 32, b"\x33" * 32, [(0, b"\x00\x00\x00\x00", b"\x00" + b"\x44" * 32)]),   # ParachainHeader
         (3, 50, [b"\x55" * 32] * 6),                                                                          # HeadProof
         (0, 20_000_000, b"\x66" * 32, 7, 1000, b"\x77" * 32),                                                 # MMRLeafPartial
         [b"\x88" * 32] * 15, 0b101)                                                                          # leafProof, order
MSG1 = (b"\x00" * 31 + b"\x01", 42, 2, b"\xab\xcd", 100_000, 10**9, 10**15, b"\x99" * 32)
MSG2 = (b"\xaa" * 32, 7, b"\xbb" * 32, [(2, 50_000, b"\xcc" * 10)])


def calldata_v1():
    return sk.SEL_SUBMIT_V1 + encode(sk.SUBMIT_V1_TYPES, [MSG1, [b"\x01" * 32, b"\x02" * 32], PROOF])


def calldata_v2():
    return sk.SEL_SUBMIT_V2 + encode(sk.SUBMIT_V2_TYPES, [MSG2, [], PROOF, b"\xdd" * 32])


class DecodeTests(unittest.TestCase):
    def test_h2i(self):
        self.assertEqual(sk.h2i("0x"), 0); self.assertEqual(sk.h2i(""), 0)
        self.assertEqual(sk.h2i("0x1a"), 26); self.assertEqual(sk.h2i(5), 5)

    def test_selectors(self):
        self.assertEqual(sk.SEL_SUBMIT_V1.hex(), "df4ed829")
        self.assertEqual(sk.SEL_SUBMIT_V2.hex(), "de469bc7")
        self.assertEqual(sk.SEL_AGGREGATE3.hex(), "82ad56cb")
        self.assertEqual(sk.TOPIC_NEW_MMR_ROOT, "0xd95fe1258d152dc91c81b09380498adc76ed36a6079bcb2ed31eff622ae2d0f1")

    def test_direct_v1(self):
        recs, method = sk.extract_submits(sk.GATEWAY, calldata_v1())
        self.assertEqual(method, "direct"); self.assertEqual(len(recs), 1)
        r = recs[0]
        self.assertEqual(r["key"], ("v1", "0x" + MSG1[0].hex(), 42, "0x" + MSG1[7].hex()))
        self.assertEqual((r["parentNumber"], r["leafProofLen"], r["leafProofOrder"]), (20_000_000, 15, 5))
        self.assertEqual((r["paraBlock"], r["headProofWidth"], r["headProofLen"], r["msgLeafProofLen"]), (1234, 50, 6, 2))
        self.assertEqual(r["nextAuthoritySetID"], 7)

    def test_direct_v2(self):
        recs, method = sk.extract_submits(sk.GATEWAY.lower(), calldata_v2())
        self.assertEqual(method, "direct")
        self.assertEqual(recs[0]["key"], ("v2", None, 7, "0x" + MSG2[2].hex())); self.assertEqual(recs[0]["msgLeafProofLen"], 0)

    def test_gateway_call_to_other_contract_is_not_direct(self):
        # calldata addressed to some other contract: only a scan may recover it
        recs, method = sk.extract_submits("0x" + "12" * 20, calldata_v1())
        self.assertEqual(method, "scan"); self.assertEqual(len(recs), 1)

    def test_aggregate3_unwrap(self):
        calls = [(sk.GATEWAY, False, calldata_v1()), ("0x" + "ab" * 20, True, b"\xde\xad"), (sk.GATEWAY, False, calldata_v2())]
        data = sk.SEL_AGGREGATE3 + encode(["(address,bool,bytes)[]"], [calls])
        recs, method = sk.extract_submits("0x" + "cc" * 20, data)   # any batching contract
        self.assertEqual(method, "aggregate3"); self.assertEqual([r["version"] for r in recs], ["v1", "v2"])

    def test_scan_inside_safe_like_wrapper(self):
        # execTransaction(address to, uint256 value, bytes data, uint8 operation, ...)-style wrapper
        inner = calldata_v1()
        wrapper = keccak(text="execTransaction(address,uint256,bytes,uint8)")[:4] + encode(
            ["address", "uint256", "bytes", "uint8"], [sk.GATEWAY, 0, inner, 0])
        recs, method = sk.extract_submits("0x" + "ee" * 20, wrapper)
        self.assertEqual(method, "scan"); self.assertEqual(recs[0]["key"][:3], ("v1", "0x" + MSG1[0].hex(), 42))
        # two embedded calls, duplicates deduplicated by key
        wrapper2 = b"\x01\x02\x03\x04" + encode(["bytes", "bytes", "bytes"], [inner, calldata_v2(), inner])
        recs, method = sk.extract_submits("0x" + "ee" * 20, wrapper2)
        self.assertEqual(method, "scan"); self.assertEqual(sorted(r["version"] for r in recs), ["v1", "v2"])

    def test_scan_rejects_garbage(self):
        data = sk.SEL_SUBMIT_V1 + os.urandom(2000)
        recs, method = sk.extract_submits("0x" + "ee" * 20, data)
        self.assertEqual((recs, method), ([], "none"))
        self.assertEqual(sk.extract_submits("0x" + "ee" * 20, b""), ([], "none"))


class MatchTests(unittest.TestCase):
    def test_key_matching_full_and_legacy(self):
        full = ("v1", "0xchan", 5, "0xid")
        self.assertTrue(sk.key_matches(full, full))
        self.assertFalse(sk.key_matches(("v1", "0xchan", 5, "0xother"), full))      # same nonce, different message id
        self.assertTrue(sk.key_matches(("v1", "0xchan", 5), full))                 # record from an older decoder
        self.assertFalse(sk.key_matches(("v1", "0xchan", 6), full))
        self.assertFalse(sk.key_matches(["v2", None, 5, "0xid"], full))


class EraTests(unittest.TestCase):
    def test_era_lookup_is_log_granular(self):
        eras = [((100, -1), "A", None), ((200, 10), "B", "implB"), ((300, 5), "C", "implC")]
        self.assertEqual(sk.era_lookup(eras, 150, 0)[1], "A")
        self.assertEqual(sk.era_lookup(eras, 200, 9)[1], "A")    # same block, before the Upgraded log
        self.assertEqual(sk.era_lookup(eras, 200, 10)[1], "B")   # the Upgraded log itself
        self.assertEqual(sk.era_lookup(eras, 200, 11)[1], "B")
        self.assertEqual(sk.era_lookup(eras, 299, 999)[1], "B")
        self.assertEqual(sk.era_lookup(eras, 300, 5)[1], "C")

    def test_upgrade_carrier_gets_previous_era(self):
        eras = [((100, -1), "A", None), ((200, 10), "B", "implB")]
        ups = [{"blockNumber": 200, "logIndex": 10, "tx": "0xup"}]
        disp = [{"blockNumber": 200, "logIndex": 8, "tx": "0xother"},   # earlier tx in the block
                {"blockNumber": 200, "logIndex": 11, "tx": "0xup"},      # the message that executed the upgrade
                {"blockNumber": 200, "logIndex": 12, "tx": "0xup"},      # a second submit batched after it
                {"blockNumber": 201, "logIndex": 0, "tx": "0xlater"}]
        carriers = sk.upgrade_carriers(ups, disp)
        self.assertEqual(carriers, {(200, 11): (200, 10)})
        self.assertEqual(sk.era_for_dispatch(eras, carriers, 200, 8)[1], "A")
        self.assertEqual(sk.era_for_dispatch(eras, carriers, 200, 11)[1], "A")   # verified before the upgrade
        self.assertEqual(sk.era_for_dispatch(eras, carriers, 200, 12)[1], "B")
        self.assertEqual(sk.era_for_dispatch(eras, carriers, 201, 0)[1], "B")

    def test_cache_key_is_order_insensitive(self):
        self.assertEqual(sk.cache_key(["0xB", "0xa"], ["t2", "t1"]), sk.cache_key(["0xA", "0xb"], ["t1", "t2"]))
        self.assertNotEqual(sk.cache_key(["0xa"], ["t1"]), sk.cache_key(["0xa", "0xb"], ["t1"]))


class MmrTests(unittest.TestCase):
    def test_formula_brute_force(self):
        selftest(200)

    def test_k_convention(self):
        # the newest leaf (i = n-1) of an MMR with n leaves; k = n - i = 1
        n = 13_588_715; i = n - 1
        self.assertEqual(n - i, 1)
        self.assertEqual(mmr_items(i, n), bin(n).count("1") - 1)   # lone right peak: one hash per left peak


if __name__ == "__main__":
    unittest.main()
