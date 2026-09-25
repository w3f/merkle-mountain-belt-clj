# Merkle Mountain Belt

Research implementation of the Merkle Mountain Belt (MMB), an append-only
hash-based commitment structure. This repository contains the Clojure
implementation, its tests, and an interactive browser demonstration.

## Interactive demonstration

Open [docs/index.html](docs/index.html) in a browser. The file is self-contained;
it needs no server, network connection, JVM, or installation. If a repository
viewer shows HTML source, download the file and open it locally, or use the
hosted web preview supplied with the artifact.

The demonstration computes append operations, Keccak-256 commitments, membership
paths, and hash counts in the browser. It shows the structure before and after
each append and supports up to 100,000 leaves. Enable **Show membership paths**
to select a leaf and verify its proof. The browser code is a JavaScript implementation validated
against Clojure reference roots and append hash counts for every state through
100,000 leaves. The selected state's root and count are compared with those
references during navigation.

See [reviewer/README.md](reviewer/README.md) for the conventions, validation
coverage, and reproduction instructions.

## Tests and reproduction

Use the Clojure CLI with JDK 21 and Node.js 18 or newer. Run the following commands
from the repository root.

Run the Clojure test suite for the paper's numerical checks, including peak
schedules, merge locality, membership proofs, proof-size formulas, and hash-work
bounds:

```sh
clojure -M:test
```

To regenerate the demonstration and run the JavaScript validation suite:

```sh
clojure -J-Xmx2g -J-Djava.awt.headless=true -J-Dmmb.verify-shortcuts=true -M reviewer/export.clj
node reviewer/check-export.mjs
node reviewer/live-test.mjs
```

The first Clojure run may download dependencies. Export runs selected paper
tests, constructs the full 100,000-leaf Clojure reference, and rebuilds
`docs/index.html`. The JavaScript check compares every root and append hash count
through that limit, plus detailed topologies, membership paths, and hash events
at the smaller reference states and additional event checkpoints.
The JavaScript suite also checks merge locality, hash-work bounds, every
membership path's interval coverage and predicted size through 1,024 leaves,
and proof-size averages for recencies 1–16, 32, 64, 128, and 256.
These numerical checks run in the test suites; the browser page focuses on the
append demonstration and interactive membership proofs.
Cached EDN fixtures in `src/` are required by the Clojure tests and are included.

The empirical Snowbridge study behind the paper's bridge table lives in
`studies/snowbridge/` (see its `README.org`): shipped data, unit tests, and
`python3 analyse.py --force-mmb`, which recomputes MMB proof sizes through
`clojure -M:mmb-sizes` and regenerates the figures. The Nix development shell
provides the Python packages it needs.

## Source layout

- `src/linked_peaks.clj`: incremental MMB construction and proof operations.
- `src/hashing.clj`: interval and Keccak-256 hashing backends.
- `src/state.clj` and `src/primitives/`: state, indexing, proof, and visualization support.
- `src/paper_test.clj` and `src/proof_size.clj`: paper-related checks and proof-size calculations.
- `src/core.clj`, `src/storage.clj`, and `src/visualization_*.clj`: related construction and visualization code.
- `reviewer/`: browser sources, exporter, and validation tools.
- `docs/index.html`: generated standalone demonstration.
- `stats/`: previously generated experimental results, separate from live browser calculations.
- `src/mmb_sizes.clj`: command-line entry point (`clojure -M:mmb-sizes`) for MMB / U-MMB proof sizes at given `(n, k)`.
- `studies/snowbridge/`: measured k distribution of Snowbridge Polkadot→Ethereum messages and on-chain MMR versus MMB proof sizes (paper Table 5).
