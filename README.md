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
each append, supports up to 100,000 leaves, and includes numerical checks against
the Clojure implementation. Enable **Show membership paths** to select a leaf and
verify its proof. The browser code is a JavaScript implementation validated
against Clojure reference results; the Clojure source tests are recorded at export.

See [reviewer/README.md](reviewer/README.md) for the conventions, validation
coverage, and reproduction instructions.

## Reproduce the artifact

Use the Clojure CLI with JDK 21 and Node.js 18 or newer. From the repository root:

```sh
clojure -J-Xmx2g -J-Djava.awt.headless=true -J-Dmmb.verify-shortcuts=true -M reviewer/export.clj
node reviewer/check-export.mjs
node reviewer/live-test.mjs
```

The first Clojure run may download dependencies. Export runs selected paper
tests and rebuilds `docs/index.html`. The JavaScript check compares roots,
topologies, paths, and hash counts with those reference values, then exercises
construction through the browser's leaf-count limit.

The repository's broader Clojure tests can be run with `clojure -M:test`.
Those are separate from the selected tests reported by the page. Cached EDN
fixtures in `src/` are required by the existing Clojure test code and are included.

## Source layout

- `src/linked_peaks.clj`: incremental MMB construction and proof operations.
- `src/hashing.clj`: interval and Keccak-256 hashing backends.
- `src/state.clj` and `src/primitives/`: state, indexing, proof, and visualization support.
- `src/paper_test.clj` and `src/proof_size.clj`: paper-related checks and proof-size calculations.
- `src/core.clj`, `src/storage.clj`, and `src/visualization_*.clj`: related construction and visualization code.
- `reviewer/`: browser sources, exporter, and validation tools.
- `docs/index.html`: generated standalone demonstration.
- `stats/`: previously generated experimental results, separate from live browser calculations.
