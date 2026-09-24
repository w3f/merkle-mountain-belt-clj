# Interactive reviewer artifact

Open `docs/index.html` in a browser. It is a standalone page with embedded data,
styles, and JavaScript; viewing it needs no server, network, JVM, or installation.

The explorer computes appends in the browser, with previous/reset controls,
Keccak-256 digests, construction counts, and hash events. **Compute to leaf count**
builds larger states in batches and can be cancelled. The interactive limit is
100,000 leaves; the slider grows as states are computed. Previous states remain
available for navigation. **Plot range** selects 64, 256, or 1,024 appends around
the selected state, or the entire computed history. At larger ranges, consecutive
appends are grouped into at most 256 marks: each shows the minimum, maximum,
and mean, and selecting it inspects an append with the group's maximum count.
No appends in the selected range are omitted.
Each append displays **MMB at n−1 and MMB at n**. Small diagrams sit side by side;
wide diagrams automatically stack before above after, using the same scale.
The layout also adapts when a membership path is expanded or the window resized.
Gray peaks on the before
side identify the merge inputs; the after side shows the resulting peak and
updated bagging nodes. The counters and hash ledger describe this transition.
Reset shows the empty MMB; the first append shows the transition from empty to
one leaf. By default the diagrams follow the paper's
notation: each entire mountain is a triangle containing its **height**, range
nodes are rhombi, and belt nodes are circles connected from left to right.
The new leaf is green, the merged peak is gray, and updated range/belt nodes and
the root are brown. Hover over a node for its leaf span and digest.

Black dots mark absent left inputs. The corresponding identity bagging node
reuses its right child's value and costs no hash. These nodes are shown to match
the paper's layout; they do not increase the displayed hash counts. The pure
`reviewer/structure.js` adapter restores them from the computed topology.

Enable **Show membership paths** to expand only the selected leaf's route and
reveal the proof panel. **Peaks remain triangles**, including the peak on the
expanded path. Nodes within mountains are squares containing their **height**,
following the paper's Figure 10. Off-path subtrees keep a square root and an
ellipsis beneath it. Height-zero nodes retain `0` inside their square (or peak
triangle), with a separate `leaf N` value label attached below, for both the
proven leaf and its leaf siblings. A leaf has no hidden descendants and therefore
no ellipsis. Triangles are reserved for actual peaks.
The same leaf is followed in both states: its route is blue and its sibling
values are gold. A newly appended leaf has no before path, which is called out
below that diagram. The proof panel verifies the after state.

Choose a leaf in the selector, or click a collapsed subtree to follow its
leftmost leaf. Above 256 leaves, enter the leaf index in a numeric field instead
of scrolling through a long dropdown. These nodes and hash-history bars also accept Enter or Space.
Alter a sibling digest to demonstrate rejection by Keccak root verification.
Hover over a proof step to inspect its digest. Disabling
the toggle restores collapsed mountains and removes proof highlighting; the
selected leaf is retained. Large graphs scroll horizontally.

The **Numerical checks** view evaluates peak schedules, merge locality, membership
path structure, proof-size predictions, and hash-work bounds over computed
prefixes up to 1,024 leaves. A separate result compares every computed root and
append hash count in that prefix with the Clojure fixtures. The reference
construction covers every prefix through 100,000 leaves; the explorer also
compares the selected state's root and count during navigation. These checks
generate paths and inspect their
intervals; the membership panel additionally recomputes the selected proof's
Keccak root. Recency settings 1–16, 32, 64, 128, and 256 generate fresh paths and
compare their mean lengths with the formulas in `src/paper_test.clj`.

## Reproduce

From the repository root, with the Clojure CLI and a compatible JDK installed:

```sh
clojure -J-Xmx2g -J-Djava.awt.headless=true -J-Dmmb.verify-shortcuts=true -M reviewer/export.clj
node reviewer/check-export.mjs
node reviewer/live-test.mjs
```

The exporter uses the existing `deps.edn`; it adds no JVM dependencies. The first
run may download those dependencies. The Node check uses only built-in modules.
The generator rebuilds `docs/index.html` and writes `docs/.nojekyll` using the
source files in this copy of the artifact.
Export constructs all 100,000 prefixes in Clojure. Its root and count references
are embedded in the page, which remains self-contained. Rerun export before the
Node checks when the Clojure source changes; those checks read the exported
references rather than launching Clojure themselves.

The Clojure export:

- Builds 64 reference topologies with the interval backend and records every
  leaf's existing membership co-path, checking each path and its predicted size.
- Rehashes every reference tree using actual Keccak-256, then checks its root
  against a separate incremental execution with the Keccak backend.
- Observes the existing counted `hash-union` wrapper without changing its
  behavior; checks the event count against both the implementation counter and
  `paper-test/hash-counts-per-append`.
- Generates recency samples and runs five existing source tests:
  `paper-figures-test`, `lemma-16-test`, `lemma-17-hash-count-test`,
  `membership-proofs-test`, and `membership-proofs-large-test`. The initial
  artifact reports 30 passing assertions from those tests. It does not claim
  that the full repository test suite ran.
- Executes the incremental construction with the Keccak backend through 100,000
  appends, exporting every root and structural hash count. Roots are stored as
  concatenated 64-character hexadecimal digests, in increasing prefix order.
  The exporter retains detailed hash events at additional checkpoints around
  binary boundaries and at n = 1,337, 50,000, 99,999, and 100,000. It also exports
  Bouncy Castle Keccak vectors spanning single- and multi-block inputs.

Generation fails on a root, counter, proof, schedule, or selected-test mismatch.
The Node check validates the exported graph, all 2,080 membership paths, altered
paths, recency comparisons, and asset constraints. For every state it also
contracts the paper view's identity nodes and checks that the result is exactly
the original hashing topology, including the single-leaf case. For all 2,080
proofs it verifies that only the selected route is expanded, every sibling is
visible exactly once, and the collapsed frontier still covers all leaves.

`live-test.mjs` independently runs the JavaScript engine and compares every
initial root, node digest, edge, proof, and hash operation with the Clojure
fixtures. It verifies every root and append hash count through 100,000 leaves
and the checkpoint hash events, checks Keccak vectors (including rate-block boundaries), and
reproduces the Clojure recency samples. It also checks valid and altered
cryptographic proofs, history preservation, input bounds, and construction
through the 100,000-leaf limit. The sparse rendering snapshots are compared with
the full topology projection for all 2,080 reference paths; larger snapshots
are checked to contain only the visible route, siblings, and bagging nodes.
The test requires the Clojure reference range to equal the browser limit, so
increasing that limit without extending the references fails validation.
Full topology and all membership co-paths are cross-checked for the first 64
states; larger prefixes have complete root/count coverage and selected proof
checks. This finite comparison uses leaf-index payloads and is not a proof of
equivalence for arbitrary inputs.

For optional browser regression tests, install Playwright and its Chromium
browser in a test environment, then run:

```sh
node reviewer/browser-test.mjs
```

`MMB_PLAYWRIGHT_MODULE` may point to an existing Playwright ESM entry point;
`MMB_BROWSER_EXECUTABLE` may point to an existing Chromium executable.
`MMB_BROWSER_ARGS` accepts a JSON array of browser launch arguments.
`MMB_SCREENSHOTS` optionally selects a screenshot output directory. The test
exercises both display modes and their toggle, the initial before/after transitions,
triangular peaks, focused paths, absent-before handling for new leaves,
keyboard selection, altered paths,
live computation through 100,000 leaves and its Clojure root/count comparison, cancellation,
automatic vertical layout, large-state leaf input, full-history plot coverage,
checks and recency samples beyond the exported ranges, mobile page width, offline file viewing, and
the sandboxed web preview's content security policy. It serves the policy test through
local Playwright interception.
It also corrupts reference roots and counts, including beyond 4,096 leaves,
and confirms that the live display remains correct while the reference
comparison reports the mismatch.

## What the prototype demonstrates

`reviewer/live.js` is a JavaScript implementation of the Clojure append schedule:
append a leaf, merge one pending equal-height pair from a LIFO stack, partition
the peaks into ranges, and bag ranges and belts from left to right. An absent
left child is an identity. Range and belt hashes are reused only when their
operands match the previous state; a mountain merge always counts as a new
hash, even when a prior bag happened to hash the same pair. Hash event order
can differ from Clojure, but the counted operand/result pairs agree.

The browser scans the short peak and bag chains and builds display/proof data
on demand. This is not a port of the Clojure implementation's pointer-level
performance optimizations. The counters measure actual construction hashes,
not JavaScript runtime or the cost of drawing a graph. Snapshots share immutable
subtrees; only a few graph representations are cached. Rendering traverses only
the visible peaks, selected path, and siblings. Prefix hash totals and maxima
are maintained during construction instead of rescanning the history on navigation.
The larger computation range still takes time and memory proportional to the
appends performed; the 100,000-leaf limit is an interactive resource bound, not
a limit of the MMB construction.

The initial exported states are **reference fixtures, not the explorer's data
source**. All displayed states, hashes, membership paths, and recency samples
are computed locally. The page is not a live JVM test runner, and finite checks
are not formal proofs. The source-test report remains a result recorded at
export. Leaf payloads are still indices, not arbitrary user-supplied data.

Membership verification hashes the leaf index and the ordered sibling digests,
checks interval adjacency, and compares the computed digest to the current root.
The compact BigInt Keccak implementation follows the permutation and sponge
construction in the [Keccak specification summary](https://keccak.team/keccak_specs_summary.html),
using the original Keccak suffix `0x01`. Keccak-256 and SHA3-256 have different
padding; substituting SHA3-256 would not reproduce these roots.

Structural hash counts exclude the one leaf hash per append, debug validation
hashes, visualization, and proof generation. Leaf indices use the implementation's
8-byte little-endian encoding. Counts come from the browser's Keccak execution because
the interval backend can collapse equal-span values and undercount work.
Lemma labels follow the source tests and may differ between paper revisions.

Only `docs/index.html` is needed for viewing. If the web preview is unavailable,
download that file and open it locally.
