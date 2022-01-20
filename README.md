# Merkle Mountain Belts

Implements merkle mountain belts (MMBs) - a cryptographic accumulator with `O(log(n))` accumulator size and `O(log(k))` `{accumulator update, membership witness, prefix witness}` cost, where `k` is the distance from the latest leaf to the leaf being considered. There exist two forms of MMBs: single- and double-bagged. The single-bagging implementation is considerably simpler to implement, but does not have as attractive performance characteristics as the double-bagging variant.

Currently, the codebase is split as follows:

- `storage.clj` the persistent storage backend and the associated pointer arithmetic
- `core.clj` contains the one-shot MMB construction method (both single- and double-bagging variants), as well as MMR construction methods
- `linked-peaks.clj` contains the incremental append operation
- `visualization.clj` contains the visualization of MMRs and MMBs

Feedback and contributions welcome :)
