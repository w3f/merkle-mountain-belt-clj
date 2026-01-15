# Merkle Mountain Belt

Implements the Merkle Mountain Belt (MMB) - a cryptographic accumulator with `O(log(n))` accumulator size and `O(log(k))` `{accumulator update, membership witness, prefix witness}` cost, where `k` is the distance from the latest leaf to the leaf being considered. There exist two forms of MMB: single- and double-bagged. The single-bagging implementation is considerably simpler to implement, but does not have as attractive performance characteristics as the double-bagging variant.

For an encompassing description of MMB, see [the preprint paper](https://arxiv.org/abs/2511.13582).

The codebase is organized as follows:

- `storage.clj` the persistent storage backend and the associated pointer arithmetic
- `core.clj` contains the one-shot MMB construction method (both single- and double-bagging variants), as well as MMR construction methods
- `state.clj` contains the state containers for constructing an MMB
- `linked-peaks.clj` contains the incremental append operation
- `visualization_mmr.clj` contains the visualization of MMRs
- `visualization_mmb.clj` contains the visualization of MMBs
- `primitives/` contains primitives shared among the above

Feedback and contributions welcome!
