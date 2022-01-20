# Merkle Mountain Belts

Implements merkle mountain belts (MMBs) - a cryptographic accumulator with `O(log(n))` accumulator size and `O(log(k))` `{accumulator update, membership witness, prefix witness}` cost. There exist two forms 

Currently, the codebase is split as follows:

- `storage.clj` the persistent storage backend and the associated pointer arithmetic
- `core.clj` contains the one-shot MMB construction method (both single- and double-bagging variants), as well as MMR construction methods
- `linked-peaks.clj` contains the incremental append operation
- `visualization.clj` contains the visualization of MMRs and MMBs

Feedback and contributions welcome :)
