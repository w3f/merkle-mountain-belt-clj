(ns hashing
  "pluggable hash backend. two modes:

   :interval (default) - the [lo hi] leaf-span proxy the verification suite is built on.
     a node's value is the contiguous leaf range it covers, so unions assert adjacency and
   spans are directly readable. NOTE: NOT a real hash: it is associative and span-determined,
     which is why no optimization may exploit it (see feedback_no_model_exploits).

   :keccak - BEEFY MMR compatible, i.e. what production would compute:
     leaf     = keccak256(SCALE(leaf payload))
     interior = keccak256(left || right)   over 32-byte digests
   digests are lowercase hex strings, so they are valid map keys (value equality),
   EDN-serializable, and printable. the leaf payload here stands in for BEEFY's MmrLeaf:
   a SCALE-encoded u64 index (8 bytes little-endian). 8 != 2*32, so leaf and interior
   preimages can never collide, see disjointness requirement of the bare profile in the spec.

   both modes share the identity convention for an absent child: a bag node with no left
   child == its right child (no hash computed, no op counted)."
  (:import [org.bouncycastle.crypto.digests KeccakDigest]))

(def ^:dynamic *backend* :interval)

(def phantom
  "the absent/empty hash sentinel, backend-neutral"
  [])

(defn phantom? [h]
  (or (nil? h) (= phantom h)))

;; ---------- keccak (BEEFY) ----------

(defn keccak256 ^bytes [^bytes bs]
  (let [d (KeccakDigest. 256)
        out (byte-array 32)]
    (.update d bs 0 (alength bs))
    (.doFinal d out 0)
    out))

(defn ->hex [^bytes bs]
  (let [sb (StringBuilder. 64)]
    (dotimes [i (alength bs)]
      (.append sb (format "%02x" (bit-and (aget bs i) 0xff))))
    (str sb)))

(defn hex-> ^bytes [^String s]
  (let [n (quot (count s) 2)
        out (byte-array n)]
    (dotimes [i n]
      (aset-byte out i (unchecked-byte (Integer/parseInt (subs s (* 2 i) (+ 2 (* 2 i))) 16))))
    out))

(defn scale-u64 ^bytes [n]
  ;; SCALE encodes a fixed-width u64 as 8 bytes little-endian
  (let [out (byte-array 8)]
    (dotimes [i 8]
      (aset-byte out i (unchecked-byte (bit-and (unsigned-bit-shift-right (long n) (* 8 i)) 0xff))))
    out))

(defn- keccak-leaf [i]
  (->hex (keccak256 (scale-u64 i))))

(defn- keccak-node [l r]
  (let [lb (hex-> l) rb (hex-> r)
        buf (byte-array (+ (alength lb) (alength rb)))]
    (System/arraycopy lb 0 buf 0 (alength lb))
    (System/arraycopy rb 0 buf (alength lb) (alength rb))
    (->hex (keccak256 buf))))

;; ---------- interval proxy ----------

(defn- interval-node [a b]
  (let [[a-lo a-hi] a
        [b-lo b-hi] b]
    (when-not (= (inc a-hi) b-lo)
      (throw (ex-info (str "interval hash: not consecutive: " a " " b) {:a a :b b})))
    [a-lo b-hi]))

;; ---------- backend-neutral api ----------

(defn leaf-hash
  "hash of the item at 1-indexed leaf position i"
  [i]
  (case *backend*
    :interval [i i]
    :keccak (keccak-leaf i)))

(defn node-hash
  "hash of an interior/bagging node from its children. an absent child (nil or phantom)
   is the identity: the node == its present child (i.e. the right child: only left kids can be absent),
   no hash computed. uncounted; the counted wrapper lives in linked-peaks/hash-union"
  ([] phantom)
  ([a] a)
  ([a b]
   (cond
     (phantom? a) (or b phantom)
     (phantom? b) a
     :else (case *backend*
             :interval (interval-node a b)
             :keccak (keccak-node a b)))))

(defn real-union?
  "true when node-hash would actually compute a hash (both children present), i.e. when
   the operation counts against the lem:hash-d budget"
  [a b]
  (not (or (phantom? a) (phantom? b))))

(defmacro with-backend [backend & body]
  `(binding [*backend* ~backend] ~@body))
