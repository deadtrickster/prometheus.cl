# Quantile summary

`make-quantile-summary` is a port of prometheus.erl's `prometheus_quantile_summary`, which uses ddskerl. Both implement DDSketch (Masson, Rim, Lee, VLDB 2019).

## Sketch

Value `v` goes to bucket `ceiling(log_gamma v)`, `gamma = (1 + a) / (1 - a)`. Each bucket is reported as `2 gamma^k / (gamma + 1)`, within `a` of every value in it. Counters are dense and preallocated over `[min-value, max-value]`. Below the range (including zero) goes to an underflow counter. Above it goes to an overflow counter. Exact min and max are tracked, and estimates are clamped to them.

Difference from ddskerl - ddskerl indexes only values above 1 and puts all of `(0, 1]` in one bucket. That loses sub-second latencies, so here the range is configurable and defaults to `1e-9 .. 1e9`.

Sketches with the same parameters merge by adding counters, with the same error bound as one sketch that saw every value. CKMS (`make-summary`) cannot do this.

## Layout and atomicity

A sketch is one double-float array:

```
[ guard | min max | count0 sum0 | count1 sum1 | ... | guard ]
```

Each `[count sum]` and `[min max]` is a 16-byte pair. On x86-64 SBCL an observe updates `min max` and then its bucket's `count sum`, each with one `lock cmpxchg16b` (`base/cas-pair.lisp`, a VOP since SBCL only has a pair CAS for `simple-vector`). Elsewhere the same array is updated under a lock.

What that guarantees:

- a bucket's count and sum change together, so a read or merge never sees one without the other
- extrema are written before the count, so a counted observation always has real extrema (an empty sketch holds placeholders: min is the largest double, max is 0)
- a merge from a sketch that is still being written reads each bucket in one step, and may leave out observations that land during the merge

Counts are doubles, exact up to 2^53, so the whole array is uniform and a pair is two lanes of one SSE register. Guards of one cache line (measured at load) keep other objects off the lines being written.

Reads merge the shards into a buffer the sharded sketch owns, one pair at a time: `movdqa` from the shard, `addpd` into the buffer, `movdqa` back, the same three instructions for every bucket. Intel (SDM 9.1.1) and AMD (APM 7.3.2) guarantee an aligned 16-byte `movdqa` is atomic on CPUs with AVX, so each pair is read in one step. On CPUs without AVX the merge reads pairs with the CAS instead. 32-byte loads have no such guarantee, which is why the merge stays at 16 bytes. Quantiles then walk the one buffer; a snapshot merges straight into the target.

## Memory

Fixed at creation. With the defaults a sketch is about 2100 buckets, two doubles each, so 34KB. A series has `shards` sketches, all allocated with the series. The default is one per configured CPU rounded up to a power of two, so about 135KB per series on 4 CPUs. For high-cardinality families, lower `:shards` or narrow the range. Parameters are limited to 65536 buckets per sketch.

Nothing is allocated on the hot paths:

- Observe - two 16-byte CAS, no boxed doubles
- Read - quantiles are computed in one pass over all shards, written into caller buffers (`ddsketch-quantiles-into`, `sharded-ddsketch-quantiles-into`). Each series owns a scrape buffer used by `with-quantile-summary-estimates`.
- Merge - `ddsketch-reset` makes any sketch a reusable target. `sharded-ddsketch-snapshot` and `quantile-summary-merged-sketch` take `:into`.

The tests check zero bytes consed on these paths.

Observations must be finite and non-negative. Sums are doubles.

## Sharding

A writer uses the shard of the CPU it runs on (`sched_getcpu`), and shards are combined on read, like prometheus.erl's per-scheduler rows. Writes stay atomic since a thread can be preempted or migrated mid-update (#20). On SBCL off Linux the shard is `os-tid mod shards`. Elsewhere than SBCL there is one locked sketch.

`sharded-ddsketch-observe`, spread values, shared 4-CPU VM (so 4 shards), million ops/s:

| threads | SBCL 2.2.9 | SBCL 2.6.10 |
|---|---|---|
| 1 | 16.4 | 13.3-13.7 |
| 4 | 37.0-45.5 | 38.5 |
| 8 | 38.5-50.0 | 41.7-47.6 |
| 32 | 50.0 | 43.5-47.6 |

Reading three quantiles over 4 shards takes 20-30us, a snapshot into a reused sketch 10-16us.
