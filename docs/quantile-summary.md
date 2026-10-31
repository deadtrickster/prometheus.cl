# Quantile summary

`make-quantile-summary` is a port of prometheus.erl's `prometheus_quantile_summary`, which uses ddskerl. Both implement DDSketch (Masson, Rim, Lee, VLDB 2019).

## Sketch

Value `v` goes to bucket `ceiling(log_gamma v)`, `gamma = (1 + a) / (1 - a)`. Each bucket is reported as `2 gamma^k / (gamma + 1)`, within `a` of every value in it. Counters are dense and preallocated over `[min-value, max-value]`. Below the range (including zero) goes to an underflow counter. Above it goes to an overflow counter. Exact min and max are tracked, and estimates are clamped to them.

Difference from ddskerl - ddskerl indexes only values above 1 and puts all of `(0, 1]` in one bucket. That loses sub-second latencies, so here the range is configurable and defaults to `1e-9 .. 1e9`.

Sketches with the same parameters merge by adding counters, with the same error bound as one sketch that saw every value. CKMS (`make-summary`) cannot do this.

## Memory

Fixed at creation. With the defaults a sketch is about 2100 words (17KB). A series has `shards` sketches, all allocated with the series. The default is one per configured CPU rounded up to a power of two, so about 70KB per series on 4 CPUs. For high-cardinality families, lower `:shards` or narrow the range.

Nothing is allocated on the hot paths:

- Observe - `atomic-incf` on the counter word. Sum, min and max are raw double bits in word slots, updated by CAS. No boxed doubles.
- Read - quantiles are computed in one pass over all shards, written into caller buffers (`ddsketch-quantiles-into`, `sharded-ddsketch-quantiles-into`). Each series owns a scrape buffer used by `with-quantile-summary-estimates`.
- Merge - `ddsketch-reset` makes any sketch a reusable target. `sharded-ddsketch-snapshot` and `quantile-summary-merged-sketch` take `:into`.

The tests check zero bytes consed on these paths.

Sums and extremes are doubles. Integer observations are exact up to 2^53. Observations must be finite and non-negative.

## Sharding

A writer uses the shard of the CPU it runs on (`sched_getcpu`), and shards are combined on read, like prometheus.erl's per-scheduler rows. Writes stay atomic since a thread can be preempted or migrated mid-update (#20). On SBCL off Linux the shard is `os-tid mod shards`. Elsewhere than SBCL there is one locked sketch.

`summary.observe` through the API, SBCL 2.6.10, shared 4-CPU VM (so 4 shards), median of 3, million ops/s:

| threads | 1 shard | 4 shards |
|---|---|---|
| 1 | 7.3 | 6.9 |
| 4 | 5.7 | 12.7 |
| 8 | 5.8 | 13.7 |
