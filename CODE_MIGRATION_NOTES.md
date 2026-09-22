# Code migration notes

This is the first building block for an Insper Cidades code-migration playbook.
It records practices learned while converting the Alvarás project from three
interactive scripts into a reproducible `targets` pipeline.

## Preserve before changing

Create an immutable branch and tag containing the project exactly as received,
including permitted binary inputs and outputs. Remove only editor, session, and
operating-system artifacts. A rewrite should remain auditable against this
snapshot even when binaries are omitted from the main working tree.

## Characterize behavior first

Before refactoring, record row counts, schemas, coordinate systems, key
cardinalities, and important aggregates from every supplied intermediate and
final output. Store small contracts in Git and keep large benchmark files local
or in a data archive. These checks distinguish implementation mistakes from
inconsistencies already present in the handoff.

## Separate pipeline stages

Keep discovery, download, import, schema validation, cleaning, geocoding,
matching, validation, and export in separate functions and targets. Reading
should not flow directly into cleaning, and joins or row binding should end in
named intermediate objects. This makes failures attributable to one stage.

## Treat network services as data sources

Geocoding is not a deterministic transformation. Normalize requests, cache the
provider response on disk, record retrieval metadata, and query only unseen
addresses. Do not call a remote geocoder in ordinary CI. Decide explicitly
whether the cache can be published before claiming full reproducibility.

## Preserve rules, not inefficient representations

The legacy matcher allocated several dense pairwise matrices. At 26,886 permit
records, one dense matrix contains more than 722 million cells. The rewrite
generates candidate pairs from shared SQL identifiers and spatial neighborhoods,
then builds sparse graphs. Pair-level tests prove equivalence of the five match
rules without preserving the memory-heavy representation.

## Do not use incidental identifiers as domain keys

Connected-component numbers are local labels whose values depend on graph and
package implementation details. They must not be compared across independently
built graphs. Convert components to stable domain identifiers or carry the graph
type as part of the key. Pinning dependencies helps reproduction but does not
turn incidental labels into a sound data model.

## Make dependencies explicit

Avoid the `tidyverse` meta-package and broad package attachment. Use explicit
namespaces where conflicts are plausible and `import::from()` for repeatedly
used, unambiguous functions. Keep `renv.lock` aligned with code and exclude the
archived original scripts from dependency discovery.

## Optimize only demonstrated bottlenecks

Profile before adding parallelism or memoisation. Prefer `targets`-level
parallelism to nested `future` workers. Use durable caches for reproducibility;
an in-memory memoised result is only a performance aid. Optimize representations
before parallelizing an algorithm with excessive memory use.

## Finish with executable evidence

Format final R code with Air, run unit and characterization tests, parse the
pipeline manifest in CI, and perform a full local run when external services and
data are available. Document divergences instead of weakening tests or silently
changing finalized analytical rules.
