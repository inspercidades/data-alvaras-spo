# Validation

The received project contains three snapshots:

- 27,009 cleaned permit records;
- 26,886 geocoded permit records;
- 11,735 final development records.

The final snapshot has the same schema and aggregate contract as version 3 in
Dataverse. The local copy is deliberately excluded from Git; tests read it from
`data/benchmark/` when available.

The supplied geocoded intermediate does **not** rebuild the supplied final
snapshot with the supplied script: the script produces 12,206 rows under the
pinned environment. Pair-level tests confirm that the new sparse matcher applies
the same five match rules as the original dense matrices. The remaining
divergence appears downstream, where the legacy code combines numeric connected-
component labels produced independently for five graphs. Those labels are
implementation details of `igraph`, not stable identifiers, so the result can
change with graph construction or package versions.

This divergence must be reviewed with the dataset authors before the next public
release. The published version 3 file remains the authoritative historical
dataset; the pipeline must not silently overwrite it.
