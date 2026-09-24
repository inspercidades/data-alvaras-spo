# Validation

The pipeline reproduces version 3 from its supplied inputs. Regression tests in
`tests/testthat/test-regression.R` check two stages against the received
snapshots.

| Stage | Input | Snapshot | Rows |
|---|---|---|---|
| Cleaning | municipal archive of 2025-04-22 | `alvaras_pde.parquet` | 27,009 |
| Developments | `geo_alvaras_pde.parquet` (26,886 permits) | `geo_alvaras_emp_pde.parquet` | 11,735 |

Both tests compare every column; the development test also compares every
point. The final snapshot matches the version 3 file in Dataverse. The
benchmark files are excluded from Git; the tests read them from
`data/benchmark/` and skip when they are missing.

A full run with fresh ArcGIS results also yields 11,735 developments and
914,401 units. One development changes match type because ArcGIS now places
one address slightly differently.

## Corrected diagnosis

An earlier version of this note reported that the pipeline produced 12,206
developments and blamed unstable `igraph` component labels. The cause was a
bug in the migrated matcher: `component_ids()` filtered edges with
`.data$match_type == match_type`, which compares the column with itself. All
five match graphs were identical and every match became "perfeito". The
original script does not have this bug.

## Questions for the dataset authors

The pipeline keeps the version 3 rules below unchanged so that it reproduces
the published data. Each rule is marked "Questão para os autores" in
`R/identify_developments.R`, and the `original-review` branch marks the same
lines in the original scripts. Any change should be a documented part of the
next release.

1. **Permits left out by the parcelamento join.** The script overwrites
   `ind_loteamento` with a lot-level flag and then joins on every shared
   column. Permits whose flag changed find no match, receive a missing
   `ind_parcelamento`, and are dropped from the developments. In version 3,
   this removes 32 approval and execution permits in 17 lots that also have a
   plano integrado permit.
2. **Permits without an SQL.** The matcher treats all 102 permits without an
   SQL as sharing one lot. The same join then drops them from the
   developments. Should they be matched by location only, or excluded
   explicitly?
3. **Correction filter in the plano integrado count.** In
   `length(which(descricao_tipo == "PLANO INTEGRADO") & ind_correcao == FALSE)`,
   the correction filter falls outside `which()`, so the count ignores it. This
   has no effect on version 3.
4. **Choice among partial matches.** The comment says a permit in several
   partial-match groups joins the one with more units. The code compares the
   permit's own units across its groups, which always tie, so the permit joins
   its lowest partial type (2, then 3, then 4).
5. **Sub-lot order in parcelamentos.** After summarising sub-lots, the rows
   are ordered by land area. Dates, SQL, and address therefore come from the
   smallest sub-lot, not from the most recent permit.
6. **Geocoding policy.** Of 254 permits without municipal coordinates, 123
   are dropped without geocoding: their address lacks a street name or
   number, gives a kilometre marker, or uses a placeholder. In the latest run,
   10 of the 131 geocoded permits were placed at a street or town centroid,
   one of them outside São Paulo. Should such matches be dropped? Esri's terms may also restrict storing results from the free
   geocoding service; publishing coordinates may require an API key.
