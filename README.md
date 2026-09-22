# Building permits for new developments in São Paulo

This repository produces the recurring public dataset *Alvarás de Novas
Edificações, São Paulo*. It downloads permits from the municipal PDE monitoring
portal, standardizes permit records, geocodes records without municipal
coordinates, and groups permits that belong to the same development.

The published dataset is available from the [Insper
Dataverse](https://doi.org/10.60873/FK2/FPCIDI). The current release covers
2010–2024. Dataset fields and domain documentation remain in Portuguese.

## Reproducibility status

The implementation has been migrated from three sequential scripts to a
`targets` pipeline. The rules in the original scripts are authoritative.

The supplied geocoded intermediate does not reproduce the supplied final file
under the pinned environment. The final file remains the authoritative
historical release. See [the validation notes](validation/README.md) for the
measured divergence and its likely cause.

## Run the pipeline

```sh
R -e "renv::restore()"
R -e "targets::tar_make()"
```

The authoritative output is `outputs/alvaras.parquet`, an EPSG:31983 GeoParquet
file. Raw downloads, intermediate data, geocoding responses, and generated
outputs are excluded from Git.

## Source pinning and geocoding

Development runs discover the newest dated municipal archive and record its URL,
retrieval time, and SHA-256 checksum in `data/raw/source.yml`. Before a release,
the dated URL is committed in `config/source.yml`; the release retains the
generated manifest so its exact bytes can be verified.

Coordinates supplied by the municipality take priority. Missing coordinates
are obtained from ArcGIS. Remote geocoding is not run in continuous integration;
its cache and publication policy will be reviewed with the dataset authors.

## Repository structure

```text
_targets.R        pipeline definition
R/                pipeline functions
data/reference/   versioned reference geography
data/raw/         downloaded source and source manifest (ignored)
data/processed/   intermediate data and geocoding cache (ignored)
outputs/          generated publication files (ignored)
original/         original scripts and project documentation
tests/testthat/   unit and regression tests
```

The `original` branch and `original-v1` tag preserve all received files. The
`main` branch retains the scripts and documentation but omits received binary
data from its working tree.

Lessons from this conversion are recorded in
[the code-migration notes](CODE_MIGRATION_NOTES.md).

## Citation

> Costa, Adriano Borges; Guizzardi, Henrique (2025), “Alvarás de Novas
> Edificações, São Paulo [2010-2024]”, Insper Dataverse, V3,
> https://doi.org/10.60873/FK2/FPCIDI

## Licenses

Code is licensed under the MIT License. Project-authored documentation and data
are licensed under CC BY 4.0. Municipal source data retain their original
attribution and terms.
