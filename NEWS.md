# data-alvaras-spo development version

- Began migration of the version 3 dataset workflow to a reproducible `targets`
  pipeline.
- Preserved the received project on the `original` branch and `original-v1`
  tag.
- Fixed the development matcher, which merged all five match types into one
  graph and produced 12,206 developments instead of 11,735.
- Fixed month names, which depended on the system locale.
- Rewrote permit cleaning and development summaries; the output still
  reproduces version 3.
- Added regression tests that rebuild the version 3 cleaned and final
  snapshots.
- Added ArcGIS match scores and match types to the geocoding cache, and a
  warning for coarse matches.
- Listed version 3 rules to review with the dataset authors in
  `validation/README.md`.
