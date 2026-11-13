# appkitr (`master` / development version)

# appkitr 2.0.0.5
* Drop the `.sps` file extension from the package description, which CRAN's spell check flagged as a misspelling.

# appkitr 2.0.0.4
* Wrap the `create_payout_scheme()` example in `if (interactive())` instead of `\dontrun{}`.

# appkitr 2.0.0.3
* Fix typo in example data.
* Change maintainer email address for CRAN resubmission.

# appkitr 2.0.0.2
* Fix participation_table(): parse timestamp columns before comparing them
* Quote 'GESIS AppKit' as a software name in `DESCRIPTION`.
* Replace `\dontrun{}` with runnable examples in `read_appkit_survey()` and `read_appkit_surveys()` and add small example data for that in `inst/extdata/`.
* Replace`cat()` with `message()` in `utils.R` (helpers for `create_payout_scheme()`).

# appkitr 2.0.0.1

* Prepared the package for CRAN submission: standard Apache license specification in `DESCRIPTION`, title in title case, corrected URLs, and exclusion of non-standard files from the built package
* `inspect_variables()`, `inspect_variable()`, and `drop_participants()` now have runnable examples based on the bundled `appkit_survey_entry` data
* Added an internal registry of AppKit export metadata columns and a shared column validator
* Ensure all functions are column-agnostic with column names supplied via function arguments
* Updated and extended the synthetic example data (#41)
* `read_appkit_survey()` and `read_appkit_surveys()` now convert numeric missing codes (e.g. `-1`) to `NA` in numeric columns correctly; the default `na_values` is now a type-preserving `list`
* Added `participation_table()` to report per participant and survey, the number of committed questionnaires or the completion percentage. Rows scheduled in the future are excluded by default via `include_future_scheduled`, since they are already present in the export but not yet answerable. (#11)
* Added `calculate_payout()` to calculate payout per participant, supporting a fixed amount per committed questionnaire and/or a bonus paid once a minimum number of committed questionnaires is reached.  (#33)
* Added `create_payout_scheme()` to interactively build a payout scheme step by step. It's optional and it requires an interactive session. (#33)

# appkitr 2.0.0.0

* `read_appkit_survey()` and `read_appkit_surveys()` now treat `"N/A"` as a missing value by default, matching the default value used in GESIS AppKit >= 2.0.0 exports (#35)
* `read_appkit_survey()` and `read_appkit_surveys()` now parse the `expired` timestamp column added in GESIS AppKit >= 2.0.0 exports (#39)

# appkitr 1.19.0.0

* Compatibility with GESIS AppKit version 1.19.0 (no package-relevant changes)

# appkitr 1.17.0.0

* Added `drop_participants()` to remove specific participants from survey data frames or lists of data frames (#27)
* `read_appkit_surveys()` now throws an informative error when no survey directories are found (#21)
* Added ISO 8601 timestamp format support for AppKit >= 1.17.0 exports (#25)
* Added parsing of `exported` and `lastStatusChange` timestamp columns (#29)

# appkitr 1.16.0.0

* Compatibility with GESIS AppKit version 1.16.0

# appkitr 1.13.0.1

* Added `duplicate_names` parameter to `read_appkit_surveys()` function (#16)
  - `"error"` (default): Throw error when duplicate survey names are found
  - `"merge"`: Combine surveys with the same name using `rbind()`
  - `"distinct"`: Keep surveys distinct by adding numeric suffixes
* Fixed an issue with documentation site generation in GitLab CI/CD pipeline
* Shorten test data folder names to avoid path length warnings when installing from GitLab (#20)


# appkitr 1.13.0.0

* Compatibility with GESIS AppKit version 1.13.0
* Support for reading AppKit survey data from CSV files
* Automatic application of variable and value labels from SPSS syntax files
* Timestamp parsing with locale-safe handling

