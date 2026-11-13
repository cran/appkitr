
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `appkitr` R package <img src="man/figures/logo.png" align="right" height="120" />

`appkitr` is an R package that helps you import, label, and process
survey data collected with the GESIS AppKit platform. It supports
parsing SPSS syntax files to extract variable labels and value mappings,
and merges them into raw CSV data exports.

> Note: The package is currently in development and not yet available on
> CRAN.

## Compatibility

This package is compatible with **GESIS AppKit version 2.0.0**. The
package version (2.0.0.5) follows the format
`<appkit_version>.<package_patch>`, where

- 2.0.0 indicates compatibility with AppKit 2.0.0
- 5 is the package patch version for this `appkitr` release

## Installation

**Recommended**: Install the latest stable release from its source
tarball:

``` r
install.packages("remotes")
remotes::install_url("https://gitlab.com/gesis-appkit/appkitr/-/releases/permalink/latest/downloads/appkitr.tar.gz")
```

Alternatively, install the development version from the
[`appkitr`](https://gitlab.com/gesis-appkit/appkitr) GitLab repository
for the latest features

``` r
remotes::install_gitlab("gesis-appkit/appkitr")
```

or a specific release by its tag

``` r
remotes::install_gitlab("gesis-appkit/appkitr@v2.0.0.5")
```

Tarballs for all versions are listed on the [releases
page](https://gitlab.com/gesis-appkit/appkitr/-/releases).

## Quick Start

### Reading a Single Survey

``` r
library(appkitr)

# Read survey from a directory containing CSV and SPS files
survey <- read_appkit_survey("path/to/survey_directory")

# Read survey with custom NA value handling
survey <- read_appkit_survey("path/to/survey_directory", na_value = -1)

# View variable label
attr(survey$duration, "label")

# View value labels
levels(survey$duration)
```

### Reading Multiple Surveys

``` r
# Read all surveys from a parent directory
surveys <- read_appkit_surveys("path/to/surveys_parent_directory")

# Access individual surveys
survey1 <- surveys[["survey_name_1"]]
survey2 <- surveys[["survey_name_2"]]
```
