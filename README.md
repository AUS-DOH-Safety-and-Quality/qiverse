
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qiverse

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/qiverse)](https://CRAN.R-project.org/package=qiverse)

<!-- badges: end -->

The goal of qiverse is to provide a single package wrapping up all
useful quality improvement packages in HQIU.

## Installation

You can install the development version of `qiverse`:

``` r
devtools::install_github("AUS-DOH-Safety-and-Quality/qiverse")
```

Branches of the development version of `qiverse` can also be installed
through the “ref” option:

``` r
devtools::install_github("AUS-DOH-Safety-and-Quality/qiverse", ref = "branch-a")
```

## Included Packages

The following packages are included in `qiverse`:

| qiverse Package    | Description                                          |
|--------------------|------------------------------------------------------|
| qiverse.azure      | Creating Azure Access Tokens                         |
| qiverse.data       | Sample data used in the qiverse                      |
| qiverse.powerbi    | Accessing data from PowerBI                          |
| qiverse.qimatrix   | Creating QI Matrices including the SPC Funnel Matrix |
| qiverse.qimisc     | Creating the Multiple Indicator Sigma Chart          |
| qiverse.qipatterns | Identify SPC Patterns and Funnel Outliers            |
| qiverse.qiplotly   | QI charts using the plotly package                   |
| qiverse.sharepoint | Accessing Microsoft Sharepoint                       |
| qiverse.snowflake  | Accessing Snowflake                                  |
