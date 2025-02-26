
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qiverse.qiplotly

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/qiverse.qiplotly)](https://CRAN.R-project.org/package=qiverse.qiplotly)

<!-- badges: end -->

The goal of qiverse.qiplotly is to provide a package ploting SPC and
Funnel Plots in Plotly.

## Installation

Before installing the development version of `qiverse.qiplotly`, add a
Github personal access token to R by:

``` r
#Go to github page to generate token
usethis::create_github_token()

#paste your PAT into pop-up that follows...
credentials::set_github_pat()
```

These credentials will now allow you to download private repositories
from the AUS-DOH-Safety-and-Quality organisation.

You can now install the development version of `qiverse.qiplotly` like
so:

``` r
devtools::install_github("AUS-DOH-Safety-and-Quality/qiverse.qiplotly")
```

Branches of the development version of `qiverse.qiplotly` can also be
installed through the “ref” option:

``` r
devtools::install_github("AUS-DOH-Safety-and-Quality/qiverse.qiplotly", ref = "branch-a")
```

### Setting up your .Renviron

In order to enjoy the full functionality of the SharePoint, PowerBI and
Snowflake connectivity features, you must have your environment
variables set up correctly in R. This step only needs to be completed
once. Modify your .Renviron file by entering the following command in R:

``` r
usethis::edit_r_environ()
```

For internal users from HQIU, you can use [this sharepoint text
file](https://wahealthdept.sharepoint.com/:t:/r/sites/SafetyandQualityIndicatorSetSQuIS/internal/SQuIS%20O365%20Confidential%20Documents/R/connectivity/example_renviron.txt)
as a template. Please replace your he number and local github path with
your unique information.

Save the .Renviron file, and restart your R session. Your envrionment
variables are now set.

## Package Vignettes

The rendered package vignettes can be viewed after installation in R
using the following command:

``` r
browseVignettes("qiverse.qiplotly")
```
