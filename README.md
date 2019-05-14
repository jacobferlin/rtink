
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tinkr

## Overview

**tinkr** is an R wrapper for the Tink REST API.

Tink is a platform for open banking. Read more at their website
[Tink](https://docs.tink.com/).

## Prerequisites

Sign up at [Tink](https://docs.tink.com/) to create an app. You will get
a TINK\_CLIENT\_ID and a TINK\_CLIENT\_SECRET. These needs to be set in
you R environment.

``` r
# Package usethis from CRAN makes it easy to set environment variables
# install.packages("usethis")
usethis::edit_r_environ()
```

## Installation

``` r
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("jacobferlin/tinkr")
```

## Usage

Create a `token` to authenticate yourself. A web page opens for you
login to your bank. The scope argument sets what is downloaded to your
app on Tink. The same page then redirects to an empty page, but the url
contains a code which needs to be copied to the R console. [Supported
markets](https://docs.tink.com/resources/market-coverage).

``` r
library(tinkr)
token <- token(scope = c("transactions", "accounts"), market = "SE")
```

This token is used to download data from your Tink app.

``` r
transactions(token)
accounts(token)
```
