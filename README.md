
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reliable <img src="man/figures/logo.png" align="right" width="126" height="77" />

[![Travis build
status](https://travis-ci.org/JonasMoss/reliable.svg?branch=master)](https://travis-ci.org/JonasMoss/reliable)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/reliable?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/reliable)
[![Codecov test
coverage](https://codecov.io/gh/JonasMoss/reliable/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/reliable?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

An `R` package for reliability coefficients and factor scores for the
linear factor model.

## Installation

From inside `R`, use the following command:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/reliable")
```

## Usage

Use `lavaan` to fit a linear factor model.

``` r
library("reliable")

model = ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 '

object = lavaan::cfa(model = model,
                     data = lavaan::HolzingerSwineford1939)
```

Now you we can find the sum-score reliability and the associated factor
scores.

``` r
reliability(object, type = "sumscore")
#> $reliability
#> [1] 0.7569798
#> 
#> $weights
#>           [,1]
#> [1,] 0.8691074
#> 
#> $iota
#>       [,1] [,2] [,3]
#>  [1,]    1    0    0
#>  [2,]    0    0    0
#>  [3,]    1    0    0
#>  [4,]    0    1    0
#>  [5,]    0    1    0
#>  [6,]    0    1    0
#>  [7,]    0    0    1
#>  [8,]    0    0    1
#>  [9,]    0    0    1
#> 
#> $inflation
#> [1] 4.114885
```

And we can find the linear reliability and the associated factor scores.

``` r
reliability(object, type = "linear")
#> $reliability
#> [1] 0.7962199
#> 
#> $weights
#>          [,1]        [,2]        [,3]
#> x1 0.41047841 0.027620586 0.043609095
#> x2 0.11002004 0.007403113 0.011688494
#> x3 0.19469026 0.013100467 0.020683831
#> x4 0.04085748 0.296314299 0.009867704
#> x5 0.03782595 0.274328456 0.009135543
#> x6 0.03943032 0.285964011 0.009523024
#> x7 0.02995246 0.004581766 0.134308145
#> x8 0.05793031 0.008861480 0.259762053
#> x9 0.04574184 0.006997034 0.205108390
#> 
#> $inflation
#> [1] 4.907251
```

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/reliable/issues). Create a
pull requests to contribute. This project follows a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.md).

The logo is adapted from [AutoDraw](https://www.autodraw.com/) and
licensed under [CC
BY 4.0](https://creativecommons.org/licenses/by/4.0/).
