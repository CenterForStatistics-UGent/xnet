# xnet R package

[![CRAN status](https://www.r-pkg.org/badges/version-last-release/xnet)](https://cran.r-project.org/package=xnet) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/xnet)](https://cran.r-project.org/package=xnet)

| Main   | Devel |
| :----: | :---: |
| [![Travis build status](https://travis-ci.com/CenterForStatistics-UGent/xnet.svg?branch=main)](https://travis-ci.com/CenterForStatistics-UGent/xnet) | [![Travis devel-build status](https://travis-ci.com/CenterForStatistics-UGent/xnet.svg?branch=devel)](https://travis-ci.com/CenterForStatistics-UGent/xnet) |
| [![Coverage status](https://codecov.io/gh/CenterForStatistics-UGent/xnet/branch/master/graph/badge.svg)](https://codecov.io/github/CenterForStatistics-UGent/xnet?branch=master) | [![Coverage status](https://codecov.io/gh/CenterForStatistics-UGent/xnet/branch/devel/graph/badge.svg)](https://codecov.io/github/CenterForStatistics-UGent/xnet/branch/devel) |



This is the github repo for the xnet package in R. The package implements
two-step kernel ridge regression, and a set of crossvalidation methods 
described by Michiel Stock et al. (  https://doi.org/10.1093/bib/bby095 )

Please note this is work in progress. All suggestions/issues/feature requests are welcomed.

## Installation

The package can be installed in R using the following code:

remotes::install_github("CenterForStatistics-UGent/xnet")

To install the dev version, use:

remotes::install_github("CenterForStatistics-UGent/xnet", ref = "devel")
