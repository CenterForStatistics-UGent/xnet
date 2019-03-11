# xnet R package

[![Travis build status](https://travis-ci.org/CenterForStatistics-UGent/xnet.svg?branch=master)](https://travis-ci.org/CenterForStatistics-UGent/xnet)

[![Coverage status](https://codecov.io/gh/CenterForStatistics-UGent/xnet/branch/master/graph/badge.svg)](https://codecov.io/github/CenterForStatistics-UGent/xnet?branch=master)

This is the github repo for the xnet package in R. The package implements
two-step kernel ridge regression, and a set of crossvalidation methods 
described by Michiel Stock et al. (  https://doi.org/10.1093/bib/bby095 )

Please note this is work in progress. All suggestions/issues/feature requests are welcomed.

## Installation

The package can be installed in R using the following code:

remotes::install_github("CenterForStatistics-UGent/xnet")

To install the dev version, use:

remotes::install_github("CenterForStatistics-UGent/xnet", ref = "devel")
