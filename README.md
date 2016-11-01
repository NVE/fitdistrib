[![Travis-CI Build Status](https://travis-ci.org/NVE/fitdistrib.svg?branch=master)](https://travis-ci.org/NVE/fitdistrib)
[![codecov](https://codecov.io/github/NVE/fitdistrib/branch/master/graphs/badge.svg)](https://codecov.io/gh/NVE/fitdistrib) 

`Warning: This package is still under development and is not fully mature`

# Package: fitdistrib

R package for fitting probability distributions to flood data at NVE

# Installation

On RStudio, install package devtools and curl:

> install.packages("devtools")
> install.packages("curl")

Afterwards, run these commands:

> library(devtools)

> install_github("fbaffie/fitdistrib")

# Example

The following example fit the generalized extreme value distribution with the MLE method 
using a randomly generated dataset based on the same distribution

> library(fitdistrib)

> gev_mle(evd::rgev(10000, loc=0, scale=1, shape=0))
