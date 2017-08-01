`dataRetrieval`
=============
Retrieval functions for USGS and EPA hydrologic and water quality data. For tutorial information, see:

[https://owi.usgs.gov/R/dataRetrieval.html](https://owi.usgs.gov/R/dataRetrieval.html)

## Package Status


|Linux|Windows| Test Coverage | USGS Status |
|----------|------------|------------|------------|
| [![travis](https://travis-ci.org/USGS-R/dataRetrieval.svg?branch=master)](https://travis-ci.org/USGS-R/dataRetrieval)|[![Build status](https://ci.appveyor.com/api/projects/status/a2kogyfplo3valdg?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/dataRetrieval)| [![Coverage Status](https://coveralls.io/repos/github/USGS-R/dataRetrieval/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/dataRetrieval?branch=master)|[![status](https://img.shields.io/badge/USGS-Core-green.svg)](https://owi.usgs.gov/R/packages.html#core)|

### Current CRAN information:

|Version|Monthly Downloads|Total Downloads|
|----------|------------|------------|
|[![CRAN version](http://www.r-pkg.org/badges/version/dataRetrieval)](https://CRAN.R-project.org/package=dataRetrieval)|[![](http://cranlogs.r-pkg.org/badges/dataRetrieval)](https://CRAN.R-project.org/package=dataRetrieval)|[![](http://cranlogs.r-pkg.org/badges/grand-total/dataRetrieval)](https://CRAN.R-project.org/package=dataRetrieval)|

### Research software impact:
[![Research software impact](http://depsy.org/api/package/cran/dataRetrieval/badge.svg)](http://depsy.org/package/r/dataRetrieval)

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/dataRetrieval/issues](https://github.com/USGS-R/dataRetrieval/issues)

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

### Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/dataRetrieval/blob/master/CONDUCT.md) for more information.

### Package Support

The Water Mission Area of the USGS supports the development and maintenance of `dataRetrieval` through FY18, and most likely further into the future. Resources are available primarily for maintenance and responding to user questions. Priorities on the development of new features are determined by the `dataRetrieval` development team.

![USGS](http://usgs-r.github.io/images/usgs.png)

## Installation

To install the dataRetrieval package, you must be using R 3.0 or greater and run the following command:

```R
install.packages("dataRetrieval")
```

To get inter-CRAN release updates, use the command:
```r
install.packages("dataRetrieval",repos="https://owi.usgs.gov/R")
```

To get cutting-edge changes, install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("USGS-R/dataRetrieval")
```


## Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)

