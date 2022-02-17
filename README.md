# dataRetrieval <img src="man/figures/logo.png" alt="dataRetrieval" style="width:150px;height:auto;" align="right" />
 
Retrieval functions for USGS and EPA hydrologic and water quality data.

For complete tutorial information, see:

<https://usgs-r.github.io/dataRetrieval>

<https://owi.usgs.gov/R/dataRetrieval.html>

# Sample Workflow

## USGS

``` r
library(dataRetrieval)
# Choptank River near Greensboro, MD
siteNumber <- "01491000" 
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyData <- readNWISdv(siteNumber,parameterCd,
                      "1980-01-01","2010-01-01")

# Sample data Nitrate:
parameterCd <- "00618"
qwData <- readNWISqw(siteNumber,parameterCd,
                      "1980-01-01","2010-01-01")

pCode <- readNWISpCode(parameterCd)
```

## Water Quality Portal

``` r
specificCond <- readWQPqw(siteNumbers = 'WIDNR_WQX-10032762',
                          parameterCd = 'Specific conductance',
                          startDate = '2011-05-01',
                          endDate = '2011-09-30')
```

## Network Linked Data Index

``` r
features <- findNLDI(nwis = "01491000",
                     nav = "UT",
                     find = c('basin', 'wqp'))
```

# Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
<https://github.com/USGS-R/dataRetrieval/issues>

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter
Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

# Citing dataRetrieval

``` r
citation(package = "dataRetrieval")
#> 
#> To cite dataRetrieval in publications, please use:
#> 
#>   De Cicco, L.A., Hirsch, R.M., Lorenz, D., Watkins, W.D., 2018,
#>   dataRetrieval: R packages for discovering and retrieving water data
#>   available from Federal hydrologic web services, doi:10.5066/P9X4L3GE
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     author = {Laura A. {De Cicco} and David Lorenz and Robert M. Hirsch and William Watkins},
#>     title = {dataRetrieval: R packages for discovering and retrieving water data available from U.S. federal hydrologic web services},
#>     publisher = {U.S. Geological Survey},
#>     address = {Reston, VA},
#>     version = {2.7.6},
#>     institution = {U.S. Geological Survey},
#>     year = {2018},
#>     doi = {10.5066/P9X4L3GE},
#>     url = {https://code.usgs.gov/water/dataRetrieval},
#>   }
```

# Package Support

The Water Mission Area of the USGS supports the development and
maintenance of `dataRetrieval`, and most likely further into the future.
Resources are available primarily for maintenance and responding to user
questions. Priorities on the development of new features are determined
by the `dataRetrieval` development team.

[![USGS](http://usgs-r.github.io/images/usgs.png)](https://www.usgs.gov/)

# Installation

To install the dataRetrieval package, you must be using R 3.0 or greater
and run the following command:

``` r
install.packages("dataRetrieval")
```

To get cutting-edge changes, install from GitHub using the `remotes`
packages:

``` r
library(remotes)
install_github("USGS-R/dataRetrieval", 
               build_opts = c("--no-resave-data", "--no-manual"),
               build_vignettes = TRUE)
```

# Disclaimer

This software is in the public domain because it contains materials that
originally came from the U.S. Geological Survey, an agency of the United
States Department of Interior. For more information, see the [official
USGS copyright
policy](https://www2.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological
Survey (USGS), no warranty, expressed or implied, is made by the USGS or
the U.S. Government as to the accuracy and functioning of the program
and related program material nor shall the fact of distribution
constitute any such warranty, and no responsibility is assumed by the
USGS in connection therewith.

This software is provided “AS IS.”

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
