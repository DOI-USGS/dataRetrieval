# dataRetrieval <img src="man/figures/hex_logo.png" class="logo"  alt="dataRetrieval" style="width:90px;height:auto;" align="right" />

[![CRAN
version](http://www.r-pkg.org/badges/version/dataRetrieval)](https://cran.r-project.org/package=EGRET)
[![](http://cranlogs.r-pkg.org/badges/dataRetrieval)](https://cran.r-project.org/package=dataRetrieval)
[![](http://cranlogs.r-pkg.org/badges/grand-total/dataRetrieval)](https://cran.r-project.org/package=dataRetrieval)

The `dataRetrieval` package was created to simplify the process of
loading hydrologic data into the R environment. It is designed to
retrieve the major data types of U.S. Geological Survey (USGS) hydrology
data that are available on the Web, as well as data from the Water
Quality Portal (WQP), which currently houses water quality data from the
Environmental Protection Agency (EPA), U.S. Department of Agriculture
(USDA), and USGS. Direct USGS data is obtained from a service called the
National Water Information System (NWIS).

For complete tutorial information, see:

<https://doi-usgs.github.io/dataRetrieval/>

<https://waterdata.usgs.gov/blog/dataretrieval/>

[dataRetrieval Introduction
1](https://rconnect.usgs.gov/NMC_dataRetrieval_1)

[dataRetrieval Introduction
2](https://rconnect.usgs.gov/NMC_dataRetrieval_2)

# Sample Workflow

## USGS

``` r
library(dataRetrieval)
# Choptank River near Greensboro, MD
siteNumber <- "01491000"
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Raw daily data:
rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "1980-01-01", "2010-01-01"
)

pCode <- readNWISpCode(parameterCd)
```

## Water Quality Portal

``` r
specificCond <- readWQPqw(
  siteNumbers = "WIDNR_WQX-10032762",
  parameterCd = "Specific conductance",
  startDate = "2011-05-01",
  endDate = "2011-09-30"
)
```

## Network Linked Data Index

``` r
features <- findNLDI(
  nwis = "01491000",
  nav = "UT",
  find = c("basin", "wqp")
)
```

# Installation of dataRetrieval

To install the `dataRetrieval` package, you must be using R 3.0 or
greater and run the following command:

``` r
install.packages("dataRetrieval")
```

To get cutting-edge changes, install from GitHub using the `remotes`
packages:

``` r
library(remotes)
install_github("DOI-USGS/dataRetrieval",
               build_vignettes = TRUE, 
               build_opts = c("--no-resave-data",
                              "--no-manual"))
```

# Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
<https://github.com/DOI-USGS/dataRetrieval/issues>

# Citations

## Citing the dataRetrieval package

``` r
citation(package = "dataRetrieval")
#> To cite dataRetrieval in publications, please use:
#> 
#>   De Cicco, L.A., Hirsch, R.M., Lorenz, D., Watkins, W.D., Johnson, M.,
#>   2023, dataRetrieval: R packages for discovering and retrieving water
#>   data available from Federal hydrologic web services, v.2.7.13,
#>   doi:10.5066/P9X4L3GE
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     author = {Laura DeCicco and Robert Hirsch and David Lorenz and David Watkins and Mike Johnson},
#>     title = {dataRetrieval: R packages for discovering and retrieving water data available from U.S. federal hydrologic web services},
#>     publisher = {U.S. Geological Survey},
#>     address = {Reston, VA},
#>     version = {2.7.13},
#>     institution = {U.S. Geological Survey},
#>     year = {2023},
#>     doi = {10.5066/P9X4L3GE},
#>     url = {https://code.usgs.gov/water/dataRetrieval},
#>   }
```

## Citing NWIS data

U.S. Geological Survey, 2023, National Water Information System data
available on the World Wide Web (USGS Water Data for the Nation),
accessed \[April 26, 2023\], at <http://waterdata.usgs.gov/nwis/>.
<http://dx.doi.org/10.5066/F7P55KJN>

This can be done using the `create_NWIS_bib` function:

``` r

dv <- readNWISdv("09010500", "00060")

NWIScitation <- create_NWIS_bib(dv)

NWIScitation
#> U.S. Geological Survey (2024). _National Water Information System data
#> available on the World Wide Web (USGS Water Data for the Nation)_.
#> doi:10.5066/F7P55KJN <https://doi.org/10.5066/F7P55KJN>, Accessed Jan
#> 26, 2024,
#> <https://waterservices.usgs.gov/nwis/dv/?site=09010500&format=waterml,1.1&ParameterCd=00060&StatCd=00003&startDT=1851-01-01>.
print(NWIScitation, style = "Bibtex")
#> @Manual{,
#>   title = {National Water Information System data available on the World Wide Web (USGS Water Data for the Nation)},
#>   author = {{U.S. Geological Survey}},
#>   doi = {10.5066/F7P55KJN},
#>   note = {Accessed Jan 26, 2024},
#>   year = {2024},
#>   url = {https://waterservices.usgs.gov/nwis/dv/?site=09010500&format=waterml,1.1&ParameterCd=00060&StatCd=00003&startDT=1851-01-01},
#> }
```

## Citing WQP data

Citations for specific datasets should use this format:

National Water Quality Monitoring Council, YYYY, Water Quality Portal,
accessed mm, dd, yyyy, hyperlink_for_query,
<https://doi.org/10.5066/P9QRKUVJ>.

This can be done using the `create_WQP_bib` function:

``` r
SC <- readWQPqw(siteNumbers = "USGS-05288705",
                parameterCd = "00300")

# Use "queryTime" and "url" attributes:
WQPcitation <- create_WQP_bib(SC)
WQPcitation
#> National Water Quality Monitoring Council (2024). _ Water Quality
#> Portal_. doi:10.5066/P9QRKUVJ <https://doi.org/10.5066/P9QRKUVJ>,
#> Accessed Jan 26, 2024,
#> <https://www.waterqualitydata.us/data/Result/search?siteid=USGS-05288705&pCode=00300&mimeType=tsv&zip=yes>.
print(WQPcitation, style = "Bibtex")
#> @Manual{,
#>   title = { Water Quality Portal},
#>   author = {{National Water Quality Monitoring Council}},
#>   doi = {10.5066/P9QRKUVJ},
#>   note = {Accessed Jan 26, 2024},
#>   year = {2024},
#>   url = {https://www.waterqualitydata.us/data/Result/search?siteid=USGS-05288705&pCode=00300&mimeType=tsv&zip=yes},
#> }
```

## Citing Water Quality Portal itself

General Water Quality Portal citations should use the following:

Water Quality Portal. Washington (DC): National Water Quality Monitoring
Council, United States Geological Survey (USGS), Environmental
Protection Agency (EPA); 2021. <https://doi.org/10.5066/P9QRKUVJ>.

# Package Support

The Water Mission Area of the USGS supports the development and
maintenance of `dataRetrieval`, and most likely further into the future.
Resources are available primarily for maintenance and responding to user
questions. Priorities on the development of new features are determined
by the `dataRetrieval` development team. This software was last released
with USGS record: IP-147158.

# Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
