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

# Introduction

:warning: USGS discrete water-quality data availability and format are
changing. Beginning March 11, 2024 USGS data obtained from legacy
services will not include new USGS data or recent updates to existing
data. To view the status of changes in data availability and code
functionality, visit:
<https://doi-usgs.github.io/dataRetrieval/articles/Status.html>

If you have additional questions about these changes, email
<CompTools@usgs.gov>.

# What would you like to do?

1.  Get instantaneous USGS discharge data. Start here: `?readNWISuv`

2.  Get daily USGS discharge data. Start here: `?readNWISdv`

3.  Get USGS groundwater data. Start here: `?readNWISgwl`

4.  Get discrete water quality data. Start here: `?readWQPdata`

5.  Discover USGS data (not including discrete water quality data).
    Start here: `?whatNWISdata`

6.  Find Hydro Network-Linked Data Index (NLDI) data. Start here:
    `?findNLDI`

For additional tutorials, see:

[Basic
Tutorial](https://doi-usgs.github.io/dataRetrieval/articles/tutorial.html)

[dataRetrieval Introduction
1](https://rconnect.usgs.gov/NMC_dataRetrieval_1)

[dataRetrieval Introduction
2](https://rconnect.usgs.gov/NMC_dataRetrieval_2)

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
#>   2025, dataRetrieval: R packages for discovering and retrieving water
#>   data available from Federal hydrologic web services, v.2.7.18,
#>   doi:10.5066/P9X4L3GE
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     author = {Laura DeCicco and Robert Hirsch and David Lorenz and Jordan Read and Jordan Walker and Lindsay Platt and David Watkins and David Blodgett and Mike Johnson and Aliesha Krall and Lee Stanish},
#>     title = {dataRetrieval: R packages for discovering and retrieving water data available from U.S. federal hydrologic web services},
#>     publisher = {U.S. Geological Survey},
#>     address = {Reston, VA},
#>     version = {2.7.18},
#>     institution = {U.S. Geological Survey},
#>     year = {2025},
#>     doi = {10.5066/P9X4L3GE},
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
#> U.S. Geological Survey (2025). _National Water Information System data
#> available on the World Wide Web (USGS Water Data for the Nation)_.
#> doi:10.5066/F7P55KJN <https://doi.org/10.5066/F7P55KJN>, Accessed Feb
#> 25, 2025,
#> <https://waterservices.usgs.gov/nwis/dv/?site=09010500&format=waterml%2C1.1&ParameterCd=00060&StatCd=00003&startDT=1851-01-01>.
print(NWIScitation, style = "Bibtex")
#> @Manual{,
#>   title = {National Water Information System data available on the World Wide Web (USGS Water Data for the Nation)},
#>   author = {{U.S. Geological Survey}},
#>   doi = {10.5066/F7P55KJN},
#>   note = {Accessed Feb 25, 2025},
#>   year = {2025},
#>   url = {https://waterservices.usgs.gov/nwis/dv/?site=09010500&format=waterml%2C1.1&ParameterCd=00060&StatCd=00003&startDT=1851-01-01},
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

WQPcitation <- create_WQP_bib(SC)
WQPcitation
#> National Water Quality Monitoring Council (2025). _Water Quality
#> Portal_. doi:10.5066/P9QRKUVJ <https://doi.org/10.5066/P9QRKUVJ>,
#> Accessed Feb 25, 2025,
#> <https://www.waterqualitydata.us/data/Result/search?siteid=USGS-05288705&count=no&pCode=00300&mimeType=csv>.
print(WQPcitation, style = "Bibtex")
#> @Manual{,
#>   title = {Water Quality Portal},
#>   author = {{National Water Quality Monitoring Council}},
#>   doi = {10.5066/P9QRKUVJ},
#>   note = {Accessed Feb 25, 2025},
#>   year = {2025},
#>   url = {https://www.waterqualitydata.us/data/Result/search?siteid=USGS-05288705&count=no&pCode=00300&mimeType=csv},
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
