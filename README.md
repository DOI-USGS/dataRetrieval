`dataRetrieval`
=============
Retrieval functions for USGS and EPA hydrologic and water quality data.

Linux: [![travis](https://travis-ci.org/USGS-R/dataRetrieval.svg?branch=master)](https://travis-ci.org/USGS-R/dataRetrieval)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/msanha92b500grr7?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/dataretrieval-787)

CRAN statistics:
[![](http://cranlogs.r-pkg.org/badges/dataRetrieval)](https://cran.r-project.org/package=dataRetrieval)

Research software impact:
[![Research software impact](http://depsy.org/api/package/cran/dataRetrieval/badge.svg)](http://depsy.org/package/r/dataRetrieval)

[![Coverage Status](https://coveralls.io/repos/USGS-R/dataRetrieval/badge.svg?branch=master&service=github)](https://coveralls.io/github/USGS-R/dataRetrieval?branch=master)

##Package Installation
To install the dataRetrieval package, you must be using R 3.0 or greater and run the following command:

```R
install.packages("dataRetrieval")
```

To get inter-CRAN release updates, use the command:
```r
install.packages("dataRetrieval",repos="http://owi.usgs.gov/R")
```

## Function Overview

Web service retrieval functions:

|Function | Inputs | Description | 
| -------------| ------------------------|:-------------|
|`readNWISdata` | `...`, `service` | NWIS data using user-specified queries |
|`readNWISdv` | `Common 3`, `parameterCd`, `statCd` | NWIS daily data with `Common` query |
|`readNWISqw` | `Common 3`, `parameterCd`, `expanded`,`reshape`,`tz` | NWIS water quality data with `Common` query |
|`readNWISuv` | `Common 3`, `parameterCd`,`tz` | NWIS instantaneous data with `Common` query |
|`readNWISpCode` | `parameterCd` | NWIS parameter code information |
|`readNWISgwl` | `Common 3` | NWIS groundwater level data with `Common` query |
|`readNWISpeak` | `Common 3`,`asDateTime` | NWIS peak flow data with `Common` query |
|`readNWISmeas` | `Common 3`, `tz` | NWIS surface-water measurement data with `Common` query |
|`readNWISrating` | `siteNumber`, `type` | NWIS rating table for an active USGS streamgage |
|`readNWISsite` | `siteNumber` | NWIS site information |
|`whatNWISsites` | `...` | NWIS site search using user-specified queries |
|`whatNWISdata` | `siteNumber`, `service` | NWIS data availability, including period of record and count |
|`readWQPdata` | `...` | WQP data using user-specified queries |
|`readWQPqw` | `Common 3`, `parameterCd`**,`tz` | WQP data with `Common 3` query and either parameter code or characteristic name|
|`whatWQPsites` | `...` | WQP site search using user-specified queries |

* `Common 3` = siteNumber, startDate, endDate
* `...` = user-defined arguments.

**`parameterCd` in WQP function can be either USGS parameter codes, or EPA characteristic names

##Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:

[https://github.com/USGS-R/dataRetrieval/issues](https://github.com/USGS-R/dataRetrieval/issues)


##Subscribe
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)

