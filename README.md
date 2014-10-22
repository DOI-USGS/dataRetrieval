`dataRetrieval`
=============
Linux: [![travis](https://travis-ci.org/USGS-R/dataRetrieval.svg?branch=master)](https://travis-ci.org/USGS-R/dataRetrieval)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/msanha92b500grr7?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/dataretrieval-787)



R package source for data retrieval specifically for the EGRET R package:

Please visit the EGRET wiki for more information:
[EGRET Wiki](https://github.com/USGS-R/EGRET/wiki)

`dataRetrieval`
=============

R package for data retrieval of water quality and hydrology data. 

## Function Overview


|Information Source | Site Query | Meta Data | Data |
| -------------| -------------| ------------- |:-------------|
|NWIS | `getNWISSites` | `getNWISSiteInfo`| `getNWISData` |
| | `getNWISDataAvailability` |  | `getNWISdvData` |
| | | `getNWISPcodeInfo` | `getNWISqwData`|
| | | | `getNWISunitData` |
| Water Quality Portal | `getWQPSites` |  | `getWQPData` |
| | | | `getWQPqwData` |



Moving `EGRET` specific functions to `EGRET`:


|Information Source | Site Query | Meta Data | Data |
| -------------| -------------| ------------- |:-------------|
|NWIS | | `getNWISInfo`* | `getNWISSample`* |
| |  |  | `getNWISDaily`* |
| Water Quality Portal |  | `getWQPInfo`* | `getWQPSample`* |
| User-supplied files | | `getUserInfo`* | `getUserDaily`* |
| | | | `getUserSample`* |



##Subscribe
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.

##Package Installation
To install the dataRetrieval package, you must be using R 3.0 or greater and run the following command:

	install.packages("dataRetrieval", 
	  repos=c("http://usgs-r.github.com",
	           "http://cran.us.r-project.org"),
	  dependencies=TRUE)

##Version updates
---------------
###dataRetrieval 1.4.0-in developement
Changed naming convention:



|Original Name | New Name |
| ------------- |:-------------|
|getDVData | getNWISDaily |
|getSampleData  |     getNWISSample |
|getSTORETData* | getWQPSample |
|getSampleDataFromFile | getUserSample |
|getDailyDataFromFile | getUserDaily |
|getMetaData | splits into getNWISInfo and getUserInfo |
|getSiteFileData | getNWISSiteInfo |
|getParameterInfo | getNWISPcodeInfo |
|getDataAvailability | getNWISDataAvailability |
|'retrieve' functions | 'get' |

*Changed WaterML2 rbind fill from plyr function to dplyr. Removed plyr import, added dplyr.
*Removing WaterML2 until fixed.


###dataRetrieval 1.3.3

* Updated getNWISSiteInfo to retrieve multiple site file datasets at once using a vector of siteNumbers as input argument.
* Updated error-handling for Web service calls. More information is returned when errors happen
* Added some basic processing to Water Quality Portal raw data retrievals. Date columns are returned as Date objects, value columns are numeric, and a column is created from the date/time/timezone columns that is POSIXct.
* Added very generalized NWIS and WQP retrieval functions (getNWISData, getNWISSites, getGeneralWQPData, and getWQPSites) which allow the user to use any argument available on the Web service platform.


###dataRetrieval 1.3.2

* Deprecated getQWData, updated getWQPData to take either parameter code or characteristic name.
* Changed the name of raw data retrievals to: getNWISqwData, getNWISunitData, getNWISdvData, and getWQPqwData (from: getNWISqwData, retrieveUnitNWISData, retrieveNWISData, getRawQWData)
* Added NA warning to getDVData function
* Updated mergeReport to allow for Sample data with different measurements taken on the same day


##Sample Workflow

Load data from web services:

	library(dataRetrieval)
	Daily <- getNWISDaily("06934500","00060","1979-10-01","2010-09-30")
	Sample <-getNWISSample("06934500","00631","1970-10-01","2011-09-30")
	INFO <-getNWISInfo("06934500","00631", interactive=FALSE)
	Sample <-mergeReport(Daily, Sample)


##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
