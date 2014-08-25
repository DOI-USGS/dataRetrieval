dataRetrieval
=============

R package source for data retrieval specifically for the EGRET R package:

Please visit the EGRET wiki for more information:
[EGRET Wiki](https://github.com/USGS-R/EGRET/wiki)

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This software is provided "AS IS".

Subscribe
---------
Please email questions, comments, and feedback to: 
egret_comments@usgs.gov

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to egret_comments@usgs.gov.

Package Installation
---------------------------------
While the dataRetreival package is in development (and not on CRAN), the zoo package must first be manually installed. To install the dataRetrieval package, you must be using R 3.0 or greater and run the following commands:

	install.packages("dataRetrieval", 
	repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
	dependencies=TRUE,
	type="both")


Version updates
---------------
* Version 1.2.2:        July 10, 2013

	* Added getDataAvailability function to find measured parameters and period of record information for a requested station.
	* Added constructNWISURL function to get the URL that is used to retrieve the data.
	* Added getSampleSTORET function to get STORET data directly in Sample dataframe form.
	* Fixed a small leap year bug by changing day of year by making Feb. 29 always 59, and March 1st always 60 (even in non-leap years).



* Version 1.2.1:	Feburary 20, 2013

	* Improved documentation, especially example functions.
	* Improved vignette for a more complete walk-through.
	* Expanded the capabilities to retrieve raw data from the web services.
	* Added STORET data retrievals in getWQPData function

* Version 1.2.0:	October 13, 2012

	* Fixed a bug that caused problems if not explicitly defining Daily and Sample in mergeReport().
	* Updating documentation (in progress)

Sample Workflow
---------------

Load data from web services:

	library(dataRetrieval)
	Daily <- getDVData("06934500","00060","1979-10-01","2010-09-30")
	Sample <-getSampleData("06934500","00631","1970-10-01","2011-09-30")
	INFO <-getMetaData("06934500","00631", interactive=FALSE)
	Sample <-mergeReport(Daily, Sample)

