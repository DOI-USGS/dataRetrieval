dataRetrieval
=============

R package source for data retrieval specifically for the EGRET R package:

Exploration and Graphics for RivEr Time-series (EGRET)
=============

Exploration and Graphics for RivEr Trends (EGRET): 
An R-package for the analysis of long-term changes in water quality and streamflow, 
including the water-quality method Weighted Regressions on Time, Discharge, and Season (WRTDS)

Overview of EGRET:  The following are 4 major features of EGRET.

1.  It is designed to obtain its water quality sample data, streamflow data, and metadata directly from the USGS NWIS (National Water Information System), but it allows for user-supplied text files as inputs.  The program is designed to ingest the data directly into R and structure them into file structures suited to the analysis.  For those familiar with WRTDS_4c, the text file inputs used in that system will also work in EGRET.

2.  It has all of the existing WRTDS functionality - computing concentrations, fluxes, flow normalized versions of those, trends in those and graphics to show results and to explore the behavior of the data (by season, by flow class...). Many graph and table outputs are possible and all are clearly labeled and suitable for presentation or publication.  It is designed for both batch and interactive processing.  It is very much oriented to graphics and should be thought of as an exploratory tool.  It is intended for use with data sets of about 200 or more samples, over a time period of about 20 or more years.  Some testing with smaller data sets has been done, and no significant problems have been identified in cases with sample sizes slightly larger than 100 but extensive testing with smaller data sets has not taken place yet.

3. It has additional statistics and graphics to help evaluate the possibility that flux estimates may be biased (it is known that in certain cases, regression-based methods can produce severely biased flux estimates).  It can also accept results from other estimation methods like LOADEST and produce the same types of graphics and statistics for them (this part is not yet documented).

4. It has a streamflow history component, not related to water quality, that is not a part of WRTDS, but uses some similar concepts and shares some of the basic software and data structures.  This component, called flowHistory provides a variety of table and graphical outputs looking only at flow statistics (like annual mean, annual 7-day low flow, annual 1-day maximum, or seasonal versions of these) all based on time-series smoothing.  It is designed to be used in long-term studies of streamflow change (associated with climate or land use or water use change) and works best for daily streamflow data sets of 50 years or longer.   It is put together with the WRTDS method because it uses the same data retrieval infrastructure as WRTDS and the same data structure.  

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

	install.packages(c("zoo","XML","RCurl))
	install.packages("dataRetrieval", repos="http://usgs-r.github.com")
	

Background Information
----------------------

WRTDS is a method of analysis for long-term surface water quality data.  It is intended for use with data sets of more than about 200 observations of water quality over a time span of about 20 years or more.  There also needs to be a daily time series of streamflow data covering the entire period of the water quality data collection.  The method can be used with somewhat smaller data sets, but it will not work with less than 100 water quality observations.  The best way to learn about the WRTDS approach and to see examples of its application to multiple large data sets is to read two journal articles.  Both are available, for free, from the journals in which they were published.

The first relates to nitrate and total phosphorus data for 9 rivers draining to Chesapeake Bay:

[Chesapeake Bay](http://onlinelibrary.wiley.com/doi/10.1111/j.1752-1688.2010.00482.x/full)

The second is an application to nitrate data for 8 monitoring sites on the Mississippi River or its major tributaries:

[Mississippi River](http://pubs.acs.org/doi/abs/10.1021/es201221s)

The manual available here assumes that the user understands the concepts underlying WRTDS.  Thus, reading at least the first of these papers is necessary to understanding the manual.

[dataRetrieval vignette](https://github.com/USGS-R/EGRET/raw/Documentation/dataRetrieval.pdf) (pdf)

[EGRET User Guide](https://github.com/USGS-R/EGRET/raw/Documentation/EGRETmanual_5.pdf) (pdf)

[EGRET vignette](https://github.com/USGS-R/EGRET/raw/Documentation/EGRET.pdf) (Word Document)



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

