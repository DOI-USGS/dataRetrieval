#' Retrieval functions for USGS and EPA data
#'
#' \tabular{ll}{
#' Package: \tab dataRetrieval\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.1\cr
#' Date: \tab 2014-11-24\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Retrieval functions for USGS and EPA hydrologic and water quality data. 
#' 
#' Please see \url{http://pubs.usgs.gov/tm/04/a10/} for more information.
#'
#' @name dataRetrieval-package
#' @docType package
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}, Laura De Cicco \email{ldecicco@@usgs.gov}
#' @keywords USGS, web services
NULL


#' List of USGS parameter codes
#'
#' Complete list of USGS parameter codes as of November 7, 2014. The data was pulled from
#' \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&
#' format=rdb&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units}
#'
#' @name parameterCdFile
#' @docType data
#' @keywords USGS parameterCd
NULL

#' Data to convert USGS parameter code to characteristic names
#'
#' Data pulled from Water Quality Portal on November 25, 2014. The data was pulled from 
#' \url{http://www.waterqualitydata.us/public_srsnames?mimeType=json}.
#'
#' @name pCodeToName
#' @docType data
#' @keywords USGS parameterCd
NULL
