#' Retrieval functions for USGS and EPA data
#'
#' \tabular{ll}{
#' Package: \tab dataRetrieval\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.1\cr
#' Date: \tab 2014-12-02\cr
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
#' @return parameterData data frame with information about USGS parameters.
#' 
#' \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   parameter_cd \tab character \tab 5-digit USGS parameter code \cr
#'   parameter_group_nm \tab character \tab USGS parameter group name\cr
#'   parameter_nm \tab character \tab USGS parameter name\cr
#'   casrn \tab character \tab Chemical Abstracts Service (CAS) Registry Number\cr
#'   srsname \tab character \tab Substance Registry Services Name\cr
#'   parameter_units \tab character \tab Parameter units\cr
#' }
#' 
#' @docType data
#' @keywords USGS parameterCd
#' @examples
#' parameterCdFile <- parameterCdFile
#' dischargeInfo <- parameterCdFile[parameterCdFile$parameter_cd=="00060",]
NULL

#' Data to convert USGS parameter code to characteristic names
#'
#' Data pulled from Water Quality Portal on November 25, 2014. The data was pulled from 
#' \url{http://www.waterqualitydata.us/public_srsnames?mimeType=json}.
#'
#' @name pCodeToName
#' @return pCodeToName data frame with information about USGS parameters and how they
#' relate to characteristic names (useful for WQP requests).
#' 
#' \tabular{lll}{
#'   Name \tab Type \tab Description\cr
#'   parm_cd \tab character \tab 5-digit USGS parameter code \cr
#'   description \tab character \tab Parameter description\cr
#'   characteristicname \tab character \tab Characteristic Name \cr
#'   measureunitcode \tab character \tab Parameter units\cr
#'   resultsamplefraction \tab character \tab Result sample fraction text\cr
#'   resulttemperaturebasis \tab character \tab Temperature basis information\cr
#'   resultstatisticalbasis \tab character \tab Statistical basis\cr
#'   resulttimebasis \tab character \tab Time basis\cr
#'   resultweightbasis \tab character \tab Weight basis\cr
#'   resultparticlesizebasis \tab character \tab Particle size basis\cr
#'   last_rev_dt \tab character \tab Latest revision of information\cr
#' }
#' @docType data
#' @keywords USGS parameterCd
#' @examples
#' pCodeToName <- pCodeToName
#' dischargeInfo <- pCodeToName[pCodeToName$parm_cd=="00060",]
NULL
