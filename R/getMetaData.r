#' Import Metadata for USGS Data
#'
#' Populates INFO data frame for WRTDS study.  If either station number or parameter code supplied, imports data about a particular USGS site from NWIS web service. 
#' This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' If either station number or parameter code is not supplied, the user will be asked to input data.
#' Additionally, the user will be asked for:
#' staAbbrev - station abbreviation, will be used in naming output files and for structuring batch jobs
#' constitAbbrev - constitute abbreviation
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @return INFO dataframe with agency, site, dateTime, value, and code columns
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 05114000 and temperature, no interaction with user
#' INFO <- getMetaData('05114000','00010')
getMetaData <- function(siteNumber="", parameterCd="",interactive=TRUE){
  if (nzchar(siteNumber)){
    INFO <- getSiteFileData(siteNumber,interactive=interactive)
  } else {
    INFO <- as.data.frame(matrix(ncol = 2, nrow = 1))
    names(INFO) <- c('site.no', 'shortName')    
  }
  INFO <- populateSiteINFO(INFO, siteNumber, interactive=interactive)
  
  if (nzchar(parameterCd)){
    parameterData <- getParameterInfo(parameterCd,interactive=interactive)
    INFO$param.nm <- parameterData$parameter_nm
    INFO$param.units <- parameterData$parameter_units
    INFO$paramShortName <- parameterData$srsname
    INFO$paramNumber <- parameterData$parameter_cd
  } 
  
  INFO <- populateParameterINFO(parameterCd, INFO, interactive=interactive)
  INFO$paStart <- 10
  INFO$paLong <- 12
  
  return(INFO)
}
