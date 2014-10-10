#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code.
#' This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd vector of USGS parameter codes.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return parameterData dataframe with all information from the USGS about the particular parameter (usually code, name, short name, units, and CAS registry numbers)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' paramINFO <- getNWISPcodeInfo(c('01075','00060','00931'))
getNWISPcodeInfo <- function(parameterCd,interactive=TRUE){
  parameterCd <- formatCheckParameterCd(parameterCd, interactive=interactive)
  
  parameterCdFile <- parameterCdFile
  
  parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]

  return(parameterData)
}
