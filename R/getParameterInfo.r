#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code.
#' This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd vector USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return parameterData dataframe with all information from the USGS about the particular parameter (usually code, name, short name, units, and CAS registry numbers)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' paramINFO <- getParameterInfo(c('01075','00060','00931'))
getParameterInfo <- function(parameterCd,interactive=TRUE){
  parameterCd <- formatCheckParameterCd(parameterCd, interactive=interactive)

  urlParameterCd <- "http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=rdb&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
  
  parameterCdFile <- read.delim(  
    urlParameterCd, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  dataType <- parameterCdFile[1,]
  parameterCdFile <- parameterCdFile[-1,]
  
  parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]

  return(parameterData)
}
