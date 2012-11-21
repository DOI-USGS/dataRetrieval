#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code.
#' This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return parameterData dataframe with all information from the USGS about the particular parameter (usually code, name, short name, units, and CAS registry numbers)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' getParameterInfo('01075')
#' getParameterInfo('00931',interactive=FALSE)
getParameterInfo <- function(parameterCd,interactive=TRUE){
  parameterCd <- formatCheckParameterCd(parameterCd, interactive=interactive)
  urlParameterCd <- paste("http://nwis.waterdata.usgs.gov/nwis/pmcodes/?radio_pm_search=pm_search&pm_search=",parameterCd,"&casrn_search=&srsname_search=&format=rdb_file&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units",sep="")
  parameterCdFile <- read.delim(  
    urlParameterCd, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  parameterData <- parameterCdFile[2,]
  return(parameterData)
}