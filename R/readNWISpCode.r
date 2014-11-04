#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code.
#' This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd vector of USGS parameter codes.  This is usually an 5 digit number.
#' @keywords data import USGS web service
#' @return parameterData dataframe with all information from the USGS about the particular parameter (usually code, name, short name, units, and CAS registry numbers)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' paramINFO <- readNWISpCode(c('01075','00060','00931'))
readNWISpCode <- function(parameterCd){
 
  
  parameterCdFile <- parameterCdFile
  
  parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]

  if(nrow(parameterData) == 0){
    
    if(length(parameterCd) == 1){
      url <- paste0("http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search",
                   "&pm_search=", parameterCd,
                   "&format=rdb", "&show=parameter_group_nm",
                   "&show=parameter_nm", "&show=casrn",
                   "&show=srsname", "&show=parameter_units")
      parameterData <- importRDB1(url,asDateTime = FALSE)
    }
    #TODO: add else...
    
  }
  
  return(parameterData)
}
