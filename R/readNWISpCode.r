#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code or codes.
#' This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd character of USGS parameter codes (or multiple parameter codes).  These are 5 digit number codes
#' that can be found here: \url{http://help.waterdata.usgs.gov/codes-and-parameters/parameters}. To get a 
#' complete list of all current parameter codes in the USGS, use "all" as the input.
#' @keywords data import USGS web service
#' @return parameterData data frame with all information from the USGS about the particular parameter.
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
#' @export
#' @examples
#' paramINFO <- readNWISpCode(c('01075','00060','00931'))
readNWISpCode <- function(parameterCd){
 
  if(any(parameterCd == "all")){
    fullURL <- "http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&format=rdb&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
    fullPcodeDownload <- importRDB1(fullURL)
    return(fullPcodeDownload)
    
  } else {
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    
    parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]
  
    if(nrow(parameterData) != length(parameterCd)){
      
      if(length(parameterCd) == 1){
        url <- paste0("http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search",
                     "&pm_search=", parameterCd,
                     "&format=rdb", "&show=parameter_group_nm",
                     "&show=parameter_nm", "&show=casrn",
                     "&show=srsname", "&show=parameter_units")
        newData <- importRDB1(url,asDateTime = FALSE)
      } else {
        
        fullURL <- "http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&format=rdb&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
        fullPcodeDownload <- importRDB1(fullURL)
        newData <- fullPcodeDownload[fullPcodeDownload$parameter_cd %in% parameterCd,]
        
      }
      
      if(nrow(newData) != length(parameterCd)){
        badPcode <- parameterCd[!(parameterCd %in% newData$parameter_cd)]
        warning("The following pCodes seem mistyped, and no information was returned: ",paste(badPcode,collapse=","))
      }
      
      return(newData)
      
    } else {
      return(parameterData)
    }
  }
  
}
