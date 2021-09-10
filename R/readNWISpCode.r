#' USGS Parameter Data Retrieval
#'
#' Imports data from NWIS about meaured parameter based on user-supplied parameter code or codes.
#' This function gets the data from here: \url{https://nwis.waterdata.usgs.gov/nwis/pmcodes}
#'
#' @param parameterCd character of USGS parameter codes (or multiple parameter codes).  These are 5 digit number codes,
#' more information can be found here: \url{https://help.waterdata.usgs.gov/}. To get a 
#' complete list of all current parameter codes in the USGS, use "all" as the input.
#' @keywords data import USGS web service
#' @return parameterData data frame with the following information: 
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
#' @seealso \code{\link{importRDB1}}
#' @examples
#' 
#' paramINFO <- readNWISpCode(c('01075','00060','00931'))
#' paramINFO <- readNWISpCode(c('01075','00060','00931', NA))
readNWISpCode <- function(parameterCd){
 
  parameterCd.orig <- parameterCd
  parameterCd <- parameterCd[!is.na(parameterCd)]
  
  baseURL <- drURL("pCode", Access=pkg.env$access)
  
  fullURL <- appendDrURL(baseURL,radio_pm_search="param_group",
                         pm_group="All+--+include+all+parameter+groups",
                         show="parameter_group_nm",
                         show="parameter_nm",
                         show="casrn",
                         show="srsname",
                         show="parameter_units",
                         format="rdb")
  
  if(any(parameterCd == "all")){
    parameterData <- importRDB1(fullURL, asDateTime = FALSE)
  } else {
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]
  
    if(nrow(parameterData) != length(parameterCd)){
      if(length(parameterCd) == 1){
        
        suburl <- appendDrURL(baseURL,radio_pm_search="pm_search",
                               pm_search=parameterCd,
                               show="parameter_group_nm",
                               show="parameter_nm",
                               show="casrn",
                               show="srsname",
                               show="parameter_units",
                               format="rdb")
        
        suburl <- paste0("https://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search",
                     "&pm_search=", parameterCd,
                     "&format=rdb", "&show=parameter_group_nm",
                     "&show=parameter_nm", "&show=casrn",
                     "&show=srsname", "&show=parameter_units")
        parameterData <- importRDB1(suburl,asDateTime = FALSE)
      } else {
        
        fullPcodeDownload <- importRDB1(fullURL)
        parameterData <- fullPcodeDownload[fullPcodeDownload$parameter_cd %in% parameterCd,]
      }
      
      if(nrow(parameterData) != length(parameterCd)){
        badPcode <- parameterCd[!(parameterCd %in% parameterData$parameter_cd)]
        warning("The following pCodes seem mistyped, and no information was returned: ",paste(badPcode,collapse=","))
      }
    } 
  }
  
  if(nrow(parameterData) != sum(is.na(parameterCd.orig))){
    na.params <- data.frame(matrix(ncol = ncol(parameterData), nrow = sum(is.na(parameterCd.orig))))
    names(na.params) <- names(parameterData)
    parameterData <- rbind(parameterData, na.params)
  }
  
  return(parameterData)
}
