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
#' \donttest{
#' all_codes <- readNWISpCode("all")
#' 
#' }
readNWISpCode <- function(parameterCd){
 
  parameterCd.orig <- parameterCd
  parameterCd <- parameterCd[!is.na(parameterCd)]
  
  baseURL <- drURL("pCode", Access=pkg.env$access)
  
  fullURL <- paste0(baseURL, "fmt=rdb&group_cd=%")
  
  if(any(parameterCd == "all")){
    temp_df <- importRDB1(fullURL, asDateTime = FALSE)
    parameterData <- data.frame(
      parameter_cd = temp_df$parm_cd, 
      parameter_group_nm = temp_df$group,
      parameter_nm = temp_df$parm_nm, 
      casrn = temp_df$CASRN,
      srsname = temp_df$SRSName,
      parameter_units = temp_df$parm_unit,
      stringsAsFactors = FALSE
    )
    attr(parameterData, "url") <- fullURL
  } else {
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    parameterData <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCd,]
    
    if(nrow(parameterData) != length(parameterCd)){
      if(length(parameterCd) == 1){
        subURL <- paste0(baseURL, "fmt=rdb&parm_nm_cd=", parameterCd)
        temp_df <- importRDB1(subURL, asDateTime = FALSE)
        parameterData <- data.frame(
          parameter_cd = temp_df$parm_cd, 
          parameter_group_nm = temp_df$group,
          parameter_nm = temp_df$parm_nm, 
          casrn = temp_df$CASRN,
          srsname = temp_df$SRSName,
          parameter_units = temp_df$parm_unit,
          stringsAsFactors = FALSE
        )
        attr(parameterData, "url") <- subURL
      } else {
        temp_df <- importRDB1(fullURL, asDateTime = FALSE)
        trim_df <- data.frame(
          parameter_cd = temp_df$parm_cd, 
          parameter_group_nm = temp_df$group,
          parameter_nm = temp_df$parm_nm, 
          casrn = temp_df$CASRN,
          srsname = temp_df$SRSName,
          parameter_units = temp_df$parm_unit,
          stringsAsFactors = FALSE
        )
        parameterData <- trim_df[trim_df$parameter_cd %in% parameterCd,]
        attr(parameterData, "url") <- fullURL
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
