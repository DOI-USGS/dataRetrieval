#' Parameter code to characteristic name
#' 
#' This function is useful to fine what characteristic name, result sample
#' fraction, unit code, and other parameters are mapped with USGS parameter
#' codes. This information is useful for converting workflows from a more
#' traditional NWIS water quality retrieval to a Water Quality Portal retrieval.
#' 
#' @export
#' @param parameterCd character that contains the code for a character vector 
#' of 5-digit parameter codes. Default is "all" which will return a complete
#' list of parameter codes that have been mapped to a characteristic name. 
#' @return a data frame with columns  "parm_cd", "description",            
#' "characteristicname", "measureunitcode", "resultsamplefraction",
#' "resulttemperaturebasis", "resultstatisticalbasis",  "resulttimebasis",
#' "resultweightbasis", "resultparticlesizebasis", "last_rev_dt"
#' @examples 
#' pcodes <- c("00070", "00075", "00430", "52642")
#' \donttest{
#' 
#' all <- pcode_to_name()
#' some <- pcode_to_name(pcodes)
#' 
#' }
pcode_to_name <- function(parameterCd = "all"){
  
  parameterCd.orig <- parameterCd
  parameterCd <- parameterCd[!is.na(parameterCd)]
  
  url_all <- "https://www.waterqualitydata.us/Codes/public_srsnames/?mimeType=json"
  doc <- get_nldi_sources(url_all)
  retval <- doc[["pcodes"]]
  
  if(all(tolower(parameterCd) != "all")){
    retval <- retval[retval$parm_cd %in% parameterCd, ]
  }
  
  attr(retval, "url") <- url_all
  
  if(any(parameterCd != "all")){
    if (nrow(retval) != length(parameterCd)) {
      badPcode <- parameterCd[!(parameterCd %in% retval$parm_cd)]
      warning(
        "The following pCodes seem mistyped, and no information was returned: ",
        paste(badPcode, collapse = ", ")
      )
    }
  }
  
  if (nrow(retval) != sum(is.na(parameterCd.orig))) {
    na.params <- data.frame(matrix(ncol = ncol(retval), nrow = sum(is.na(parameterCd.orig))))
    names(na.params) <- names(retval)
    retval <- rbind(retval, na.params)
  }
    
  # order by parameterCd.orig
  if (!isTRUE(parameterCd.orig == "all")) {
    retval <- retval[match(parameterCd.orig, retval$parm_cd), ]
    retval$parm_cd<- parameterCd.orig
  }
  
  return(retval)
}
