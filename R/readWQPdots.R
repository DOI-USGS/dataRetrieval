#' 
#' Format and organize WQP arguments that are passed in as \code{...}.
#' 
#' @keywords internal
readWQPdots <- function(...){
  matchReturn <- list(...)
  
  values <- sapply(matchReturn, function(x) as.character(paste(eval(x),collapse=";",sep="")))
  
  if("bBox" %in% names(values)){
    values['bBox'] <- gsub(pattern = ";", replacement = ",", x = values['bBox'])
  }
  
  values <- checkWQPdates(values)
  
  names(values)[names(values) == "siteNumber"] <- "siteid"
  names(values)[names(values) == "siteNumbers"] <- "siteid"
  
  if("statecode" %in% names(values)){
    stCd <- values["statecode"]
    if(!grepl("US:",stCd)){
      values["statecode"] <- paste0("US:",stateCdLookup(stCd, "id"))
    }
  }
  
  if("stateCd" %in% names(values)){
    stCd <- values["stateCd"]
    if(!grepl("US:",stCd)){
      values["stateCd"] <- paste0("US:",stateCdLookup(stCd, "id"))
    }
    names(values)[names(values) == "stateCd"] <- "statecode"
  }
  
  if("tz" %in% names(values)){
    tz <- values["tz"]
    if(tz != ""){
      rTZ <- c("America/New_York","America/Chicago",
               "America/Denver","America/Los_Angeles",
               "America/Anchorage","America/Honolulu",
               "America/Jamaica","America/Managua",
               "America/Phoenix","America/Metlakatla","UTC")
      tz <- match.arg(tz, rTZ)
      if("UTC" == tz) tz <- ""
    }
    values["tz"] <- tz
  }
  
  return(values)
}