#' 
#' Format and organize WQP arguments that are passed in as \code{...}.
#' 
#' @keywords internal
readWQPdots <- function(...){
  
  if(length(list(...)) == 0){
    stop("No arguments supplied")
  }
  
  matchReturn <- convertLists(...)
  
  values <- sapply(matchReturn, function(x) as.character(paste(eval(x),collapse=";",sep="")))
  
  if("bBox" %in% names(values)){
    values['bBox'] <- gsub(pattern = ";", replacement = ",", x = values['bBox'])
  }
  
  if("service" %in% names(matchReturn)){
    service <- matchReturn$service
    matchReturn$service <- NULL
  } else {
    service <- "Result"
  }
  
  match.arg(service, c("Result", "Station", "Activity", "Organization",
                       "ActivityMetric", "SiteSummary",
                       "Project", "ProjectMonitoringLocationWeighting",
                       "ResultDetectionQuantitationLimit", "BiologicalMetric"))
  
  values <- checkWQPdates(values)
  
  names(values)[names(values) == "siteNumber"] <- "siteid"
  names(values)[names(values) == "siteNumbers"] <- "siteid"
  names(values)[names(values) == "parameterCd"] <- "pCode"
  names(values)[names(values) == "USGSPCode"] <- "pCode"
  
  names(values)[names(values) == "stateCd"] <- "statecode"
  if("statecode" %in% names(values)){
    stCd <- values["statecode"]
    stCdPrefix <- "US:"
    if(!grepl(stCdPrefix, stCd)){
      values["statecode"] <- paste0(stCdPrefix, zeroPad(stateCdLookup(stCd, "id"),2))
    }
  }
  
  names(values)[names(values) == "countyCd"] <- "countycode"
  if(all(c("countycode","statecode") %in% names(values))){
    stCd <- gsub("US:", "", values["statecode"])
    values["countycode"] <- paste(values["statecode"], 
                                  countyCdLookup(stCd, values["countycode"], "id"),
                                  sep=":")
  }
  
  if("zip" %in% names(values)){
    if(class(values["zip"]) == "logical"){
      values["zip"] <- ifelse(values["zip"], "yes","no")
    }
  } else {
    values["zip"] <- "yes"
  }
  
  return(list(values=values, service=service))
}
