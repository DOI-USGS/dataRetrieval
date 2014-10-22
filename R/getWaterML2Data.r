#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation 
#'
#' @param obs_url string containing the url for the retrieval
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @import XML
#' @importFrom dplyr rbind_all
#' @examples
#' baseURL <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
#' URL <- paste(baseURL, "sites=01646500",
#'      "startDT=2014-09-01",
#'      "endDT=2014-09-08",
#'      "statCd=00003",
#'      "parameterCd=00060",sep="&")
#' \dontrun{dataReturned3 <- getWaterML2Data(URL)}
getWaterML2Data <- function(obs_url){
  
  h <- basicHeaderGatherer()
  doc = tryCatch({
    returnedDoc <- getURL(obs_url, headerfunction = h$update)
    if(h$value()["Content-Type"] == "text/xml;charset=UTF-8"){
      xmlTreeParse(returnedDoc, getDTD = FALSE, useInternalNodes = TRUE)
    } else {
      message(paste("URL caused an error:", obs_url))
      message("Content-Type=",h$value()["Content-Type"])
      return(NA)
    }   
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", obs_url))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", obs_url))
    message(e)
    return(NA)
  }) 
  
  doc <- xmlRoot(doc)
  
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  
  timeseries2 <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point", namespaces = ns)
  
  xp <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP", xpathSApply, ".//*[not(*)]", function(x)
    setNames(ifelse(nzchar(xmlValue(x)), xmlValue(x), 
                    ifelse("qualifier" == xmlName(x),xpathSApply(x,"./@xlink:title",namespaces = ns),"")), #originally I had the "" as xmlAttr(x) 
             xmlName(x)), namespaces = ns)

  DF2 <- do.call(rbind_all, lapply(xp, t))
  DF2 <- as.data.frame(DF2,stringsAsFactors=FALSE)
  DF2$time <- gsub(":","",DF2$time)
  DF2$time <- with(DF2, ifelse(nchar(time) > 18,as.POSIXct(strptime(time, format="%Y-%m-%dT%H%M%S%z")),
                     ifelse("Z" == substr(time,(nchar(time)),nchar(time)),as.POSIXct(strptime(time, format="%Y-%m-%dT%H%M%S",tz="GMT")),
                            as.POSIXct(strptime(time, format="%Y-%m-%dT%H%M%S",tz="")))))
  
  DF2$time <- with(DF2, as.POSIXct(time,origin=as.POSIXct(strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC"))))
  
  DF2$value <- as.numeric(gsub("true","",DF2$value))
  
  # Very specific to USGS:
  defaultQualifier <- as.character(xpathApply(doc, "//wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title",namespaces = ns))
  
  if (length(defaultQualifier) == 0 && (typeof(defaultQualifier) == "character")) {
    defaultQualifier <- "NA"
  }
  
  if("qualifier" %in% names(DF2)){
    DF2$qualifier <- ifelse(defaultQualifier != DF2$qualifier,DF2$qualifier,defaultQualifier)
  } else {
    DF2$qualifier <- rep(defaultQualifier,nrow(DF2))
  }
  
  
  DF2$qualifier <- ifelse("Provisional data subject to revision." == DF2$qualifier, "P",
                             ifelse("Approved for publication. Processing and review completed." == DF2$qualifier, "A", DF2$qualifier))
  
  return (DF2)
}
