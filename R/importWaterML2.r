#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @param tz string to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @import XML
#' @import RCurl
#' @importFrom plyr rbind.fill.matrix
#' @examples
#' baseURL <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
#' URL <- paste(baseURL, "sites=01646500",
#'      "startDT=2014-09-01",
#'      "endDT=2014-09-08",
#'      "statCd=00003",
#'      "parameterCd=00060",sep="&")
#' URL2 <- paste("http://cida.usgs.gov/noreast-sos/simple?request=GetObservation",
#'      "featureID=MD-BC-BC-05",
#'      "offering=RAW",
#'      "observedProperty=WATER",sep="&")
#' \dontrun{
#' dataReturned1 <- importWaterML2(URL)
#' dataReturn2 <- importWaterML2(URL2, TRUE)
#' URLmulti <-  paste(baseURL,
#'   "sites=04024430,04024000",
#'   "startDT=2014-09-01",
#'   "endDT=2014-09-08",
#'   "statCd=00003",
#'   "parameterCd=00060",sep="&")
#' dataReturnMulti <- importWaterML2(URLmulti)
#' filePath <- system.file("extdata", package="dataRetrievaldemo")
#' fileName <- "WaterML2Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' UserData <- importWaterML2(fullPath)
#' }
importWaterML2 <- function(obs_url, asDateTime=FALSE, tz=""){
  
  if(url.exists(obs_url)){
    doc = tryCatch({
      h <- basicHeaderGatherer()
      returnedDoc <- getURL(obs_url, headerfunction = h$update)
      if(h$value()["Content-Type"] == "text/xml;charset=UTF-8" | 
           h$value()["Content-Type"] == "text/xml; subtype=gml/3.1.1;charset=UTF-8"){
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
  } else {
    doc <- xmlTreeParse(obs_url, getDTD = FALSE, useInternalNodes = TRUE)
  }
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  
  doc <- xmlRoot(doc)
  
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  
  
  timeSeries <- xpathApply(doc, "//wml2:Collection", namespaces = ns)
  
  for (i in 1:length(timeSeries)){
  
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)
    
    xp <- xpathApply(chunk, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP", 
                     xpathSApply, ".//*[not(*)]", 
                     function(x) setNames(ifelse(nzchar(xmlValue(x)), 
                                                 xmlValue(x), 
                                                    ifelse("qualifier" == xmlName(x),
                                                           xpathSApply(x,"./@xlink:title",namespaces = ns),"")), #originally I had the "" as xmlAttr(x) 
                                                            xmlName(x)), 
                     namespaces = chunkNS)
  
    if(length(xpathApply(doc, 
                  "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:TVPMeasurementMetadata", 
                  xmlValue, namespaces = ns)) != 0){
      xp <- xp[-1]
    }
      
    DF2 <- do.call(rbind.fill.matrix, lapply(xp, t))
    DF2 <- as.data.frame(DF2,stringsAsFactors=FALSE)
    
    if(asDateTime){
    
      DF2$time <- gsub(":","",DF2$time)
      DF2$time <- ifelse(nchar(DF2$time) > 18,
                                   as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S%z",tz="UTC"),
                                         as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S",tz="UTC"))
      
      DF2$time <- as.POSIXct(DF2$time, origin = "1970-01-01", tz="UTC")
      
      if(tz != ""){
        attr(DF2$time, "tzone") <- tz
      }
      
    } else {
      DF2$time <- as.Date(DF2$time)
    }
  
    DF2$value <- as.numeric(DF2$value)
    # Very specific to USGS:
    defaultQualifier <- as.character(xpathApply(chunk, "//wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title",namespaces = chunkNS))
    
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
    
  
    id <- as.character(xpathApply(chunk, "//gml:identifier", xmlValue, namespaces = chunkNS))
    DF2$identifier <- rep(id, nrow(DF2))
    
    if (1 == i ){
      mergedDF <- DF2
    } else {
      similarNames <- intersect(names(mergedDF), names(DF2))
      mergedDF <- merge(mergedDF, DF2,by=similarNames,all=TRUE)
    }
  }

  return (mergedDF)
}
