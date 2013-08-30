#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation 
#'
#' @param obs_url string containing the url for the retrieval
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @import XML
#' @import plyr
#' @examples
#' url <- "http://webvastage6.er.usgs.gov/ogc-swie/wml2/uv/sos?request=GetObservation&featureID=01446500&observedProperty=00065&offering=UNIT&beginPosition=2013-08-20"
#' dataReturned <- getWaterML2Data(url)
#' URL2 <- "http://cida-test.er.usgs.gov/ngwmn_cache/sos?REQUEST=GetObservation&featureOfInterest=VW_GWDP_GEOSERVER.USGS.401532085085301"
#' dataReturned2 <- getWaterML2Data(URL2)
#' URL3 <- "http://webvastage6.er.usgs.gov/ogc-swie/wml2/dv/sos?request=GetObservation&featureID=435601087432701&observedProperty=00045&beginPosition=2012-01-01&offering=Sum"
#' dataReturned3 <- getWaterML2Data(URL3)
getWaterML2Data <- function(obs_url){
  
  doc <- xmlTreeParse(obs_url, getDTD = FALSE, useInternalNodes = TRUE)
  doc <- xmlRoot(doc)
  
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  
  timeseries <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP", namespaces = ns)
  DF <- xmlToDataFrame(timeseries,stringsAsFactors=FALSE)
  DF$time <- gsub(":","",DF$time)
  
  DF$time <- ifelse(nchar(DF$time) > 18,as.POSIXct(strptime(DF$time, format="%Y-%m-%dT%H%M%S%z")),
                    ifelse("Z" == substr(DF$time,(nchar(DF$time)),nchar(DF$time)),as.POSIXct(strptime(DF$time, format="%Y-%m-%dT%H%M%S",tz="GMT")),
                           as.POSIXct(strptime(DF$time, format="%Y-%m-%dT%H%M%S",tz=""))))
    
  DF$time <- as.POSIXct(DF$time,origin=as.POSIXct(strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  DF$value<- as.numeric(DF$value)
   
  # Very specific to USGS:
  defaultQualifier <- as.character(xpathApply(doc, "//wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title",namespaces = ns))

  defaultQualifier <- ifelse("Provisional data subject to revision." == defaultQualifier, "P",
                           ifelse("Approved for publication. Processing and review completed." == defaultQualifier, "A", defaultQualifier))
  qualifier <- rep(defaultQualifier,nrow(DF))
 
  
  realQual <- as.character(xpathSApply(doc,"//wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title", namespaces = ns))
  
  if (length(realQual) > 0){
    timeseriesSub <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata", namespaces = ns)
    DF2 <- xmlToDataFrame(timeseriesSub,stringsAsFactors=FALSE)
  } else {
    if (length(qualifier) > 0){
      DF$qualifier <- qualifier
    }
  }
  
  timeseries2 <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point", namespaces = ns)
  
  xp <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP", xpathSApply, ".//*[not(*)]", function(x)
    setNames(ifelse(nzchar(xmlValue(x)), xmlValue(x), 
                    ifelse("qualifier" == xmlName(x),xpathSApply(x,"./@xlink:title",namespaces = ns),xmlAttrs(x))), 
             xmlName(x)), namespaces = ns)
  library(plyr)
  DF <- do.call(rbind.fill.matrix, lapply(xp, t))
  return (DF)
}
