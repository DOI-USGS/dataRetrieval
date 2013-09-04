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
  
  timeseries2 <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point", namespaces = ns)
  
  xp <- xpathApply(doc, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP", xpathSApply, ".//*[not(*)]", function(x)
    setNames(ifelse(nzchar(xmlValue(x)), xmlValue(x), 
                    ifelse("qualifier" == xmlName(x),xpathSApply(x,"./@xlink:title",namespaces = ns),"")), #originally I had the "" as xmlAttr(x) 
             xmlName(x)), namespaces = ns)
  library(plyr)
  DF2 <- do.call(rbind.fill.matrix, lapply(xp, t))
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
