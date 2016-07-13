#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#'
#' @param obs_url character containing the url for the retrieval or a file path to the data file.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return mergedDF a data frame time, value, description, qualifier, and identifier
#' @export
#' @importFrom XML xmlRoot
#' @importFrom XML xmlDoc
#' @importFrom XML xpathApply
#' @importFrom XML xpathSApply
#' @importFrom XML xmlNamespaceDefinitions
#' @importFrom XML xmlValue
#' @importFrom XML xmlAttrs
#' @importFrom XML xmlName
#' @importFrom dplyr rbind_all
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
#' }
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "WaterML2Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' UserData <- importWaterML2(fullPath)
#' 
importWaterML2 <- function(obs_url, asDateTime=FALSE, tz=""){
  
  if(file.exists(obs_url)){
    rawData <- obs_url
    doc <- xmlTreeParse(rawData, getDTD = FALSE, useInternalNodes = TRUE)
  } else {
    doc <- xmlTreeParse(getWebServiceData(obs_url), getDTD = FALSE, useInternalNodes = TRUE)
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

  if(0 == length(timeSeries)){
    df <- data.frame()
    attr(df, "url") <- obs_url
    return(df)
  }
  
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
                                          xmlName(x,full=TRUE)), 
                     namespaces = chunkNS)
  
    if(length(xpathApply(doc, 
                  "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:TVPMeasurementMetadata", 
                  xmlValue, namespaces = ns)) != 0){
      xp <- xp[-1]
    }

    y <- lapply(xp,t)
    z <- lapply(y, as.data.frame)
    DF2 <- suppressWarnings(rbind_all(z))

    names(DF2)[grep("wml2",names(DF2))] <- sub("wml2:","",names(DF2)[grep("wml2",names(DF2))])
    
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
    
#########################################
    # Very specific to USGS:
    defaultQualifier <- as.character(xpathApply(chunk, "//wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title",namespaces = chunkNS))
    
    if (length(defaultQualifier) == 0 && (typeof(defaultQualifier) == "character")) {
      defaultQualifier <- "NA"
    }
    
    if("swe:value" %in% names(DF2)){
      isQual <- as.character(xpathApply(chunk, 
                                        "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title",
                                        namespaces = chunkNS))
      DF2$qualifier <- ifelse(defaultQualifier != isQual,isQual,defaultQualifier)
      DF2$`swe:value` <- NULL
    } else {
      DF2$qualifier <- rep(defaultQualifier,nrow(DF2))
    }
   
#########################################

    id <- as.character(xpathApply(chunk, "//gml:identifier", xmlValue, namespaces = chunkNS))
    if(length(id) > 1){
      for (j in 1:length(id)){
        idName <- paste0("identifier",j)
        DF2[,eval(idName)] <- id[j]
      }
      message("There were multiple identifier elements")
    } else if (length(id) == 0){
      DF2$identifier <- NA
    } else{
      DF2$identifier <- rep(id, nrow(DF2))
    }
    
    if (1 == i ){
      mergedDF <- DF2
    } else {
      similarNames <- intersect(names(mergedDF), names(DF2))
      mergedDF <- merge(mergedDF, DF2,by=similarNames,all=TRUE)
    }
  }

  return (mergedDF)
}
