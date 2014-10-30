#' Function to return data from the NWISWeb WaterML1.1 service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @import XML
#' @import RCurl
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' data <- importWaterML1(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),
#'             startDate,endDate,'dv',statCd=c("00003","00001"))
#' multiData <- importWaterML1(urlMulti)
#' groundWaterSite <- "431049071324301"
#' startGW <- "2013-10-01"
#' endGW <- "2014-06-30"
#' groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
#'           startGW,endGW, service="gwlevels", format="xml")
#' groundWater <- importWaterML1(groundwaterExampleURL)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2013-11-03","2013-11-03",'uv',format='xml')
#' unitData <- importWaterML1(unitDataURL,TRUE)
#' filePath <- system.file("extdata", package="dataRetrievaldemo")
#' fileName <- "WaterML1Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' importUserWM1 <- importWaterML1(fullPath)
importWaterML1 <- function(obs_url,asDateTime=FALSE){
  
  if(url.exists(obs_url)){
    doc = tryCatch({
      h <- basicHeaderGatherer()
      returnedDoc <- getURI(obs_url, headerfunction = h$update)
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
  } else {
    doc <- xmlTreeParse(obs_url, getDTD = FALSE, useInternalNodes = TRUE)
  }
  
  
  doc <- xmlRoot(doc)
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  for (i in 1:length(timeSeries)){
    
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)  
      
    site <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode", namespaces = chunkNS, xmlValue))
    agency <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode/@agencyCode", namespaces = chunkNS))
    pCode <-as.character(xpathApply(chunk, "ns1:variable/ns1:variableCode", namespaces = chunkNS, xmlValue))
    statCd <- as.character(xpathApply(chunk, "ns1:variable/ns1:options/ns1:option/@optionCode", namespaces = chunkNS))

    valuesIndex <- as.numeric(which("values" == names(chunk)))

        
    zoneAbbrievs <- c(as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:defaultTimeZone/@zoneAbbreviation", namespaces = chunkNS)),
                      as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:daylightSavingsTimeZone/@zoneAbbreviation", namespaces = chunkNS)))
    names(zoneAbbrievs) <- c(as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:defaultTimeZone/@zoneOffset", namespaces = chunkNS)),
                      as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:timeZoneInfo/ns1:daylightSavingsTimeZone/@zoneOffset", namespaces = chunkNS)))
    

    for (j in valuesIndex){
      subChunk <- xmlRoot(xmlDoc(chunk[[j]]))
      
      methodID <- as.character(xpathSApply(subChunk, "ns1:method/@methodID", namespaces = chunkNS))
      
      methodID <- zeroPad(methodID,2)
      
      value <- as.numeric(xpathSApply(subChunk, "ns1:value",namespaces = chunkNS, xmlValue))  
      
      if(asDateTime){
        datetime <- as.POSIXct(strptime(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),"%Y-%m-%dT%H:%M:%S"), tz="UTC")
        
        tzHours <- as.numeric(substr(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),
                          24,
                          nchar(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS))-3))
        tzHoursOff <- substr(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),
                                     24,
                                     nchar(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS)))
        tzAbbriev <- as.character(zoneAbbrievs[tzHoursOff])
  
        datetime <- datetime - tzHours*60*60
      } else {
        datetime <- as.Date(strptime(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),"%Y-%m-%dT%H:%M:%S"))
      }
      
      qualifier <- as.character(xpathSApply(subChunk, "ns1:value/@qualifiers",namespaces = chunkNS))

      valueName <- paste(methodID,pCode,statCd,sep="_")
      qualName <- paste(methodID,pCode,statCd,"cd",sep="_")
      valueName <- paste("X",valueName,sep="")
      qualName <- paste("X",qualName,sep="")
      
      assign(valueName,value)
      assign(qualName,qualifier)
      
      if(length(get(qualName))!=0){
        if(asDateTime){
          df <- data.frame(rep(agency,length(datetime)),
                           rep(site,length(datetime)),
                           datetime,
                           tzAbbriev,
                           get(valueName),
                           get(qualName),
                           stringsAsFactors=FALSE)
          
          names(df) <- c("agency_cd","site_no","datetime","tz_cd",valueName,qualName)
        } else {
          df <- data.frame(rep(agency,length(datetime)),
                           rep(site,length(datetime)),
                           datetime,
                           get(valueName),
                           get(qualName),
                           stringsAsFactors=FALSE)
          
          names(df) <- c("agency_cd","site_no","datetime",valueName,qualName)
        }
      } else {
        if(asDateTime){
          df <- data.frame(rep(agency,length(datetime)),
                           rep(site,length(datetime)),
                           datetime,
                           tzAbbriev,
                           get(valueName),stringsAsFactors=FALSE)
          
          names(df) <- c("agency_cd","site_no","datetime","tz_cd",valueName)
        } else {
          df <- data.frame(rep(agency,length(datetime)),
                           rep(site,length(datetime)),
                           datetime,
                           get(valueName),stringsAsFactors=FALSE)
          
          names(df) <- c("agency_cd","site_no","datetime",valueName)
        }
      }
 
      if (1 == i & valuesIndex[1] == j){
        mergedDF <- df
      } else {
        similarNames <- intersect(names(mergedDF), names(df))
        mergedDF <- merge(mergedDF, df,by=similarNames,all=TRUE)
      }
    }
  }
  
  return (mergedDF)
}
