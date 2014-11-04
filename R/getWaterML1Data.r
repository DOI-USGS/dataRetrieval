#' Function to return data from the NWISWeb WaterML1.1 service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @import XML
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' urlBase <- "http://waterservices.usgs.gov/nwis"
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' data <- getWaterML1Data(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),
#'             startDate,endDate,'dv',statCd=c("00003","00001"))
#' multiData <- getWaterML1Data(urlMulti)
#' groundWaterSite <- "431049071324301"
#' startGW <- "2013-10-01"
#' endGW <- "2014-06-30"
#' groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
#'           startGW,endGW, service="gwlevels", format="xml",interactive=FALSE)
#' groundWater <- getWaterML1Data(groundwaterExampleURL)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2014-10-10","2014-10-10",'uv',format='xml')
#' unitData <- getWaterML1Data(unitDataURL)
getWaterML1Data <- function(obs_url){
  
  h <- basicHeaderGatherer()
  doc = tryCatch({
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
  
  
  doc <- xmlRoot(doc)
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  for (i in 1:length(timeSeries)){
    
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)  
    
    #     site <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteProperty[@name='hucCd']", namespaces = chunkNS, xmlValue))
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
      
      methodID <- padVariable(methodID,2)
      
      value <- as.numeric(xpathSApply(subChunk, "ns1:value",namespaces = chunkNS, xmlValue))
      if(length(value)!=0){
        datetime <- as.POSIXct(strptime(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),"%Y-%m-%dT%H:%M:%S"))
        tzHours <- substr(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),
                          24,
                          nchar(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS)))
        if(mean(nchar(tzHours),rm.na=TRUE) == 6){
          tzAbbriev <- zoneAbbrievs[tzHours]
        } else {
          tzAbbriev <- rep(as.character(zoneAbbrievs[1]),length(datetime))
        }
        
        timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                      "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                      "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                    c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
        timeZone <- as.character(timeZoneLibrary[tzAbbriev])
        if(length(unique(timeZone)) == 1){
          datetime <- as.POSIXct(as.character(datetime), tz = unique(timeZone))
        } else {
          warning("Mixed time zone information")
          for(i in seq_along(datetime)){
            datetime[i] <- as.POSIXct(as.character(datetime[i]), tz = timeZone[i])
          }
        }
        
        qualifier <- as.character(xpathSApply(subChunk, "ns1:value/@qualifiers",namespaces = chunkNS))
        
        valueName <- paste(methodID,pCode,statCd,sep="_")
        qualName <- paste(methodID,pCode,statCd,"cd",sep="_")
        valueName <- paste("X",valueName,sep="")
        qualName <- paste("X",qualName,sep="")
        
        assign(valueName,value)
        assign(qualName,qualifier)
        
        if(length(get(qualName))!=0){
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
                           tzAbbriev,
                           get(valueName),stringsAsFactors=FALSE)
          
          names(df) <- c("agency_cd","site_no","datetime","tz_cd",valueName)       
        }
        
        if (1 == i & valuesIndex[1] == j){
          mergedDF <- df
        } else {
          similarNames <- intersect(names(mergedDF), names(df))
          mergedDF <- merge(mergedDF, df,by=similarNames,all=TRUE)
          #         mergedDF <- merge(mergedDF, df,by=c("agency_cd","site_no","datetime","tz_cd"),all=TRUE)
        }
      }
    }
  }
  
  return (mergedDF)
}
