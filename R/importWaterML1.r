#' Function to return data from the NWISWeb WaterML1.1 service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
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
importWaterML1 <- function(obs_url,asDateTime=FALSE, tz=""){
  
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
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  
  doc <- xmlRoot(doc)
  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  if(0 == length(timeSeries)){
    stop("No data to return for URL:", obs_url)
  }
  
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
      
          
      attNames <- xpathSApply(subChunk, "ns1:value/@*",namespaces = chunkNS)
      attributeNames <- unique(names(attNames))

      x <- lapply(attributeNames, function(x) xpathSApply(subChunk, paste0("ns1:value/@",x),namespaces = chunkNS))
      
      valueName <- paste(methodID,pCode,statCd,sep="_")      
      valueName <- paste("X",valueName,sep="")      
      assign(valueName,value)
      
      df <- data.frame(agency = rep(agency,length(value)),
                       site_no = rep(site,length(value)),
                       stringsAsFactors=FALSE)
      
      for(k in 1:length(attributeNames)){
        attVal <- as.character(x[[k]])
        if(length(attVal) == nrow(df)){
          df$temp <- as.character(x[[k]])
          
        } else {
          attrList <- xpathApply(subChunk, "ns1:value", namespaces = chunkNS, xmlAttrs)
          df$temp <- sapply(1:nrow(df),function(x) as.character(attrList[[x]][attributeNames[k]]))
          df$temp[is.na(df$temp)] <- ""
        }
        names(df)[which(names(df) %in% "temp")] <- attributeNames[k]
        
      }

      df <- cbind(df, get(valueName))
      names(df)[length(df)] <- valueName
      
      if("qualifiers" %in% names(df)){
        qualName <- paste(methodID,pCode,statCd,"cd",sep="_")
        qualName <- paste("X",qualName,sep="")
        names(df)[which(names(df) == "qualifiers")] <- qualName       
      }
      
      if("dateTime" %in% attributeNames){
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
          
          if(tz != ""){
            attr(datetime, "tzone") <- tz
          }
          df$tz_cd <- tzAbbriev
          
        } else {
          datetime <- as.character(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS))
        }
        
        df$dateTime <- datetime     
        
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
