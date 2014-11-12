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
#' @import reshape2
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' data <- importWaterML1(obs_url,TRUE)
#' 
#' groundWaterSite <- "431049071324301"
#' startGW <- "2013-10-01"
#' endGW <- "2014-06-30"
#' groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
#'           startGW,endGW, service="gwlevels")
#' groundWater <- importWaterML1(groundwaterExampleURL)
#' 
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2013-11-03","2013-11-03",'uv')
#' unitData <- importWaterML1(unitDataURL,TRUE)
#' 
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "WaterML1Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' importUserWM1 <- importWaterML1(fullPath,TRUE)
#'
#' # Two sites, two pcodes, one site has two data descriptors:
#' siteNumber <- c('01480015',"04085427")
#' obs_url <- constructNWISURL(siteNumber,c("00060","00010"),startDate,endDate,'dv')
#' data <- importWaterML1(obs_url)
#' data$dateTime <- as.Date(data$dateTime)
#' data <- renameNWISColumns(data)
#' names(attributes(data))
#' attr(data, "url")
#' attr(data, "disclaimer")
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
  queryInfo <- xmlToList(xmlRoot(xmlDoc(doc[["queryInfo"]])))
  names(queryInfo) <- make.unique(names(queryInfo))
  
  noteIndex <- grep("note",names(queryInfo))
  
  noteTitles <- as.character(lapply(queryInfo[noteIndex], function(x) x$.attrs))
  notes <- as.character(lapply(queryInfo[noteIndex], function(x) x$text))
  names(notes) <- noteTitles
  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  
  if(0 == length(timeSeries)){
    message("Returning an empty dataset")
    df <- data.frame()
    attr(df, "queryInfo") <- queryInfo
    return(df)
  }
  
  attList <- list()
  dataColumns <- c()
  qualColumns <- c()
  
  for (i in 1:length(timeSeries)){
    
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)  
      
    uniqueName <- as.character(xpathApply(chunk, "@name", namespaces = chunkNS))
    site <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode", namespaces = chunkNS, xmlValue))
    agency <- as.character(xpathApply(chunk, "ns1:sourceInfo/ns1:siteCode/@agencyCode", namespaces = chunkNS))
    pCode <-as.character(xpathApply(chunk, "ns1:variable/ns1:variableCode", namespaces = chunkNS, xmlValue))
    statCd <- as.character(xpathApply(chunk, "ns1:variable/ns1:options/ns1:option/@optionCode", namespaces = chunkNS))
    noValue <- as.numeric(xpathApply(chunk, "ns1:variable/ns1:noDataValue", namespaces = chunkNS, xmlValue))
    
    extraSiteData <-  xmlToList(xmlRoot(xmlDoc(chunk[["sourceInfo"]])))
    extraVariableData <-  xmlToList(xmlRoot(xmlDoc(chunk[["variable"]])))
    
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
      
      if(length(value)!=0){
      
        value[value == noValue] <- NA
            
        attNames <- xpathSApply(subChunk, "ns1:value/@*",namespaces = chunkNS)
        attributeNames <- unique(names(attNames))
  
        x <- lapply(attributeNames, function(x) xpathSApply(subChunk, paste0("ns1:value/@",x),namespaces = chunkNS))
        
        
        methodDescription <- as.character(xpathApply(subChunk, "ns1:method/ns1:methodDescription", namespaces = chunkNS, xmlValue))
        
        valueName <- paste("X",pCode,statCd,sep="_")
        
        if(length(methodDescription) > 0 && methodDescription != ""){
          valueName <- paste("X",methodDescription,pCode,statCd,sep="_") 
        }
        
         
        assign(valueName,value)
        
        df <- data.frame(agency = rep(agency,length(value)),
                         site_no = rep(site,length(value)),
                         stringsAsFactors=FALSE)
        
        if(length(attributeNames) > 0){
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
        }
        
        df <- cbind(df, get(valueName))
        names(df)[length(df)] <- valueName
        
        if("qualifiers" %in% names(df)){
          qualName <- paste(valueName,"cd",sep="_")
          names(df)[which(names(df) == "qualifiers")] <- qualName
          qualColumns <- c(qualColumns, qualName)
        }
        
        dataColumns <- c(dataColumns, valueName)
        
        if("dateTime" %in% attributeNames){
          
          datetime <- xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS)
          
          numChar <- nchar(datetime)
          
          if(asDateTime){
            
            # Common options:
            # YYYY numChar=4
            # YYYY-MM-DD numChar=10
            # YYYY-MM-DDTHH:MM numChar=16
            # YYYY-MM-DDTHH:MM:SS numChar=19
            # YYYY-MM-DDTHH:MM:SSZ numChar=20
            # YYYY-MM-DDTHH:MM:SS.000 numChar=23
            # YYYY-MM-DDTHH:MM:SS.000-XX:00 numChar=29
                        
            if(abs(max(numChar) - min(numChar)) != 0){
              message("Mixed date types, not converted to POSIXct")
            } else {
              numChar <- numChar[1]
              if(numChar == 4){
                datetime <- as.POSIXct(datetime, "%Y", tz = "UTC")
              } else if(numChar == 10){
                datetime <- as.POSIXct(datetime, "%Y-%m-%d", tz = "UTC")
              } else if(numChar == 16){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M", tz = "UTC")
              } else if(numChar == 19){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
              } else if(numChar == 20){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
              }  else if(numChar == 23){
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
              } else if(numChar == 24){
                datetime <- substr(datetime,1,23)
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
                df$tz_cd <- rep(zoneAbbrievs[1], nrow(df))
              } else if(numChar == 29){
                tzOffset <- as.character(substr(datetime,24,numChar))
                
                tzHours <- as.numeric(substr(tzOffset,1,3))
  
                datetime <- substr(datetime,1,23)
                datetime <- as.POSIXct(datetime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
                datetime <- datetime + tzHours*60*60
                df$tz_cd <- as.character(zoneAbbrievs[tzOffset]) 
              }
              
              if(!("tz_cd" %in% names(df))){
                df$tz_cd <- zoneAbbrievs[1]
                tzHours <- as.numeric(substr(names(zoneAbbrievs[1]),1,3))
                datetime <- datetime + tzHours*60*60
              }
            }
            
            if(tz != ""){
              attr(datetime, "tzone") <- tz
            }
            
            
          } else {
            
            datetime <- as.character(datetime)
            if(any(numChar) == 29){
              tzOffset <- as.character(substr(datetime,24,numChar))
              df$tz_cd <- as.character(zoneAbbrievs[tzOffset]) 
              df$tz_cd[is.na(df$tz_cd)] <- zoneAbbrievs[1]
            } else {
              df$tz_cd <- zoneAbbrievs[1]
            }
            
          }
          
          df$dateTime <- datetime     
          
        }
        
        colNames <- names(df)
        
        if( exists("qualName")){
          columnsOrdered <- c("agency","site_no","dateTime","tz_cd",attributeNames[attributeNames != "dateTime"],qualName,valueName)
        } else {
          columnsOrdered <- c("agency","site_no","dateTime","tz_cd",attributeNames[attributeNames != "dateTime"],valueName)
        }
        
        columnsOrderd <- columnsOrdered[columnsOrdered %in% names(df)]
        
        
        
        df <- df[,columnsOrderd]
                  
        names(extraSiteData) <- make.unique(names(extraSiteData))
        
        sitePropertyIndex <- grep("siteProperty",names(extraSiteData))
        
        siteInfo <- data.frame(station_nm=extraSiteData$siteName,
                               site_no=extraSiteData$siteCode$text,
                               agency=extraSiteData$siteCode$.attrs[["agencyCode"]],
                               timeZoneOffset=extraSiteData$timeZoneInfo$defaultTimeZone[1],
                               timeZoneAbbreviation=extraSiteData$timeZoneInfo$defaultTimeZone[2],
                               dec_lat_va=as.numeric(extraSiteData$geoLocation$geogLocation$latitude),
                               dec_lon_va=as.numeric(extraSiteData$geoLocation$geogLocation$longitude),
                               srs=extraSiteData$geoLocation$geogLocation$.attrs[["srs"]],
                               stringsAsFactors=FALSE)

        properties <- as.character(lapply(extraSiteData[sitePropertyIndex], function(x) {
          if(".attrs" %in% names(x)){
            x$.attrs
          } else {
            NA
          }              
          }))
    
        propertyValues <- as.character(lapply(extraSiteData[sitePropertyIndex], function(x) {
          if("text" %in% names(x)){
            x$text
          } else {
            NA
          }              
          }))
        
        names(propertyValues) <- properties
        propertyValues <- propertyValues[propertyValues != "NA"]
        siteInfo <- cbind(siteInfo, t(propertyValues))            
        
        names(extraVariableData) <- make.unique(names(extraVariableData))
        variableInfo <- data.frame(parameterCd=extraVariableData$variableCode$text,
                                   parameter_nm=extraVariableData$variableName,
                                   parameter_desc=extraVariableData$variableDescription,
                                   valueType=extraVariableData$valueType,
                                   param_units=extraVariableData$unit$unitCode,
                                   noDataValue=as.numeric(extraVariableData$noDataValue),
                                   stringsAsFactors=FALSE)
        
        if (1 == i & valuesIndex[1] == j){
          mergedDF <- df
          siteInformation <- siteInfo
          variableInformation <- variableInfo
          
        } else {
          similarNames <- intersect(names(mergedDF), names(df))
          mergedDF <- merge(mergedDF, df,by=similarNames,all=TRUE)
          
          similarSites <- intersect(names(siteInformation), names(siteInfo))
          siteInformation <- merge(siteInformation, siteInfo, by=similarSites, all=TRUE)
          
          similarVariables <- intersect(names(variableInformation),names(variableInfo))
          variableInformation <- merge(variableInformation, variableInfo, by=similarVariables, all=TRUE)
        }
      }
    }
    attList[[uniqueName]] <- list(extraSiteData, extraVariableData)

    
  }
  
#   sortingColumns <- c("agency","site_no","dateTime","tz_cd")
#   dataColumns <- names(mergedDF)[!(names(mergedDF) %in% sortingColumns)]
#   dataColumns <- dataColumns[-grep("_cd",dataColumns)]
  
  sortingColumns <- names(mergedDF)[!(names(mergedDF) %in% c(dataColumns,qualColumns))]

  meltedmergedDF  <- melt(mergedDF,id.vars=sortingColumns)
  meltedmergedDF  <- meltedmergedDF[!is.na(meltedmergedDF$value),] 

  castFormula <- as.formula(paste(paste(sortingColumns, collapse="+"),"variable",sep="~"))
  mergedDF2 <- dcast(meltedmergedDF, castFormula, drop=FALSE)
  dataColumns2 <- !(names(mergedDF2) %in% sortingColumns)
  mergedDF <- mergedDF2[rowSums(is.na(mergedDF2[,dataColumns2])) != sum(dataColumns2),]

  if(length(dataColumns) > 1){
    mergedDF[,dataColumns] <- lapply(mergedDF[,dataColumns], function(x) as.numeric(x))
  } else {
    mergedDF[,dataColumns] <- as.numeric(mergedDF[,dataColumns])
  }
  
  
  row.names(mergedDF) <- NULL
  attr(mergedDF, "url") <- obs_url
  attr(mergedDF, "attributeList") <- attList
  attr(mergedDF, "siteInfo") <- siteInformation
  attr(mergedDF, "variableInfo") <- variableInformation
  attr(mergedDF, "disclaimer") <- notes["disclaimer"]
  attr(mergedDF, "queryInfo") <- queryInfo
  attr(mergedDF, "queryTime") <- Sys.time()
  
  return (mergedDF)
}
