#' Function to return data from the NWISWeb WaterML1.1 daily values service
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @return mergedDF a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @examples
#' url <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
#' sites <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(sites,property,startDate,endDate,'dv')
#' data <- getWaterML1Data(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),startDate,endDate,'dv',statCd=c("00003","00001"))
#' multiData <- getWaterML1Data(urlMulti)
getWaterML1Data <- function(obs_url){

  # This is more elegent, but requires yet another package dependency RCurl...which I now require for wqp
#   content <- getURLContent(obs_url,.opts=list(timeout.ms=500000))
#   test <- capture.output(tryCatch(xmlTreeParse(content, getDTD=FALSE, useInternalNodes=TRUE),"XMLParserErrorList" = function(e) {cat("incomplete",e$message)}))
#   while (length(grep("<?xml",test))==0) {
#     content <- getURLContent(obs_url,.opts=list(timeout.ms=500000))
#     test <- capture.output(tryCatch(xmlTreeParse(content, getDTD=FALSE, useInternalNodes=TRUE),"XMLParserErrorList" = function(e) {cat("incomplete",e$message)}))
#   }
#   doc <- htmlTreeParse(content, getDTD=TRUE, useInternalNodes=TRUE)
#   require(XML)
  
  doc <- xmlTreeParse(obs_url, getDTD = FALSE, useInternalNodes = TRUE)
  doc <- xmlRoot(doc)

  ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces = ns)
  
  for (i in 1:length(timeSeries)){

    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)  
      
    pCode <-as.character(xpathApply(chunk, "ns1:variable/ns1:variableCode", namespaces = chunkNS, xmlValue))
    statCd <- as.character(xpathApply(chunk, "ns1:variable/ns1:options/ns1:option/@optionCode", namespaces = chunkNS))

    valuesIndex <- as.numeric(which("values" == names(chunk)))

    for (j in valuesIndex){
      subChunk <- xmlRoot(xmlDoc(chunk[[j]]))
      
      methodID <- as.character(xpathSApply(subChunk, "ns1:method/@methodID", namespaces = chunkNS))
      
      methodID <- padVariable(methodID,2)
      
      value <- as.numeric(xpathSApply(subChunk, "ns1:value",namespaces = chunkNS, xmlValue))  
      dateTime <- strptime(xpathSApply(subChunk, "ns1:value/@dateTime",namespaces = chunkNS),"%Y-%m-%dT%H:%M:%S.000")
      qualifier <- as.character(xpathSApply(subChunk, "ns1:value/@qualifiers",namespaces = chunkNS))

      valueName <- paste(methodID,pCode,statCd,sep="_")
      qualName <- paste(methodID,pCode,statCd,"cd",sep="_")
      valueName <- paste("X",valueName,sep="")
      qualName <- paste("X",qualName,sep="")
      
      assign(valueName,value)
      assign(qualName,qualifier)
      
      df <- data.frame(dateTime,
                       get(valueName),
                       get(qualName)
      )
      names(df) <- c("dateTime",valueName,qualName)
 
      if (1 == i & valuesIndex[1] == j){
        mergedDF <- df
      } else {
        mergedDF <- merge(mergedDF, df,by="dateTime",all=TRUE)
      }
    }
  }

  agencyCd <- as.character(xpathSApply(timeSeries[[1]], "ns1:sourceInfo/ns1:siteCode/@agencyCode",namespaces = chunkNS))
  siteNo <- as.character(xpathSApply(timeSeries[[1]], "ns1:sourceInfo/ns1:siteCode",namespaces = chunkNS, xmlValue))
  
  mergedDF$agency <- rep(agencyCd, nrow(mergedDF))
  mergedDF$site <- rep(siteNo, nrow(mergedDF))
  
  reorder <- c(ncol(mergedDF)-1, ncol(mergedDF), 1:(ncol(mergedDF)-2))
  
  mergedDF <- mergedDF[,reorder]
  
  return (mergedDF)
}