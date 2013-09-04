#' renameColumns
#'
#' Rename columns coming back from NWIS data retrievals
#'
#' @param rawData dataframe returned from retrieval functions
#' @keywords data import USGS web service
#' @return rawData dataframe with improved column names
#' @export
#' @examples
#' # These examples require an internet connection to run
#' siteNumber <- '05114000'
#' ParameterCd <- c('00060','00065')
#' StartDate <- as.character(Sys.Date())
#' EndDate <- as.character(Sys.Date())
#' # These examples require an internet connection to run
#' rawData <- retrieveUnitNWISData(siteNumber,ParameterCd,StartDate,EndDate,interactive=FALSE)
#' rawData <- renameColumns(rawData)
#' rawData2 <- retrieveNWISData(siteNumber,c("00010","00060","00300"),"2001-01-01","2002-01-01",StatCd=c("00001","00003"),interactive=FALSE)
#' rawData2 <- renameColumns(rawData2)
#' site <- '04027000'
#' pCodes <- c("00010","00060","00095","00300","00400","63680")
#' rawData3 <- retrieveUnitNWISData(site,pCodes,StartDate,EndDate,interactive=FALSE)
#' rawData3 <- renameColumns(rawData3)
renameColumns <- function(rawData){
  
  columnNames <- names(rawData)
  
  dataCols <- columnNames["X" == substring(columnNames, 1, 1)]
  dataCol_cds <- dataCols["cd" == substring(dataCols, nchar(dataCols)-1, nchar(dataCols))]
  dataCol_names <- dataCols[!(dataCols %in% dataCol_cds)]
  
  pCodes <- sapply(strsplit(dataCol_names, "_"), function(x) x[2])
  statCd <- sapply(strsplit(dataCol_names, "_"), function(x) x[3])
  
  pcodeINFO <- getParameterInfo(pCodes,interactive=FALSE)
  multipleCodes <- anyDuplicated(pCodes)
  
  statCd <- sub("00001", "_Max", statCd)
  statCd <- sub("00002", "_Min", statCd)
  statCd <- sub("00003", "", statCd) # Leave mean blank
  statCd <- sub("00011", "", statCd) # Also leaving blank
  
  DDnum <- sapply(strsplit(dataCol_names, "_"), function(x) x[1])
  DDnum <- gsub("X","",DDnum)
  
  if (!any(duplicated(pCodes))){
    dataColNames <- pcodeINFO$srsname[which(pcodeINFO$parameter_cd %in% pCodes)]    
    dataColNames <- paste(dataColNames,statCd,sep="")
  } else {
    dataColNames <- rep(NA,length(dataCol_names))    
    for (i in 1:length(dataCol_names)){
      dataColNames[i] <- pcodeINFO$srsname[which(pcodeINFO$parameter_cd %in% pCodes[i])]
      if((!(pCodes[i] %in% duplicated(pCodes))) && (pCodes[i] != pCodes[anyDuplicated(pCodes)])){
        dataColNames[i] <- paste(dataColNames[i],statCd[i],sep="")
      } else {
        dataColNames[i] <- paste(dataColNames[i],statCd[i],"_",DDnum[i],sep="")        
      }
      
    }
    
  }
  dataColCDS <- paste(dataColNames, "_cd")
  columnNames[which(columnNames %in% dataCol_names)] <- dataColNames
  columnNames[which(columnNames %in% dataCol_cds)] <- dataColCDS
  names(rawData) <- columnNames
  
  return(rawData)
}
