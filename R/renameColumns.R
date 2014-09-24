#' renameColumns
#'
#' Rename columns coming back from NWIS data retrievals
#'
#' @param rawData dataframe returned from retrieval functions
#' @keywords data import USGS web service
#' @return rawData dataframe with improved column names
#' @export
#' @examples
#' # This example requires an internet connection to run
#' siteNumber <- '05114000' 
#' rawData <- getNWISdvData(siteNumber,c("00010","00060","00300"),
#'           "2001-01-01","2002-01-01",statCd=c("00001","00003"))
#' rawData <- renameColumns(rawData)
#' today <- as.character(Sys.Date())
#' rawData2 <- getNWISunitData(siteNumber,c("00010","00060"),today,today)
#' rawData2 <- renameColumns(rawData2)
renameColumns <- function(rawData){
  
  columnNames <- names(rawData)
  
  dataCols <- columnNames["X" == substring(columnNames, 1, 1)]
  dataCol_cds <- dataCols["cd" == substring(dataCols, nchar(dataCols)-1, nchar(dataCols))]
  dataCol_names <- dataCols[!(dataCols %in% dataCol_cds)]
  
  pCodes <- sapply(strsplit(dataCol_names, "_"), function(x) x[2])
  statCd <- sapply(strsplit(dataCol_names, "_"), function(x) x[3])
  
  pcodeINFO <- getNWISPcodeInfo(pCodes,interactive=FALSE)
  multipleCodes <- anyDuplicated(pCodes)
  
  statCd <- sub("00001", "_Max", statCd)
  statCd <- sub("00002", "_Min", statCd)
  statCd <- sub("00003", "", statCd) # Leave mean blank
  statCd <- sub("00011", "", statCd) # Also leaving blank
  
  DDnum <- sapply(strsplit(dataCol_names, "_"), function(x) x[1])
  DDnum <- gsub("X","",DDnum)
  
  if (!any(duplicated(pCodes))){
    dataColNames <- pcodeINFO$parameter_nm[which(pcodeINFO$parameter_cd %in% pCodes)]    
#     dataColNames <- pcodeINFO$srsname[which(pcodeINFO$parameter_cd %in% pCodes)]  
    dataColNames <- paste(dataColNames,statCd,sep="")
  } else {
    dataColNames <- rep(NA,length(dataCol_names))    
    for (i in 1:length(dataCol_names)){
      dataColNames[i] <- pcodeINFO$parameter_nm[which(pcodeINFO$parameter_cd %in% pCodes[i])]
#       dataColNames[i] <- pcodeINFO$srsname[which(pcodeINFO$parameter_cd %in% pCodes[i])]
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
  
  columnNames <- gsub("[$,. ]","_",columnNames)
  columnNames <- gsub("__","_",columnNames)
  
  names(rawData) <- columnNames
  
  return(rawData)
}
