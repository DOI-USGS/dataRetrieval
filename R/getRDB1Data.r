#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @return data a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @examples
#' sites <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(sites,property,startDate,endDate,'dv',format='tsv')
#' data <- getRDB1Data(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),startDate,endDate,'dv',statCd=c("00003","00001"),'tsv')
#' multiData <- getRDB1Data(urlMulti)
getRDB1Data <- function(obs_url,asDateTime=FALSE){
  tmp <- read.delim(  
    obs_url, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  dataType <- tmp[1,]
  data <- tmp[-1,]
  
  if (asDateTime){
    data[,regexpr('d$', dataType) > 0] <- as.POSIXct(strptime(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M"))
  } else {
    data[,regexpr('d$', dataType) > 0] <- as.Date(data[,regexpr('d$', dataType) > 0])
  }
  
  tempDF <- data[,which(regexpr('n$', dataType) > 0)]
  tempDF <- suppressWarnings(sapply(tempDF, function(x) as.numeric(x)))  
  data[,which(regexpr('n$', dataType) > 0)] <- tempDF
  row.names(data) <- NULL
  return(data)
}
