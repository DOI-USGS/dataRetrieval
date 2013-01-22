#' Raw Data Import for USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param ParameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param StatCd string USGS statistic code. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return retval dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @examples
#' # These examples require an internet connection to run
#' rawDailyFlowData <- retrieveNWISData('01594440','00060', '1985-01-01', '1985-01-31')
#' rawDailyTemperatureData <- retrieveNWISData('05114000','00010', '1985-01-01', '1985-01-31', StatCd='00001',interactive=FALSE)
retrieveNWISData <- function (siteNumber,ParameterCd,StartDate,EndDate,StatCd="00003",interactive=TRUE){  
  
  # Checking for 8 digit site ID:
  siteNumber <- formatCheckSiteNumber(siteNumber, interactive=interactive)
  
  # Check for 5 digit parameter code:
  ParameterCd <- formatCheckParameterCd(ParameterCd, interactive=interactive)
  
  # Check date format:
  StartDate <- formatCheckDate(StartDate, "StartDate", interactive=interactive)
  EndDate <- formatCheckDate(EndDate, "EndDate", interactive=interactive)
  
  # Check that start date happens before end date:
  dateReturn <- checkStartEndDate(StartDate, EndDate, interactive=interactive)
  StartDate <- dateReturn[1]
  EndDate <- dateReturn[2]
  
  baseURL <- "http://waterservices.usgs.gov/nwis/dv?site="
  
  url <- paste(baseURL,siteNumber, "&ParameterCd=",ParameterCd, "&StatCd=", StatCd, "&format=rdb,1.0", sep = "")
  
  if (nzchar(StartDate)) {
    url <- paste(url,"&startDT=",StartDate,sep="")
  } else url <- paste(url,"&startDT=","1851-01-01",sep="")
  
  if (nzchar(EndDate)) {
    url <- paste(url,"&endDT=",EndDate,sep="")
  }
  
  tmp <- read.delim(  
    url, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  retval <- lapply(tmp, function(x) {
    Typ <- x[1] # The type 
    x <- x[-c(1)] # the data - takes away the first 1st row (non-header)
    if(regexpr('d$', Typ) > 0) { # Must be date
      ret.val <- try(as.Date(x)) # The data are in standard format, but...
      if(class(ret.val) == "try-error")
        ret.val <- x
    }
#     else if(regexpr('n$', Typ) > 0) # Must be numeric...be careful of ice
#       ret.val <- as.numeric(x)
    else # Must be character
      ret.val <- x
    return(ret.val)})
  
  retval <- as.data.frame(retval, stringsAsFactors=FALSE)  
#   colNames <- names(retval)
  
  
  return (retval)
}