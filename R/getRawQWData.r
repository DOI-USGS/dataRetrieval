#' Raw Data Import for USGS NWIS Water Quality Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://www.waterqualitydata.us}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd vector of USGS 5-digit parameter code or string of characteristicNames. Leaving this blank will return all of the measured values during the specified time period.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @import RCurl
#' @examples
#' # These examples require an internet connection to run
#' rawSample <- retrieveWQPqwData('USGS-01594440','01075', '1985-01-01', '1985-03-31')
#' rawSampleAll <- retrieveWQPqwData('USGS-05114000','', '1985-01-01', '1985-03-31')
#' rawSampleSelect <- retrieveWQPqwData('USGS-05114000',c('00915','00931'), '1985-01-01', '1985-04-30')
#' rawStoret <- retrieveWQPqwData('WIDNR_WQX-10032762','Specific conductance', '', '')
retrieveWQPqwData <- function(siteNumber,parameterCd,startDate,endDate,interactive=TRUE){

  url <- constructNWISURL(siteNumber,parameterCd,startDate,endDate,"wqp",interactive=interactive)

  retval = tryCatch({
    h <- basicHeaderGatherer()
    doc <- getURL(url, headerfunction = h$update)

  }, warning = function(w) {
    message(paste("URL caused a warning:", url))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", url))
    message(e)
    return(NA)
  })
  
  if(h$value()["Content-Type"] == "text/tab-separated-values;charset=UTF-8"){
  
    numToBeReturned <- as.numeric(h$value()["Total-Result-Count"])
    
    if (!is.na(numToBeReturned) | numToBeReturned != 0){
    
      retval <- read.delim(textConnection(doc), header = TRUE, quote="\"", 
                 dec=".", sep='\t', 
                 colClasses=c('character'), 
                 fill = TRUE)    
      actualNumReturned <- nrow(retval)
      
      if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
      
      return(retval)
  
    } else {
      warning("No data to retrieve")
      return(NA)
    }
  } else {
    message(paste("URL caused an error:", url))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
  
}
