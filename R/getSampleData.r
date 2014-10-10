#' Import NWIS Sample Data for EGRET analysis
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#' For raw data, use getQWData.  This function will retrieve the raw data, and compress it (summing constituents). See
#' section 3.4 of the vignette for more details.
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return Sample dataframe
#' @seealso \code{\link{compressData}}, \code{\link{populateSampleColumns}}, , \code{\link{getNWISSample}}
#' @examples
#' # These examples require an internet connection to run
#' Sample_01075 <- getNWISSample('01594440','01075', '1985-01-01', '1985-03-31')
#' Sample_All2 <- getNWISSample('05114000',c('00915','00931'), '1985-01-01', '1985-03-31')
#' Sample_Select <- getNWISSample('05114000',c('00915','00931'), '', '')
getNWISSample <- function(siteNumber,parameterCd,startDate,endDate,interactive=TRUE){
  
  rawSample <- getNWISqwData(siteNumber,parameterCd,startDate,endDate)
  dataColumns <- grep("p\\d{5}",names(rawSample))
  remarkColumns <- grep("r\\d{5}",names(rawSample))
  totalColumns <-c(grep("sample_dt",names(rawSample)), dataColumns, remarkColumns)
  totalColumns <- totalColumns[order(totalColumns)]
  compressedData <- compressData(rawSample[,totalColumns], interactive=interactive)
  Sample <- populateSampleColumns(compressedData)
  return(Sample)
}


