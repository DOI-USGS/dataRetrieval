#' Import NWIS Daily Data for EGRET analysis
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @param convert logical Option to include a conversion from cfs to cms (35.314667). The default is TRUE, 
#' which is appropriate for using NWIS data in the EGRET package.  Set this to FALSE to not include the conversion. If the parameter code is not 00060 (NWIS discharge),
#' there is no conversion applied.
#' @param format string, can be "tsv" or "xml", and is only applicable for daily and unit value requests.  "tsv" returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower, 
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the "tsv" option, 
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is "tsv".
#' @keywords data import USGS WRTDS
#' @export
#' @return Daily dataframe
#' @seealso \code{\link{getNWISdvData}}, \code{\link{populateDaily}}
#' @examples
#' # These examples require an internet connection to run
#' Daily <- getNWISDaily('01594440','00060', '1985-01-01', '1985-03-31')
#' DailyCFS <- getNWISDaily('01594440','00060', '1985-01-01', '1985-03-31',convert=FALSE)
#' DailySuspSediment <- getNWISDaily('01594440','80154', '1985-01-01', '1985-03-31')
getNWISDaily <- function (siteNumber,parameterCd,startDate,endDate,interactive=TRUE,convert=TRUE,format="tsv"){

  data <- getNWISdvData(siteNumber,parameterCd,startDate,endDate,interactive=interactive,format=format)
  
  #  need to setup conversion factor because the NWIS data are in cfs but we store in cms
  names(data) <- c('agency', 'site', 'dateTime', 'value', 'code')  # do a merge instead?
  
  qConvert <- ifelse("00060" == parameterCd, 35.314667, 1)
  qConvert<- ifelse(convert,qConvert,1)
  
  localDaily <- populateDaily(data,qConvert,interactive=interactive)
  return (localDaily)
}
