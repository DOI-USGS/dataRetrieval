#' Raw Data Import for USGS NWIS QW Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumbers character of USGS site numbers.  This is usually an 8 digit number
#' @param pCodes character of USGS parameter code(s).  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param expanded logical defaults to TRUE. If TRUE, retrieves additional information. Expanded data includes
#' remark_cd (remark code), result_va (result value), val_qual_tx (result value qualifier code), meth_cd (method code),
#' dqi_cd (data-quality indicator code), rpt_lev_va (reporting level), and rpt_lev_cd (reporting level type).
#' @param reshape logical. Will reshape the data if TRUE (default)
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @keywords data import USGS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = TRUE), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' }
#' @export
#' @import reshape2
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readWQPqw}}, \code{\link{constructNWISURL}}
#' @examples
#' siteNumbers <- c('04024430','04024000')
#' startDate <- '2010-01-01'
#' endDate <- ''
#' pCodes <- c('34247','30234','32104','34220')
#' 
#' rawNWISqwData <- readNWISqw(siteNumbers,pCodes,startDate,endDate)
#' rawNWISqwDataReshaped <- readNWISqw(siteNumbers,pCodes,
#'           startDate,endDate,reshape=TRUE)
#'           
readNWISqw <- function (siteNumbers,pCodes,startDate="",endDate="",
                        expanded=TRUE,reshape=FALSE,tz=""){  
  
  url <- constructNWISURL(siteNumbers,pCodes,startDate,endDate,"qw",expanded=expanded)
  
  data <- importRDB1(url,asDateTime=TRUE, qw=TRUE, tz = tz)
  originalHeader <- comment(data)
  
  if(reshape & expanded){
    columnsToMelt <- c("agency_cd","site_no","sample_dt","sample_tm",
                       "sample_end_dt","sample_end_tm","sample_start_time_datum_cd","tm_datum_rlbty_cd",
                       "parm_cd","startDateTime","endDateTime")
    columnsToMelt <- columnsToMelt[columnsToMelt %in% names(data)]
    longDF <- melt(data, columnsToMelt)
    wideDF <- dcast(longDF, ... ~ variable + parm_cd )
    wideDF[,grep("_va_",names(wideDF))] <- sapply(wideDF[,grep("_va_",names(wideDF))], function(x) as.numeric(x))
    
    groupByPCode <- as.vector(sapply(pCodes, function(x) grep(x, names(wideDF)) ))
    data <- wideDF[,c(1:length(columnsToMelt)-1,groupByPCode)]
    comment(data) <- originalHeader
    
  }
  
  siteInfo <- readNWISsite(siteNumber)
  varInfo <- readNWISpCode(pCodes)
  
  attr(data, "siteInfo") <- siteInfo
  attr(data, "variableInfo") <- varInfo
  attr(data, "statisticInfo") <- NULL
  
  return (data)

}
