#' Raw Data Import for USGS NWIS QW Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string or vector of of USGS site numbers.  This is usually an 8 digit number
#' @param pCodes string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param expanded logical defaults to FALSE. If TRUE, retrieves additional information. Expanded data includes
#' remark_cd (remark code), result_va (result value), val_qual_tx (result value qualifier code), meth_cd (method code),
#' dqi_cd (data-quality indicator code), rpt_lev_va (reporting level), and rpt_lev_cd (reporting level type).
#' @param reshape logical. Will reshape the data if TRUE (default)
#' @keywords data import USGS web service
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @import reshape2
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readWQPqw}}, \code{\link{constructNWISURL}}
#' @examples
#' siteNumber <- c('04024430','04024000')
#' startDate <- '2010-01-01'
#' endDate <- ''
#' pCodes <- c('34247','30234','32104','34220')
#' rawNWISqwData <- readNWISqw(siteNumber,pCodes,startDate,endDate)
#' rawNWISqwDataExpandReshaped <- readNWISqw(siteNumber,pCodes,
#'           startDate,endDate,expanded=TRUE)
#' rawNWISqwDataExpand <- readNWISqw(siteNumber,pCodes,
#'           startDate,endDate,expanded=TRUE,reshape=FALSE)
readNWISqw <- function (siteNumber,pCodes,startDate,endDate,expanded=FALSE,reshape=TRUE){  
  
  url <- constructNWISURL(siteNumber,pCodes,startDate,endDate,"qw",expanded=expanded)
  
  data <- importRDB1(url,asDateTime=TRUE, qw=TRUE)
  
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
    
  }
  
  return (data)

}
