#' Raw Data Import for USGS NWIS QW Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param pCodes string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param expanded logical defaults to FALSE. If TRUE, retrieves additional information. Expanded data includes
#' remark_cd (remark code), result_va (result value), val_qual_tx (result value qualifier code), meth_cd (method code),
#' dqi_cd (data-quality indicator code), rpt_lev_va (reporting level), and rpt_lev_cd (reporting level type).
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @import reshape2
#' @examples
#' # These examples require an internet connection to run
#' siteNumber <- c('04024430','04024000')
#' startDate <- '2010-01-01'
#' endDate <- ''
#' pCodes <- c('34247','30234','32104','34220')
#' rawNWISqwData <- retrieveNWISqwData(siteNumber,pCodes,startDate,endDate)
#' rawNWISqwDataExpand <- retrieveNWISqwData(siteNumber,pCodes,startDate,endDate,expanded=TRUE)
#' # To get data in Sample dataframe format:
#' data <- rawNWISqwData[,names(rawNWISqwData) != "site"]
#' data$dateTime <- as.Date(data$dateTime)
#' compressedData <- compressData(data)
#' Sample <- populateSampleColumns(compressedData)
retrieveNWISqwData <- function (siteNumber,pCodes,startDate,endDate,expanded=FALSE,interactive=TRUE){  
  
  url <- constructNWISURL(siteNumber,pCodes,startDate,endDate,"qw",expanded=expanded,interactive=interactive)
  
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
  
  if(h$value()["Content-Type"] == "text/plain"){
  
    tmp <- read.delim(  
      textConnection(doc), 
      header = TRUE, 
      quote="\"", 
      dec=".", 
      sep='\t',
      colClasses=c('character'),
      fill = TRUE, 
      comment.char="#")
  
    dataType <- tmp[1,]
    data <- tmp[-1,]
    row.names(data) <- NULL
    
    
    
    if(expanded){
      data$site <- with(data,paste(agency_cd,site_no,sep="-"))
      data$dateTime <- with(data, as.POSIXct(paste(sample_dt,sample_tm,sep=" "),tz="UTC"))
      data$dateTimeEnd <- rep(as.POSIXct(NA), length(data$sample_end_tm))
      
      if (any("" != data[["sample_end_dt"]])){
        data$sample_end_dt["" == data$sample_end_dt] <- NA
        data$sample_end_tm["" == data$sample_end_tm] <- NA
        
        data$dateTimeEnd[!is.na(data$sample_end_tm) & !is.na(data$sample_end_dt)] <- as.POSIXct(paste(data$sample_end_dt[!is.na(data$sample_end_tm) & !is.na(data$sample_end_dt)],
                              data$sample_end_tm[!is.na(data$sample_end_tm) & !is.na(data$sample_end_dt)],sep=" "),tz="UTC")
      } 
      
      data$result_va <- as.numeric(data$result_va)
      data$rpt_lev_va <- as.numeric(data$rpt_lev_va)
      rmCol <- c("agency_cd","site_no","tm_datum_rlbty_cd",
                 "coll_ent_cd","medium_cd","tu_id","body_part_id",
                 "sample_end_dt","sample_end_tm","sample_dt","sample_tm",
                 "sample_start_time_datum_cd","anl_ent_cd","lab_std_va")
      data <- data[,!(names(data) %in% rmCol)]
      
      longDF <- melt(data, c("parm_cd","dateTime","site","dateTimeEnd"))
      wideDF <- dcast(longDF, ... ~ variable + parm_cd )
      wideDF[,grep("_va_",names(wideDF))] <- sapply(wideDF[,grep("_va_",names(wideDF))], function(x) as.numeric(x))
      
      data <- wideDF[,c(1,2,3,(3+order(sapply(strsplit(names(wideDF)[c(-1:-3)],"_"), function(x) x[length(x)]))))]
      if (all(is.na(data$dateTimeEnd))){
        data$dateTimeEnd <- NULL
      }    
      
    } else {
      data$site <- with(data,paste(agency_cd,site_no,sep="-"))
      data$dateTime <- with(data, as.POSIXct(paste(sample_dt,sample_tm,sep=" "),tz="UTC"))
      rmCol <- c("agency_cd","site_no","tm_datum_rlbty_cd",
                 "coll_ent_cd","medium_cd","tu_id","body_part_id",
                 "sample_end_dt","sample_end_tm","sample_dt","sample_tm","sample_start_time_datum_cd")
      data <- data[,!(names(data) %in% rmCol)]
      
      names(data) <- c(gsub("r", "qualifier_",names(data)[1:(length(names(data))-2)]),names(data)[(length(names(data))-1):length(names(data))])
      names(data) <- c(gsub("p", "value_",names(data)[1:(length(names(data))-2)]),names(data)[(length(names(data))-1):length(names(data))])
      
      data[,grep("value",names(data))] <- sapply( data[,grep("value",names(data))], function(x) as.numeric(x))
      
      data <- data[,c(ncol(data):(ncol(data)-1),(1:(ncol(data)-2)))]
    }
    
    return (data)
  } else {
    message(paste("URL caused an error:", url))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
}
