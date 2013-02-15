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
#' @keywords data import USGS web service
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @examples
#' # These examples require an internet connection to run
#' siteNumber <- c('04024430','04024000')
#' startDate <- '2010-01-01'
#' endDate <- ''
#' pCodes <- c('34247', '30234','32104','34220')
#' rawNWISqwData <- retrieveNWISqwData(siteNumber,pCodes,startDate,endDate)
#' # To get data in Sample dataframe format:
#' data <- rawNWISqwData[,names(rawNWISqwData) != "site"]
#' data$dateTime <- as.Date(data$dateTime)
#' compressedData <- compressData(data, interactive=interactive)
#' Sample <- populateSampleColumns(compressedData)
retrieveNWISqwData <- function (siteNumber,pCodes,startDate,endDate){  
  
  if(length(siteNumber) > 1){    
    siteNumber <- paste(siteNumber, collapse=",")
    siteNumber <- paste("multiple_site_no",siteNumber,sep="=")
    searchCriteria <- "multiple_site_no"
  } else {
    siteNumber <- paste("search_site_no",siteNumber,sep="=")
    siteNumber <- paste(siteNumber,"search_site_no_match_type=exact",sep="&")
    searchCriteria <- "search_site_no"
  }
  
  if(length(pCodes)>1){
    pCodes <- paste(pCodes, collapse=",")
    pCodes <- paste('multiple_parameter_cds', pCodes, sep="=")
    pCodes <- paste(pCodes, "param_cd_operator=OR",sep="&")
  } else {
    pCodes <- paste("multiple_parameter_cds", pCodes, sep="=")
    pCodes <- paste(pCodes, "param_cd_operator=AND",sep="&")
  }
  
  searchCriteria <- paste(searchCriteria, "multiple_parameter_cds", sep=",")
  searchCriteria <- paste("list_of_search_criteria",searchCriteria,sep="=")
  baseURL <- "http://nwis.waterdata.usgs.gov/nwis/qwdata"
  
  url <- paste(baseURL,siteNumber,sep="?")
  url <- paste(url, pCodes,searchCriteria,
               "group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd",
               "column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file",
               "TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0",
               "format=rdb&qw_sample_wide=separated_wide&rdb_qw_attributes=0&date_format=YYYY-MM-DD",
               "rdb_compression=value", sep = "&")
  
  if (nzchar(startDate)) {
    url <- paste(url,"&begin_date=",startDate,sep="")
  }
  
  if (nzchar(endDate)) {
    url <- paste(url,"&end_date=",endDate,sep="")
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
  
  # This takes slightly longer than the original method, but handles "Ice" or other characters in the numeric columns without error.
  dataType <- tmp[1,]
  data <- tmp[-1,]
  row.names(data) <- NULL
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
  
  return (data)
}