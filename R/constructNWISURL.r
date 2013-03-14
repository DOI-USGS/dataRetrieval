#' Construct NWIS url for data retrieval
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param statCd string or vector USGS statistic code only used for daily value service. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param service string USGS service to call. Possible values are "dv" (daily values), "uv" (unit/instantaneous values), "qw" (water quality data), and "wqp" (water quality portal, which can include STORET).
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @examples
#' siteNumber <- '04085427'
#' startDate <- '2012-01-01'
#' endDate <- '2012-06-30'
#' pCode <- c("00060","00010")
#' url_daily <- constructNWISURL(siteNumber,pCode,startDate,endDate,'dv',statCd=c("00003","00001"))
#' url_unit <- constructNWISURL(siteNumber,pCode,startDate,endDate,'iv')
#' url_qw_single <- constructNWISURL(siteNumber,"34220",startDate,endDate,'qwdata')
#' url_qw <- constructNWISURL(siteNumber,c('34247','30234','32104','34220'),startDate,endDate,'qwdata')
#' url_wqp <- constructNWISURL(siteNumber,"34220",startDate,endDate,'wqp')
constructNWISURL <- function(siteNumber,parameterCd,startDate,endDate,service,statCd="00003"){

  startDate <- formatCheckDate(startDate, "StartDate", interactive=interactive)
  endDate <- formatCheckDate(endDate, "EndDate", interactive=interactive)
  
  dateReturn <- checkStartEndDate(startDate, endDate, interactive=interactive)
  startDate <- dateReturn[1]
  endDate <- dateReturn[2]
  
  switch(service,
         qw = {
             if(length(siteNumber) > 1){    
               siteNumber <- paste(siteNumber, collapse=",")
               siteNumber <- paste("multiple_site_no",siteNumber,sep="=")
               searchCriteria <- "multiple_site_no"
             } else {
               siteNumber <- paste("search_site_no",siteNumber,sep="=")
               siteNumber <- paste(siteNumber,"search_site_no_match_type=exact",sep="&")
               searchCriteria <- "search_site_no"
             }
             
             if(length(parameterCd)>1){
               pCodes <- paste(parameterCd, collapse=",")
               pCodes <- paste('multiple_parameter_cds', pCodes, sep="=")
               pCodes <- paste(pCodes, "param_cd_operator=OR",sep="&")
             } else {
               pCodes <- paste("multiple_parameter_cds", parameterCd, sep="=")
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
           },
         wqp = {
           siteNumber <- formatCheckSiteNumber(siteNumber, interactive=interactive)
           
           if(length(parameterCd)>1){
             parameterCd <- paste(parameterCd, collapse=";")
           }
           
           if (nzchar(startDate)){
             startDate <- format(as.Date(startDate), format="%m-%d-%Y")
           }
           if (nzchar(endDate)){
             endDate <- format(as.Date(endDate), format="%m-%d-%Y")
           }
           
           baseURL <- "http://www.waterqualitydata.us/Result/search?siteid=USGS-"
           url <- paste(baseURL,
                        siteNumber,
                        "&pCode=",
                        parameterCd,
                        "&startDateLo=",
                        startDate,
                        "&startDateHi=",
                        endDate,
                        "&countrycode=US&mimeType=tsv",sep = "")
           },
        { # this will be either dv or uv
          siteNumber <- formatCheckSiteNumber(siteNumber, interactive=interactive)
             
          # Check for 5 digit parameter code:
          if(length(parameterCd)>1){
            parameterCd <- paste(parameterCd, collapse=",")
          } else {
            parameterCd <- formatCheckParameterCd(parameterCd, interactive=interactive)
          }
          
          if ("uv"==service) service <- "iv"
             
          baseURL <- paste("http://waterservices.usgs.gov/nwis/",service,sep="")  
          
          url <- paste(baseURL,"?site=",siteNumber, "&ParameterCd=",parameterCd, "&format=rdb,1.0", sep = "")
          
          if("dv"==service) {
            if(length(statCd) > 1){
              statCd <- paste(statCd, collapse=",")
            }            
            
            url <- paste(url, "&StatCd=", statCd, sep = "")
          }
          
          if (nzchar(startDate)) {
            url <- paste(url,"&startDT=",startDate,sep="")
          } else {
            url <- paste(url,"&startDT=","1851-01-01",sep="")
          }
          
          if (nzchar(endDate)) {
            url <- paste(url,"&endDT=",endDate,sep="")
          }
        }
         
    )
  
  return(url)
}