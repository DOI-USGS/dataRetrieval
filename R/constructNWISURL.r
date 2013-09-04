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
#' @param format string, can be "tsv" or "xml", and is only applicable for daily and unit value requests.  "tsv" returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower, 
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the "tsv" option, 
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is therefore "xml". 
#' @param interactive logical Option for interactive mode.  If TRUE, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @examples
#' siteNumber <- '01594440'
#' startDate <- '1985-01-01'
#' endDate <- ''
#' pCode <- c("00060","00010")
#' url_daily <- constructNWISURL(siteNumber,pCode,startDate,endDate,'dv',statCd=c("00003","00001"))
#' url_unit <- constructNWISURL(siteNumber,pCode,"2012-06-28","2012-06-30",'iv')
#' url_qw_single <- constructNWISURL(siteNumber,"01075",startDate,endDate,'qw')
#' url_qw <- constructNWISURL(siteNumber,c('01075','00029','00453'),startDate,endDate,'qw')
#' url_wqp <- constructNWISURL(siteNumber,c('01075','00029','00453'),startDate,endDate,'wqp')
#' url_daily_tsv <- constructNWISURL(siteNumber,pCode,startDate,endDate,'dv',statCd=c("00003","00001"),format="tsv")
constructNWISURL <- function(siteNumber,parameterCd,startDate,endDate,service,statCd="00003", format="xml",interactive=TRUE){

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
          
          if ("xml"==format){ 
            format <- "waterml,1.1"
          } else if ("tsv" == format){
            format <- "rdb,1.0"
          } else {
            warning("non-supported format requested, please choose xml or tsv")
          }
             
          baseURL <- paste("http://waterservices.usgs.gov/nwis/",service,sep="")  
          
          url <- paste(baseURL,"/?site=",siteNumber, "&ParameterCd=",parameterCd, "&format=", format, sep = "")
          
          if("dv"==service) {
            if(length(statCd) > 1){
              statCd <- paste(statCd, collapse=",")
            }            
            url <- paste(url, "&StatCd=", statCd, sep = "")
          }
          
          if (nzchar(startDate)) {
            url <- paste(url,"&startDT=",startDate,sep="")
          } else {
            startorgin <- "1851-01-01"
            if ("iv" == service) startorgin <- "1900-01-01"            
            url <- paste(url,"&startDT=",startorgin,sep="")
          }
          
          if (nzchar(endDate)) {
            url <- paste(url,"&endDT=",endDate,sep="")
          }
        }
         
    )
  
  return(url)
}
