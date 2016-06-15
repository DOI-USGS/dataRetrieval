#' Construct NWIS url for data retrieval
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param statCd string or vector USGS statistic code only used for daily value service. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param service string USGS service to call. Possible values are "dv" (daily values), "uv" (unit/instantaneous values), 
#'  "qw" (water quality data), "gwlevels" (groundwater),and "rating" (rating curve), "peak", "meas" (discrete streamflow measurements).
#' @param format string, can be "tsv" or "xml", and is only applicable for daily and unit value requests.  "tsv" returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower, 
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the "tsv" option, 
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is therefore "xml". 
#' @param expanded logical defaults to \code{TRUE}. If \code{TRUE}, retrieves additional information, only applicable for qw data.
#' @param ratingType can be "base", "corr", or "exsa". Only applies to rating curve data.
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @import utils
#' @examples
#' siteNumber <- '01594440'
#' startDate <- '1985-01-01'
#' endDate <- ''
#' pCode <- c("00060","00010")
#' url_daily <- constructNWISURL(siteNumber,pCode,
#'            startDate,endDate,'dv',statCd=c("00003","00001"))
#' url_unit <- constructNWISURL(siteNumber,pCode,"2012-06-28","2012-06-30",'iv')
#' 
#' url_qw_single <- constructNWISURL(siteNumber,"01075",startDate,endDate,'qw')
#' url_qw <- constructNWISURL(siteNumber,c('01075','00029','00453'),
#'            startDate,endDate,'qw')
#' url_daily_tsv <- constructNWISURL(siteNumber,pCode,startDate,endDate,'dv',
#'            statCd=c("00003","00001"),format="tsv")
#' url_rating <- constructNWISURL(siteNumber,service="rating",ratingType="base")
#' url_peak <- constructNWISURL(siteNumber, service="peak")
#' url_meas <- constructNWISURL(siteNumber, service="meas")
#' urlQW <- constructNWISURL("450456092225801","70300",startDate="",endDate="","qw",expanded=TRUE)
constructNWISURL <- function(siteNumber,parameterCd="00060",startDate="",endDate="",
                             service,statCd="00003", format="xml",expanded=TRUE,
                             ratingType="base",statReportType="daily",statType="mean"){

  service <- match.arg(service, c("dv","uv","iv","qw","gwlevels","rating","peak","meas","stat"))
  
  if(any(!is.na(parameterCd) & parameterCd != "all")){
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    
    if(!pcodeCheck){
      badIndex <- which(nchar(parameterCd) != 5 | is.na(suppressWarnings(as.numeric(parameterCd))))
      stop("The following pCodes appear mistyped:",paste(parameterCd[badIndex],collapse=","))
    }
    
    if(length(parameterCd) > 200){
      stop("Maximum parameter codes allowed is 200, please adjust data request.")
    }
  }
  
  multipleSites <- length(siteNumber) > 1
  
  siteNumber <- paste(siteNumber, collapse=",")
  
  switch(service,
         qw = {
             if(multipleSites){    
               
               siteNumber <- paste("multiple_site_no",siteNumber,sep="=")
               searchCriteria <- "multiple_site_no"
             } else {
               siteNumber <- paste("search_site_no",siteNumber,sep="=")
               siteNumber <- paste(siteNumber,"search_site_no_match_type=exact",sep="&")
               searchCriteria <- "search_site_no"
             }
             
             multiplePcodes <- length(parameterCd)>1
             
             if(multiplePcodes){
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
                          "format=rdb&rdb_qw_attributes=0&date_format=YYYY-MM-DD",
                          "rdb_compression=value", sep = "&")
             if(expanded){
               url <- paste0(url,"&qw_sample_wide=0")
               url <- gsub("rdb_qw_attributes=0","rdb_qw_attributes=expanded",url)
             } else {
               url <- paste0(url,"&qw_sample_wide=separated_wide")
             }
             
             if (nzchar(startDate)) {
               url <- paste(url,"&begin_date=",startDate,sep="")
             }
             
             if (nzchar(endDate)) {
               url <- paste(url,"&end_date=",endDate,sep="")
             }
           },
        rating = {
          ratingType <- match.arg(ratingType, c("base", "corr", "exsa"))
          url <- paste0("http://waterdata.usgs.gov/nwisweb/get_ratings?site_no=",
                siteNumber, "&file_type=", ratingType)
        },
        peak = {
          url <- paste0("http://nwis.waterdata.usgs.gov/usa/nwis/peak/?site_no=", siteNumber,
                "&range_selection=date_range&format=rdb")
          if (nzchar(startDate)) {
            url <- paste0(url,"&begin_date=",startDate)
          }
          if(nzchar(endDate)){
            url <- paste0(url, "&end_date=", endDate)
          }
        },
        meas = {
          url <- paste0("http://waterdata.usgs.gov/nwis/measurements?site_no=", siteNumber,
                "&range_selection=date_range")
          if (nzchar(startDate)) {
            url <- paste0(url,"&begin_date=",startDate)
          }
          if(nzchar(endDate)){
            url <- paste0(url, "&end_date=", endDate)
          }
          if(expanded){
            url <- paste0(url,"&format=rdb_expanded")
          } else {
            url <- paste0(url,"&format=rdb")
          }

        },
        stats = { #for statistics service
          #make sure only allowed statTypes are being requested
          if(!grepl("(?i)daily",statReportType) && !all(grepl("(?i)mean",statType)) && !all(grepl("(?i)all",statType))){
            stop("Monthly and yearly report types can only provide means")
          }
          statType <- paste(statType,collapse=",")
          parameterCd <- paste(parameterCd,collapse=",")
          url <- paste0("http://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=",siteNumber,
                        "&statType=",statType,"&statReportType=",statReportType,"&parameterCd=",parameterCd)
          if (nzchar(startDate)) {
            url <- paste0(url,"&begin_date=",startDate)
          }
          if (nzchar(endDate)) {
            url <- paste0(url,"&end_date=",endDate)
          }
          if (!grepl("(?i)daily",statReportType)){
            url <- paste0(url,"&missingData=off")
          }
          
        },
        
        { # this will be either dv or uv
          multiplePcodes <- length(parameterCd)>1
          # Check for 5 digit parameter code:
          if(multiplePcodes){
            parameterCd <- paste(parameterCd, collapse=",")
          } 
          
          if ("uv"==service) {
            service <- "iv"
          }
          
          format <- match.arg(format, c("xml","tsv","wml1","wml2","rdb"))
          
          formatURL <- switch(format,
            xml = {if ("gwlevels" == service) {
                "waterml"
              } else {
                "waterml,1.1"
              }
            },
            rdb = "rdb,1.0",
            tsv = "rdb,1.0",
            wml2 = "waterml,2.0",
            wml1 = {if ("gwlevels" == service) {
                "waterml"
              } else {
                "waterml,1.1"
              }
            }
          )

          url <- drURL(service, Access=pkg.env$access, site=siteNumber, format=formatURL)
          
          if("gwlevels"!= service){
            url <- appendDrURL(url, ParameterCd=parameterCd)
          }
          
          if("dv"==service) {
            if(length(statCd) > 1){
              statCd <- paste(statCd, collapse=",")
            }            
            url <- appendDrURL(url, StatCd=statCd)
          }
          
          if (nzchar(startDate)) {
            url <- appendDrURL(url, startDT=startDate)
          } else {
            startorgin <- "1851-01-01"
            if ("iv" == service) startorgin <- "1900-01-01"            
            url <- appendDrURL(url, startDT=startorgin)
          }
          
          if (nzchar(endDate)) {
            url <- appendDrURL(url, endDT=endDate)
          }
        }
         
    )

  return(url)
}





#' Construct WQP url for data retrieval
#'
#' Imports data from WQP web service. This function gets the data from here: \url{http://nwis.waterdata.usgs.gov/nwis/qwdata}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param zip logical to request data via downloading zip file. Default set to FALSE.
#' @keywords data import WQP web service
#' @return url string
#' @export
#' @examples
#' siteNumber <- '01594440'
#' startDate <- '1985-01-01'
#' endDate <- ''
#' pCode <- c("00060","00010")
#' url_wqp <- constructWQPURL(paste("USGS",siteNumber,sep="-"),
#'            c('01075','00029','00453'),
#'            startDate,endDate)
constructWQPURL <- function(siteNumber,parameterCd,startDate,endDate,zip=FALSE){
  
  multipleSites <- length(siteNumber) > 1
  multiplePcodes <- length(parameterCd)>1
  siteNumber <- paste(siteNumber, collapse=";")

  if(all(nchar(parameterCd) == 5)){
    suppressWarnings(pCodeLogic <- all(!is.na(as.numeric(parameterCd))))
  } else {
    pCodeLogic <- FALSE
    parameterCd <- URLencode(parameterCd, reserved = TRUE)
  }
  
  if(multiplePcodes){
    parameterCd <- paste(parameterCd, collapse=";")
  }
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?siteid="
  url <- paste0(baseURL,
                siteNumber,
                ifelse(pCodeLogic,"&pCode=","&characteristicName="),
                parameterCd)
  
  if (nzchar(startDate)){
    startDate <- format(as.Date(startDate), format="%m-%d-%Y")
    url <- paste0(url, "&startDateLo=",startDate)
  }
  
  if (nzchar(endDate)){
    endDate <- format(as.Date(endDate), format="%m-%d-%Y")
    url <- paste0(url, "&startDateHi=",endDate)
  }
  
  url <- paste0(url,"&sorted=no&mimeType=tsv")
  
  if(zip){
    url <- paste0(url,"&zip=yes")
  }
  
  return(url)

}
