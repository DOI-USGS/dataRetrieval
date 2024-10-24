#' Construct NWIS url for data retrieval
#'
#' Using USGS water web services to construct urls.
#'
#' @param siteNumbers string or vector of strings USGS site number.  This is usually an 8 digit number
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param statCd string or vector USGS statistic code only used for daily value service.
#' This is usually 5 digits.  Daily mean (00003) is the default.
#' @param service string USGS service to call. Possible values are "dv" (daily values),
#' "uv" (unit/instantaneous values),
#' "gwlevels" (groundwater),and "rating" (rating curve),
#' "peak", "meas" (discrete streamflow measurements),
#' "stat" (statistics web service BETA).
#' @param format string, can be "tsv" or "xml", and is only applicable for daily
#' and unit value requests.  "tsv" returns results faster, but there is a possibility
#' that an incomplete file is returned without warning. XML is slower,
#' but will offer a warning if the file was incomplete (for example, if there was a
#' momentary problem with the internet connection). It is possible to safely use the "tsv" option,
#' but the user must carefully check the results to see if the data returns matches
#' what is expected. The default is therefore "xml".
#' @param expanded logical defaults to \code{TRUE}. If \code{TRUE}, retrieves additional
#' information, only applicable for qw data.
#' @param ratingType can be "base", "corr", or "exsa". Only applies to rating curve data.
#' @param statReportType character Only used for statistics service requests.  Time
#' division for statistics: daily, monthly, or annual.  Default is daily.
#' Note that daily provides statistics for each calendar day over the specified
#' range of water years, i.e. no more than 366 data points will be returned for
#' each site/parameter.  Use \code{readNWISdata} or \code{readNWISdv} for daily averages.
#' Also note that "annual" returns statistics for the calendar year.  Use
#' \code{readNWISdata} for water years. Monthly and yearly
#' provide statistics for each month and year within the range individually.
#' @param statType character Only used for statistics service requests. Type(s)
#' of statistics to output for daily values.  Default is mean, which is the only
#' option for monthly and yearly report types. See the statistics service documentation
#' at \url{https://waterservices.usgs.gov/docs/statistics/} for a
#' full list of codes.
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @examples
#' site_id <- "01594440"
#' startDate <- "1985-01-01"
#' endDate <- ""
#' pCode <- c("00060", "00010")
#' url_daily <- constructNWISURL(site_id, pCode,
#'   startDate, endDate, "dv",
#'   statCd = c("00003", "00001")
#' )
#' url_unit <- constructNWISURL(site_id, pCode, "2012-06-28", "2012-06-30", "iv")
#'
#' url_qw_single <- constructNWISURL(site_id, "01075", startDate, endDate, "qw")
#' url_qw <- constructNWISURL(
#'   site_id, c("01075", "00029", "00453"),
#'   startDate, endDate, "qw"
#' )
#' url_daily_tsv <- constructNWISURL(site_id, pCode, startDate, endDate, "dv",
#'   statCd = c("00003", "00001"), format = "tsv"
#' )
#' url_rating <- constructNWISURL(site_id, service = "rating", ratingType = "base")
#' url_peak <- constructNWISURL(site_id, service = "peak")
#' url_meas <- constructNWISURL(site_id, service = "meas")
#' url_gwl <- constructNWISURL(site_id, service = "gwlevels",
#'                             startDate = "2024-05-01", endDate = "2024-05-30")
constructNWISURL <- function(siteNumbers,
                             parameterCd = "00060",
                             startDate = "",
                             endDate = "",
                             service,
                             statCd = "00003",
                             format = "xml",
                             expanded = TRUE,
                             ratingType = "base",
                             statReportType = "daily",
                             statType = "mean") {
  
  service <- match.arg(service, c(
    "dv", "uv", "iv", "iv_recent", "qw", "gwlevels",
    "rating", "peak", "meas", "stat"))
  
  service[service == "meas"] <- "measurements"
  service[service == "uv"] <- "iv"
  
  baseURL <- httr2::request(pkg.env[[service]])
  
  if (any(!is.na(parameterCd) & parameterCd != "all")) {
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    
    if (!pcodeCheck) {
      badIndex <- which(nchar(parameterCd) != 5 | is.na(suppressWarnings(as.numeric(parameterCd))))
      stop("The following pCodes appear mistyped:", paste(parameterCd[badIndex], collapse = ", "))
    }
    
    if (length(parameterCd) > 200) {
      stop("Maximum parameter codes allowed is 200, please adjust data request.")
    }
  }

  switch(service,
         rating = {
           ratingType <- match.arg(ratingType, c("base", "corr", "exsa"))
           url <- httr2::req_url_query(baseURL, 
                                       site_no = siteNumbers, 
                                       file_type = ratingType)
         },
         peak = {
           url <- httr2::req_url_query(baseURL,
                              range_selection = "date_range",
                              format = "rdb")
           url <- httr2::req_url_query(url, 
                                       site_no = siteNumbers,
                                       .multi = "comma")
           if (nzchar(startDate)) {
             url <- httr2::req_url_query(url, begin_date = startDate)
           }
           if (nzchar(endDate)) {
             url <- httr2::req_url_query(url, end_date = endDate)
           }
         },
         measurements = {
           url <- httr2::req_url_query(baseURL,
                              site_no = siteNumbers,
                              range_selection = "date_range"
           )
           if (nzchar(startDate)) {
             url <- httr2::req_url_query(url,
                                begin_date = startDate
             )
           }
           if (nzchar(endDate)) {
             url <- httr2::req_url_query(url, end_date = endDate)
           }
           if (expanded) {
             url <- httr2::req_url_query(url, format = "rdb_expanded")
           } else {
             url <- httr2::req_url_query(url, format = "rdb")
           }
         },
         stat = { # for statistics service
           
           message("Please be aware the NWIS data service feeding this function is in BETA.\n
          Data formatting could be changed at any time, and is not guaranteed")
           
           # make sure only statTypes allowed for the statReportType are being requested
           if (!grepl("(?i)daily", statReportType) &&
               !all(grepl("(?i)mean", statType)) &&
               !all(grepl("(?i)all", statType))) {
             stop("Monthly and annual report types can only provide means")
           }
           
           # make sure dates aren't too specific for statReportType
           if (grepl("(?i)monthly", statReportType) &&
               (length(unlist(gregexpr("-", startDate))) > 1 ||
                length(unlist(gregexpr("-", endDate))) > 1)) {
             stop("Start and end dates for monthly statReportType can only include months and years")
           }
           if (grepl("(?i)annual", statReportType) && (grepl("-", startDate) || grepl("-", endDate))) {
             stop("Start and end dates for annual statReportType can only include years")
           }

           url <- httr2::req_url_query(baseURL,
                              sites = siteNumbers,
                              statReportType = statReportType)
           url <- httr2::req_url_query(url, statType = statType,
                                       .multi = "comma")
           url <- httr2::req_url_query(url, parameterCd = parameterCd, 
                                       .multi = "comma")
                                       
           if (nzchar(startDate)) {
             url <- httr2::req_url_query(url, startDT = startDate)
           }
           if (nzchar(endDate)) {
             url <- httr2::req_url_query(url, endDT = endDate)
           }
           if (!grepl("(?i)daily", statReportType)) {
             url <- httr2::req_url_query(url, missingData = "off")
           }
         },
         gwlevels = {
           
           url <- httr2::req_url_query(baseURL,
                              site_no = siteNumbers,.multi = "comma")
           url <- httr2::req_url_query(url,format = "rdb")
           if (nzchar(startDate)) {
             url <- httr2::req_url_query(url, begin_date = startDate)
           }
           if (nzchar(endDate)) {
             url <- httr2::req_url_query(url, end_date = endDate)
           }
           url <- httr2::req_url_query(url, 
                                       group_key = "NONE",
                                       date_format = "YYYY-MM-DD",
                                       rdb_compression = "value")
         },
         { # this will be either dv, uv, groundwater
           
           format <- match.arg(format, c("xml", "tsv", "wml1", "wml2", "rdb"))
           
           formatURL <- switch(format,
                               xml = "waterml,1.1",
                               rdb = "rdb,1.0",
                               tsv = "rdb,1.0",
                               wml2 = "waterml,2.0",
                               wml1 = "waterml,1.1"
           )
           
           url <- httr2::req_url_query(baseURL,
                                       site = siteNumbers,
                                       .multi = "comma") 
           url <- httr2::req_url_query(url, 
                                       format = formatURL)
           
           if (!all(is.na(parameterCd))) {
             url <- httr2::req_url_query(url, 
                                         ParameterCd = parameterCd,
                                         .multi = "comma")
           }
           
           if ("dv" == service) {
             url <- httr2::req_url_query(url, 
                                         StatCd = statCd,
                                         .multi = "comma")
           }
           
           if (nzchar(startDate)) {
             url <- httr2::req_url_query(url, startDT = startDate)
           } else {
             startorgin <- "1851-01-01"
             if ("iv" == service) startorgin <- "1900-01-01"
             url <- httr2::req_url_query(url, startDT = startorgin)
           }
           
           if (nzchar(endDate)) {
             url <- httr2::req_url_query(url, endDT = endDate)
           }
         }
  )
  
  url <- httr2::req_headers(url,
                     `Accept-Encoding` = c("compress", "gzip", "deflate")) 
  
  return(url)
}





#' Construct WQP url for data retrieval
#'
#' Construct WQP url for data retrieval. This function gets the data from here: \url{https://www.waterqualitydata.us}
#'
#' @param siteNumbers string or vector of strings USGS site number.  
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param legacy Logical. If TRUE, uses legacy WQP services. Default is TRUE.
#' Setting legacy = FALSE uses WQX3.0 WQP services, which are in-development, use with caution.
#' @keywords data import WQP web service
#' @return url string
#' @export
#' @examples
#' site_ids <- c("USGS-02292010", "USGS-02276877")
#' startDate <- "2020-01-01"
#' endDate <- ""
#' pCode <- c("80154", "00613")
#' url_wqp <- constructWQPURL(
#'   site_ids,
#'   pCode,
#'   startDate, endDate
#' )
#' url_wqp
#' charNames <- c(
#'   "Temperature",
#'   "Temperature, sample",
#'   "Temperature, water",
#'   "Temperature, water, deg F"
#' )
#' obs_url_orig <- constructWQPURL(
#'   siteNumbers = c(
#'     "IIDFG-41WSSPAHS",
#'     "USGS-02352560"
#'   ),
#'   parameterCd = charNames,
#'   startDate, ""
#' )
#' obs_url_orig
constructWQPURL <- function(siteNumbers,
                            parameterCd,
                            startDate,
                            endDate,
                            legacy = TRUE) {
  
  allPCode <- any(toupper(parameterCd) == "ALL")
  
  if(!allPCode){
    multiplePcodes <- length(parameterCd) > 1
    
    if (all(nchar(parameterCd) == 5)) {
      suppressWarnings(pCodeLogic <- all(!is.na(as.numeric(parameterCd))))
    } else {
      pCodeLogic <- FALSE
    }
  }
  
  if(legacy){
    baseURL <- httr2::request(pkg.env[["Result"]])
    siteNumbers <- paste(siteNumbers, collapse = ";")
    baseURL <- httr2::req_url_query(baseURL,
                                    siteids = siteNumbers)
  } else {
    baseURL <- httr2::request(pkg.env[["ResultWQX3"]])
    baseURL <- httr2::req_url_query(baseURL,
                                    siteids = siteNumbers,
                                    .multi = "explode" )
  }
  
  if(!allPCode){
    if(legacy){
      if (multiplePcodes) {
        parameterCd <- paste(parameterCd, collapse = ";")
        if(pCodeLogic){
          baseURL <- httr2::req_url_query(baseURL, pCode = parameterCd)
        } else {
          baseURL <- httr2::req_url_query(baseURL, characteristicName = parameterCd)
        }
      }
    } else {
      if(pCodeLogic){
        baseURL <- httr2::req_url_query(baseURL, 
                                        pCode = parameterCd, 
                                        .multi = "explode")
      } else {
        baseURL <- httr2::req_url_query(baseURL,
                                        characteristicName = parameterCd, 
                                        .multi = "explode")
      }
    }    
  }
  
  if (nzchar(startDate)) {
    startDate <- format(as.Date(startDate), format = "%m-%d-%Y")
    baseURL <- httr2::req_url_query(baseURL, 
                                    startDateLo = startDate)
  }
  
  if (nzchar(endDate)) {
    endDate <- format(as.Date(endDate), format = "%m-%d-%Y")
    baseURL <- httr2::req_url_query(baseURL,
                                    startDateHi = endDate)
  }
  
  baseURL <- httr2::req_url_query(baseURL, mimeType = "csv")
  if(!legacy){
    baseURL <- httr2::req_url_query(baseURL, 
                                    dataProfile = "basicPhysChem")
  }
  
  baseURL <- httr2::req_headers(baseURL,
                                `Accept-Encoding` = c("compress", "gzip", "deflate")) 
  
  return(baseURL)
}

#' Construct URL for NWIS water use data service
#'
#' Reconstructs URLs to retrieve data from here: \url{https://waterdata.usgs.gov/nwis/wu}
#'
#' @param years integer Years for data retrieval. Must be years ending in 0 or 5,
#' or "ALL", which retrieves all available years.
#' @param stateCd could be character (full name, abbreviation, id), or numeric (id)
#' @param countyCd could be numeric (County IDs from countyCdLookup) or character ("ALL")
#' @param categories character Two-letter cateogory abbreviation(s)
#' @return url string
#' @export
#' @examples
#' url <- constructUseURL(
#'   years = c(1990, 1995),
#'   stateCd = "Ohio",
#'   countyCd = c(1, 3),
#'   categories = "ALL"
#' )
#'
constructUseURL <- function(years, stateCd, countyCd, categories) {
  if (is.null(stateCd)) {
    baseURL <- drURL("useNat",
                     format = "rdb",
                     rdb_compression = "value",
                     Access = pkg.env$access
    )
  } else {
    stateCd <- stateCdLookup(input = stateCd, outputType = "postal")
    baseURL <- "https://waterdata.usgs.gov/"
    base2 <- "nwis/water_use?format=rdb&rdb_compression=value"
    baseURL <- paste0(baseURL, paste0(stateCd, "/"), base2)
    
    if (!is.null(countyCd)) {
      if (length(countyCd) > 1) {
        countyCd <- paste(countyCd, collapse = "%2C")
      }
      baseURL <- paste0(baseURL, "&wu_area=county&wu_county=", countyCd)
    } else {
      baseURL <- paste0(baseURL, "&wu_area=State%20Total")
    }
  }
  years <- paste(years, collapse = "%2C")
  categories <- paste(categories, collapse = "%2C")
  retURL <- paste0(baseURL, "&wu_year=", years, "&wu_category=", categories)
  
  return(retURL)
}