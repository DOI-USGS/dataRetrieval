#' Raw Data Import for Instantaneous USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param format string, can be "tsv" or "xml", and is only applicable for daily and unit value requests.  "tsv" returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower, 
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the "tsv" option, 
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is therefore "xml". 
#' @keywords data import USGS web service
#' @return data dataframe with agency, site, dateTime, time zone, value, and code columns
#' @export
#' @examples
#' siteNumber <- '05114000'
#' parameterCd <- '00060'
#' startDate <- "2014-10-10"
#' endDate <- "2014-10-10"
#' # These examples require an internet connection to run
#' rawData <- readNWISunit(siteNumber,parameterCd,startDate,endDate)
#' summary(rawData)
#' rawData2 <- readNWISunit(siteNumber,parameterCd,startDate,endDate,"tsv")
#' summary(rawData2)
#' timeZoneChange <- readNWISunit(siteNumber,parameterCd,
#'          "2013-11-03","2013-11-03","tsv")
readNWISunit <- function (siteNumber,parameterCd,startDate,endDate,format="xml"){  
  
  url <- constructNWISURL(siteNumber,parameterCd,startDate,endDate,"uv",format=format)
  if (format == "xml") {
    data <- importWaterML1(url,asDateTime=TRUE)
  } else {
    data <- importRDB1(url,asDateTime=TRUE)
  }

  return (data)
}

#' Reads peak flow data from NWISweb.
#' 
#' 
#' 
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @export
#' @examples
#' siteNumber <- '01594440'
#' data <- readNWISpeak(siteNumber, '','')
readNWISpeak <- function (siteNumber,startDate,endDate){  
  
  url <- constructNWISURL(siteNumber,NA,startDate,endDate,"peak")
  data <- importRDB1(url)
    
  return (data)
}

#' Reads the current rating table for an active USGS streamgage.
#' 
#' 
#' 
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param type string can be "base", "corr", or "exsa"
#' @export
#' @examples
#' siteNumber <- '01594440'
#' data <- readNWISrating(siteNumber, "base")
readNWISrating <- function (siteNumber,type){  
  
  url <- constructNWISURL(siteNumber,service="rating",ratingType = type)
  data <- importRDB1(url)
  
  return (data)
}

#'Reads surface-water measurement data from NWISweb.
#'
#'
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @export
#' @examples
#' siteNumber <- '01594440'
#' data <- readNWISmeas(siteNumber, '','')
readNWISmeas <- function (siteNumber,startDate,endDate){  
  
  url <- constructNWISURL(siteNumber,NA,startDate,endDate,"meas")
  data <- importRDB1(url)
  
  return (data)
}
