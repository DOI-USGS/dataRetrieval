#' Raw Data Import for Instantaneous USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}.
#' More information on the web service can be found here: \url{http://waterservices.usgs.gov/rest/IV-Service.html}.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param tz character to set timezone attribute of dateTime. Default is an empty quote, which converts the 
#' dateTimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @keywords data import USGS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' dateTime \tab POSIXct \tab The date and time of the value converted to UTC \cr 
#' tz_cd \tab character \tab The time zone code for dateTime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form: 
#' X_D_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' statisticInfo \tab data.frame \tab A data frame containing information on the requested statistics on the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' 
#' @seealso \code{\link{renameNWISColumns}}, \code{\link{importWaterML1}}
#' @export
#' @examples
#' siteNumber <- '05114000'
#' parameterCd <- '00060'
#' startDate <- "2014-10-10"
#' endDate <- "2014-10-10"
#' # These examples require an internet connection to run
#' rawData <- readNWISuv(siteNumber,parameterCd,startDate,endDate)
#' \dontrun{
#' timeZoneChange <- readNWISuv(c('04024430','04024000'),parameterCd,
#'          "2013-11-03","2013-11-03")
#' }
readNWISuv <- function (siteNumbers,parameterCd,startDate="",endDate="", tz=""){  
  
  url <- constructNWISURL(siteNumbers,parameterCd,startDate,endDate,"uv",format="xml")

  data <- importWaterML1(url,asDateTime=TRUE,tz=tz)

  return (data)
}

#' Reads peak flow data from NWISweb.
#' 
#' Reads peak flow from NWISweb. 
#' Data is retrieved from \url{http://waterdata.usgs.gov/nwis}. 
#' 
#' @param siteNumbers character USGS site number(or multiple sites).  This is usually an 8 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' peak_dt \tab Date \tab Date of peak streamflow \cr
#' peak_tm \tab character \tab Time of peak streamflow as character \cr
#' peak_va \tab numeric \tab Annual peak streamflow value in cfs \cr
#' peak_cd \tab character \tab Peak Discharge-Qualification codes (see \code{comment} for more information) \cr
#' gage_ht \tab numeric \tab Gage height for the associated peak streamflow in feet \cr
#' gage_ht_cd \tab character \tab Gage height qualification codes \cr
#' year_last_pk \tab character \tab Peak streamflow reported is the highest since this year \cr
#' ag_dt \tab character \tab Date of maximum gage-height for water year (if not concurrent with peak) \cr
#' ag_tm \tab character \tab Time of maximum gage-height for water year (if not concurrent with peak) \cr
#' ag_gage_ht \tab character \tab maximum Gage height for water year in feet (if not concurrent with peak) \cr
#' ag_gage_ht_cd \tab character \tab maximum Gage height code \cr
#' }
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' }
#' @export
#' @examples
#' siteNumbers <- c('01594440','040851325')
#' data <- readNWISpeak(siteNumbers)
readNWISpeak <- function (siteNumbers,startDate="",endDate=""){  
  
  # Doesn't seem to be a peak xml service
  url <- constructNWISURL(siteNumbers,NA,startDate,endDate,"peak")
  
  data <- importRDB1(url, asDateTime=FALSE)
  
  data$peak_dt <- as.Date(data$peak_dt)
  data$gage_ht <- as.numeric(data$gage_ht)
  
  siteInfo <- readNWISsite(siteNumbers)
  
  attr(data, "siteInfo") <- siteInfo
  attr(data, "variableInfo") <- NULL
  attr(data, "statisticInfo") <- NULL
    
  return (data)
}

#' Reads the current rating table for an active USGS streamgage.
#' 
#' Reads current rating table for an active USGS streamgage from NWISweb. 
#' Data is retrieved from \url{http://waterdata.usgs.gov/nwis}.
#' 
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param type character can be "base", "corr", or "exsa"
#' @return A data frame. If \code{type} is "base," then the columns are
#'INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#'in cubic feet per second; and STOR, where "*" indicates that the pair are
#'a fixed point of the rating curve. If \code{type} is "exsa," then an
#'additional column, SHIFT, is included that indicates the current shift in
#'the rating for that value of INDEP. If \code{type} is "corr," then the
#'columns are INDEP, typically the gage height, in feet; CORR, the correction
#'for that value; and CORRINDEP, the corrected value for CORR.\cr
#'If \code{type} is "base," then the data frame has an attribute called "RATING"
#'that describes the rating curve is included.
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' RATING \tab character \tab Rating information \cr
#' }
#'
#' @note Not all active USGS streamgages have traditional rating curves that
#'relate flow to stage.
#' @export
#' @examples
#' siteNumber <- '01594440'
#' data <- readNWISrating(siteNumber, "base")
#' attr(data, "RATING")
readNWISrating <- function (siteNumber,type="base"){  
  
  # No rating xml service 
  url <- constructNWISURL(siteNumber,service="rating",ratingType = type)
    
  data <- importRDB1(url, asDateTime=FALSE)
  
  if(type == "base") {
    Rat <- grep("//RATING ", comment(data), value=TRUE, fixed=TRUE)
    Rat <- sub("# //RATING ", "", Rat)
    Rat <- scan(text=Rat, sep=" ", what="")
    attr(data, "RATING") <- Rat
  }
  
  siteInfo <- readNWISsite(siteNumber)
  
  attr(data, "siteInfo") <- siteInfo
  attr(data, "variableInfo") <- NULL
  attr(data, "statisticInfo") <- NULL
  
  return (data)
}

#'Reads surface-water measurement data from NWISweb.
#'
#'Reads surface-water measurement data from NWISweb. Data is retrieved from \url{http://waterdata.usgs.gov/nwis}.
#'See \url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param tz character to set timezone attribute of dateTime. Default is an empty quote, which converts the 
#' dateTimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' tz_cd \tab character \tab The time zone code for dateTime \cr
#' }
#' 
#' See \url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water, and 
#' \url{http://waterdata.usgs.gov/nwis/help?output_formats_help#streamflow_measurement_data}
#' for help on the columns and codes.
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' }
#' @export
#' @examples
#' siteNumbers <- c('01594440','040851325')
#' data <- readNWISmeas(siteNumbers)
readNWISmeas <- function (siteNumbers,startDate="",endDate="", tz=""){  
  
  # Doesn't seem to be a WaterML1 format option
  url <- constructNWISURL(siteNumbers,NA,startDate,endDate,"meas")
  
  data <- importRDB1(url,asDateTime=FALSE,tz=tz)
  
  if("diff_from_rating_pc" %in% names(data)){
    data$diff_from_rating_pc <- as.numeric(data$diff_from_rating_pc)
  }
  
  siteInfo <- readNWISsite(siteNumbers)
  
  attr(data, "siteInfo") <- siteInfo
  attr(data, "variableInfo") <- NULL
  attr(data, "statisticInfo") <- NULL
  
  return (data)
}

#' Reads groundwater level measurements from NWISweb.
#'
#' Reads groundwater level measurements from NWISweb. Mixed date/times come back from the service 
#' depending on the year that the data was collected. See \url{http://waterdata.usgs.gov/usa/nwis/gw}
#' for details about groundwater. Groundwater dates and times are returned in many different formats, therefore the 
#' date/time information is returned as a character string. Users will need to convert to a date object.
#' See \url{http://waterservices.usgs.gov/rest/GW-Levels-Service.html} for more information.
#' 
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' site_tp_cd \tab character \tab Site type code \cr 
#' lev_dt \tab Date \tab Date level measured\cr
#' lev_tm \tab character \tab Time level measured \cr
#' lev_tz_cd \tab character \tab Time datum \cr
#' lev_va \tab numeric \tab Water level value in feet below land surface\cr
#' sl_lev_va \tab numeric \tab Water level value in feet above specific vertical datum \cr
#' lev_status_cd \tab character \tab The status of the site at the time the water level was measured \cr
#' lev_agency_cd \tab character \tab The agency code of the person measuring the water level \cr
#' }
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' 
#' @seealso \code{\link{importRDB1}}
#' @export
#' @examples
#' siteNumber <- "434400121275801"
#' data <- readNWISgwl(siteNumber, '','')
#' sites <- c("434400121275801", "375907091432201")
#' data2 <- readNWISgwl(sites, '','')
#' data3 <- readNWISgwl("420125073193001", '','')
readNWISgwl <- function (siteNumbers,startDate="",endDate=""){  
  
#   url <- constructNWISURL(siteNumbers,NA,startDate,endDate,"gwlevels",format="wml1")
#   data <- importWaterML1(url,asDateTime=FALSE)
#   data$tz_cd <- NULL
  url <- constructNWISURL(siteNumbers,NA,startDate,endDate,"gwlevels",format="tsv")
  data <- importRDB1(url,asDateTime=FALSE)
  data$lev_dt <- as.Date(data$lev_dt)
  
  siteInfo <- readNWISsite(siteNumbers)
  
  attr(data, "siteInfo") <- siteInfo
  return (data)
}

