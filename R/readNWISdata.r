#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov} service calls.
#' See examples below for ideas of constructing queries.
#'
#' @param service character. Possible values are "iv" (for instantaneous), "dv" (for daily values), "gwlevels" 
#' (for groundwater levels), "site" (for site service), "qw" (water-quality),"measurement", and "stat" (for 
#' statistics service). Note: "qw" and "measurement" calls go to: 
#' \url{http://nwis.waterdata.usgs.gov/usa/nwis} for data requests, and use different call requests schemes.
#' The statistics service has a limited selection of arguments (see \url{http://waterservices.usgs.gov/rest/Statistics-Service-Test-Tool.html}). 
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @import utils
#' @import stats
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency \tab character \tab The NWIS code for the agency reporting the data\cr
#' site \tab character \tab The USGS site number \cr
#' dateTime \tab POSIXct \tab The date and time (if applicable) of the measurement, 
#'           converted to UTC for unit value data. R only allows one time zone attribute per column. For unit data 
#'           spanning a time zone change, converting the data to UTC solves this problem. For daily data,
#'           the time zone attribute is the time zone of the first returned measurement.
#'            \cr
#' tz_cd \tab character \tab The time zone code for dateTime column\cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
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
#' @seealso \code{\link{renameNWISColumns}},  \code{\link{importWaterML1}}, \code{\link{importRDB1}}
#' @export
#' @examples
#' \dontrun{
#' # Examples not run for time considerations
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010", service="dv")
#' instFlow <- readNWISdata(sites="05114000", service="iv", 
#'                    parameterCd="00060", 
#'                    startDate="2014-05-01T00:00Z",endDate="2014-05-01T12:00Z")
#'                                                    
#' instFlowCDT <- readNWISdata(sites="05114000", service="iv", 
#'                    parameterCd="00060", 
#'                    startDate="2014-05-01T00:00",endDate="2014-05-01T12:00",
#'                    tz="America/Chicago")
#'
#' #Empty:
#' multiSite <- readNWISdata(sites=c("04025000","04072150"), service="iv", 
#'                            parameterCd="00010")
#' #Not empty:
#' multiSite <- readNWISdata(sites=c("04025500","040263491"), 
#'                            service="iv", parameterCd="00060")
#' bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
#' 
#' startDate <- as.Date("2013-10-01")
#' endDate <- as.Date("2014-09-30")
#' waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
#'                   service="dv", startDate=startDate, endDate=endDate)
#' siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",
#'                   hasDataTypeCd="iv", service="site")
#' qwData <- readNWISdata(bBox=c(-82.5,41.52,-81,41),startDate=as.Date("2000-01-01"),
#'                   drain_area_va_min=50, qw_count_nu=50,qw_attributes="expanded",
#'                   qw_sample_wide="wide",list_of_search_criteria=c("lat_long_bounding_box",
#'                   "drain_area_va","obs_count_nu"),service="qw")
#' temp <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", service="site", 
#'                    seriesCatalogOutput=TRUE)
#' wiGWL <- readNWISdata(stateCd="WI",service="gwlevels")
#' meas <- readNWISdata(state_cd="WI",service="measurements",format="rdb_expanded")
#' 
#' waterYearStat <- readNWISdata(site=c("03112500"),service="stat",statReportType="annual",
#'                  statYearType="water", missingData="on")
#' monthlyStat <- readNWISdata(site=c("03112500","03111520"),
#'                             service="stat",
#'                             statReportType="monthly")                                   
#' dailyStat <- readNWISdata(site=c("03112500","03111520"),
#'                           service="stat",
#'                           statReportType="daily",
#'                           statType=c("p25","p50","p75","min","max"),
#'                           parameterCd="00065")
#' allDailyStats <- readNWISdata(site=c("03111548"),
#'                               service="stat",
#'                               statReportType="daily")
#'                               service="stat",statReportType="daily",
#'                               statType=c("p25","p50","p75","min","max"),
#'                               parameterCd="00065")
#'
#'dailyWV <- readNWISdata(stateCd = "West Virginia", parameterCd = "00060")
#' }
readNWISdata <- function(service="dv", ..., asDateTime=TRUE,convertType=TRUE){
  
  matchReturn <- list(...)
  
  match.arg(service, c("dv","iv","gwlevels","site", "uv","qw","measurements","qwdata","stat"))
  
  if(service == "uv"){
    service <- "iv"
  } else if (service == "qw"){
    service <- "qwdata"
  }
  
  if(length(service) > 1){
    stop("Only one service call allowed.")
  }
  
  values <- sapply(matchReturn, function(x) as.character(paste(eval(x),collapse=",",sep="")))
  
  names(values)[names(values) == "startDate"] <- "startDT"
  names(values)[names(values) == "endDate"] <- "endDT"
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  format.default <- "waterml,1.1"
  baseURL <- "http://waterservices.usgs.gov/nwis/"
  
  if("stateCd" %in% names(values)){
    values["stateCd"] <- stateCdLookup(values["stateCd"], "postal")
  }
  
  if("statecode" %in% names(values)){
    values["statecode"] <- stateCdLookup(values["statecode"], "postal")
    names(values)[names(values) == "statecode"] <- "stateCd"
  }
  

  if (service %in% c("qwdata","measurements")){

    format.default <- "rdb"
    
    names(values)[names(values) == "startDT"] <- "begin_date"
    names(values)[names(values) == "endDT"] <- "end_date"
    
    if("bBox" %in% names(values)){
      values["nw_longitude_va"] <- as.character(matchReturn$bBox[1])
      values["nw_latitude_va"] <- as.character(matchReturn$bBox[2])
      values["se_longitude_va"] <- as.character(matchReturn$bBox[3])
      values["se_latitude_va"] <- as.character(matchReturn$bBox[4])
      values["coordinate_format"] <- "decimal_degrees"
      values <- values[-which("bBox" %in% names(values))] 
    }
    
    values["date_format"] <- "YYYY-MM-DD"
    values["rdb_inventory_output"] <- "file"
    values["TZoutput"] <- "0"
    
    if(all(c("begin_date","end_date") %in% names(values))){
      values["range_selection"] <- "date_range"
    }
    
    if(service == "qwdata"){
      values["qw_sample_wide"] <- "wide"
    }
    
  } 
  
  if("tz" %in% names(values)){
    tz <- values["tz"]
    if(tz != ""){
      rTZ <- c("America/New_York","America/Chicago",
               "America/Denver","America/Los_Angeles",
               "America/Anchorage","America/Honolulu",
               "America/Jamaica","America/Managua",
               "America/Phoenix","America/Metlakatla","UTC")
      tz <- match.arg(tz, rTZ)
      if("UTC" == tz) tz <- ""
    }
    values <- values[!(names(values) %in% "tz")]
  } else {
    tz <- ""
  }
  
  if(service %in% c("site","gwlevels","stat")){
    format.default <- "rdb"
  }
  
  if(service == "stat"){
    message("Please be aware the NWIS data service feeding this function is in BETA.\n
          Data formatting could be changed at any time, and is not guaranteed")
    
  }
  
  if(!("format" %in% names(values))){
    values["format"] <- format.default
  }
  
  values <- sapply(values, function(x) URLencode(x))
  
  baseURL <- drURL(service, arg.list=values)
  
  if(service %in% c("site","dv","iv","gwlevels")) {
    baseURL <- appendDrURL(baseURL, Access=pkg.env$access)
  }
  #actually get the data
  if(length(grep("rdb",values["format"])) >0){
    retval <- importRDB1(baseURL, tz = tz, asDateTime=asDateTime, convertType=convertType)
  } else {
    retval <- importWaterML1(baseURL, tz= tz, asDateTime=asDateTime)
  }
  
  if("dv" == service){
    
    tzLib <- setNames(c("America/New_York","America/New_York",
                                "America/Chicago","America/Chicago",
                                "America/Denver","America/Denver",
                                "America/Los_Angeles","America/Los_Angeles",
                                "America/Anchorage","America/Anchorage",
                                "America/Honolulu","America/Honolulu","UTC"),
                              c("EST","EDT",
                                "CST","CDT",
                                "MST","MDT",
                                "PST","PDT",
                                "AKST","AKDT",
                                "HAST","HST","UTC"))
    #TODO: Think about dates that cross a time zone boundary.
    if(values["format"] == "waterml,1.1" & nrow(retval) > 0){
      retval$dateTime <- as.POSIXct(retval$dateTime, tzLib[tz=retval$tz_cd[1]])
    }
    
  }
    
  if("iv" == service){
    if(tz == ""){
      retval$tz_cd <- rep("UTC", nrow(retval))
    } else {
      retval$tz_cd <- rep(tz, nrow(retval))
    }
  }
  
  return(retval)
}

#' State code look up 
#'
#' Function to simplify finding state and state code definitions. Used in \code{readNWISdata}
#' and \code{readWQPdata}.
#'
#' @param input could be character (full name, abbreviation, id), or numeric (id)
#' @param outputType character can be "postal","fullName","tableIndex", or "id". 
#' @export
#' @examples
#' fullName <- stateCdLookup("wi", "fullName")
#' abbriev <- stateCdLookup("Wisconsin", "postal")
#' id <- stateCdLookup("WI", "id")
#' name <- stateCdLookup(55, "fullName")
#' index <- stateCdLookup("WI", "tableIndex")
#' stateCd[index,]
#' stateCdLookup(c("West Virginia", "Wisconsin", 55, "MN"))
stateCdLookup <- function(input, outputType="postal"){
  
  outputType <- match.arg(outputType, c("postal","fullName","tableIndex","id"))
  
  retVal <- NA
  
  for(i in input){
    if(is.numeric(i) | !is.na(suppressWarnings(as.numeric(i)))){
      i <- which(as.numeric(i) == as.numeric(stateCd$STATE))
    } else if(nchar(i) == 2){
      i <- which(tolower(i) == tolower(stateCd$STUSAB))
    } else {
      i <- which(tolower(i) == tolower(stateCd$STATE_NAME))
    }
    
    output <- switch(outputType,
                     postal = stateCd$STUSAB[i],
                     fullName = stateCd$STATE_NAME[i],
                     tableIndex = i,
                     id = as.integer(stateCd$STATE[i])
    )
    
    retVal <- c(retVal,output)
  }
  
  return(retVal[-1])
}

#' County code look up 
#'
#' Function to simplify finding county and county code definitions. Used in \code{readNWISdata}
#' and \code{readNWISuse}.
#'
#' @param state could be character (full name, abbreviation, id), or numeric (id)
#' @param county could be character (name, with or without "County") or numeric (id)
#' @param outputType character can be "fullName","tableIndex", "id", or "fullEntry". 
#' @export
#' @examples
#' id <- countyCdLookup(state = "WI", county = "Dane")
#' name <- countyCdLookup(state = "OH", county = 13, output = "fullName")
#' index <- countyCdLookup(state = "Pennsylvania", county = "ALLEGHENY COUNTY", output = "tableIndex")
#' fromIDs <- countyCdLookup(state = 13, county = 5, output = "fullName")
countyCdLookup <- function(state, county, outputType = "id"){
  outputType <- match.arg(outputType, c("fullName","tableIndex","id","fullEntry"))
  
  #first turn state into stateCd postal name
  stateCd <- stateCdLookup(state,outputType = "postal")
  
  if(is.numeric(county) | !is.na(suppressWarnings(as.numeric(county)))){
    county <- which(as.numeric(county) == as.numeric(countyCd$COUNTY) & stateCd == countyCd$STUSAB)
  } else {
    #check if "County" was included on string - need it to match countyCd data frame
    county <- ifelse(!grepl('(?i)\\County$',county),paste(county,"County"),county)
    county <- which(tolower(county) == tolower(countyCd$COUNTY_NAME) & stateCd == countyCd$STUSAB)
  }
  
  retVal <- switch(outputType,
                   fullName = countyCd$COUNTY_NAME[county],
                   tableIndex = county,
                   id = countyCd$COUNTY[county],
                   fullEntry = countyCd[county,]
  )
  
  return(retVal)
}