#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{https://waterservices.usgs.gov} service calls.
#' See examples below for ideas of constructing queries.
#'
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the 
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()} 
#' for more information on time zones.
#' @param \dots see \url{https://waterservices.usgs.gov/rest/Site-Service.html} for a complete list of options.  A 
#' list of arguments can also be supplied. One important argument to include is 'service'. Possible values are "iv" 
#' (for instantaneous), "iv_recent" (for instantaneous values within the last 120 days),
#' "dv" (for daily values), "gwlevels" (for groundwater levels),
#' "site" (for site service), "qw" (water-quality), "measurement", and "stat" (for 
#' statistics service). Note: "qw" and "measurement" calls go to: 
#' \url{https://nwis.waterdata.usgs.gov/usa/nwis} for data requests, and use different call requests schemes.
#' The statistics service has a limited selection of arguments (see \url{https://waterservices.usgs.gov/rest/Statistics-Service-Test-Tool.html}). 
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
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' # Examples not run for time considerations
#' 
#' dataTemp <- readNWISdata(stateCd= "OH",parameterCd= "00010", service= "dv")
#' instFlow <- readNWISdata(sites= "05114000", service= "iv", 
#'                    parameterCd= "00060", 
#'                    startDate= "2014-05-01T00:00Z",endDate= "2014-05-01T12:00Z")
#'                                                    
#' instFlowCDT <- readNWISdata(sites= "05114000", service= "iv", 
#'                    parameterCd= "00060", 
#'                    startDate= "2014-05-01T00:00",endDate= "2014-05-01T12:00",
#'                    tz= "America/Chicago")
#'
#' #Empty:
#' multiSite <- readNWISdata(sites=c("04025000", "04072150"), service= "iv", 
#'                            parameterCd= "00010")
#' #Not empty:
#' multiSite <- readNWISdata(sites=c("04025500", "040263491"), 
#'                            service= "iv", parameterCd= "00060")
#' bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd= "00010")
#' 
#' startDate <- as.Date("2013-10-01")
#' endDate <- as.Date("2014-09-30")
#' waterYear <- readNWISdata(bBox=c(-83,36.5,-82.5,36.75), parameterCd= "00010", 
#'                   service= "dv", startDate=startDate, endDate=endDate)
#' siteInfo <- readNWISdata(stateCd= "WI", parameterCd= "00010",
#'                   hasDataTypeCd= "iv", service= "site")
#' temp <- readNWISdata(bBox=c(-83,36.5,-82.5,36.75), parameterCd= "00010", service= "site", 
#'                    seriesCatalogOutput=TRUE)
#' wiGWL <- readNWISdata(stateCd = "WI", service = "gwlevels")
#' meas <- readNWISdata(state_cd = "WI", service = "measurements",
#'                      format = "rdb_expanded")
#' 
#' waterYearStat <- readNWISdata(site = c("01646500"), 
#'                               service = "stat",
#'                               statReportType= "annual",
#'                               statYearType = "water",
#'                               missingData = "on")
#' monthlyStat <- readNWISdata(site=c("01646500"),
#'                             service= "stat",
#'                             statReportType= "monthly")  
#'                                                              
#' dailyStat <- readNWISdata(site = c("01646500"),
#'                           service = "stat",
#'                           statReportType = "daily",
#'                           statType = c("p25", "p50", "p75", "min", "max"),
#'                           parameterCd = "00060")
#'
#' arg.list <- list(site = "03111548",
#'                  statReportType = "daily",
#'                  statType = c("p25", "p50", "p75", "min", "max"),
#'                  parameterCd = "00060")
#' allDailyStats_2 <- readNWISdata(arg.list, service= "stat")
#'
#' # use county names to get data
#' dailyStaffordVA <- readNWISdata(stateCd = "Virginia",
#'                                 countyCd= "Stafford",
#'                                 parameterCd = "00060",
#'                                 startDate = "2015-01-01",
#'                                 endDate = "2015-01-30")
#' va_counties <- c("51001", "51003", "51005", "51007", "51009", "51011", "51013", "51015")
#' va_counties_data <- readNWISdata(startDate = "2015-01-01", endDate = "2015-12-31", 
#' parameterCd = "00060", countycode = va_counties)
#'  
#' site_id <- '01594440'
#' rating_curve <- readNWISdata(service = "rating", site_no = site_id, file_type= "base")
#' all_sites_base <- readNWISdata(service = "rating", file_type= "base")
#' all_sites_core <- readNWISdata(service = "rating", file_type= "corr")
#' all_sites_exsa <- readNWISdata(service = "rating", file_type= "exsa")
#' all_sites_24hrs <- readNWISdata(service = "rating", file_type= "exsa", period = 24)
#'                       
#' peak_data <- readNWISdata(service = "peak", 
#'                           site_no = c("01594440", "040851325"),
#'                           range_selection = "data_range")
#' 
#' peak_data <- readNWISdata(service = "peak", 
#'                           state_cd = "PA")
#'                           
#' peak_data <- readNWISdata(service = "peak", 
#'                           huc2_cd = "20")
#' 
#' }
readNWISdata <- function(..., asDateTime=TRUE,convertType=TRUE,tz= "UTC") {
  
  tz <- match.arg(tz, OlsonNames())
  
  valuesList <- readNWISdots(...)
  
  service <- valuesList$service
  if(length(service) > 1) {
    warning("Only one service value is allowed. Service: ", service[1], " will be used.")
    service <- service[1]
  }
  
  if(any(service %in% c("qw", "qwdata"))) {
    .Deprecated(old = "readNWISdata", package = "dataRetrieval",
                new = "readWQPdata",
                msg = "NWIS qw web services are being retired. 
Please see vignette('qwdata_changes', package = 'dataRetrieval') 
for more information.
https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html")
  }
  
  values <- sapply(valuesList$values, function(x) URLencode(x))
  
  baseURL <- drURL(service, arg.list=values)
  
  if(service %in% c("site", "dv", "iv", "gwlevels")) {
    baseURL <- appendDrURL(baseURL, Access=pkg.env$access)
  }
  #actually get the data
  if(length(grep("rdb",values["format"])) >0) {
    if(service == "rating") {
      baseURL <- gsub(pattern = "&format=rdb",replacement = "", baseURL)
    }
    retval <- importRDB1(baseURL, tz = tz, asDateTime=asDateTime, convertType=convertType)
  } else {
    retval <- importWaterML1(baseURL, tz= tz, asDateTime=asDateTime)
  }
  
  if("dv" == service) {
    
    tzLib <- setNames(c("America/New_York", "America/New_York",
                                "America/Chicago", "America/Chicago",
                                "America/Denver", "America/Denver",
                                "America/Los_Angeles", "America/Los_Angeles",
                                "America/Anchorage", "America/Anchorage",
                                "America/Honolulu", "America/Honolulu", "UTC"),
                              c("EST", "EDT",
                                "CST", "CDT",
                                "MST", "MDT",
                                "PST", "PDT",
                                "AKST", "AKDT",
                                "HAST", "HST", "UTC"))
    #TODO: Think about dates that cross a time zone boundary.
    if(values["format"] == "waterml,1.1" & nrow(retval) > 0) {
      retval$dateTime <- as.POSIXct(retval$dateTime, tzLib[tz=retval$tz_cd[1]])
    }
    
  }
    
  if("iv" == service | "iv_recent" == service) {
    if(tz == "") {
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
#' @param outputType character can be "postal", "fullName", "tableIndex", or "id". 
#' @export
#' @examples
#' fullName <- stateCdLookup("wi", "fullName")
#' abbriev <- stateCdLookup("Wisconsin", "postal")
#' id <- stateCdLookup("WI", "id")
#' name <- stateCdLookup(55, "fullName")
#' index <- stateCdLookup("WI", "tableIndex")
#' stateCd[index,]
#' stateCdLookup(c("West Virginia", "Wisconsin", 200, 55, "MN"))
stateCdLookup <- function(input, outputType= "postal") {
  
  outputType <- match.arg(outputType, c("postal", "fullName", "tableIndex", "id"))
  
  retVal <- rep(NA, length(input))
  index <- 1
  for(i in input) {
    if(is.numeric(i) | !is.na(suppressWarnings(as.numeric(i)))) {
      i <- which(as.numeric(i) == as.numeric(stateCd$STATE))
    } else if(nchar(i) == 2) {
      i <- which(tolower(i) == tolower(stateCd$STUSAB))
    } else {
      i <- which(tolower(i) == tolower(stateCd$STATE_NAME))
    }
    
    if(length(i) > 0) {
          output <- switch(outputType,
                     postal = stateCd$STUSAB[i],
                     fullName = stateCd$STATE_NAME[i],
                     tableIndex = i,
                     id = as.integer(stateCd$STATE[i])
      )
      retVal[index] <- output
    }
    
    index <- index + 1
  }
  
  if(length(retVal[-1]) == 0) {
    paste("Could not find", input, "in the state lookup table. See `stateCd` for complete list.")
  }
  
  return(retVal)
}

#' County code look up 
#'
#' Function to simplify finding county and county code definitions. Used in \code{readNWISdata}
#' and \code{readNWISuse}.
#'
#' @param state could be character (full name, abbreviation, id), or numeric (id)
#' @param county could be character (name, with or without "County") or numeric (id)
#' @param outputType character can be "fullName", "tableIndex", "id", or "fullEntry". 
#' @export
#' @examples
#' id <- countyCdLookup(state = "WI", county = "Dane")
#' name <- countyCdLookup(state = "OH", county = 13, output = "fullName")
#' index <- countyCdLookup(state = "Pennsylvania", county = "ALLEGHENY COUNTY", output = "tableIndex")
#' fromIDs <- countyCdLookup(state = 13, county = 5, output = "fullName")
#' already_correct <- countyCdLookup(county = "51001")
countyCdLookup <- function(state, county, outputType = "id") {
  outputType <- match.arg(outputType, c("fullName", "tableIndex", "id", "fullEntry"))
  
  if(missing(state)) {
    return(county)
  }
  
  if(missing(county)) {
    stop("No county code provided")
  }
  
  if(length(state) > 1) {
    stop("Only one state allowed in countyCdLookup.")
  }
  
  #first turn state into stateCd postal name
  stateCd <- stateCdLookup(state,outputType = "postal")
  state_counties <- countyCd[countyCd$STUSAB == stateCd,]
  
  if(is.numeric(county) | !is.na(suppressWarnings(as.numeric(county)))) {
    county_i <- which(as.numeric(county) == as.numeric(countyCd$COUNTY) & stateCd == countyCd$STUSAB)
  } else {

    county_in_state <- grep(tolower(county), tolower(state_counties$COUNTY_NAME))
    
    county_i <- which(countyCd$STUSAB == stateCd &
                        countyCd$COUNTY_NAME == state_counties$COUNTY_NAME[county_in_state] )
    
    if(length(county_i) == 0) {
      stop(paste("Could not find", county, "(county), ", stateCd, 
                 "(state) in the county lookup table. See `countyCd` for complete list."))
    } else if(length(county_i) > 1) {
      stop(paste(county, "(county), ", stateCd, "(state) matched more than one county. See `countyCd` for complete list."))
    } 
    
  }
  
  retVal <- switch(outputType,
                   fullName = countyCd$COUNTY_NAME[county_i],
                   tableIndex = county_i,
                   id = countyCd$COUNTY[county_i],
                   fullEntry = countyCd[county_i,]
  )
  
  return(retVal)
}

#' 
#' Format and organize NWIS arguments that are passed in as \code{...}.
#'
#' @keywords internal
readNWISdots <- function(...) {
  
  if(length(list(...)) == 0) {
    stop("No arguments supplied")
  }
  
  matchReturn <- convertLists(...)
  
  if(anyNA(unlist(matchReturn))) {
    stop("NA's are not allowed in query")
  }
  
  if("service" %in% names(matchReturn)) {
    service <- matchReturn$service
    matchReturn$service <- NULL
  } else {
    service <- "dv"
  }
  
  match.arg(service, c("dv", "iv", "iv_recent", "gwlevels",
                       "site", "uv", "qw", "measurements",
                       "qwdata", "stat", "rating", "peak"))
  
  if(service == "uv") {
    service <- "iv"
  } else if (service == "qw") {
    service <- "qwdata"
  }
  
  if(length(service) > 1) {
    stop("Only one service call allowed.")
  }
  
  values <- sapply(matchReturn, function(x) as.character(paste(eval(x),collapse= ", ",sep= "")))
  
  names(values)[names(values) == "startDate"] <- "startDT"
  names(values)[names(values) == "endDate"] <- "endDT"
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  format.default <- "waterml,1.1"
  
  if(service == "iv" && "startDT" %in% names(values)) {
    if(as.Date(values[["startDT"]]) >= Sys.Date()-120) {
      service <- "iv_recent"
    }
  }
  
  names(values)[names(values) == "statecode"] <- "stateCd"
  if("stateCd" %in% names(values)) {
    values["stateCd"] <- stateCdLookup(values["stateCd"], "postal")
    values[["stateCd"]][values["stateCd"] == "AS"] <- "AQ" #Leaving the correct abb in stateCd
    if(values["stateCd"] == "UM") {
      stop("NWIS does not include U.S. Minor Outlying Islands")
    }
  }
		
  if("parameterCd" %in% names(matchReturn)) {
    pcodeCheck <- (nchar(matchReturn$parameterCd) == 5) & !is.na(suppressWarnings(as.numeric(matchReturn$parameterCd)))
    if(!all(pcodeCheck)) {
      badPcode <- matchReturn$parameterCd[which(!pcodeCheck)]
      stop("The following pCodes appear mistyped:",paste(badPcode,collapse= ", "))
    } 
  }
  
  names(values)[names(values) == "countycode"] <- "countyCd"
  if("countyCd" %in% names(values)) {
    if("stateCd" %in% names(values)) {
      values["countyCd"] <- paste0(zeroPad(stateCdLookup(values["stateCd"], "id"), padTo = 2), 
                                   countyCdLookup(values["stateCd"], values["countyCd"], "id"))
      values <- values[names(values) != "stateCd"]      
    }
  }
  
  if (service %in% c("peak", "qwdata", "measurements")) {
    
    format.default <- "rdb"
    
    names(values)[names(values) == "startDT"] <- "begin_date"
    names(values)[names(values) == "endDT"] <- "end_date"
    
    if("bBox" %in% names(values)) {
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
    
    if(all(c("begin_date", "end_date") %in% names(values))) {
      values["range_selection"] <- "date_range"
    }
    
    if(service == "qwdata") {
      values["qw_sample_wide"] <- "wide"
    }
    
  } 
  
  if (service == "peak" & "state_cd" %in% names(values)) {
    values["list_of_search_criteria"] <- "state_cd"
  }
  
  if (service == "peak" & "huc2_cd" %in% names(values)) {
    values["list_of_search_criteria"] <- "huc2_cd"
  }  
  
  if (service == "peak" & "bBox" %in% names(values)) {
    values["list_of_search_criteria"] <- "lat_long_bounding_box"
  }   
  
  if(service %in% c("site", "gwlevels", "stat", "rating", "peak")) {
    format.default <- "rdb"
  }
  
  if(service == "stat") {
    message("Please be aware the NWIS data service feeding this function is in BETA.\n
            Data formatting could be changed at any time, and is not guaranteed")
    
  }
  
  if(!("format" %in% names(values))) {
    values["format"] <- format.default
  }
  
  return(list(values=values, service=service))
}

#' convert variables in dots to usable format
#' 
#' @keywords internal
convertLists <- function(...) {
  matchReturn <- c(do.call("c",list(...)[sapply(list(...), class) == "list"]), #get the list parts
                   list(...)[sapply(list(...), class) != "list"]) # get the non-list parts
  return(matchReturn)
  
}

