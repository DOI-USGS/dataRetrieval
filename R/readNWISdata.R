#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{https://waterservices.usgs.gov} service calls.
#' See examples below for ideas of constructing queries.
#'
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the
#' function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()}
#' for more information on time zones.
#' @param \dots see \url{https://waterservices.usgs.gov/docs/site-service/} for
#' a complete list of options.  A
#' list of arguments can also be supplied. One important argument to include is
#' "service". Possible values are "iv"
#' (for instantaneous), 
#' "dv" (for daily values), "gwlevels" (for groundwater levels),
#' "site" (for site service), "measurement", and "stat" (for
#' statistics service). Note: "measurement" calls go to:
#' \url{https://nwis.waterdata.usgs.gov/usa/nwis} for data requests, and use different call requests schemes.
#' The statistics service has a limited selection of arguments
#' (see \url{https://waterservices.usgs.gov/docs/site-service/}).
#' 
#' @details This function requires users to create their own arguments
#' based on the NWIS web services. It is a more complicated function to use
#' compared to other NWIS functions such as \code{\link{readNWISdv}}, \code{\link{readNWISuv}},
#' \code{\link{readNWISgwl}}, etc. However, this function adds a lot of
#' flexibility to the possible queries. This function will also behave exactly 
#' as NWIS when it comes to date queries. NWIS by default will only return the latest
#' value for the daily and instantaneous services. So if you do not provide
#' a starting date, you will only get back the latest value. If you want
#' the full period of record, you can use "startDate = '1900-01-01'". Other options
#' for dates are periods, such as "period = 'P7D'" which translates to a period
#' of 7 days. For period, use only a positive ISO-8601 duration format, which should
#' not be expressed in periods of less than a day, or in increments of months M or years Y. 
#' period returns data for a site generally from now to a time in the past. 
#' Note that when period is used all data up to the most recent value are returned. 
#' 
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
#' dataTemp <- readNWISdata(stateCd = "OH", parameterCd = "00010", service = "dv")
#' instFlow <- readNWISdata(
#'   sites = "05114000", service = "iv",
#'   parameterCd = "00060",
#'   startDate = "2014-05-01T00:00Z", endDate = "2014-05-01T12:00Z"
#' )
#'
#' instFlowCDT <- readNWISdata(
#'   sites = "05114000", service = "iv",
#'   parameterCd = "00060",
#'   startDate = "2014-05-01T00:00", endDate = "2014-05-01T12:00",
#'   tz = "America/Chicago"
#' )
#'
#' multiSite <- readNWISdata(
#'   sites = c("04025500", "040263491"),
#'   service = "iv", parameterCd = "00060"
#' )
#' 
#' bBoxEx <- readNWISdata(bBox = c(-83, 36.5, -81, 38.5), parameterCd = "00010")
#'
#' startDate <- as.Date("2013-10-01")
#' endDate <- as.Date("2014-09-30")
#' waterYear <- readNWISdata(
#'   bBox = c(-83, 36.5, -82.5, 36.75),
#'   parameterCd = "00010",
#'   service = "dv",
#'   startDate = startDate,
#'   endDate = endDate
#' )
#'
#' siteInfo <- readNWISdata(
#'   stateCd = "WI", parameterCd = "00010",
#'   hasDataTypeCd = "iv", service = "site"
#' )
#' temp <- readNWISdata(
#'   bBox = c(-83, 36.5, -82.5, 36.75), parameterCd = "00010", service = "site",
#'   seriesCatalogOutput = TRUE
#' )
#' GWL <- readNWISdata(site_no = c("392725077582401", 
#'                                 "375907091432201"),
#'                     parameterCd = "62610",
#'                     service = "gwlevels")
#'                     
#' levels <- readNWISdata(stateCd = "WI", 
#'                        service = "gwlevels",
#'                        startDate = "2024-05-01",
#'                        endDate = "2024-05-30") 
#'                     
#' meas <- readNWISdata(
#'   state_cd = "WI", service = "measurements",
#'   format = "rdb_expanded"
#' )
#'
#' waterYearStat <- readNWISdata(
#'   site = c("01646500"),
#'   service = "stat",
#'   statReportType = "annual",
#'   statYearType = "water",
#'   missingData = "on"
#' )
#' monthlyStat <- readNWISdata(
#'   site = c("01646500"),
#'   service = "stat",
#'   statReportType = "monthly"
#' )
#'
#' dailyStat <- readNWISdata(
#'   site = c("01646500"),
#'   service = "stat",
#'   statReportType = "daily",
#'   statType = c("p25", "p50", "p75", "min", "max"),
#'   parameterCd = "00060"
#' )
#'
#' arg.list <- list(
#'   site = "03111548",
#'   statReportType = "daily",
#'   statType = c("p25", "p50", "p75", "min", "max"),
#'   parameterCd = "00060"
#' )
#' allDailyStats_2 <- readNWISdata(arg.list, service = "stat")
#'
#' # use county names to get data
#' dailyStaffordVA <- readNWISdata(
#'   stateCd = "Virginia",
#'   countyCd = "Stafford",
#'   parameterCd = "00060",
#'   startDate = "2015-01-01",
#'   endDate = "2015-01-30"
#' )
#' va_counties <- c("51001", "51003", "51005", "51007", "51009", "51011", "51013", "51015")
#' va_counties_data <- readNWISdata(
#'   startDate = "2015-01-01", endDate = "2015-12-31",
#'   parameterCd = "00060", countycode = va_counties
#' )
#'
#' site_id <- "01594440"
#' rating_curve <- readNWISdata(service = "rating", site_no = site_id, file_type = "base")
#' all_sites_base <- readNWISdata(service = "rating", file_type = "base")
#' all_sites_core <- readNWISdata(service = "rating", file_type = "corr")
#' all_sites_exsa <- readNWISdata(service = "rating", file_type = "exsa")
#' all_sites_24hrs <- readNWISdata(service = "rating", file_type = "exsa", period = 24)
#'
#' peak_data <- readNWISdata(
#'   service = "peak",
#'   site_no = c("01594440", "040851325"),
#'   range_selection = "data_range"
#' )
#'
#' peak_data <- readNWISdata(
#'   service = "peak",
#'   state_cd = "PA"
#' )
#'
#' peak_data <- readNWISdata(
#'   service = "peak",
#'   huc2_cd = "20"
#' )
#' }
readNWISdata <- function(..., asDateTime = TRUE, convertType = TRUE, tz = "UTC") {
  tz <- match.arg(tz, OlsonNames())
  
  valuesList <- readNWISdots(...)
  
  values <- valuesList[["values"]]
  values <- values[names(values) != "format"]
  format <- valuesList[["values"]][["format"]]
  
  service <- valuesList$service
  if (length(service) > 1) {
    warning("Only one service value is allowed. Service: ", service[1], " will be used.")
    service <- service[1]
  }
  
  if (any(service %in% c("qw", "qwdata"))) {
    .Deprecated(
      old = "readNWISdata", package = "dataRetrieval",
      new = "readWQPdata",
      msg = "NWIS qw web services are being retired.
Please see vignette('qwdata_changes', package = 'dataRetrieval')
for more information.
https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html"
    )
  }
  
  baseURL <- httr2::request(pkg.env[[service]])
  if (service != "rating") {
    baseURL <- httr2::req_url_query(baseURL, 
                                    format = format)
  }
  POST = nchar(paste0(unlist(values), collapse = "")) > 2048
  
  baseURL <- get_or_post(baseURL,
                         POST = POST,  
                         !!!values, 
                         .multi = "comma")

  if (length(grep("rdb",  format)) > 0) {
    retval <- importRDB1(baseURL, tz = tz, asDateTime = asDateTime, convertType = convertType)
  } else {
    retval <- importWaterML1(baseURL, tz = tz, asDateTime = asDateTime)
  }
  
  if ("dv" == service) {
    tzLib <- stats::setNames(
      c(
        "America/New_York", "America/New_York",
        "America/Chicago", "America/Chicago",
        "America/Denver", "America/Denver",
        "America/Los_Angeles", "America/Los_Angeles",
        "America/Anchorage", "America/Anchorage",
        "America/Honolulu", "America/Honolulu", "UTC"
      ),
      c(
        "EST", "EDT",
        "CST", "CDT",
        "MST", "MDT",
        "PST", "PDT",
        "AKST", "AKDT",
        "HAST", "HST", "UTC"
      )
    )
    # TODO: Think about dates that cross a time zone boundary.
    if (format == "waterml,1.1" && nrow(retval) > 0) {
      retval$dateTime <- as.POSIXct(retval$dateTime, tzLib[tz = retval$tz_cd[1]])
    }
  }
  
  if ("iv" == service || "iv_recent" == service) {
    if (tz == "") {
      retval$tz_cd <- rep("UTC", nrow(retval))
    } else {
      retval$tz_cd <- rep(tz, nrow(retval))
    }
  } else if("gwlevels" == service && "parameterCd" %in% names(values)){
    retval <- retval[retval$parameter_cd %in% values[["parameterCd"]], ]
  }
  
  return(retval)
}

#' State code look up
#'
#' Function to simplify finding state and state code definitions. Used in \code{readNWISdata}
#' and \code{readWQPdata}.
#'
#' @param input could be character (full name, abbreviation, id), or numeric (id)
#' @param country description
#' @param outputType character can be "postal", "fullName", "tableIndex", or "id".
#' @export
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' fullName <- stateCdLookup("wi", "fullName")
#' abbriev <- stateCdLookup("Wisconsin", "postal")
#' id <- stateCdLookup("WI", "id")
#' name <- stateCdLookup(55, "fullName")
#' fips <- stateCdLookup("WI", "fips")
#' canada_st <- stateCdLookup(13, "fullName", country = "CA")
#' mexico_st <- stateCdLookup(13, "fullName", country = "MX")
#' stateCdLookup(c("West Virginia", "Wisconsin", 200, 55, "MN"))
#' }
stateCdLookup <- function(input,
                          outputType = "postal",
                          country = "US") {
  
  outputType <- match.arg(outputType, c("postal", "fullName",
                                        "id", "fips"))
  
  states <- check_USGS_sample_params("states")
  country <- match.arg(country, choices = unique(states$countryCode), 
                       several.ok = FALSE)
  states <- states[states$countryCode == country,]
  retVal <- rep(NA, length(input))
  index <- 1
  for (i in input) {
    if (is.numeric(i) || !is.na(suppressWarnings(as.numeric(i)))) {
      out_i <- which(as.numeric(i) == as.numeric(states$fipsCode))
    } else if (nchar(i) == 2) {
      out_i <- which(tolower(i) == tolower(states$stateAbbrev))
    } else {
      out_i <- which(tolower(i) == tolower(states$stateName))
    }
    

    if (length(out_i) > 0) {
      output <- switch(outputType,
                       postal = states$stateAbbrev[out_i],
                       fullName = states$stateName[out_i],
                       id = as.integer(states$fipsCode[out_i]),
                       fips = paste(states$countryCode[out_i], states$fipsCode[out_i], sep = ":"))

      retVal[index] <- output
    }
    
    index <- index + 1
  }
  
  if (length(retVal[-1]) == 0) {
    paste("Could not find", input, "in the state lookup table. See `stateCd` for complete list.")
  }
  
  return(retVal)
}

#' US county code look up
#'
#' Function to simplify finding county and county code definitions. Used in \code{readNWISdata}
#' and \code{readNWISuse}. Currently only has US counties.
#'
#' @param state could be character (full name, abbreviation, id), or numeric (id)
#' @param county could be character (name, with or without "County") or numeric (id)
#' @param outputType character can be "fullName", "tableIndex", "id", or "fullEntry".
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' 
#' fips <- countyCdLookup(state = "WI", county = "Dane")
#' id <- countyCdLookup(state = "WI", county = "Dane", outputType = "id")
#' name <- countyCdLookup(state = "OH", county = 13, output = "fullName")
#' entry <- countyCdLookup(state = "Pennsylvania", county = "ALLEGHENY COUNTY", output = "fullEntry")
#' fromIDs <- countyCdLookup(state = 13, county = 5, output = "fullName")
#' }
countyCdLookup <- function(state, county, outputType = "fips") {
  outputType <- match.arg(outputType, c("fullName", 
                                        "fullEntry",
                                        "fips",
                                        "id"))
  
  if (missing(state)) {
    stop("No state code provided")
  }
  
  if (missing(county)) {
    stop("No county code provided")
  }
  
  if (length(state) > 1) {
    stop("Only one state allowed in countyCdLookup.")
  }
  
  counties <- check_USGS_sample_params("counties")
  
  # first turn state into stateCd postal name
  state_postal <- stateCdLookup(state, 
                                country = unique(counties$countryCode),
                                outputType = "postal")
  state_counties <- counties[counties$stateAbbrev == state_postal, ]
  
  if (is.numeric(county) || !is.na(suppressWarnings(as.numeric(county)))) {
    county_i <- which(as.numeric(county) == as.numeric(state_counties$countyCode))
  } else {
    county_i <- grep(tolower(gsub(" ", "", county)),
                     tolower(gsub(" ", "", state_counties$countyName))) # takes care of questionable spaces...DeWitt vs De Witt
    if(length(county_i) == 0){
      county <- gsub(" county", "", county, ignore.case = TRUE)
      county_i <- grep(tolower(gsub(" ", "", county)),
                       tolower(gsub(" ", "", state_counties$countyName)))
    }
  }
  
  if (length(county_i) == 0) {
    stop(paste(
      "Could not find", county, "(county), ", state_postal,
      "(state) in the county lookup table. See `countyCd` for complete list."
    ))
  } else if (length(county_i) > 1) {
    stop(paste(
      county, "(county), ", state_postal,
      "(state) matched more than one county. See `countyCd` for complete list."
    ))
  }
  
  fips_val <- paste(stateCdLookup(state_counties$stateAbbrev[county_i], 
                                  outputType = "fips"), 
                    state_counties$countyCode[county_i],
                    sep = ":")
  retVal <- switch(outputType,
                   fullName = state_counties$countyName[county_i],
                   fullEntry = state_counties[county_i, ],
                   fips = fips_val,
                   id = state_counties$countyCode[county_i])
  
  return(retVal)
}

#'
#' Format and organize NWIS arguments that are passed in as \code{...}.
#'
#' @keywords internal
readNWISdots <- function(...) {
  if (length(list(...)) == 0) {
    stop("No arguments supplied")
  }
  
  matchReturn <- convertLists(...)
  
  if (anyNA(unlist(matchReturn))) {
    stop("NA's are not allowed in query")
  }
  
  if ("service" %in% names(matchReturn)) {
    service <- matchReturn$service
    matchReturn$service <- NULL
  } else {
    service <- "dv"
  }
  
  match.arg(service, c(
    "dv", "iv", "iv_recent", "gwlevels",
    "site", "uv", "measurements",
    "qwdata", "stat", "rating", "peak"
  ))
  
  if (service == "uv") {
    service <- "iv"
  } 
  
  if (length(service) > 1) {
    stop("Only one service call allowed.")
  }
  
  values <- matchReturn
  
  names(values)[names(values) == "startDate"] <- "startDT"
  names(values)[names(values) == "endDate"] <- "endDT"
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  format.default <- "waterml,1.1"
  
  if (service == "iv" && "startDT" %in% names(values)) {
    if (as.Date(values[["startDT"]]) >= Sys.Date() - 120) {
      service <- "iv_recent"
    }
  }
  
  names(values)[names(values) == "statecode"] <- "stateCd"
  if ("stateCd" %in% names(values)) {
    
    state <- stateCdLookup(values["stateCd"], "postal")
    
    if(length(state) > 1){
      stop("Multiple states are not allowed in NWIS queries.")
    }
    
    values["stateCd"] <- state
    values[["stateCd"]][values["stateCd"] == "AS"] <- "AQ" # Leaving the correct abb in stateCd
    if (values["stateCd"] == "UM") {
      stop("NWIS does not include U.S. Minor Outlying Islands")
    }
  }
  
  if ("parameterCd" %in% names(matchReturn)) {
    pcodeCheck <- (nchar(matchReturn$parameterCd) == 5) & !is.na(suppressWarnings(as.numeric(matchReturn$parameterCd)))
    if (!all(pcodeCheck)) {
      badPcode <- matchReturn$parameterCd[which(!pcodeCheck)]
      stop("The following pCodes appear mistyped:", paste(badPcode, collapse = ", "))
    }
  }
  
  names(values)[names(values) == "countycode"] <- "countyCd"
  if ("countyCd" %in% names(values)) {
    if ("stateCd" %in% names(values)) {
      values["countyCd"] <- paste0(
        zeroPad(stateCdLookup(values["stateCd"], "id"), padTo = 2),
        countyCdLookup(values["stateCd"], values["countyCd"], "id")
      )
      values <- values[names(values) != "stateCd"]
    }
  }
  
  if (service %in% c("peak", "measurements", "gwlevels")) {
    format.default <- "rdb"
    
    names(values)[names(values) == "startDT"] <- "begin_date"
    names(values)[names(values) == "endDT"] <- "end_date"
    names(values)[names(values) == "sites"] <- "site_no"
    
    if ("bBox" %in% names(values)) {
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
    
    if (all(c("begin_date", "end_date") %in% names(values))) {
      values["range_selection"] <- "date_range"
    }
    
  }
  
  if("bbox" %in% names(values)){
    values[["bbox"]] <- paste0(values[["bbox"]], collapse = ",")
  }
  
  if (service %in% c("peak", "gwlevels") && "stateCd" %in% names(values)) {
    names(values)[names(values) == "stateCd"] <- "state_cd"
    values["list_of_search_criteria"] <- "state_cd"
  }
  
  if (service %in% c("peak", "gwlevels") && "huc2_cd" %in% names(values)) {
    values["list_of_search_criteria"] <- "huc2_cd"
  }
  
  if(service == "gwlevels" && "aquiferCd" %in% names(values)){
    values["aquiferCd"] <- "nat_aqfr_cd"
  }
  
  if (service %in% c("peak", "gwlevels") && "bBox" %in% names(values)) {
    values["list_of_search_criteria"] <- "lat_long_bounding_box"
  }
  
  if (service %in% c("site", "gwlevels", "stat", "rating", "peak")) {
    format.default <- "rdb"
  }
  
  if (service == "stat") {
    message("Please be aware the NWIS data service feeding this function is in BETA.\n
            Data formatting could be changed at any time, and is not guaranteed")
  }
  
  if (!("format" %in% names(values))) {
    values["format"] <- format.default
  }
  return_list <- list()
  return_list["values"] <- list(values)
  return_list["service"] <- service
  return(return_list)
}

#' convert variables in dots to usable format
#'
#' @keywords internal
convertLists <- function(...) {
  matchReturn <- c(
    do.call("c", list(...)[sapply(list(...), class) == "list"]), # get the list parts
    list(...)[sapply(list(...), class) != "list"]
  ) # get the non-list parts
  return(matchReturn)
}