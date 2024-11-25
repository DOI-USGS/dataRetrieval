#' Function to return data from the National Ground Water Monitoring Network waterML2 format
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#'
#' @param input character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data's provided time zone offset.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()}
#' for more information on time zones.
#' @return mergedDF a data frame source, time, value, uom, uomTitle, comment, gmlID
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' 
#' params <- list(request = "GetObservation",
#'                service = "SOS",
#'                version = "2.0.0",
#'                observedProperty = "urn:ogc:def:property:OGC:GroundWaterLevel",
#'                responseFormat = "text/xml",
#'                featureOfInterest = "VW_GWDP_GEOSERVER.USGS.403836085374401")
#' 
#' obs_url <- httr2::request("https://cida.usgs.gov") |>
#'  httr2::req_url_path_append("ngwmn_cache") |> 
#'  httr2::req_url_path_append("sos") |>
#'  httr2::req_url_query(!!!params)
#'
#' #data_returned <- importNGWMN(obs_url)
#' }
#'
importNGWMN <- function(input, asDateTime = FALSE, tz = "UTC") {
  if (tz != "") {
    tz <- match.arg(tz, OlsonNames())
  } else {
    tz <- "UTC"
  }

  raw <- FALSE
  
  if(inherits(input, "httr2_request")){
    returnedDoc <- getWebServiceData(input)
    if (is.null(returnedDoc)) {
      return(invisible(NULL))
    }
    returnedDoc <- xml2::xml_root(returnedDoc)
  } else if (is.character(input) && file.exists(input)) {
    returnedDoc <- xml2::read_xml(input)
  } else if (is.raw(input)) {
    returnedDoc <- xml2::read_xml(input)
    raw <- TRUE
  } 

  response <- xml2::xml_name(returnedDoc)
  if (response == "GetObservationResponse") {
    timeSeries <- xml2::xml_find_all(returnedDoc, "//wml2:MeasurementTimeseries") # each parameter/site combo

    if (0 == length(timeSeries)) {
      df <- data.frame()
      if (!raw) {
        attr(df, "url") <- input
      }
      return(df)
    }

    mergedDF <- NULL

    for (t in timeSeries) {
      df <- importWaterML2(t, asDateTime, tz)

      if (is.null(mergedDF)) {
        mergedDF <- df
      } else {
        similarNames <- intersect(colnames(mergedDF), colnames(df))
        mergedDF <- merge(
          x = mergedDF,
          y = df,
          by = similarNames,
          all = TRUE
        )
      }
    }

    if (!raw) {
      url <- input
      attr(mergedDF, "url") <- url
    }
    if (asDateTime) {
      mergedDF$date <- as.Date(mergedDF$date)
    }
    nonDateCols <- grep("date", names(mergedDF), value = TRUE, invert = TRUE)

    if (nrow(mergedDF) > 0) {
      mergedDF[nonDateCols][mergedDF[nonDateCols] == "" | mergedDF[nonDateCols] == -999999.0] <- NA
    }
    attr(mergedDF, "gml:identifier") <- xml2::xml_text(xml2::xml_find_all(returnedDoc, ".//gml:identifier"))
    attr(mergedDF, "generationDate") <- xml2::xml_text(xml2::xml_find_all(returnedDoc, ".//wml2:generationDate"))
    meta <- xml2::xml_find_all(returnedDoc, ".//gmd:contact")
    attr(mergedDF, "contact") <- xml2::xml_attr(meta, "href")
    attr(mergedDF, "responsibleParty") <- xml2::xml_text(xml2::xml_find_all(meta, ".//gco:CharacterString"))
  } else if (response == "GetFeatureOfInterestResponse") {
    featureMembers <- xml2::xml_find_all(returnedDoc, ".//sos:featureMember")
    site <- xml2::xml_text(xml2::xml_find_all(featureMembers, ".//gml:identifier"))
    site <- substring(site, 8)

    # some sites don't have a description
    siteDesc <- xml2::xml_text(xml2::xml_find_first(featureMembers, ".//gml:description"))

    siteLocs <- strsplit(xml2::xml_text(xml2::xml_find_all(featureMembers, ".//gml:pos")), " ")
    siteLocs <- data.frame(matrix(unlist(siteLocs), nrow = length(siteLocs), byrow = TRUE), stringsAsFactors = FALSE)
    names(siteLocs) <- c("dec_lat_va", "dec_lon_va")

    siteLocs$dec_lat_va <- as.numeric(siteLocs$dec_lat_va)
    siteLocs$dec_lon_va <- as.numeric(siteLocs$dec_lon_va)
    mergedDF <- cbind.data.frame(site, description = siteDesc, siteLocs, stringsAsFactors = FALSE)
  } else if (response == "ExceptionReport") {
    return(data.frame())
  } else {
    stop("Unrecognized response from the web service")
    return(data.frame())
  }
  return(mergedDF)
}

#' Parse the WaterML2 timeseries portion of a waterML2 file
#'
#' Returns data frame columns of all information with each time series measurement;
#' Anything defined as a default, is returned as an attribute of that data frame.
#'
#' @param input XML with only the wml2:MeasurementTimeseries node and children
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided time zone offset).
#' Possible values are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", "America/Honolulu", "America/Jamaica", "America/Managua",
#' "America/Phoenix", and "America/Metlakatla"
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' baseURL <- httr2::request("https://waterservices.usgs.gov/nwis/dv")
#' baseURL <- httr2::req_url_query(baseURL,
#'                                 format = "waterml,2.0",
#'                                 sites = "01646500",
#'                                 startDT = "2014-09-01",
#'                                 endDT = "2014-09-08",
#'                                 statCd = "00003",
#'                                 parameterCd = "00060" )
#' 
#' timesereies <- importWaterML2(baseURL, asDateTime = TRUE, tz = "UTC")
#' }
importWaterML2 <- function(input, asDateTime = FALSE, tz = "UTC") {
  returnedDoc <- check_if_xml(input)

  gmlID <- xml2::xml_attr(returnedDoc, "id") # TODO: make this an attribute
  TVP <- xml2::xml_find_all(returnedDoc, ".//wml2:MeasurementTVP") # time-value pairs
  if (length(TVP) == 0) { # empty nodes on some sites
    return(data.frame(
      site = character(0), source = character(0), date = character(0),
      time = character(0), dateTime = character(0), value = numeric(0),
      uom = character(0), comment = character(0), stringsAsFactors = FALSE
    ))
  }
  rawTime <- xml2::xml_text(xml2::xml_find_all(returnedDoc, ".//wml2:MeasurementTVP/wml2:time"))

  valueNodes <- xml2::xml_find_all(returnedDoc, ".//wml2:MeasurementTVP/wml2:value")
  charValues <- xml2::xml_text(valueNodes)
  nilValues <- as.logical(xml2::xml_attr(valueNodes, "nil"))
  charValues[nilValues] <- NA
  values <- as.numeric(charValues)
  nVals <- length(values)

  # df of date, time, dateTime
  oneCol <- rep(NA, nVals)
  timeDF <- data.frame(date = oneCol, time = oneCol, dateTime = oneCol)
  splitTime <- data.frame(matrix(unlist(strsplit(rawTime, "T")),
    nrow = nVals,
    byrow = TRUE
  ),
  stringsAsFactors = FALSE
  )
  if (ncol(splitTime) > 1) { # some sites only have a date
    names(splitTime) <- c("date", "time")
  } else {
    names(splitTime) <- "date"
    splitTime$time <- NA
  }

  timeDF <- splitTime
  timeDF$dateTime <- NA
  logicVec <- nchar(rawTime) > 19
  if (!all(!logicVec)) { # otherwise sets it to char <NA>
    timeDF$dateTime[logicVec] <- rawTime[logicVec]
  }
  if (asDateTime) {
    timeDF$dateTime <- lubridate::parse_date_time(timeDF$dateTime, c(
      "%Y",
      "%Y-%m-%d",
      "%Y-%m-%dT%H:%M",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%dT%H:%M:%OS%z"
    ),
    exact = TRUE
    )
    # ^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
    attr(timeDF$dateTime, "tzone") <- tz
  }

  uom <- xml2::xml_attr(valueNodes, "uom", default = NA)

  source <- xml2::xml_attr(
    xml2::xml_find_all(
      returnedDoc,
      "./wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:source"
    ),
    "title"
  )
  comment <- xml2::xml_text(xml2::xml_find_all(
    returnedDoc,
    "./wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:comment"
  ))

  defaultMeta <- xml2::xml_find_all(returnedDoc, ".//wml2:DefaultTVPMeasurementMetadata")
  defaultQuals <- xml2::xml_text(xml2::xml_find_all(defaultMeta, ".//swe:description"))
  defaultUOM <- xml2::xml_attr(xml2::xml_find_all(defaultMeta, ".//wml2:uom"), "title", default = NA)

  df_vars <- list(
    source = source, timeDF, value = values,
    uom = uom, comment = comment
  )
  df_use <- df_vars[sapply(df_vars, function(x) length(x) > 0 && !all(is.na(x)))]
  df <- data.frame(df_use, stringsAsFactors = FALSE)
  if (!"value" %in% names(df)) df$value <- NA_real_
  # from the default metadata section
  # append to existing attributes if they aren't empty
  mdAttribs <- list(
    defaultQualifier = defaultQuals,
    defaultUOM = defaultUOM,
    gmlID = gmlID
  ) # all attributes must have names
  mdAttribs_use <- mdAttribs[sapply(mdAttribs, function(x) length(x) > 0)]
  attributes(df) <- append(attributes(df), mdAttribs_use)
  return(df)
}
