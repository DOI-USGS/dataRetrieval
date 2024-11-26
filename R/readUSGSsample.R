#' Construct request for USGS Samples Data
#' 
#' This function creates the call for discrete water quality samples data
#' service described at \url{https://waterdata.usgs.gov/download-samples}.
#' Note: all possible arguments are included, but it is strongly recommended
#' to only use the NECESSARY arguments. Leave unnecessary arguments as the default
#' NA. 
#' 
#' See also: \url{https://api.waterdata.usgs.gov/samples-data/docs}.
#'  
#' @param monitoringLocationIdentifier A monitoring location identifier has two parts: the agency code
#' and the location number. Location identifiers should be separated with commas,
#' for example: AZ014-320821110580701, CAX01-15304600, USGS-040851385. Location
#' numbers without an agency prefix are assumed to have the prefix USGS.
#' @param USstate US state. Could be full names, postal abbreviations, or fips codes.
#' @param activityMediaName Sample media refers to the environmental medium that
#' was sampled or analyzed.
#' @param siteTypeCode Site type code query parameter.
#' @param boundingBox North and South are latitude values; East and West are longitude values.
#' A vector of 4 (west, south, east, north) is expected.
#' An example would be: c(-92.8, 44.2, -88.9, 46.0).
#' @param hydrologicUnit Hydrologic Unit Codes (HUCs) identify physical areas
#' within the US that drain to a certain portion of the stream network. 
#' This filter accepts values containing 2, 4, 6, 8, 10 or 12 digits. 
#' @param activityMediaName Sample media refers to the environmental medium that
#' was sampled or analyzed.
#' @param activityStartDateLower Start date.
#' @param activityStartDateUpper End date.
#' @param characteristicGroup Characteristic Group is a broad category describing the sample.
#' @param characteristicUserSupplied Observed Property is the USGS term for the
#' constituent sampled and the property name gives a detailed description of what
#' was sampled. Observed Property replaces the parameter name and pcode USGS
#' previously used to describe discrete sample data. Find more information in the
#' Observed Properties and Parameter Codes section of the Code Dictionary.
#' @param characteristic description
#' @param stateFips description
#' @param countyFips description
#' @param countryFips description
#' @param projectIdentifier description
#' @param recordIdentifierUserSupplied description
#' @param siteTypeName description
#' @param usgsPCode description
#' @param pointLocationLatitude description
#' @param pointLocationLongitude description
#' @param pointLocationWithinMiles description
#' @param dataType Options include: "Results", "Monitoring locations", "Activities",
#' "Projects", "Organizations", and "Summary"
#' @param dataProfile Options include: "Full physical chemical", "Basic physical chemical",
#' "Full biological", "Basic biological", "Narrow"
#' @export
#' @return data frame returned from web service call.
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' req <- construct_USGS_sample_request(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field")
#' rawData <- importWQP(req)
#'
#' req_site <- construct_USGS_sample_request(
#'                USstate = "Wisconsin",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                dataType = "Monitoring locations",
#'                activityStartDateUpper = "2000-01-01",
#'                dataProfile = "Site")
#' rawData_sites <- importWQP(req_site)
#' }
construct_USGS_sample_request <- function(monitoringLocationIdentifier = NA,
                           USstate = NA,
                           siteTypeCode = NA,
                           boundingBox = NA,
                           hydrologicUnit = NA,
                           activityMediaName = NA,
                           characteristicGroup = NA,
                           characteristic = NA,
                           characteristicUserSupplied = NA,
                           activityStartDateLower = NA,
                           activityStartDateUpper = NA,
                           countryFips = NA,
                           stateFips = NA,
                           countyFips = NA,
                           projectIdentifier = NA,
                           recordIdentifierUserSupplied = NA,
                           siteTypeName = NA,
                           usgsPCode = NA,
                           pointLocationLatitude = NA,
                           pointLocationLongitude = NA,
                           pointLocationWithinMiles = NA,
                           dataType = "Results",
                           dataProfile = NA){
  
  message("Function in development, use at your own risk.")
  
  dataType <- match.arg(dataType, c("Results",
                        "Monitoring locations",
                        "Activities",
                        "Projects",
                        "Organizations"), 
            several.ok = FALSE)

  # When RMLS comes out...spring 2025ish,
  # we can verify these values hopefully easier:
  baseURL <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data") |>
    httr2::req_url_query(mimeType = "text/csv")
  
  switch(dataType,
         Results = {
           available_profiles <- c("Full physical chemical",
                                   "Basic physical chemical",
                                   "Full biological",
                                   "Basic biological",
                                   "Narrow",
                                   "Count",
                                   "Lab Sample Prep",
                                   "Result Detection Quantitation Limit")
           profile_convert <- stats::setNames(c("fullphyschem",
                                "basicphyschem",
                                "fullbio",
                                "basicbio",
                                "narrow",
                                "count",
                                "labsampleprep",
                                "resultdetectionquantitationlimit"),
                                available_profiles)
           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "results")
         },
         `Monitoring locations` = {
           profile_convert <- stats::setNames(c("site", "count"),
                                              c("Site", "Count"))

           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "locations")
         },
         Activities = {
           profile_convert <- stats::setNames(c("sampact", "actmetric", "actgroup", "count"),
                                              c("Sample Activities", "Activiey Metrics", "Activity Groups", "Count"))

           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "activities")
         },
         Projects = {
           profile_convert <- stats::setNames(c("project", "projectmonitoringlocationweight"),
                                              c("Project", "Project Monitoring Location Weight"))
           
           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "projects") 
         },
         Organizations = {
           profile_convert <- stats::setNames(c("organization", "Count"),
                                              c("Organization", "Count"))

           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "organizations")
         })

  dataProfile <- check_profile(dataProfile, profile_convert)
  dataProfile <- match.arg(dataProfile, 
                           names(profile_convert),
                           several.ok = FALSE)  
  
  baseURL <- httr2::req_url_path_append(baseURL,
                                        profile_convert[[dataProfile]]) 
    
  
  if(all(!is.na(siteTypeCode))){
    siteTypeCode <- match.arg(siteTypeCode, 
              check_param("sitetype")$typeCode, 
              several.ok = TRUE)
  }
    
  if(all(!is.na(activityMediaName))){
    activityMediaName <- match.arg(activityMediaName, 
              check_param("samplemedia")$activityMedia, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(characteristicGroup))){
    characteristicGroup <- match.arg(characteristicGroup, 
              check_param("characteristicgroup")$characteristicGroup, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(countryFips))){
    countryFips <- match.arg(countryFips, 
              check_param("countries")$countryCode, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(siteTypeName))){
    siteTypeName <- match.arg(siteTypeName, 
              check_param("sitetype")$typeName, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(stateFips))){
    states <- check_param("states")
    state_codes <- paste(states$countryCode, 
                         states$fipsCode, sep = ":")
    stateFips <- match.arg(stateFips, state_codes, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(countyFips))){
    states <- check_param("states")
    state_codes <- paste(states$countryCode, 
                         states$fipsCode, sep = ":")
    counties <- check_param("counties")
    state_cd <- stats::setNames(states$fipsCode,
                                states$stateAbbrev)
    county_codes <- paste(counties$countryCode, 
                          state_cd[counties$stateAbbrev],
                          counties$countyCode, sep = ":")
    
    countyFips <- match.arg(countyFips, county_codes, 
              several.ok = TRUE)
  }
  
  if(!sum(is.na(c(pointLocationLatitude, 
                 pointLocationLongitude,
                 pointLocationWithinMiles))) == 3){
    if(!sum(!is.na(c(pointLocationLatitude, 
                    pointLocationLongitude,
                    pointLocationWithinMiles))) == 3 ){
      stop("pointLocationLatitude, pointLocationLongitude, and pointLocationWithinMiles
           must all be defined, or none defined.")
    }
  }
  
  baseURL <- explode_query(baseURL,
                           list(hydrologicUnit = hydrologicUnit,
                                projectIdentifier = projectIdentifier,
                                recordIdentifierUserSupplied = recordIdentifierUserSupplied,
                                monitoringLocationIdentifier = monitoringLocationIdentifier,
                                siteTypeCode = siteTypeCode,
                                activityMediaName = activityMediaName,
                                characteristicGroup = characteristicGroup,
                                characteristic = characteristic,
                                characteristicUserSupplied = characteristicUserSupplied,
                                countryFips = countryFips,
                                siteTypeName = siteTypeName,
                                usgsPCode = usgsPCode,
                                stateFips = stateFips,
                                countyFips = countyFips,
                                pointLocationLatitude = pointLocationLatitude,
                                pointLocationLongitude = pointLocationLongitude,
                                pointLocationWithinMiles = pointLocationWithinMiles
                           ))

  if(all(!is.na(activityStartDateLower))){
    start <- checkWQPdates(list(activityStartDateLower = activityStartDateLower))
    start <- as.character(as.Date(start$activityStartDateLower, format = "%m-%d-%Y"))
    baseURL <- httr2::req_url_query(baseURL,
                                    activityStartDateLower = start)
  }
  
  if(all(!is.na(activityStartDateUpper))){
    end <- checkWQPdates(list(activityStartDateUpper = activityStartDateUpper))
    end <- as.character(as.Date(end$activityStartDateUpper, format = "%m-%d-%Y"))
    baseURL <- httr2::req_url_query(baseURL,
                                    activityStartDateUpper = end)
  }
  

  if(all(!is.na(boundingBox))){
    baseURL <- httr2::req_url_query(baseURL,
                                    boundingBox = boundingBox,
                                    .multi = "comma")      
  }
  
  if(all(!is.na(USstate))){
    stCdPrefix <- "US:"
    if (!grepl(stCdPrefix, USstate)) {
      USstate <- paste0(stCdPrefix, zeroPad(stateCdLookup(USstate, "id"), 2))
    } 
    baseURL <- httr2::req_url_query(baseURL,
                                    stateFips = USstate,
                                    .multi = "explode")
  }
  
  return(baseURL)
}

check_profile <- function(dataProfile, profile_convert){
  if(is.na(dataProfile)){
    message(paste0("No profile specified, defaulting to '",
                   names(profile_convert)[1], "'\n",
                   "Possible values are: \n",
                   paste0(names(profile_convert), collapse = ", ")))
    dataProfile <- names(profile_convert)[1]
  } else {
    dataProfile <- match.arg(dataProfile, names(profile_convert), several.ok = FALSE)
  }
  return(dataProfile)
}

explode_query <- function(baseURL, x){
  
  if(!is.list(x)){
    return(baseURL)
  }
  
  if(any(!is.na(x))){
    x <- Filter(Negate(anyNA), x)
    baseURL <- httr2::req_url_query(baseURL,
                                    !!!x,
                                    .multi = "explode")      
  }
  return(baseURL)
}

#' Check values from codeservice
#' 
#' Call a service to check on values from:
#' \url{https://api.waterdata.usgs.gov/samples-data/codeservice/docs}.
#' 
#' @param service Options are: "characteristicgroup", "states", "counties",
#' "countries", "sitetype", "samplemedia", "characteristics", "observedproperty"
#' @param ... Optional additional query arguments. Only "characteristics" and
#' "observedproperty" have additional parameters options. 
#' @return List, structure depends on service.
#' @export
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' groups <- check_param("characteristicgroup")
#' states <- check_param("states")
#' countries <- check_param("countries")
#' counties <- check_param("counties")
#' sitetypes <- check_param("sitetype")
#' samplemedia <- check_param("samplemedia")
#' characteristics <- check_param("characteristics",
#'                                group = "Biological")
#' observedProperties <- check_param("observedproperty",
#'                                   text = "phosphorus")
#' 
#' }
check_param <- function(service = "characteristicgroup",
                        ...){
  
  service_options <- c("characteristicgroup", "states", "counties",
                       "countries", "sitetype", "samplemedia",
                       "characteristics", "observedproperty")
  
  match.arg(service, choices = service_options, several.ok = FALSE)
  
  check_group_req <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data",
                               "codeservice",
                               service) |> 
    httr2::req_url_query(mimeType = "application/json") 

  if (length(list(...)) > 0) {
    params <- list(...)
    extra_params <- c("group", "pageNumber", "pageSize",
                      "text")
    match.arg(names(params), extra_params, several.ok = TRUE)
    check_group_req <- httr2::req_url_query(check_group_req,
                                            !!!params)
  }

  check_group <- httr2::req_perform(check_group_req) |> 
    httr2::resp_body_string() |> 
    jsonlite::fromJSON()
    
  return(check_group$data)
  
}

#' USGS Samples Data
#' 
#' This function creates the call and gets the data for discrete water quality samples data
#' service described at \url{https://waterdata.usgs.gov/download-samples}.
#'
#' @inheritParams construct_USGS_sample_request
#' @param tz character to set timezone attribute of datetime. Default is UTC
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values include "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua",
#' "America/Phoenix", and "America/Metlakatla"
#' @export
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' ph_data <- read_USGS_sample(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                activityStartDateUpper = "2000-01-01",
#'                dataProfile = "Narrow")
#'                
#' nameToUse <- "pH"
#' pHData <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315", 
#'                          characteristic = nameToUse)
#' ncol(pHData)
#' attr(pHData, "url")
#' attr(pHData, "queryTime")
#' 
#' summary_data <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315", 
#'                                dataType = "Projects")
#' 
#' }
read_USGS_samples <- function(monitoringLocationIdentifier = NA,
                           USstate = NA,
                           siteTypeCode = NA,
                           boundingBox = NA,
                           hydrologicUnit = NA,
                           activityMediaName = NA,
                           characteristicGroup = NA,
                           characteristic = NA,
                           characteristicUserSupplied = NA,
                           activityStartDateLower = NA,
                           activityStartDateUpper = NA,
                           countryFips = NA,
                           stateFips = NA,
                           countyFips = NA,
                           projectIdentifier = NA,
                           recordIdentifierUserSupplied = NA,
                           siteTypeName = NA,
                           usgsPCode = NA,
                           pointLocationLatitude = NA,
                           pointLocationLongitude = NA,
                           pointLocationWithinMiles = NA,
                           dataType = "Results",
                           dataProfile = NA,
                           tz = "UTC"){
  
  request_url <- construct_USGS_sample_request(monitoringLocationIdentifier = monitoringLocationIdentifier,
                                               USstate = USstate,
                                               siteTypeCode = siteTypeCode,
                                               boundingBox = boundingBox,
                                               hydrologicUnit = hydrologicUnit,
                                               activityMediaName = activityMediaName,
                                               characteristicGroup = characteristicGroup,
                                               characteristic = characteristic,
                                               characteristicUserSupplied = characteristicUserSupplied,
                                               activityStartDateLower = activityStartDateLower,
                                               activityStartDateUpper = activityStartDateUpper,
                                               countryFips = countryFips,
                                               stateFips = stateFips,
                                               countyFips = countyFips,
                                               projectIdentifier = projectIdentifier,
                                               recordIdentifierUserSupplied = recordIdentifierUserSupplied,
                                               siteTypeName = siteTypeName,
                                               usgsPCode = usgsPCode,
                                               pointLocationLatitude = pointLocationLatitude,
                                               pointLocationLongitude = pointLocationLongitude,
                                               pointLocationWithinMiles = pointLocationWithinMiles,
                                               dataType = dataType,
                                               dataProfile = dataProfile)

  df <- importWQP(request_url, tz = tz)
  attr(df, "url") <- request_url$url
  attr(df, "queryTime") <- Sys.time()
  return(df)
}


#' USGS Samples Summary Data
#' 
#' This function creates the call and gets the data for discrete water quality samples summary data
#' service described at \url{https://waterdata.usgs.gov/download-samples}.
#'
#' @param monitoringLocationIdentifier A monitoring location identifier has two parts: the agency code
#' and the location number. Location identifiers should be separated with commas,
#' for example: AZ014-320821110580701, CAX01-15304600, USGS-040851385. Location
#' numbers without an agency prefix are assumed to have the prefix USGS.
#' @export
#' @return data frame with summary of data available based on the monitoringLocationIdentifier
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' monitoringLocationIdentifier <- "USGS-04074950"
#' 
#' what_data <- summary_USGS_samples(monitoringLocationIdentifier)
#' 
#' }
summary_USGS_samples <- function(monitoringLocationIdentifier){
  message("Function in development, use at your own risk.")
  
  if(length(monitoringLocationIdentifier) > 1){
    stop("Summary service only available for one site at a time.")
  }
  
  baseURL <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data") |> 
    httr2::req_url_path_append("summary",
                               monitoringLocationIdentifier) |>
    httr2::req_url_query(mimeType = "text/csv")
  
  df <- importWQP(baseURL)
  
  if(all(c("firstActivity", "mostRecentActivity") %in% names(df))){
    df$firstActivity <- as.Date(df$firstActivity)
    df$mostRecentActivity <- as.Date(df$mostRecentActivity)
  }
  
  attr(df, "url") <- baseURL$url
  attr(df, "queryTime") <- Sys.time()
  
  return(df)
}