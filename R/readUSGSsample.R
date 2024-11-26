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
#' @param dataType Options include: "Results", "Monitoring locations", "Activities"
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
#'                activityStartDateUpper = "2000-01-01")
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
                           dataProfile = "Full physical chemical"){
  
  message("Function in development, use at your own risk.")
  
  match.arg(dataType, c("Results",
                        "Monitoring locations",
                        "Activities"), 
            several.ok = FALSE)
  
  available_profiles <- c("Full physical chemical",
                          "Basic physical chemical",
                          "Full biological",
                          "Basic biological",
                          "Narrow")
  match.arg(dataProfile, available_profiles, 
            several.ok = FALSE)
  
  profile_convert <- c("fullphyschem",
                       "basicphyschem",
                       "fullbio",
                       "basicbio",
                       "narrow")
  names(profile_convert) <- available_profiles
  
  # When RMLS comes out...spring 2025ish,
  # we can verify these values easier, perhaps:
  baseURL <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data") |>
    httr2::req_url_query(mimeType = "text/csv")
  
  switch(dataType,
         Results = {
           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "results") |> 
             httr2::req_url_path_append(profile_convert[[dataProfile]])
         },
         `Monitoring locations` = {
           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "locations",
                                                 "site") 
           
         },
         Activities = {
           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "activities",
                                                 "sampact")
         })

  if(all(!is.na(siteTypeCode))){
    match.arg(siteTypeCode, 
              check_param("sitetype")$typeCode, 
              several.ok = TRUE)
  }
    
  if(all(!is.na(activityMediaName))){
    match.arg(activityMediaName, 
              check_param("samplemedia")$activityMedia, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(characteristicGroup))){
    match.arg(characteristicGroup, 
              check_param("characteristicgroup")$characteristicGroup, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(countryFips))){
    match.arg(countryFips, 
              check_param("countries")$countryCode, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(siteTypeName))){
    match.arg(siteTypeName, 
              check_param("sitetype")$typeName, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(stateFips))){
    states <- check_param("states")
    state_codes <- paste(states$countryCode, 
                         states$fipsCode, sep = ":")
    match.arg(stateFips, state_codes, 
              several.ok = TRUE)
  }
  
  if(all(!is.na(characteristic))){
    #check?    
  }
  
  if(!is.na(characteristicUserSupplied)){
    #check? 
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
                                countryFips = countryFips,
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
#' @examples
#' # example code
#' 
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' ph_data <- readUSGSsample(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                activityStartDateUpper = "2000-01-01",
#'                dataProfile = "Narrow")
#'                
#' nameToUse <- "pH"
#' pHData <- readUSGSsample(monitoringLocationIdentifier = "USGS-04024315", 
#'                          characteristic = nameToUse)
#' ncol(pHData)
#' attr(pHData, "url")
#' attr(pHData, "queryTime")
#'                
#' }
readUSGSsample <- function(monitoringLocationIdentifier = NA,
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
                           dataProfile = "Full physical chemical",
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
                                               dataType = dataType,
                                               dataProfile = dataProfile)

  df <- importWQP(request_url, tz = tz)
  attr(df, "url") <- request_url$url
  attr(df, "queryTime") <- Sys.time()
  return(df)
}

