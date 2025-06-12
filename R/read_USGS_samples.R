#' Construct request for USGS Samples Data
#' 
#' This function creates the call for discrete water quality samples data
#' service described at <https://waterdata.usgs.gov/download-samples/>.
#' Note: all possible arguments are included, but it is strongly recommended
#' to only use the NECESSARY arguments. Leave unnecessary arguments as the default
#' NA. 
#' 
#' See also: <https://api.waterdata.usgs.gov/samples-data/docs>.
#'  
#' @param monitoringLocationIdentifier A monitoring location identifier has two parts: the agency code
#' and the location number, separated by a dash (-). Location identifiers should be separated with commas,
#' for example: AZ014-320821110580701, CAX01-15304600, USGS-040851385. Location
#' numbers without an agency prefix are assumed to have the prefix USGS.
#' @param activityMediaName Sample media refers to the environmental medium that
#' was sampled or analyzed. See available options by running 
#' `check_USGS_sample_params("samplemedia")$activityMedia`.
#' @param siteTypeCode Site type code query parameter. See available
#' options by running `check_USGS_sample_params("sitetype")$typeCode`.
#' @param boundingBox North and South are latitude values; East and West are longitude values.
#' A vector of 4 (west, south, east, north) is expected.
#' An example would be: c(-92.8, 44.2, -88.9, 46.0).
#' @param hydrologicUnit Hydrologic Unit Codes (HUCs) identify physical areas
#' within the US that drain to a certain portion of the stream network. 
#' This filter accepts values containing 2, 4, 6, 8, 10 or 12 digits. 
#' @param activityMediaName Sample media refers to the environmental medium that
#' was sampled or analyzed.
#' @param activityStartDateLower The service will return records with dates earlier
#' than the value entered for activityStartDateUpper. Can be an R Date object, or
#' a string with format YYYY-MM-DD. The logic is inclusive, i.e. it will also return
#' records that match the date.
#' @param activityStartDateUpper The service will return records with dates later
#' than the value entered for activityStartDateLower. Can be an R Date object, or
#' a string with format YYYY-MM-DD. The logic is inclusive, i.e. it will also return
#' records that match the date.
#' @param characteristicGroup Characteristic group is a broad category describing the sample.
#' See available options by running
#' `check_USGS_sample_params("characteristicgroup")$characteristicGroup`.
#' @param characteristicUserSupplied Observed property is the USGS term for the
#' constituent sampled and the property name gives a detailed description of what
#' was sampled. Observed property is mapped to characteristicUserSupplied and replaces
#' the parameter name and pcode USGS
#' previously used to describe discrete sample data. Find more information in the
#' Observed Properties and Parameter Codes section of the Code Dictionary found here:
#' <https://waterdata.usgs.gov/code-dictionary/>.
#' @param characteristic Characteristic is a specific category describing the sample.
#' See available options by running 
#' `check_USGS_sample_params("characteristics")$characteristicName`.
#' @param stateFips State query parameter. To get a list of available state fips, 
#' run `check_USGS_sample_params("states")`. The "fips" can be created using the function
#' `stateCdLookup` - for example: `stateCdLookup("WI", "fips")`. 
#' FIPs codes for states take the format: 
#' CountryAbbrev:StateNumber, like US:55 for Wisconsin.
#' @param countyFips County query parameter. To get a list of available counties,
#' run `check_USGS_sample_params("counties")`. The "Fips" can be created using the function
#' `countyCdLookup` - for example: `countyCdLookup("WI", "Dane", "fips")` 
#' for Dane County, WI.
#' FIPs codes for counties take the format: 
#' CountryAbbrev:StateNumber:CountyNumber, like US:55:025 for Dane County, WI.
#' @param countryFips Country query parameter. Do not set redundant parameters. 
#' If another query parameter contains the country information, leave this parameter
#' set to the default NA. See available options by running `check_USGS_sample_params("countries")`,
#' where the "id" field contains the value to use in the countryFips input.
#' @param projectIdentifier Project identifier query parameter. This information
#' would be needed from prior project information. 
#' @param recordIdentifierUserSupplied Record identifier, user supplied identifier. This
#' information would be needed from the data supplier.
#' @param siteTypeName Site type name query parameter. See available
#' options by running `check_param("sitetype")$typeName`.
#' @param usgsPCode USGS parameter code. See available options by running 
#' `check_USGS_sample_params("characteristics")$parameterCode`.
#' @param pointLocationLatitude Latitude for a point/radius query (decimal degrees). Must be used
#' with pointLocationLongitude and pointLocationWithinMiles.
#' @param pointLocationLongitude Longitude for a point/radius query (decimal degrees). Must be used
#' with pointLocationLatitude and pointLocationWithinMiles.
#' @param pointLocationWithinMiles Radius for a point/radius query. Must be used
#' with pointLocationLatitude and pointLocationLongitude
#' @param dataType Options include: "Results", "Monitoring locations", "Activities",
#' "Projects", and "Organizations".
#' @param dataProfile Profile depends on type. Options for "results" dataType are: 
#' "fullphyschem", "basicphyschem", "fullbio", "basicbio", "narrow",
#' "resultdetectionquantitationlimit", "labsampleprep", "count". Options for "locations" are: 
#' "site" and "count". Options for "activities" are "sampact", "actmetric", "actgroup", 
#' and "count". Options for "projects" are:
#' "project" and "projectmonitoringlocationweight". Options for "organizations" are:
#' "organization" and "count".
#' @export
#' @keywords internal
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
#' }
construct_USGS_sample_request <- function(monitoringLocationIdentifier = NA,
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
                                          dataType = "results",
                                          dataProfile = NA){
  
  dataType <- match.arg(dataType, c("results",
                                    "locations",
                                    "activities",
                                    "projects",
                                    "organizations"), 
                        several.ok = FALSE)
  
  baseURL <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data") |>
    httr2::req_url_query(mimeType = "text/csv")
  
  switch(dataType,
         results = {
           available_profiles <- c("fullphyschem", "basicphyschem",
                                   "fullbio", "basicbio", "narrow", 
                                   "resultdetectionquantitationlimit", 
                                   "labsampleprep", "count")
           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "results")
         },
         locations = {
           available_profiles <- c("site", "count")
           
           baseURL <- httr2::req_url_path_append(baseURL, 
                                                 "locations")
         },
         activities = {
           available_profiles <- c("sampact", "actmetric", "actgroup", "count")
           
           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "activities")
         },
         projects = {
           available_profiles <- c("project", "projectmonitoringlocationweight")
           
           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "projects") 
         },
         organizations = {
           available_profiles <- c("organization", "count")
           
           baseURL <- httr2::req_url_path_append(baseURL,
                                                 "organizations")
         })
  
  dataProfile <- check_profile(dataProfile, available_profiles)
  dataProfile <- match.arg(dataProfile, 
                           available_profiles,
                           several.ok = FALSE)  
  
  baseURL <- httr2::req_url_path_append(baseURL,
                                        dataProfile) 
  
  
  if(all(!is.na(siteTypeCode))){
    siteTypeCode <- match.arg(siteTypeCode, 
                              check_USGS_sample_params("sitetype")$typeCode, 
                              several.ok = TRUE)
  }
  
  if(all(!is.na(activityMediaName))){
    activityMediaName <- match.arg(activityMediaName, 
                                   check_USGS_sample_params("samplemedia")$activityMedia, 
                                   several.ok = TRUE)
  }
  
  if(all(!is.na(characteristicGroup))){
    characteristicGroup <- match.arg(characteristicGroup, 
                                     check_USGS_sample_params("characteristicgroup")$characteristicGroup, 
                                     several.ok = TRUE)
  }
  
  if(all(!is.na(countryFips))){
    countryFips <- match.arg(countryFips, 
                             check_USGS_sample_params("countries")$countryCode, 
                             several.ok = TRUE)
  }
  
  if(all(!is.na(siteTypeName))){
    siteTypeName <- match.arg(siteTypeName, 
                              check_USGS_sample_params("sitetype")$typeLongName, 
                              several.ok = TRUE)
  }
  
  if(all(!is.na(stateFips))){
    states <- check_USGS_sample_params("states")
    state_codes <- paste(states$countryCode, 
                         states$fipsCode, sep = ":")
    stateFips <- match.arg(stateFips, state_codes, 
                           several.ok = TRUE)
  }
  
  if(all(!is.na(countyFips))){
    states <- check_USGS_sample_params("states")
    state_codes <- paste(states$countryCode, 
                         states$fipsCode, sep = ":")
    counties <- check_USGS_sample_params("counties")
    state_cd <- stats::setNames(states$fipsCode,
                                states$stateAbbrev)
    county_codes <- paste(counties$countryCode, 
                          state_cd[counties$stateAbbrev],
                          counties$countyCode, sep = ":")
    
    countyFips <- match.arg(countyFips, county_codes, 
                            several.ok = TRUE)
  }
  
  check_radius <- sum(is.na(c(pointLocationLatitude, 
                              pointLocationLongitude,
                              pointLocationWithinMiles)))
  
  if(!check_radius %in% c(3, 0)){
    stop("pointLocationLatitude, pointLocationLongitude, and pointLocationWithinMiles
           must all be defined, or none defined.")
  }
  
  baseURL <- explode_query(baseURL, POST = FALSE,
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
  
  return(baseURL)
}

check_profile <- function(dataProfile, profile_convert){
  if(is.na(dataProfile)){
    message(paste0("No profile specified, defaulting to '",
                   profile_convert[1], "'\n",
                   "Possible values are: \n",
                   paste0(profile_convert, collapse = ", ")))
    dataProfile <- profile_convert[1]
  } else {
    dataProfile <- match.arg(dataProfile, profile_convert, several.ok = FALSE)
  }
  return(dataProfile)
}

explode_query <- function(baseURL, POST = FALSE, x){
  
  if(!is.list(x)){
    return(baseURL)
  }
  
  if(any(!is.na(x))){
    x <- Filter(Negate(anyNA), x)
    if(POST){
      baseURL <- httr2::req_body_json(req = baseURL,
                                      data = x)
    } else {
      baseURL <- httr2::req_url_query(baseURL,
                                      !!!x,
                                      .multi = "explode")   
    }
    
  }
  return(baseURL)
}


#' Check values from codeservice
#' 
#' Call a service to check on values from:
#' <https://api.waterdata.usgs.gov/samples-data/codeservice/docs>.
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
#' groups <- check_USGS_sample_params("characteristicgroup")
#' states <- check_USGS_sample_params("states")
#' countries <- check_USGS_sample_params("countries")
#' counties <- check_USGS_sample_params("counties")
#' sitetypes <- check_USGS_sample_params("sitetype")
#' samplemedia <- check_USGS_sample_params("samplemedia")
#' characteristics <- check_USGS_sample_params("characteristics",
#'                                group = "Biological")
#' observedProperties <- check_USGS_sample_params("observedproperty",
#'                                   text = "phosphorus")
#' 
#' }
check_USGS_sample_params <- function(service = "characteristicgroup",
                                     ...){
  
  service_options <- c("characteristicgroup", "states", "counties",
                       "countries", "sitetype", "samplemedia",
                       "characteristics", "observedproperty")
  
  match.arg(service, choices = service_options, several.ok = FALSE)
  
  check_group_req <- httr2::request("https://api.waterdata.usgs.gov") |> 
    httr2::req_url_path_append("samples-data",
                               "codeservice",
                               service) |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_url_query(mimeType = "application/json") 
  
  if (length(list(...)) > 0) {
    params <- list(...)
    extra_params <- c("group", "pageNumber", "pageSize",
                      "text")
    match.arg(names(params), extra_params, several.ok = TRUE)
    check_group_req <- httr2::req_url_query(check_group_req,
                                            !!!params)
  }
  
  message("GET: ", check_group_req$url) 
  
  check_group <- httr2::req_perform(check_group_req) |> 
    httr2::resp_body_string() |> 
    jsonlite::fromJSON()
  
  return(check_group$data)
  
}

#' USGS Samples Data
#' 
#' This function creates the call and gets the data for discrete water quality samples data
#' service described at <https://waterdata.usgs.gov/download-samples/>.
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
#' ph_data <- read_USGS_samples(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                activityStartDateUpper = "2000-01-01",
#'                dataProfile = "narrow")
#'                
#' nameToUse <- "pH"
#' pHData <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315", 
#'                          characteristic = nameToUse)
#' ncol(pHData)
#' attr(pHData, "url")
#' attr(pHData, "queryTime")
#' 
#' summary_data <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315", 
#'                                dataType = "projects")
#' 
#' }
read_USGS_samples <- function(monitoringLocationIdentifier = NA,
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
                              dataType = "results",
                              dataProfile = NA,
                              tz = "UTC"){
  
  request_url <- construct_USGS_sample_request(monitoringLocationIdentifier = monitoringLocationIdentifier,
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
#' service described at <https://api.waterdata.usgs.gov/samples-data/docs>.
#'
#' @param monitoringLocationIdentifier A monitoring location identifier has two parts,
#' separated by a dash (-): the agency code and the location number. Location identifiers should be separated with commas,
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
#' what_data <- summarize_USGS_samples(monitoringLocationIdentifier)
#' 
#' }
summarize_USGS_samples <- function(monitoringLocationIdentifier){
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