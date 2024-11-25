#' Custruct request for USGS Samples Data
#' 
#' This function creates the call for discrete water quality samples data
#' service described at \url{https://waterdata.usgs.gov/download-samples}.
#'  
#' @param monitoringLocationIdentifier A monitoring location identifier has two parts: the agency code
#' and the location number. Location identifiers should be separated with commas,
#' for example: AZ014-320821110580701, CAX01-15304600, USGS-040851385. Location
#' numbers without an agency prefix are assumed to have the prefix USGS.
#' @param state_abb description
#' @param activityMediaName Sample media refers to the environmental medium that
#' was sampled or analyzed.
#' @param siteTypeCode Site type code query parameter.
#' @param boundingBox North and South are latitude values; East and West are longitude values.
#' @param hydrologicUnit Hydrologic Unit Codes (HUCs) identify physical areas
#' within the US that drain to a certain portion of the stream network. 
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
#' @param dataType Options include: "Results", "Monitoring locations", "Activities"
#' @param dataProfile Options include: "Full physical chemical", "Basic physical chemical",
#' "Full biological", "Basic biological", "Narrow"
#' @export
#' @return data frame returned from web service call.
#' 
#' @examples
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' req <- construct_USGS_sample_request(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field")
#' rawData <- getWebServiceData(req)
#' df <- readr::read_delim(rawData)
#'
#' req_site <- construct_USGS_sample_request(
#'                state_abb = "Wisconsin",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                dataType = "Monitoring locations",
#'                activityStartDateUpper = "2000-01-01")
#' rawData_sites <- getWebServiceData(req_site)
#' df_site <- readr::read_delim(rawData_sites)
#' }
construct_USGS_sample_request <- function(monitoringLocationIdentifier = NA,
                           state_abb = NA,
                           siteTypeCode = NA,
                           boundingBox = NA,
                           hydrologicUnit = NA,
                           activityMediaName = NA,
                           characteristicGroup = NA,
                           characteristicUserSupplied = NA,
                           activityStartDateLower = NA,
                           activityStartDateUpper = NA,
                           dataType = "Results",
                           dataProfile = "Full physical chemical"
                           ){
  
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
  # we can verify these values before sending them out
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
  
  if(all(!is.na(monitoringLocationIdentifier))){
    baseURL <- httr2::req_url_query(baseURL,
                                    monitoringLocationIdentifier = monitoringLocationIdentifier,
                                    .multi = "explode")
  }
  if(all(!is.na(siteTypeCode))){
    baseURL <- httr2::req_url_query(baseURL,
                                    siteTypeCode = siteTypeCode,
                                    .multi = "explode")
  }
  if(all(!is.na(activityMediaName))){
    baseURL <- httr2::req_url_query(baseURL,
                                    activityMediaName = activityMediaName,
                                    .multi = "explode")
  }
  if(all(!is.na(activityStartDateLower))){
    start <- as.character(as.Date(activityStartDateLower))
    baseURL <- httr2::req_url_query(baseURL,
                                    activityStartDateLower = start)
  }
  if(all(!is.na(activityStartDateUpper))){
    end <- as.character(as.Date(activityStartDateUpper))
    baseURL <- httr2::req_url_query(baseURL,
                                    activityStartDateUpper = end)
  }
  if(all(!is.na(characteristicUserSupplied))){
    baseURL <- httr2::req_url_query(baseURL,
                                    characteristicUserSupplied = characteristicUserSupplied,
                                    .multi = "explode")
  }
  if(!is.na(characteristicUserSupplied)){
    baseURL <- httr2::req_url_query(baseURL,
                                    characteristicUserSupplied = characteristicUserSupplied,
                                    .multi = "explode")
  }
  if(all(!is.na(state_abb))){
    stCdPrefix <- "US:"
    if (!grepl(stCdPrefix, state_abb)) {
      statecode <- paste0(stCdPrefix, zeroPad(stateCdLookup(state_abb, "id"), 2))
      baseURL <- httr2::req_url_query(baseURL,
                                      stateFips = statecode,
                                      .multi = "explode")
    }
  }
  
  return(baseURL)
}

#' USGS Samples Data
#' 
#' This function creates the call and gets the data for discrete water quality samples data
#' service described at \url{https://waterdata.usgs.gov/download-samples}.
#'
#' @inheritParams construct_USGS_sample_request
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' ph_data <- readUSGSsample(
#'                monitoringLocationIdentifier = "USGS-04074950",
#'                characteristicUserSupplied = "pH, water, unfiltered, field",
#'                activityStartDateUpper = "2000-01-01",
#'                dataProfile = "Narrow")
#' }
readUSGSsample <- function(monitoringLocationIdentifier = NA,
                           state_abb = NA,
                           siteTypeCode = NA,
                           boundingBox = NA,
                           hydrologicUnit = NA,
                           activityMediaName = NA,
                           characteristicGroup = NA,
                           characteristicUserSupplied = NA,
                           activityStartDateLower = NA,
                           activityStartDateUpper = NA,
                           dataType = "Results",
                           dataProfile = "Full physical chemical"){
  
  request_url <- construct_USGS_sample_request(monitoringLocationIdentifier = monitoringLocationIdentifier,
                                               state_abb = state_abb,
                                               siteTypeCode = siteTypeCode,
                                               boundingBox = boundingBox,
                                               hydrologicUnit = hydrologicUnit,
                                               activityMediaName = activityMediaName,
                                               characteristicGroup = characteristicGroup,
                                               characteristicUserSupplied = characteristicUserSupplied,
                                               activityStartDateLower = activityStartDateLower,
                                               activityStartDateUpper = activityStartDateUpper,
                                               dataType = dataType,
                                               dataProfile = dataProfile)

  df <- importWQP(request_url)
  
  return(df)
}

