#' Generalized USGS Water Meta Data API retrieval function
#' 
#' Function to get metadata collections from Water Data API.
#' 
#' @export
#' @param collection character, can be any existing collection such
#' as "parameter-codes", "agency-codes", "altitude-datums", "aquifer-codes",
#' "aquifer-types", "coordinate-accuracy-codes", "coordinate-datum-codes",
#' "coordinate-method-codes", "hydrologic-unit-codes", "medium-codes", 
#' "national-aquifer-codes", "reliability-codes", "site-types", "statistic-codes",
#' "topographic-codes", "time-zone-codes"
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 10000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' agency_codes <- read_waterdata_metadata("agency-codes")
#' altitude_datums <- read_waterdata_metadata("altitude-datums")
#' aquifer_codes <- read_waterdata_metadata("aquifer-codes")
#' aquifer_types <- read_waterdata_metadata("aquifer-types")
#' coordinate_accuracy_codes <- read_waterdata_metadata("coordinate-accuracy-codes")
#' coordinate_datum_codes <- read_waterdata_metadata("coordinate-datum-codes")
#' coordinate_method_codes <- read_waterdata_metadata("coordinate-method-codes")
#' national_aquifer_codes <- read_waterdata_metadata("national-aquifer-codes")
#' parameter_codes <- read_waterdata_metadata("parameter-codes")
#' reliability_codes <- read_waterdata_metadata("reliability-codes")
#' site_types <- read_waterdata_metadata("site-types")
#' statistic_codes <- read_waterdata_metadata("statistic-codes")
#' topographic_codes <- read_waterdata_metadata("topographic-codes")
#' time_zone_codes <- read_waterdata_metadata("time-zone-codes")
#' }
read_waterdata_metadata <- function(collection, 
                          max_results = NA,
                          limit = NA){

  available <- c("parameter-codes", "agency-codes", "altitude-datums", "aquifer-codes",
                 "aquifer-types", "coordinate-accuracy-codes", "coordinate-datum-codes",
                 "coordinate-method-codes", "hydrologic-unit-codes", "medium-codes", 
                 "national-aquifer-codes", "reliability-codes", "site-types", "statistic-codes",
                 "topographic-codes", "time-zone-codes")
  
  match.arg(collection, available)

  output_id <- gsub("-", "_", collection)
  last_letter <- substr(output_id, 
                        start = nchar(output_id),
                        stop = nchar(output_id))
  if(last_letter == "s"){
    output_id <- substr(output_id, 
                        start = 1,
                        stop = nchar(output_id)-1)    
  }
  
  data_req <- suppressWarnings(construct_api_requests(service = collection, 
                                                      skipGeometry = TRUE, 
                                                      properties = NA,
                                                      limit = limit, 
                                                      max_results = max_results))
  
  return_list <- walk_pages(data_req, max_results)

  return_list <- rejigger_cols(df = return_list,
                               properties =  NA,
                               output_id =  output_id)
  
  attr(return_list, "request") <- data_req
  attr(return_list, "queryTime") <- Sys.time()
  
  return(return_list)
}



