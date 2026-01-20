#' Generalized USGS Water Meta Data API retrieval function
#' 
#' Function to get metadata from Water Data API. These are useful to get the
#' human readable words and other metadata associated with USGS codes.
#' 
#' @export
#' @param collection character, can be any existing collection such
#' as "parameter-codes", "agency-codes", "altitude-datums", "aquifer-codes",
#' "aquifer-types", "coordinate-accuracy-codes", "coordinate-datum-codes",
#' "coordinate-method-codes", "hydrologic-unit-codes", "medium-codes", 
#' "national-aquifer-codes", "reliability-codes", "site-types", "statistic-codes",
#' "topographic-codes", "time-zone-codes".
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param \dots Optional arguments to pass to the query. Available parameters
#' can be found with the \code{get_ogc_params} function.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' agency_codes <- read_waterdata_metadata("agency-codes")
#' altitude_datums <- read_waterdata_metadata("altitude-datums")
#' aquifer_codes <- read_waterdata_metadata("aquifer-codes")
#' aquifer_types <- read_waterdata_metadata("aquifer-types")
#' counties <- read_waterdata_metadata("counties")
#' us_counties <- read_waterdata_metadata("counties", country_code = "US")
#' coordinate_accuracy_codes <- read_waterdata_metadata("coordinate-accuracy-codes")
#' coordinate_datum_codes <- read_waterdata_metadata("coordinate-datum-codes")
#' coordinate_method_codes <- read_waterdata_metadata("coordinate-method-codes")
#' huc_codes <- read_waterdata_metadata("hydrologic-unit-codes")
#' national_aquifer_codes <- read_waterdata_metadata("national-aquifer-codes")
#' parameter_codes <- read_waterdata_metadata("parameter-codes")
#' reliability_codes <- read_waterdata_metadata("reliability-codes")
#' site_types <- read_waterdata_metadata("site-types")
#' states <- read_waterdata_metadata("states")
#' us_states_territories <- read_waterdata_metadata("states", country_code = "US")
#' statistic_codes <- read_waterdata_metadata("statistic-codes")
#' topographic_codes <- read_waterdata_metadata("topographic-codes")
#' time_zone_codes <- read_waterdata_metadata("time-zone-codes")
#' time_zone_limited <- read_waterdata_metadata("time-zone-codes",
#'                         time_zone_description = c("Alaska", "Hawaii", "Pacific North America"))
#' }
read_waterdata_metadata <- function(collection, 
                                    limit = NA,
                                    ...){
  
  match.arg(collection, pkg.env$metadata)

  output_id <- names(pkg.env$metadata)[pkg.env$metadata == collection]
  
  args <- list(...)
  
  if(length(args) > 0){
    possible_args <- get_ogc_params(collection)
    if(!all(names(args) %in% names(possible_args))){
      wrong_args <- names(args)[!names(args) %in% names(possible_args)]
      wrong_args <- paste0(wrong_args, collapse = ", ")
      stop(paste0("Unknown argument: ", wrong_args))
    }
  }
  
  args[["limit"]] <- limit
  args[["convertType"]] <- FALSE
  args[["skipGeometry"]] <- TRUE
  args[["bbox"]] <- NA
  args[["no_paging"]] <- FALSE # drops id if TRUE
  
  return_list <- get_ogc_data(args = args,
                              output_id = output_id,
                              service =  collection)
  
  return(return_list)
}



