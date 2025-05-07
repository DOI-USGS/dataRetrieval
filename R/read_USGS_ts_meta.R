#' Get USGS Timeseries metadata
#' 
#' Daily data and continuous measurements are grouped into timeseries, which
#' represent a collection of observations of a single parameter, potentially
#' aggregated using a standard statistic, at a single site. This endpoint provides
#' metadata about those timeseries, including their operational thresholds, units
#' of measurement, and when the earliest and most recent observations in a timeseries
#' occurred.
#' 
#' @export
#' @param parameter_code Parameter codes are 5-digit codes used to identify the
#' constituent measured and the units of measure. 
#' @param parameter_name A human-understandable name corresponding to parameter_code.
#' @param statistic_id A code corresponding to the statistic an observation represents.
#' Example codes include 00001 (max), 00002 (min), and 00003 (mean). 
#' @param last_modified The last time a record was refreshed in our database. This
#' may happen due to regular operational processes and does not necessarily indicate
#' anything about the measurement has changed. You can query this field using
#' date-times or intervals.
#' @param begin The datetime of the earliest observation in the timeseries.
#' Together with end, this field represents the period of record of a timeseries.
#' Note that some timeseries may have large gaps in their collection record.
#' @param end The datetime of the most recent observation in the timeseries.
#' Data returned by this endpoint updates at most once per day, and potentially
#' less frequently than that, and as such there may be more recent observations
#' within a timeseries than the timeseries end value reflects. Together with begin,
#' this field represents the period of record of a timeseries. It is additionally
#' used to determine whether a timeseries is "active".
#' @param computation_period_identifier Indicates the period of data used for
#' any statistical computations.
#' @param computation_identifier Indicates whether the data from this timeseries
#' represent a specific statistical computation.
#' @param thresholds Thresholds represent known numeric limits for a timeseries,
#' for example the historic maximum value for a parameter or a level below which
#' a sensor is non-operative. These thresholds are sometimes used to automatically
#' determine if an observation is erroneous due to sensor error, and therefore
#' shouldn't be included in the timeseries.
#' @param sublocation_identifier An optional human-readable identifier used to
#' specify where measurements are recorded at a monitoring location.
#' @param primary A flag identifying if the timeseries is a "primary" timeseries.
#' "Primary" timeseries (which have this flag) are standard observations which
#' undergo Bureau review and approval processes. Non-primary timeseries, which
#' will have missing values for "primary", are provisional datasets made available
#' to meet the need for timely best science and to assist with daily operations
#' which need real-time information. Non-primary timeseries data are only retained
#' by this system for 120 days. See the USGS Provisional Data Statement for more information.
#' @param web_description A description of what this timeseries represents, as
#' used by WDFN and other USGS data dissemination products.
#' @param crs Indicates the coordinate reference system for the results.
#' @param limit The optional limit parameter limits the number of items that are
#' presented in the response document. Only items are counted that are on the
#' first level of the collection in the response document. Nested objects
#' contained within the explicitly requested items shall not be counted.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' meta_1 <- read_NWIS_ts_meta(monitoring_location_id = site)
#' }
read_NWIS_ts_meta <- function(monitoring_location_id = NA_character_,
                              parameter_code = NA_character_,
                              parameter_name = NA_character_,
                              properties = c("monitoring_location_id",
                                             "unit_of_measure",
                                             "parameter_name",
                                             "begin",
                                             "end",
                                             "primary",
                                             "computation_identifier",
                                             "sublocation_identifier",
                                             "id",
                                             "last_modified",
                                             "web_description"),
                              limit = 10000,
                              statistic_id = NA_character_,
                              last_modified = NA_character_,
                              begin = NA_character_,
                              end = NA_character_,
                              unit_of_measure = NA_character_,
                              computation_period_identifier = NA_character_,
                              computation_identifier = NA_character_,
                              thresholds = NA,
                              sublocation_identifier = NA_character_,
                              primary = NA_character_,
                              id = NA_character_,
                              web_description = NA_character_,
                              use_sf = TRUE,
                              convertType = FALSE){
  
  message("Function in development, use at your own risk.")
  
  req_ts_meta <- construct_api_requests("timeseries-metadata",
                                        monitoring_location_id = monitoring_location_id,
                                        parameter_code = parameter_code,
                                        parameter_name = parameter_name,
                                        properties = properties,
                                        statistic_id = statistic_id,
                                        last_modified = last_modified,
                                        begin = begin,
                                        end = end,
                                        limit = limit,
                                        unit_of_measure = unit_of_measure,
                                        computation_period_identifier = computation_period_identifier,
                                        computation_identifier = computation_identifier,
                                        thresholds = thresholds,
                                        sublocation_identifier = sublocation_identifier,
                                        primary = primary,
                                        id = id,
                                        web_description = web_description)
        
  return_list <- walk_pages(req_ts_meta, use_sf)
  
  return_list <- return_list[, properties]
  
  if(convertType) return_list <- cleanup_cols(return_list)
  
  if(!"id" %in% properties){
    return_list <- return_list[, names(return_list)[!names(return_list) %in% "id"]]
  }
  
  return(return_list)
  
}