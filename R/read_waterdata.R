#' Generalized USGS Water Data API retrieval function
#' 
#' Function that allows complex CQL queries. 
#' See <https://api.waterdata.usgs.gov/docs/ogcapi/complex-queries/> 
#' for more information.
#' 
#' @export
#' @param service character, can be any existing collection such
#' as "daily", "monitoring-locations", "time-series-metadata"
#' @param CQL A string in a Common Query Language format.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @param \dots Additional arguments to send to the request. 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' cql <- '{
#' "op": "and",
#' "args": [
#'   {
#'     "op": "in",
#'     "args": [
#'       { "property": "parameter_code" },
#'       [ "00060", "00065" ]
#'     ]
#'   },
#'  {
#'     "op": "in",
#'     "args": [
#'       { "property": "monitoring_location_id" },
#'       [ "USGS-07367300", "USGS-03277200" ]
#'     ]
#'   }
#' ]
#' }'
#' 
#' dv_data <- read_waterdata(service = "daily",
#'                           CQL = cql,
#'                           time = c("2023-01-01", "2024-01-01"))
#' 
#' }
read_waterdata <- function(service, 
                           CQL,
                           ...,
                           convertType = TRUE){

  query_req <- get_collection()
  
  endpoints <- sapply(query_req$tags, function(x) x[["name"]])
  
  match.arg(service, endpoints)
  
  args <- list(...)
  args[["service"]] <-  service
  
  if(!"properties" %in% names(args)){
    args[["properties"]] <- NA_character_
  }
  
  data_req <- suppressWarnings(do.call(construct_api_requests, args))
  
  data_req <- data_req |>
    httr2::req_headers(`Content-Type` = "application/query-cql-json") |> 
    httr2::req_body_raw(CQL) 
  
  if("max_results" %in% names(args)){
    max_results <- args[["max_results"]]
  } else {
    max_results <- NA
  }
  
  return_list <- walk_pages(data_req, max_results)
  
  return_list <- deal_with_empty(return_list, args[["properties"]], service)
  
  if(convertType) return_list <- cleanup_cols(return_list)
  
  # Add other time series services when they come online
  if(service %in% c("daily")){
    return_list <- return_list[order(return_list$time, return_list$monitoring_location_id), ]
  }
  
  return_list <- rejigger_cols(return_list, args[["properties"]], service)
  
  return(return_list)
}



