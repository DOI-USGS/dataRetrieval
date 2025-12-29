#' Get summary statistic data from /statistics API
#' 
#' @description 
#' 
#' \code{read_waterdata_stats_normal} fetches day- or month-of-year data from the observationNormals endpoint.
#' 
#' \code{read_waterdata_stats} fetches calendar month-year, calendar year, or water year data from the observationIntervals endpoint.
#' 
#' @export
#' 
#' @param approval_status asdf
#' @param computation_type asdf
#' @param country_code asdf 
#' @param state_code asdf
#' @param county_code asdf
#' @param start_date asdf
#' @param end_date asdf
#' @param mime_type asdf
#' @param monitoring_location_id asfd
#' @param next_token asdf
#' @param parent_time_series_id aasdf
#' @param site_type_code asdf
#' @param site_type_name asdf
#' @param parameter_code asf
#' @param next_token asdf
#' @param page_size asdf
#' 
#' @rdname get_waterdata_stats
#' @seealso \url{https://api.waterdata.usgs.gov/statistics/v0/docs}
read_waterdata_stats_normal <- function(
  approval_status = NA,
  computation_type = NA_character_,
  country_code = NA_character_,
  state_code = NA_character_,
  county_code = NA_character_,
  start_date = NA_character_,
  end_date = NA_character_,
  mime_type = NA_character_,
  monitoring_location_id = NA_character_,
  parent_time_series_id = NA_character_,
  site_type_code = NA_character_,
  site_type_name = NA_character_,
  parameter_code = NA_character_,
  next_token = NA_character_,
  page_size = NA
) {
  
  args <- mget(names(formals()))

  get_statistics_data(args = args, service = "Normals")
}

#' @export
#' @rdname get_waterdata_stats
read_waterdata_stats_interval <- function(
  approval_status = NA,
  computation_type = NA_character_,
  country_code = NA_character_,
  state_code = NA_character_,
  county_code = NA_character_,
  start_date = NA_character_,
  end_date = NA_character_,
  mime_type = NA_character_,
  monitoring_location_id = NA_character_,
  next_token = NA_character_,
  parent_time_series_id = NA_character_,
  site_type_code = NA_character_,
  site_type_name = NA_character_,
  parameter_code = NA_character_,
  page_size = NA
){

  args <- mget(names(formals()))

  get_statistics_data(args = args, service = "Intervals")

}

#' Retrieve data from /statistics API
#' 
#' @param args arguments from individual functions.
#' @param service Ednpoint name.
#' 
#' @noRd
#' @return data.frame with attributes
get_statistics_data <- function(args, service) {

  base_request <- construct_statistics_request(service = service, version = 0)

  # TODO?: arg type checking here

  full_request <- explode_query(base_request, POST = FALSE, x = args)

  return_list <- walk_pages(full_request, max_results = NA)

  # TODO?: pull the following double-for loop out to other function(s)
  return_list_tmp <- list()
  for (i in 1:nrow(return_list)) {
    x <- jsonlite::parse_json(return_list$data[i])
    z <- data.frame(rbind_fill(x))
    zz <- list()
    for (j in 1:nrow(z)) {
      zz[[j]] <- cbind(
        subset(z[j, ], select = -values),
        rbind_fill(z$values[[j]]),
        row.names = NULL
      )
    }
    zz <- do.call(rbind, zz)

    return_list_tmp[[i]] <- cbind_sf_metadata(
      subset(return_list[i, ], select = -data),
      zz
    )
  }

  return_list <- do.call(rbind, return_list_tmp)

  attr(return_list, "request") <- full_request
  attr(return_list, "queryTime") <- Sys.time()

  return(return_list)
}

#' Create a request object for the /statistics service
#' 
#' @param service chr; "Normals" or "Intervals"
#' @param version int; /statistics API version number (default: 0)
#' 
#' @return a httr2 request object
#' 
#' @noRd
construct_statistics_request <- function(service = "Normals", version = 0){
 
  httr2::request("https://api.waterdata.usgs.gov/statistics/") |>
    httr2::req_url_path_append(paste0("v", version)) |>
    httr2::req_url_path_append(paste0("observation", service))
  
}

#' Bind a list of data frames by row
#' 
#' Meant to emulate data.table::rbindlist in how it handles missing data
#' 
#' @param dfs a list of data frames
#' 
#' @return a data frame
#' 
#' @noRd
rbind_fill <- function(dfs) {
  
  # drop NULL elements
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) return(NULL)
  
  # union of all column names
  all_names <- unique(unlist(lapply(dfs, names)))
  
  # align columns
  dfs_aligned <- lapply(dfs, function(x) {
    missing <- setdiff(all_names, names(x))
    if (length(missing)) {
      for (m in missing) x[[m]] <- NA
    }
    x[all_names]
  })
  
  # bind
  out <- do.call(rbind, dfs_aligned)
  rownames(out) <- NULL
  out
}

#' Combine sf metadata with observations from /statistics
#' 
#' @param meta_sf a single-row sf data frame object containing metadata about observations
#' @param obs_df a multi-row data frame of observations 
#' 
#' @return an sf data frame object
#' 
#' @noRd
cbind_sf_metadata <- function(meta_sf, obs_df) {
  
  stopifnot(
    inherits(meta_sf, "sf"),
    nrow(meta_sf) == 1
  )
  
  n <- nrow(obs_df)
  
  # replicate metadata rows
  meta_rep <- meta_sf[rep(1, n), , drop = FALSE]
  
  # drop geometry before cbind, then restore
  geom <- sf::st_geometry(meta_rep)
  meta_nogeo <- sf::st_drop_geometry(meta_rep)
  
  out <- cbind(meta_nogeo, obs_df)
  sf::st_sf(out, geometry = geom)
}
