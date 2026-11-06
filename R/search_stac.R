#' Search STAC services
#'
#' The USGS Water Data STAC API provides access to USGS water data using the
#' SpatioTemporal Asset Catalog (STAC) family of standards. These APIs will
#' provide access to a wide array of USGS water data stored in native formats.
#' That means this API will provide you with links to where the data you're
#' interested in is stored, rather than providing you the data directly as a
#'  result of your query.
#'
#' @param \dots filter arguments to filter on
#' @param collections Could be "daily", "continuous", "ratings".
#' @param limit description
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth).
#' @param custom_filter Custom STAC filter. Default is NA. Uses CQL2 formatting.
#' @return list of features available in the STAC service.
#' @export
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#'
#' features_2_sites <- search_stac(
#'     monitoring_location_id = monitoring_location_id,
#'     collections = c("daily", "continuous"))
#'
#' features_county <- search_stac(
#'     county_name = "Dane County",
#'     state_name = "Wisconsin",
#'     site_type = "Stream",
#'     #start_datetime = c(NA, "2000-01-01"), # starts before 2000
#'     end_datetime = c("2026-01-01", NA), # ends after 2026
#'     collections = "daily"
#' )
#'
#' # HUCs return all HUCs that start with the entered value
#' features_huc <- search_stac(
#'   hydrologic_unit_code = "07090002"
#' )
#'
#' unlist(unique(sapply(features_huc, function(x)
#'     x[["properties"]]["hydrologic_unit_code"])))
#'
#' features_huc2 <- search_stac(
#'   hydrologic_unit_code = c("0709000203", "0709000204")
#' )
#'
#' unlist(unique(sapply(features_huc2, function(x)
#'     x[["properties"]]["hydrologic_unit_code"])))
#'
#' # Paging example:
#' features_OH <- search_stac(
#'   state_name = "Ohio",
#'   limit = 500
#' )
#'
#'}
search_stac <- function(
  ...,
  collections = c("daily", "continuous", "ratings"),
  bbox = NA,
  limit = getOption("dataRetrieval.limit"),
  custom_filter = NA_character_
) {
  match.arg(
    collections,
    choices = c("daily", "continuous", "ratings"),
    several.ok = TRUE
  )

  request <- httr2::request("https://api.waterdata.usgs.gov/stac") |>
    httr2::req_url_path_append(getOption("dataRetrieval.api_version_stac")) |>
    httr2::req_url_path_append("search")

  if (!all(is.na(collections))) {
    request <- request |>
      httr2::req_url_query(collections = collections, .multi = "comma")
  }

  filter_args <- list(...)

  dt_index <- grep("datetime|updated", names(filter_args))
  like_index <- grep("hydrologic_unit_code", names(filter_args))

  if (length(dt_index) > 0) {
    in_filters <- filter_args[-dt_index]
  } else {
    in_filters <- filter_args
  }

  if (length(like_index) > 0) {
    in_filters <- in_filters[-grep("hydrologic_unit_code", names(in_filters))]
  } else {
    in_filters <- in_filters
  }

  in_filters <- do.call(add_in_filter, args = in_filters)
  date_filters <- do.call(add_datetime_filter, args = filter_args[dt_index])
  like_filters <- do.call(add_like_filter, args = filter_args[like_index])

  if (!all(is.na(date_filters))) {
    request <- httr2::req_url_query(
      request,
      !!!date_filters
    )
  }

  all_filters <- c(in_filters, custom_filter)

  if (all(!is.na(bbox))) {
    request <- httr2::req_url_query(
      request,
      bbox = as.numeric(bbox),
      .multi = "comma"
    )
  }

  if (!all(is.na(all_filters))) {
    all_filters <- all_filters[!is.na(all_filters)]
    filters <- paste0(all_filters, collapse = " AND ")

    if (length(like_filters) > 0) {
      filters <- paste0(filters, " OR ", like_filters)
    }

    request <- request |>
      httr2::req_url_query(filter = filters) |>
      httr2::req_url_query(`filter-lang` = "cql2-text")
  } else {
    if (length(like_filters) > 0) {
      filters <- like_filters
    }

    request <- request |>
      httr2::req_url_query(filter = filters) |>
      httr2::req_url_query(`filter-lang` = "cql2-text")
  }

  request <- request |>
    httr2::req_url_query(limit = limit) |>
    basic_request()

  message("Requesting:\n", request$url)

  resps <- httr2::req_perform_iterative(
    request,
    next_req = next_req_url,
    max_reqs = Inf,
    on_error = "stop"
  )

  failures <- resps |> httr2::resps_failures()

  features <- resps |>
    httr2::resps_successes() |>
    httr2::resps_data(\(resp) get_stac_features(resp))

  return(features)
}


get_stac_features <- function(resp) {
  features <- httr2::resp_body_json(resp)[["features"]]
  return(features)
}


#' Download STAC files
#'
#' Download STAC files based on filter parameters.
#'
#' @param features Output from search_stac
#' @param file_directory Folder to store parquet files.
#' @param output_returned Output of this function is either a vector of file
#' paths that point to each individual file ("targets"), or the subfolder of
#' partitioned parquet files ("partition_folder")
#' @param showWarnings Logical, whether to show a warning or not if the
#' file_directory already has subdirectory of the requested collection.
#' @export
#' @details
#' Downloaded files are structured as partitioned parquet files.
#' The partitioning is defined by the feature id, which in most cases is
#' monitoring_location_id. Each collection is stored in a
#' subfolder of the file_directory, and within those subfolders are partitioned
#' parquet files.
#'
#' @return vector of file paths to downloaded data
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#' file_out_dir <- tempdir()
#'
#' features_2_sites <- search_stac(
#'     monitoring_location_id = monitoring_location_id,
#'     collections = c("daily", "continuous"))
#' files_out <- download_stac(
#'     features = features_2_sites,
#'     file_directory = file_out_dir)
#' files_out
#'
#' file_out_dane <- tempdir()
#' dane_county_features <- search_stac(
#'     county_name = "Dane County",
#'     state_name = "Wisconsin",
#'     site_type = "Stream",
#'     collections = "daily",
#'     end_datetime = c("2026-10-01", NA)
#' )
#'
#' files_out_county <- download_stac(
#'   features = dane_county_features,
#'   file_directory = file_out_dane)
#'
#' }
download_stac <- function(
  features,
  file_directory = tempdir(),
  output_returned = c("partition_folder"),
  # file_structure = c("partitioned", "original"),
  showWarnings = TRUE
) {
  match.arg(output_returned, c("partition_folder", "targets"))
  multiple_files <- length(features) > 1
  collections <- unlist(unique(sapply(features, function(x) x["collection"])))
  multiple_assests <- length(collections) > 1

  if (showWarnings) {
    unique_folders <- file.path(file_directory, collections)
    for (i in unique_folders) {
      if (length(list.dirs(i)) > 0) {
        warning("Files already existed within: ", i)
      }
    }
  }

  paths_out <- c()

  for (f in features) {
    assets <- f[["assets"]]

    urls <- sapply(assets, function(x) x[["href"]])

    id <- f[["id"]]

    collection <- f[["collection"]]

    p1 <- file.path(
      file_directory,
      collection
    )

    dir.create(p1, showWarnings = FALSE)

    partition_path <- file.path(
      p1,
      paste0("monitoring_location_id=", id)
    )

    dir.create(partition_path, showWarnings = FALSE)

    partition_index <- 0

    for (i in seq_along(urls)) {
      path_to_save <- file.path(
        partition_path,
        paste0("part-", partition_index, ".parquet")
      )
      partition_index <- partition_index + 1

      message("Downloading ", id, ": ", collection)

      resp <- httr2::request(urls[i]) |>
        basic_request() |>
        httr2::req_perform(path = path_to_save)

      if (output_returned == "targets") {
        paths_out <- c(paths_out, path_to_save)
      }
    }

    if (output_returned == "partition_folder") {
      paths_out <- unique(c(paths_out, p1))
    }
  }

  return(paths_out)
}

add_datetime_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (any(grepl("P", args[[i]]))) {
      stop(
        "Periods are not supported in datetime argument in the STAC service."
      )
    }
    datetime <- format_api_dates(args[[i]], date = FALSE)
    out <- c(out, datetime)
  }
  names(out) <- names(args)
  return(out)
}

add_in_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (length(args[[i]]) > 1) {
      vals <- paste0(args[[i]], collapse = "', '")

      out <- c(out, sprintf("%s IN ('%s')", names(args[i]), vals))
    } else {
      vals <- args[[i]]
      out <- c(out, sprintf("%s = '%s'", names(args[i]), vals))
    }
  }

  return(out)
}

add_like_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (length(args[[i]]) > 1) {
      for (j in seq_along(args[[i]])) {
        vals <- args[[i]][j]
        like_single <- sprintf("%s LIKE '%s%%'", names(args[i]), vals)
        out <- c(out, like_single)
      }
    } else {
      vals <- args[[i]]
      out <- c(out, sprintf("%s LIKE '%s%%'", names(args[i]), vals))
    }
  }

  out <- paste0(out, collapse = " OR ")
  return(out)
}
