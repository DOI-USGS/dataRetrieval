#' Get USGS Rating Curve Data
#'
#' Reads current rating table for an active USGS streamgages.
#'
#' @param monitoring_location_id A unique identifier representing a single
#' monitoring location. Monitoring location IDs are created by combining the
#' agency code of the agency responsible for the monitoring location (e.g. USGS)
#' with the ID number of the monitoring location (e.g. 02238500), separated by
#' a hyphen (e.g. USGS-02238500).
#' @param file_type Rating file time. Could be any of "exsa", "corr", or "base".
#' If `file_type` is "base" then the columns are
#' INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#' in cubic feet per second; and STOR, where "*" indicates that the pair are
#' a fixed point of the rating curve. If `file_type` is "exsa" then an
#' additional column, SHIFT, is included that indicates the current shift in
#' the rating for that value of INDEP. If `file_type` is "corr" then the
#' columns are INDEP, typically the gage height, in feet; CORR, the correction
#' for that value; and CORRINDEP, the corrected value for CORR.
#' @param file_path Path to save the rating curve rdb files. The
#' default is `tempdir()`, which will wipe out the files.
#' @param datetime Only return items that have a temporal property that
#' intersects this value. Either a date-time or an interval, open or closed.
#' See Details below.
#' @param \dots Not used.
#' @param limit Limits the number of results that are included in each page of
#' the response (capped at the default 10,000).
#' @param download_and_parse Logical to define whether or not to download, parse,
#' and return a list of data frames with rating curve data (`TRUE`), or to return
#' just a list of available rating curve files (`FALSE`). Default is `TRUE`.
#' @export
#' @inherit read_waterdata_continuous details
#'
#' @return List of data frames which contain the requested rating curves.
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' monitoring_location_id <- c("USGS-01104475", "USGS-01104460")
#' ratings_exsa <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = "exsa")
#'
#' head(ratings_exsa[["USGS-01104475.exsa.rdb"]])
#' comment(ratings_exsa[["USGS-01104475.exsa.rdb"]])[1:15]
#'
#' ratings_corr <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = "corr")
#'
#' head(ratings_corr[["USGS-01104460.corr.rdb"]])
#' comment(ratings_corr[["USGS-01104460.corr.rdb"]])[1:15]
#'
#' rating_2 <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = c("corr", "exsa"))
#'
#' bbox <- c(-95.00, 40.0, -92.0, 42)
#'
#' bbox_query <- read_waterdata_ratings(bbox = bbox,
#'                                      download_and_parse = FALSE)
#' length(bbox_query)
#' recent_query <- read_waterdata_ratings(bbox = bbox,
#'                                        datetime = c(Sys.Date()-7, NA),
#'                                        download_and_parse = FALSE)
#' length(recent_query)
#'}
read_waterdata_ratings <- function(
  monitoring_location_id = NA_character_,
  file_type = "exsa",
  file_path = tempdir(),
  bbox = NA,
  datetime = NA_character_,
  ...,
  limit = 10000,
  download_and_parse = TRUE
) {
  match.arg(
    arg = file_type,
    choices = c("exsa", "base", "corr"),
    several.ok = TRUE
  )
  rlang::check_dots_empty()

  request <- httr2::request("https://api.waterdata.usgs.gov/stac/v0/") |>
    httr2::req_url_path_append("search")

  if (!all(is.na(monitoring_location_id))) {
    if (length(monitoring_location_id) > 1) {
      monitoring_location_id <- paste0(
        monitoring_location_id,
        collapse = "', '"
      )
    }

    request <- request |>
      httr2::req_url_query(
        filter = sprintf(
          "monitoring_location_id IN ('%s')",
          monitoring_location_id
        )
      )
  }

  if (!all(is.na(datetime))) {
    if (any(grepl("P", datetime))) {
      stop(
        "Periods are not supported in datetime argument in the rating curve service."
      )
    }
    datetime <- format_api_dates(datetime, date = FALSE)

    request <- request |>
      httr2::req_url_query(datetime = datetime)
  }

  if (all(!is.na(bbox))) {
    request <- httr2::req_url_query(
      request,
      bbox = as.numeric(bbox),
      .multi = "comma"
    )
  }

  request <- request |>
    httr2::req_url_query(limit = limit) |>
    dataRetrieval:::basic_request()

  resp <- httr2::req_perform(request)
  features <- httr2::resp_body_json(resp)[["features"]]

  if (download_and_parse) {
    return_list <- list()
    for (feature in features) {
      id <- feature$id
      df <- download_convert(feature, file_path, file_type)
      if (!is.null(df)) {
        return_list[[id]] <- df
      }
    }

    return(return_list)
  } else {
    return(features)
  }
}

download_convert <- function(feature, file_path, file_type) {
  links <- feature$links
  id <- feature$id
  url <- feature$assets$data$href

  if (any(sapply(file_type, function(x) grepl(x, url)))) {
    full_file_path <- file.path(file_path, id)
    download.file(url = url, destfile = full_file_path)
    rating <- dataRetrieval::importRDB1(full_file_path)
    return(rating)
  }

  return(NULL)
}
