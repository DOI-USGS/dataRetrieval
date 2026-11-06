#' Get USGS Rating Curve Data
#'
#' Reads current rating table for an active USGS streamgages. More information
#' can be found at https://api.waterdata.usgs.gov/docs/stac/.
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
#' @param agency_code Agency that operates the monitoring location (e.g. USGS).
#' @param monitoring_location_number Site number without the agency prefix (e.g. 01451500).
#' @param updated When the rating file was last updated (ISO 8601, UTC).
#' Either a date-time or an interval, open or closed. See Details below.
#' @param file_path Path to save the rating curve rdb files. The
#' default is `tempdir()`, which will wipe out the files.
#' @param datetime Only return items that have a temporal property that
#' intersects this value. Either a date-time or an interval, open or closed.
#' See Details below.
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax).
#' Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param \dots Not used.
#' @param limit Limits the number of results that are included in each page of
#' the response (capped at the default 10,000).
#' @param download_and_parse Logical to define whether or not to download, parse,
#' and return a list of data frames with rating curve data (`TRUE`), or to return
#' just a list of available rating curve files (`FALSE`). Default is `TRUE`.
#' @export
#' @inherit read_waterdata_continuous details
#'
#' @return List of named lists, one per requested rating file. Each element
#' contains:
#' \describe{
#'   \item{ratings}{Data frame of the rating curve.}
#'   \item{metadata}{Data frame of header/value pairs parsed from the comment
#'     attribute of the ratings file.}
#' }
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
#' head(ratings_exsa[["USGS-01104475.exsa.rdb"]]$ratings)
#' m1 <- ratings_exsa[["USGS-01104475.exsa.rdb"]]$metadata
#' m1
#'
#' ratings_corr <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = "corr")
#'
#' head(ratings_corr[["USGS-01104460.corr.rdb"]]$ratings)
#' m2 <- ratings_corr[["USGS-01104460.corr.rdb"]]$metadata
#' m2
#'
#' rating_2 <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = c("corr", "exsa"))
#' names(rating_2)
#' m3 <- rating_2$`USGS-01104460.exsa.rdb`$metadata
#' rat_data <- rating_2$`USGS-01104460.exsa.rdb`$ratings
#'
#' bbox <- c(-95.00, 40.0, -92.0, 42)
#'
#' bbox_query <- read_waterdata_ratings(bbox = bbox,
#'                                      download_and_parse = FALSE)
#' length(bbox_query)
#' recent_query <- read_waterdata_ratings(bbox = bbox,
#'                                        datetime = c(Sys.Date()-24, NA),
#'                                        download_and_parse = FALSE)
#'
#' recent_query_up <- read_waterdata_ratings(bbox = bbox,
#'                                        updated = c(Sys.Date()-24, NA),
#'                                        download_and_parse = FALSE)
#'}
read_waterdata_ratings <- function(
  monitoring_location_id = NA_character_,
  file_type = c("exsa", "base", "corr"),
  file_path = tempdir(),
  bbox = NA,
  datetime = NA_character_,
  updated = NA_character_,
  agency_code = NA_character_,
  monitoring_location_number = NA_character_,
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

  search_features <- list(
    monitoring_location_id = monitoring_location_id,
    file_type = file_type,
    bbox = bbox,
    datetime = datetime,
    collections = "ratings"
  )

  search_features <- search_features[!is.na(search_features)]

  features <- do.call(search_stac, search_features)

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

# Download and convert a rating curve feature to a tidy list
download_convert <- function(feature, file_path, file_type) {
  links <- feature$links
  id <- feature$id
  url <- feature$assets$data$href

  req <- httr2::request(url) |>
    basic_request()

  if (any(sapply(file_type, function(x) grepl(x, url)))) {
    full_file_path <- file.path(file_path, id)
    message("Requesting: \n", url)
    resp <- httr2::req_perform(req, path = full_file_path)
    rating <- importRDB1(full_file_path)
    return(list(
      ratings = rating,
      metadata = parse_ratings_metadata(rating)
    ))
  }

  return(NULL)
}

# Parse a single key=value or key="value" token into a named list
parse_kv_tokens <- function(line) {
  kv_pat <- '([A-Za-z0-9_]+)\\s*=\\s*"([^"]*)"|([A-Za-z0-9_]+)\\s*=\\s*(\\S+)'
  m <- gregexpr(kv_pat, line, perl = TRUE)
  tokens <- regmatches(line, m)[[1]]
  result <- list()
  for (tok in tokens) {
    key <- sub("^([A-Za-z0-9_]+)\\s*=.*", "\\1", tok, perl = TRUE)
    val <- sub('^[A-Za-z0-9_]+\\s*=\\s*"?([^"]*)"?$', "\\1", tok, perl = TRUE)
    result[[key]] <- val
  }
  result
}


parse_ratings_metadata <- function(ratings_df) {
  raw_comments <- comment(ratings_df)

  if (is.null(raw_comments)) {
    return(data.frame())
  }

  # strip leading # // or //
  lines <- sub("^#\\s*//?\\s*", "", raw_comments)
  lines <- sub("^//\\s*", "", lines)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # collect WARNING lines
  warn_idx <- grepl("^WARNING", lines)
  warn_text <- trimws(sub("^WARNING\\s*", "", lines[warn_idx]))
  warning_value <- paste(warn_text[warn_text != ""], collapse = " ")
  lines <- lines[!warn_idx]

  # split each line into header and the rest
  # header is the first all-caps+underscore token
  header_pat <- "^([A-Z][A-Z0-9_]*)\\s*(.*)"
  headers <- sub(header_pat, "\\1", lines, perl = TRUE)
  bodies <- sub(header_pat, "\\2", lines, perl = TRUE)

  # group lines by header, preserving order of first appearance
  unique_headers <- unique(headers)

  result <- list(WARNING = warning_value)
  i <- 1
  for (hdr in unique_headers) {
    idx <- which(headers == hdr)
    body_lines <- bodies[idx]

    # parse each line's key=value pairs
    parsed_rows <- lapply(body_lines, parse_kv_tokens)

    # check if any line has key=value pairs; if not, treat as plain text
    has_kv <- sapply(parsed_rows, length) > 0

    if (any(has_kv)) {
      p_rows <- unlist(parsed_rows)
      if (anyDuplicated(names(p_rows))) {
        columns <- unique(names(p_rows))
        df <- data.frame(matrix(
          NA_character_,
          nrow = length(p_rows) / length(columns),
          ncol = length(columns)
        ))
        names(df) <- columns
        for (i in columns) {
          df[[i]] <- p_rows[names(p_rows) %in% i]
        }
      } else {
        df <- data.frame(t(p_rows))
      }

      if (nrow(df) == 1 && ncol(df) == 1) {
        result[[hdr]] <- df[[1]]
      } else {
        result[[hdr]] <- df
      }
    } else {
      if (substr(body_lines, start = 1, stop = 1) == "=") {
        result[[hdr]] <- substr(body_lines, start = 2, stop = nchar(body_lines))
      } else {
        comment_label <- paste("Comment", i)
        result[[comment_label]] <- paste(
          trimws(c(hdr, body_lines)),
          collapse = " "
        )
        i <- i + 1
      }
    }
  }

  return(result)
}
