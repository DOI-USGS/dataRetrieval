#' Coordinate the request and retrieval of STAC calls
#'
#' @param features return from search_stac
#' @param con Connection
#'
#' @export
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#'
#' features <- search_stac(
#'     monitoring_location_id = monitoring_location_id,
#'     collections = c("continuous"))
#'
#' ###################################################
#' # No download, no load: Returns lazy table
#' con_1 <- initial_connection()
#' data_tbl <- open_stac(features = features,
#'                       con = con_1)
#' head(data_tbl)
#'
#' summarize_data <- data_tbl |>
#'    dplyr::mutate(value = as.numeric(value)) |>
#'    dplyr::summarize(count = dplyr::n(),
#'                     start = min(time, na.rm = TRUE),
#'                     end = max(time, na.rm = TRUE),
#'                     max_val = max(value, na.rm = TRUE),
#'                     .by = c(time_series_id, monitoring_location_id, parameter_code)) |>
#'    dplyr::collect()
#'
#' summarize_data
#'
#' DBI::dbDisconnect(con_1)
#'
#' #########################################################
#' # Download data locally, no load: Returns lazy table
#' raw_data_store <- file.path(tempdir(), "test_1")
#' dir.create(raw_data_store, showWarnings = FALSE)
#'
#' local_path <- download_stac(
#'   features = features,
#'   file_directory = raw_data_store)
#'
#' con_2 <- initial_connection()
#'
#' data_tbl <- open_stac(features = local_path,
#'                       con = con_2)
#' head(data_tbl)
#' DBI::dbDisconnect(con_2)
#'
#'
#' }
open_stac <- function(
  con,
  features
) {
  if (is.list(features)) {
    assets <- sapply(features, function(x) x[["assets"]])
    urls <- get_all_hrefs(assets)
    path_to_data <- dplyr::sql(sprintf(
      "SELECT * FROM read_parquet([%s], union_by_name = true)",
      paste(sprintf("'%s'", urls), collapse = ", ")
    ))
  } else {
    # Check if files exist
    if (file.exists(features)) {
      path_to_data <- dplyr::sql(sprintf(
        "SELECT * FROM read_parquet('%s/**/*.parquet')",
        features
      ))
    } else {
      stop("Unknown features.")
    }
  }

  data_con <- dplyr::tbl(con, path_to_data)

  return(data_con)
}

clean_stac_cols <- function(x) {
  if ("value" %in% names(x)) {
    x$value <- as.numeric(x$value)
  }

  if ("qualifier" %in% names(x)) {
    x$qualifier <- as.character(x$qualifier)
    x$qualifier[x$qualifier == "NULL"] <- NA_character_
  }
  return(x)
}


get_all_hrefs <- function(x) {
  # Base case: if it's not a list, it can't contain an href key
  if (!is.list(x)) {
    return(NULL)
  }

  current_href <- x[["href"]]

  # Recursively get hrefs from all sub-lists
  sub_hrefs <- lapply(x, get_all_hrefs)

  # Combine current level with deeper levels and remove NULLs
  unlist(c(current_href, sub_hrefs), use.names = FALSE)
}

#' Initialize connection using duckdb and USGS API token
#'
#' Uses DBI and duckdb to establish a connection with a API_USGS_PAT X-Api-Key.
#'
#' @export
#' @return Database connection
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' con <- initial_connection()
#'
#' DBI::dbDisconnect(con)
#' }
initial_connection <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE))

  token <- Sys.getenv("API_USGS_PAT")
  if (token != "") {
    DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
    extra_headers <- paste0(
      "CREATE
SECRET http_headers (
TYPE http,
EXTRA_HTTP_HEADERS MAP {
  'X-Api-Key': '",
      Sys.getenv("API_USGS_PAT"),
      "'
});"
    )

    DBI::dbExecute(con, extra_headers)
  }
  return(con)
}
