#' Get period of record daily data
#'
#' Using the STAC Water Data service, get full period of record
#' returns for daily data. The most recent data may be missing,
#' but can be retrieved using `read_waterdata_daily`.
#'
#' @param monitoring_location_id Each monitoring location in the USGS data base has a unique 8- to 15-digit identification number.
#' @param county_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$county_name$description`
#' @param state_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$state_name$description`
#' @param site_type `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type$description`
#' @param start_datetime Earliest date. Either a date-time or an interval,
#' open or closed. See Details below.
#' @param end_datetime Latest date. Either a date-time or an interval, open
#' or closed. See Details below.
#' @param datetime Only return items that have a temporal property that
#' intersects this value. Either a date-time or an interval, open or closed.
#' See Details below.
#' @param site_type_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type_code$description`
#' @param hydrologic_unit_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$hydrologic_unit_code$description`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax).
#' Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param \dots Not used.
#' @param site_limits Set a limit to how many sites will be returned
#' from the STAC search.
#' @param download Logical, whether to download the files or not. Default
#' is `TRUE`.
#' @param file_directory Location to download files.
#' @param load_all Logical, whether to load data or not. Default is `TRUE`.
#' @param showWarnings Logical, whether to show a warning if the file_directory
#' already contains data for the daily collection. Default is `TRUE`.
#' @return What is returned depends on the arguments download and load_all.
#' * If download = `TRUE` and load_all = `TRUE`, the data has been downloaded
#' locally, and the data is returned as a data frame. An attribute is
#' attached to the data frame that shows the path to the downloaded data.
#' * If download = `FALSE` and load_all = `TRUE`, the data will be returned
#' as a data frame. Under the hood, a duckdb connection was created to hit
#' the STAC service endpoints.
#' * If download = `TRUE` and load_all = `FALSE`, the data has been
#' downloaded locally. The path to the local data is returned.
#' * If download = `FALSE` and load_all = `FALSE`, the url(s) to the data
#' are returned.
#'
#' @export
#' @inherit read_waterdata_continuous details
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' dir.create("raw_data_store", showWarnings = FALSE)
#' # Download and load data:
#' df <- read_waterdata_daily_por(
#'     monitoring_location_id = "USGS-0208458892",
#'     file_directory = "raw_data_store",
#'     download = TRUE,
#'     load_all = TRUE,
#'     showWarnings = FALSE
#' )
#' nrow(df)
#' attr(df, "download_location")
#'
#' # Just load data:
#' df_no_download <- read_waterdata_daily_por(
#'     monitoring_location_id = "USGS-0208458892",
#'     download = FALSE,
#'     load_all = TRUE
#' )
#' nrow(df)
#'
#' # Just download returns path to partition folder:
#' df_only_download <- read_waterdata_daily_por(
#'     monitoring_location_id = "USGS-0208458892",
#'     download = TRUE,
#'     file_directory = "raw_data_store",
#'     load_all = FALSE,
#'     showWarnings = FALSE
#' )
#' df_only_download
#'
#' # Don't download or load returns URLs to data:
#' feature_urls <- read_waterdata_daily_por(
#'     monitoring_location_id = "USGS-0208458892",
#'     download = FALSE,
#'     load_all = FALSE
#' )
#' feature_urls
#'
#' # Multiple features:
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#'
#' df <- read_waterdata_daily_por(
#'     monitoring_location_id = monitoring_location_id,
#'     download = FALSE,
#'     load_all = TRUE
#' )
#' nrow(df)
#'
#' # Download data, return path to partitioned folder:
#' files_explore <- read_waterdata_daily_por(
#'     monitoring_location_id = monitoring_location_id,
#'     file_directory = "raw_data_store",
#'     download = TRUE,
#'     load_all = FALSE,
#'     showWarnings = FALSE
#' )
#'
#' # Example of how to use partitioned files:
#' # These examples will connect to all the daily data in "raw_data_store":
#' con <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE))
#' df_explore <- dplyr::tbl(con,
#'                          paste0(files_explore,
#'                           "/**/*.parquet")) |>
#'    dplyr::mutate(value = as.numeric(value)) |>
#'    dplyr::filter(
#'       parameter_code == "00095",
#'       approval_status == "Approved",
#'       time > as.POSIXct("2015-01-01"),
#'       value > 2000
#'      ) |>
#'    dplyr::collect()
#'
#' df_summarize <- dplyr::tbl(con,
#'                          paste0(files_explore,
#'                           "/**/*.parquet")) |>
#'    dplyr::mutate(value = as.numeric(value),
#'                  month = lubridate::month(time)) |>
#'    dplyr::summarize(
#'       min_val = min(value, na.rm = TRUE),
#'       mean_val = mean(value, na.rm = TRUE),
#'       max_val = max(value, na.rm = TRUE),
#'       .by = c(monitoring_location_id, parameter_code, month)
#'       ) |>
#'    dplyr::arrange(monitoring_location_id, parameter_code, month) |>
#'    dplyr::collect()
#'
#' DBI::dbDisconnect(con)
#'
#' # HUC example:
#' # Here's a GOTCHA!
#' # If you keep saving the files in the same directory,
#' # when you pull the data later, if you just generally pull
#' # from that file, you will get all the data you've ever downloaded,
#' # not just these 2 HUCs. Therefore, you will want to make a new
#' # directory when you are specific filters you will be using:
#' dir.create("raw_HUC", showWarnings = FALSE)
#' huc_files <- read_waterdata_daily_por(
#'     hydrologic_unit_code = c("0709000203", "0709000204"),
#'     file_directory = "raw_HUC",
#'     download = TRUE,
#'     load_all = FALSE,
#'     showWarnings = FALSE
#' )
#'
#' con <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE))
#'
#' huc_explore <- dplyr::tbl(con,
#'                          paste0(huc_files,
#'                           "/**/*.parquet")) |>
#'    dplyr::mutate(value = as.numeric(value)) |>
#'    dplyr::filter(
#'       parameter_code %in% c("00060", "00065")
#'      ) |>
#'    dplyr::collect()
#'
#' DBI::dbDisconnect(con)
#' }
read_waterdata_daily_por <- function(
  monitoring_location_id = NA_character_,
  county_name = NA_character_,
  state_name = NA_character_,
  site_type = NA_character_,
  start_datetime = NA_character_,
  end_datetime = NA_character_,
  datetime = NA_character_,
  site_type_code = NA_character_,
  hydrologic_unit_code = NA_character_,
  bbox = NA,
  ...,
  site_limits = 1000,
  download = TRUE,
  file_directory = tempdir(),
  load_all = getOption("dataRetrieval.limit"),
  showWarnings = TRUE
) {
  rlang::check_dots_empty()

  search_features <- list(
    monitoring_location_id = monitoring_location_id,
    county_name = county_name,
    state_name = state_name,
    site_type = site_type,
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    bbox = bbox,
    hydrologic_unit_code = hydrologic_unit_code,
    datetime = datetime,
    site_type_code = site_type_code,
    collections = "daily",
    limit = site_limits
  )

  df_out <- get_stac_data(
    args = search_features,
    file_directory = file_directory,
    download = download,
    load_all = load_all,
    showWarnings = showWarnings
  )
}
