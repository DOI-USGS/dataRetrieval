#' Coordinate the request and retrieval of STAC calls
#'
#' @param args arguments from individual functions
#' @param file_directory Where to download
#' @param download Whether to download data or not
#' @param load_all Whether to load data or not
#'
#' @noRd
get_stac_data <- function(args, file_directory, download, load_all) {
  search_features <- args[!is.na(args)]

  features <- do.call(search_stac, search_features)

  if (download) {
    download_files <- download_stac(
      features = features,
      file_directory = file_directory,
      output_returned = c("partition_folder")
    )

    if (!load_all) {
      return(unique(download_files))
    } else {
      con <- DBI::dbConnect(duckdb::duckdb(), allow_extensions = FALSE)
      on.exit(DBI::dbDisconnect(con))

      query <- sprintf(
        "SELECT * FROM read_parquet('%s/**/*.parquet')",
        download_files
      )
      df_out <- DBI::dbGetQuery(con, query)

      if ("value" %in% names(df_out)) {
        df_out$value <- as.numeric(df_out$value)
      }

      if ("qualifier" %in% names(df_out)) {
        df_out$qualifier <- as.character(df_out$qualifier)
        df_out$qualifier[df_out$qualifier == "NULL"] <- NA_character_
      }

      attr(df_out, "download_location") <- download_files
      return(df_out)
    }
  } else {
    assets <- sapply(features, function(x) x[["assets"]])
    urls <- get_all_hrefs(assets)

    if (load_all) {
      sql_url_array <- paste0(
        "[",
        paste(paste0("'", urls, "'"), collapse = ", "),
        "]"
      )
      con <- DBI::dbConnect(duckdb::duckdb(), allow_extensions = FALSE)
      on.exit(DBI::dbDisconnect(con))
      add_duck_header(con)

      query <- sprintf("SELECT * FROM read_parquet(%s)", sql_url_array)

      df_out <- DBI::dbGetQuery(con, query)
      if ("value" %in% names(df_out)) {
        df_out$value <- as.numeric(df_out$value)
      }

      if ("qualifier" %in% names(df_out)) {
        df_out$qualifier <- as.character(df_out$qualifier)
        df_out$qualifier[df_out$qualifier == "NULL"] <- NA_character_
      }
      return(df_out)
    } else {
      return(urls)
    }
  }
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

add_duck_header <- function(con) {
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  token <- Sys.getenv("API_USGS_PAT")
  if (token != "") {
    DBI::dbExecute(
      con,
      paste0(
        "
CREATE SECRET http_headers (
    TYPE http,
    EXTRA_HTTP_HEADERS MAP {
      'X-Api-Key': '",
        Sys.getenv("API_USGS_PAT"),
        "'
    }
  );
"
      )
    )
  }
}
