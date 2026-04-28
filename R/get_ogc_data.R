#' Coordinate the request and retrieval of OGC calls
#'
#' @param args arguments from individual functions
#' @param output_id Name of id column to return
#' @param service Endpoint name.
#'
#' @noRd
#' @return data.frame with attributes
get_ogc_data <- function(args, output_id, service) {
  chunk_size <- args[["chunk_size"]]
  args[["..."]] <- NULL

  if (
    !is.na(chunk_size) & length(args[["monitoring_location_id"]]) > chunk_size
  ) {
    ml_splits <- split(
      args[["monitoring_location_id"]],
      ceiling(seq_along(args[["monitoring_location_id"]]) / chunk_size)
    )

    rl <- lapply(ml_splits, function(x) {
      args[["monitoring_location_id"]] <- x
      get_ogc_data(args = args, output_id = output_id, service = service)
    })

    rl_filtered <- rl[
      vapply(rl, FUN = function(x) dim(x)[1], FUN.VALUE = NA_integer_) > 0
    ]

    return_list <- data.frame(data.table::rbindlist(
      rl_filtered,
      use.names = TRUE,
      ignore.attr = TRUE
    ))
  } else {
    args[["output_id"]] <- output_id
    args[["service"]] <- service

    req <- do.call(construct_api_requests, args)

    no_paging <- grepl("f=csv", req$url)

    message("Requesting:\n", req$url)

    if (no_paging) {
      return_list <- get_csv(req, limit = args[["limit"]])
    } else {
      return_list <- walk_pages(req)
    }

    return_list <- deal_with_empty(
      return_list,
      args[["properties"]],
      service,
      isTRUE(args[["skipGeometry"]]),
      args[["convertType"]],
      no_paging
    )

    return_list <- rejigger_cols(return_list, args[["properties"]], output_id)

    if (args[["convertType"]]) {
      return_list <- cleanup_cols(return_list, service)
      return_list <- order_results(return_list)

      # Mostly drop the id column except ts-meta, monitoring location:
      if (
        !service %in%
          c(
            "monitoring-locations",
            "time-series-metadata",
            "field-measurements-metadata",
            "combined-metadata",
            "parameter-codes"
          )
      ) {
        return_list <- return_list[, names(return_list)[
          names(return_list) != output_id
        ]]
      }
      # Move other id columns:
      return_list <- move_id_col(return_list, output_id)
    }

    if (args[["attach_request"]]) {
      attr(return_list, "request") <- req
    }
  }

  if (
    !isTRUE(args[["skipGeometry"]]) &
      "geometry" %in% names(return_list)
  ) {
    if (
      all(sf::st_is_empty(return_list[["geometry"]])) &
        !"geometry" %in% args[["properties"]]
    ) {
      return_list <- sf::st_drop_geometry(return_list)
    } else {
      return_list <- sf::st_as_sf(return_list)
    }
  }

  attr(return_list, "queryTime") <- Sys.time()
  return(return_list)
}

order_results <- function(df) {
  if (all(c("time", "monitoring_location_id") %in% names(df))) {
    df <- df[order(df$time, df$monitoring_location_id), ]
  } else if ("time" %in% names(df)) {
    df <- df[order(df$time), ]
  }

  return(df)
}

move_id_col <- function(df, output_id) {
  if ("time_series_id" %in% names(df)) {
    df <- df[, c(names(df)[names(df) != "time_series_id"], "time_series_id")]
  }

  if ("field_series_id" %in% names(df)) {
    df <- df[, c(names(df)[names(df) != "field_series_id"], "field_series_id")]
  }

  return(df)
}

#' Switch properties id
#'
#' If a user asks for either "id" or "output_id", it is only included in the
#' properties if that's the only column requested. "id" will always come back,
#' so it is not needed in the properties call.
#'
#' @noRd
#' @return list
#' @examples
#'
#' properties <- c("id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties,
#'                               id = "monitoring_location_id")
#'
#' properties2 <- c("monitoring_location_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties2,
#'                               id = "monitoring_location_id")
#'
#' properties3 <- c("monitoring_locations_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties3,
#'                               id = "monitoring_location_id")
#'
#' properties4 <- c("monitoring_location_id")
#' dataRetrieval:::switch_properties_id(properties4,
#'                               id = "monitoring_location_id")
#'
#' properties5 <- c("monitoring_location_id", "geometry")
#' dataRetrieval:::switch_properties_id(properties5,
#'                               id = "monitoring_location_id")
#'
switch_properties_id <- function(properties, id) {
  orig_properties <- properties
  if (!all(is.na(properties))) {
    if ("id" %in% properties) {
      properties <- properties[properties != "id"]
    } else if (id %in% properties) {
      properties <- properties[properties != id]
    }

    if ("geometry" %in% properties) {
      properties <- properties[properties != "geometry"]
    }

    if (length(properties) == 0) {
      # If a user requested only id and/or geometry, properties would now be empty
      # geometry is taken care of with skipGeometry
      properties <- "id"
    }
  }

  return(properties)
}

#' Check non-API arguments
#'
#' Function to check types and create parameter descriptions.
#'
#' @param convertType logical, defaults to `r getOption("dataRetrieval.convertType")`.
#' If `TRUE`, the function will convert the data to dates, any qualifiers to string
#' vector and reorder the returned data frame.
#' @param no_paging logical, defaults to `r getOption("dataRetrieval.no_paging")`.
#' If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution.
#' @param limit numeric, The optional limit parameter is used to control the subset of the
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50,000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param attach_request logical, defaults to `r getOption("dataRetrieval.attach_request")`.
#' If set to `TRUE`, the full request sent to the Water Data API is attached
#' as an attribute to the data set.
#' @param chunk_size Number of monitoring_location_ids to chunk requests into.
#' The default for functions that don't generally return long-term data records
#' is `r getOption("dataRetrieval.site_chunk_size_meta")`, while
#' the default for time series functions is
#' `r getOption("dataRetrieval.site_chunk_size_data")`.
#' Setting to `NA` will eliminate site chunking, giving users full control.
#' @param \dots Not used. Included to help differentiate official Water Data API arguments
#' from more seldom used, optional dataRetrieval-specific arguments.
#' @keywords internal
check_arguments_non_api <- function(
  convertType,
  no_paging,
  limit,
  attach_request,
  chunk_size,
  ...
) {
  if (!is.null(convertType)) {
    if (!is.na(convertType) & !is.logical(convertType)) {
      stop("convertType should be a logical TRUE/FALSE")
    }
  }

  if (!is.null(no_paging)) {
    if (!is.na(no_paging) & !is.logical(no_paging)) {
      stop("no_paging should be a logical TRUE/FALSE")
    }
  }

  if (!is.null(attach_request)) {
    if (!is.na(attach_request) & !is.logical(attach_request)) {
      stop("attach_request should be a logical TRUE/FALSE")
    }
  }

  if (!is.null(limit)) {
    if (!is.na(limit) & !is.numeric(limit)) {
      stop("limit should be an integer")
    }
  }

  if (!is.null(chunk_size)) {
    if (!is.na(chunk_size) & !is.numeric(chunk_size)) {
      stop("chunk_size should be an integer")
    }
  }
}

#' Check other arguments
#'
#' Additional functions to check types and create parameter descriptions.
#'
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax).
#' Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param skipGeometry This parameter can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information. The default `NA` will not specify the argument in the request.
#'
#' @keywords internal
check_arguments_api <- function(bbox, skipGeometry) {
  if (!is.null(skipGeometry)) {
    if (!is.na(skipGeometry) & !is.logical(skipGeometry)) {
      stop("skipGeometry should be a logical TRUE/FALSE")
    }
  }

  if (!is.null(bbox)) {
    if (!all(is.na(bbox))) {
      if (!length(bbox) %in% c(1, 4)) {
        stop("bbox is not set up correctly")
      }
    }
  }
}
