#' Coordinate the request and retrieval of OGC calls
#'
#' @param args arguments from individual functions
#' @param output_id Name of id column to return
#' @param service Endpoint name.
#' @param \dots Used to force users to fully name the details argument.
#' @param chunk_size Number of monitoring_location_ids to chunk requests into.
#'
#' @noRd
#' @return data.frame with attributes
get_ogc_data <- function(args, output_id, service, ..., chunk_size = 250) {
  rlang::check_dots_empty()

  filter_chunks <- chunkable_filter(args[["filter"]])

  if (length(args[["monitoring_location_id"]]) > chunk_size) {
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
  } else if (length(filter_chunks) > 1) {
    rl <- lapply(filter_chunks, function(chunk) {
      sub_args <- args
      sub_args[["filter"]] <- chunk
      get_ogc_data(args = sub_args, output_id = output_id, service = service)
    })

    rl_filtered <- rl[
      vapply(rl, FUN = function(x) dim(x)[1], FUN.VALUE = NA_integer_) > 0
    ]

    return_list <- data.frame(data.table::rbindlist(
      rl_filtered,
      use.names = TRUE,
      ignore.attr = TRUE
    ))

    if (output_id %in% names(return_list)) {
      return_list <- return_list[
        !duplicated(return_list[[output_id]]), ,
        drop = FALSE
      ]
    }
  } else {
    args[["chunk_sites_by"]] <- NULL

    args <- switch_arg_id(args, id_name = output_id, service = service)

    args <- check_limits(args)

    properties <- args[["properties"]]
    args[["properties"]] <- switch_properties_id(properties, id = output_id)
    convertType <- args[["convertType"]]
    args[["convertType"]] <- NULL
    args[["service"]] <- service

    req <- do.call(construct_api_requests, args)

    no_paging <- grepl("f=csv", req$url)

    message("Requesting:\n", req$url)

    if (no_paging) {
      return_list <- get_csv(req, limit = args[["limit"]])
    } else {
      return_list <- walk_pages(req)
    }

    if (is.na(args[["skipGeometry"]])) {
      skipGeometry <- FALSE
    } else {
      skipGeometry <- args[["skipGeometry"]]
    }

    return_list <- deal_with_empty(
      return_list,
      properties,
      service,
      skipGeometry,
      convertType,
      no_paging
    )

    return_list <- rejigger_cols(return_list, properties, output_id)

    if (convertType) {
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

    if (getOption("dataRetrieval.attach_request")) {
      attr(return_list, "request") <- req
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

# Conservative budget (characters) for a single CQL `filter` query parameter
# before the URL risks exceeding the server's URI length limit. The continuous
# endpoint has been observed to return HTTP 414 around ~7 KB of filter text;
# 5000 leaves headroom for URL encoding and the other query parameters.
.CQL_FILTER_CHUNK_LEN <- 5000L


#' Decide whether a `filter` arg can be split into smaller OR-chunks
#'
#' Returns the (possibly singleton) list of chunks. The caller chunks
#' the request when this returns more than one element.
#'
#' @noRd
chunkable_filter <- function(filter_expr) {
  if (
    !is.character(filter_expr) ||
      length(filter_expr) != 1 ||
      is.na(filter_expr)
  ) {
    return(filter_expr)
  }
  chunk_cql_or(filter_expr)
}


#' Split a CQL expression at each top-level `OR` separator
#'
#' Respects parentheses and single/double-quoted string literals so that
#' `OR` tokens inside `(A OR B)` or `'word OR word'` are left alone.
#' Matching is case-insensitive. Whitespace around each emitted part is
#' stripped; empty parts are dropped.
#'
#' @noRd
#' @examples
#' dataRetrieval:::split_top_level_or("A OR B OR C")
#' dataRetrieval:::split_top_level_or("(A OR B) OR (C OR D)")
#' dataRetrieval:::split_top_level_or("name = 'foo OR bar' OR id = 1")
split_top_level_or <- function(expr) {
  chars <- strsplit(expr, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (n == 0L) {
    return(character(0))
  }

  ws <- c(" ", "\t", "\n", "\r")
  starts <- integer(0)
  ends <- integer(0)

  depth <- 0L
  in_quote <- ""
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (in_quote != "") {
      if (ch == in_quote) in_quote <- ""
      i <- i + 1L
    } else if (ch == "'" || ch == '"') {
      in_quote <- ch
      i <- i + 1L
    } else if (ch == "(") {
      depth <- depth + 1L
      i <- i + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      i <- i + 1L
    } else if (depth == 0L && ch %in% ws) {
      j <- i + 1L
      while (j <= n && chars[j] %in% ws) j <- j + 1L
      if (
        j + 1L <= n &&
          (chars[j] == "o" || chars[j] == "O") &&
          (chars[j + 1L] == "r" || chars[j + 1L] == "R")
      ) {
        k <- j + 2L
        if (k <= n && chars[k] %in% ws) {
          starts <- c(starts, i - 1L)
          m <- k + 1L
          while (m <= n && chars[m] %in% ws) m <- m + 1L
          ends <- c(ends, m)
          i <- m
          next
        }
      }
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }

  if (length(starts) == 0L) {
    out <- trimws(expr)
    return(if (nzchar(out)) out else character(0))
  }

  segment_starts <- c(1L, ends)
  segment_ends <- c(starts, n)
  parts <- substring(expr, segment_starts, segment_ends)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}


#' Split a CQL expression into OR-chunks that each fit under `max_len`
#'
#' Only top-level `OR` chains are split, since that is the only shape
#' that can be recombined losslessly as a disjunction of independent
#' sub-queries. Returns the input unchanged when the whole expression
#' already fits, when it contains no top-level `OR`, or when any single
#' clause is larger than `max_len` on its own (we would rather send a
#' too-long request and surface the server's 414 than silently drop
#' data).
#'
#' @noRd
#' @examples
#' clause <- "(time >= '2023-01-01' AND time <= '2023-01-02')"
#' expr <- paste(rep(clause, 3), collapse = " OR ")
#' dataRetrieval:::chunk_cql_or(expr, max_len = 100)
chunk_cql_or <- function(expr, max_len = .CQL_FILTER_CHUNK_LEN) {
  if (nchar(expr) <= max_len) {
    return(expr)
  }
  parts <- split_top_level_or(expr)
  if (length(parts) < 2L || any(nchar(parts) > max_len)) {
    return(expr)
  }

  join_len <- nchar(" OR ")
  chunks <- character(0)
  current <- character(0)
  current_len <- 0L
  for (part in parts) {
    join_cost <- if (length(current) > 0L) join_len else 0L
    if (length(current) > 0L && current_len + join_cost + nchar(part) > max_len) {
      chunks <- c(chunks, paste(current, collapse = " OR "))
      current <- part
      current_len <- nchar(part)
    } else {
      current <- c(current, part)
      current_len <- current_len + join_cost + nchar(part)
    }
  }
  if (length(current) > 0L) {
    chunks <- c(chunks, paste(current, collapse = " OR "))
  }
  chunks
}
