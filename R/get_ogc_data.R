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
  } else {
    args[["chunk_sites_by"]] <- NULL

    args <- switch_arg_id(args, id_name = output_id, service = service)

    args <- check_limits(args)

    properties <- args[["properties"]]
    args[["properties"]] <- switch_properties_id(properties, id = output_id)
    convertType <- args[["convertType"]]
    args[["convertType"]] <- NULL
    args[["service"]] <- service

    chunks <- plan_filter_chunks(args)
    fetched <- fetch_chunks(args, chunks)
    return_list <- combine_chunk_frames(fetched$frames)
    req <- fetched$req
    no_paging <- grepl("f=csv", req$url)

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

# Conservative fallback budget (characters) for a single CQL `filter` query
# parameter, used when `chunk_cql_or` is called directly without a `max_len`.
# `get_ogc_data` computes a tighter per-request budget from
# `.WATERDATA_URL_BYTE_LIMIT` below.
.CQL_FILTER_CHUNK_LEN <- 5000L

# Total URL byte limit the Water Data API will accept before replying
# HTTP 414 (Request-URI Too Large). Empirically the cliff sits at
# ~8,200 bytes of full URL, which lines up with nginx's default
# `large_client_header_buffers` of 8 KB (8192). 8000 leaves ~200 bytes of
# headroom for request-line framing and any intermediate proxy variance.
.WATERDATA_URL_BYTE_LIMIT <- 8000L


#' Decide how to fan `args[["filter"]]` out across HTTP calls
#'
#' Returns one entry per request to send. A `NULL` entry means "send
#' `args` as-is" -- either there is no filter, or the filter language
#' is not one we can safely split (only cql-text top-level `OR` chains
#' are chunkable). Otherwise each character entry is a chunked cql-text
#' expression that replaces `args[["filter"]]` for its sub-request.
#' Overlapping user OR-clauses are deduplicated by feature id later in
#' [combine_chunk_frames].
#'
#' @noRd
plan_filter_chunks <- function(args) {
  filter_expr <- args[["filter"]]
  filter_lang <- args[["filter_lang"]]
  chunkable <- is.character(filter_expr) &&
    length(filter_expr) == 1L &&
    !is.na(filter_expr) &&
    nzchar(filter_expr) &&
    (is.null(filter_lang) ||
      is.na(filter_lang) ||
      identical(filter_lang, "cql-text"))
  if (!chunkable) {
    return(list(NULL))
  }
  raw_budget <- effective_filter_budget(args, filter_expr)
  as.list(chunk_cql_or(filter_expr, max_len = raw_budget))
}


#' Send one request per chunk; return per-chunk frames and the request
#'
#' A `NULL` chunk means "send `args` as-is" (no filter override). The
#' returned `req` is the *first* sub-request, which is the
#' representative URL to attach as the `request` attribute when the
#' caller asked for one.
#'
#' @noRd
fetch_chunks <- function(args, chunks) {
  frames <- vector("list", length(chunks))
  first_req <- NULL
  for (i in seq_along(chunks)) {
    chunk_args <- if (is.null(chunks[[i]])) {
      args
    } else {
      replace(args, "filter", list(chunks[[i]]))
    }
    chunk_req <- do.call(construct_api_requests, chunk_args)
    message("Requesting:\n", chunk_req$url)
    if (grepl("f=csv", chunk_req$url)) {
      frames[[i]] <- get_csv(chunk_req, limit = chunk_args[["limit"]])
    } else {
      frames[[i]] <- walk_pages(chunk_req)
    }
    if (i == 1L) {
      first_req <- chunk_req
    }
  }
  list(frames = frames, req = first_req)
}


#' Concatenate per-chunk frames, handling the edge cases
#'
#' Drops empty frames before concat: `walk_pages` returns a plain
#' empty `data.frame` on no-feature responses, which would downgrade
#' a concat of real `sf` results back to a plain frame and strip
#' geometry / CRS. Also dedups on the pre-rename feature `id` so
#' overlapping user-supplied OR-clauses don't produce duplicate rows
#' across chunks.
#'
#' @noRd
combine_chunk_frames <- function(frames) {
  non_empty <- Filter(function(df) nrow(df) > 0L, frames)
  if (length(non_empty) == 0L) {
    return(frames[[1L]])
  }
  if (length(non_empty) == 1L) {
    return(non_empty[[1L]])
  }
  combined <- do.call(rbind, non_empty)
  if ("id" %in% names(combined)) {
    combined <- combined[!duplicated(combined$id), , drop = FALSE]
  }
  combined
}


#' Compute the raw CQL byte budget for `filter_expr` in this request
#'
#' The server limits total URL length (see `.WATERDATA_URL_BYTE_LIMIT`),
#' not raw CQL length. To derive a raw-byte budget we can hand to
#' `chunk_cql_or`:
#'
#' 1. Probe the URL space consumed by the other query params by building
#'    the request with a 1-byte placeholder filter.
#' 2. Subtract from the URL limit to get the bytes available for the
#'    encoded filter value.
#' 3. Convert back to raw CQL bytes using the *maximum* per-clause
#'    encoding ratio, not the whole-filter average. A chunk can end up
#'    containing only the heavier-encoding clauses (e.g. heavy ones
#'    clustered at one end of the filter), so budgeting against the
#'    average lets such a chunk overflow the URL limit.
#'
#' @noRd
effective_filter_budget <- function(args, filter_expr) {
  probe_args <- args
  probe_args[["filter"]] <- "x"
  probe_req <- do.call(construct_api_requests, probe_args)
  non_filter_url_bytes <- nchar(probe_req$url) - 1L
  available_url_bytes <- .WATERDATA_URL_BYTE_LIMIT - non_filter_url_bytes
  if (available_url_bytes <= 0L) {
    # The non-filter URL already exceeds the byte limit, so no chunk we
    # could produce would fit. Return a budget larger than the filter so
    # chunk_cql_or passes it through unchanged -- one clear 414 from the
    # server is better feedback than a burst of N failing sub-requests.
    return(nchar(filter_expr) + 1L)
  }
  parts <- split_top_level_or(filter_expr)
  if (length(parts) == 0L) parts <- filter_expr
  parts <- parts[nzchar(parts)]
  # Include the " OR " joiner: it is 4 raw bytes but encodes to
  # "%20OR%20" (8 bytes, ratio 2.0). For inputs whose clauses encode
  # lighter than that (e.g. time-interval clauses at ~1.6), the joiner
  # is what pushes the full URL past the limit -- leaving it out made
  # the budget too loose.
  ratio <- function(p) nchar(utils::URLencode(p, reserved = TRUE)) / nchar(p)
  encoding_ratio <- max(vapply(c(parts, " OR "), ratio, numeric(1)))
  max(100L, as.integer(available_url_bytes / encoding_ratio))
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
