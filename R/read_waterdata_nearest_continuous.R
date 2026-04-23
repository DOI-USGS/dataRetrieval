#' For each target timestamp, return the nearest continuous observation
#'
#' Builds one bracketed `(time >= t-window AND time <= t+window)` clause per
#' target, joins them into a top-level CQL `OR` filter, and lets
#' [read_waterdata_continuous()] (with its URL-length-safe auto-chunking)
#' fetch every observation that falls in any window. Then, per
#' `(monitoring_location_id, target)` pair, picks the single observation
#' whose time is closest to the target.
#'
#' The USGS continuous endpoint matches `time` parameters exactly rather
#' than fuzzily, and it does not implement `sortby` for arbitrary fields;
#' this function is the single-round-trip way to ask "what reading is
#' nearest this timestamp?" for many timestamps at once.
#'
#' @export
#' @param targets Target timestamps. Accepts a `POSIXct` vector, a
#'   character vector in ISO 8601 form, a `Date` vector, or anything
#'   [lubridate::as_datetime()] accepts. Character input without a
#'   timezone is interpreted as UTC.
#' @param monitoring_location_id `r get_ogc_params("continuous")$monitoring_location_id`
#'   Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("continuous")$parameter_code`
#'   Multiple parameter_codes can be requested as a character vector.
#' @param window Half-window around each target. Accepts:
#'   * a `"MM:SS"` or `"HH:MM:SS"` string (e.g. `"07:30"`, `"15:00"`,
#'     `"00:30:00"`, `"01:00:00"`);
#'   * an ISO 8601 duration string (e.g. `"PT7M30S"`, `"PT15M"`,
#'     `"PT1H"`) or any other string `lubridate::duration()` parses
#'     (e.g. `"7 minutes 30 seconds"`);
#'   * a number of seconds, a `difftime`, or a `lubridate::Period` /
#'     `lubridate::Duration`.
#'
#'   Defaults to `"07:30"` (7.5 minutes), which is half of the
#'   typical 15-minute continuous cadence — most targets' windows
#'   contain exactly one observation. Widen for irregular cadences or
#'   resilience to data gaps.
#' @param on_tie How to resolve ties when two observations are exactly
#'   equidistant from a target (which happens when the target falls at
#'   the midpoint between grid points — e.g. target 10:22:30 for a
#'   15-minute gauge). One of:
#'   * `"first"` — keep the earlier observation (default).
#'   * `"last"`  — keep the later observation.
#'   * `"mean"`  — average numeric columns; set `time` to the target,
#'     since no real observation exists at the midpoint.
#' @param ... Additional arguments forwarded to [read_waterdata_continuous()]
#'   (e.g. `statistic_id`, `approval_status`, `properties`). Passing
#'   `time`, `filter`, or `filter_lang` raises an error — this function
#'   builds those itself.
#'
#' @details
#' *Window sizing and ties.* When `window` is exactly half the service
#' cadence, most targets' windows contain a single observation and
#' `on_tie` is moot. Ties arise only when a target sits exactly at the
#' window edge — rare in practice but possible. Setting `window` to a
#' full cadence (or larger) guarantees at least one observation per
#' target in steady state at the cost of more bytes per response.
#'
#' *Why windowed CQL rather than sort+limit.* The API's advertised
#' `sortby` parameter would make this a one-liner per target (`filter`
#' by `time <= t` and `limit 1`), but it is per-query — you would need
#' one HTTP round-trip per target. The CQL `OR`-chain approach folds
#' all N targets into one request (auto-chunked when the URL is long).
#'
#' @return A data frame with one row per `(target, monitoring_location_id)`
#' combination that had at least one observation in its window.
#' A `target_time` column is added identifying which target each row
#' corresponds to. Targets with no observations in their window are
#' silently dropped.
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' # Pair three off-grid timestamps with nearby observations
#' targets <- as.POSIXct(
#'   c("2023-06-15 10:30:31", "2023-06-15 14:07:12", "2023-06-16 03:45:19"),
#'   tz = "UTC"
#' )
#'
#' near <- read_waterdata_nearest_continuous(
#'   targets = targets,
#'   monitoring_location_id = "USGS-02238500",
#'   parameter_code = "00060"
#' )
#'
#' # Widen the window for an irregular-cadence gauge and average on ties
#' near_wide <- read_waterdata_nearest_continuous(
#'   targets = targets,
#'   monitoring_location_id = "USGS-02238500",
#'   parameter_code = "00060",
#'   window = "30:00",
#'   on_tie = "mean"
#' )
#' }
read_waterdata_nearest_continuous <- function(
  targets,
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  window = "07:30",
  on_tie = c("first", "last", "mean"),
  ...
) {
  on_tie <- match.arg(on_tie)

  dots <- list(...)
  forbidden <- c("time", "filter", "filter_lang")
  bad <- intersect(forbidden, names(dots))
  if (length(bad) > 0L) {
    stop(
      "read_waterdata_nearest_continuous constructs its own ",
      paste(shQuote(bad), collapse = ", "),
      "; do not pass it directly",
      call. = FALSE
    )
  }

  targets <- to_utc_posixct(targets)
  window_secs <- as_window_seconds(window)

  if (length(targets) == 0L) {
    # Nothing to ask about — return an empty frame shaped like a real
    # read_waterdata_continuous response (via a trivially-empty time range).
    empty <- read_waterdata_continuous(
      monitoring_location_id = monitoring_location_id,
      parameter_code = parameter_code,
      time = "1900-01-01T00:00:00Z/1900-01-01T00:00:00Z",
      ...
    )
    empty$target_time <- as.POSIXct(character(0), tz = "UTC")
    return(empty[0, , drop = FALSE])
  }

  fmt <- "%Y-%m-%dT%H:%M:%SZ"
  starts <- format(targets - window_secs, fmt, tz = "UTC")
  ends <- format(targets + window_secs, fmt, tz = "UTC")
  clauses <- sprintf("(time >= '%s' AND time <= '%s')", starts, ends)
  filter_expr <- paste(clauses, collapse = " OR ")

  df <- read_waterdata_continuous(
    monitoring_location_id = monitoring_location_id,
    parameter_code = parameter_code,
    filter = filter_expr,
    filter_lang = "cql-text",
    ...
  )

  if (nrow(df) == 0L) {
    df$target_time <- as.POSIXct(character(0), tz = "UTC")
    return(df)
  }

  df$time <- as.POSIXct(df$time, tz = "UTC")

  if ("monitoring_location_id" %in% names(df)) {
    site_groups <- split(df, df$monitoring_location_id, drop = FALSE)
  } else {
    site_groups <- list(df)
  }

  selected <- vector("list", 0L)
  for (site_df in site_groups) {
    for (ti in seq_along(targets)) {
      # Subscript instead of iteration: for-loop on a POSIXct vector
      # strips the class from the loop variable, which would make the
      # target_time column numeric after rbind.
      target <- targets[ti]
      lo <- target - window_secs
      hi <- target + window_secs
      in_window <- site_df$time >= lo & site_df$time <= hi
      window_df <- site_df[in_window, , drop = FALSE]
      if (nrow(window_df) == 0L) next

      deltas <- abs(as.numeric(
        difftime(window_df$time, target, units = "secs")
      ))
      candidates <- window_df[deltas == min(deltas), , drop = FALSE]
      candidates <- candidates[order(candidates$time), , drop = FALSE]

      if (nrow(candidates) == 1L || on_tie == "first") {
        row <- candidates[1L, , drop = FALSE]
      } else if (on_tie == "last") {
        row <- candidates[nrow(candidates), , drop = FALSE]
      } else {
        # "mean" — average numeric columns, pin time to the target since
        # no real observation exists at the midpoint.
        row <- candidates[1L, , drop = FALSE]
        num_cols <- vapply(candidates, is.numeric, logical(1L))
        for (nc in names(candidates)[num_cols]) {
          row[[nc]] <- mean(candidates[[nc]])
        }
        row$time <- target
      }

      row$target_time <- target
      selected[[length(selected) + 1L]] <- row
    }
  }

  if (length(selected) == 0L) {
    empty <- df[0, , drop = FALSE]
    empty$target_time <- as.POSIXct(character(0), tz = "UTC")
    return(empty)
  }

  result <- do.call(rbind, selected)
  row.names(result) <- NULL
  result
}


#' Coerce various datetime-like inputs to a UTC `POSIXct` vector
#'
#' @noRd
to_utc_posixct <- function(x) {
  if (length(x) == 0L) {
    return(as.POSIXct(character(0), tz = "UTC"))
  }
  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(format(x, tz = "UTC"), tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  # Character and anything lubridate::as_datetime can parse. Naive strings
  # are treated as UTC to match Python pd.to_datetime(..., utc=True).
  lubridate::as_datetime(x, tz = "UTC")
}


#' Coerce `window` to a number of seconds
#'
#' Accepts a `"MM:SS"` or `"HH:MM:SS"` clock-style string, an ISO 8601
#' duration string (or anything `lubridate::duration()` parses), a
#' plain number of seconds, a `difftime`, or a `lubridate::Period` /
#' `Duration`.
#'
#' @noRd
as_window_seconds <- function(window) {
  if (inherits(window, "difftime")) {
    return(as.numeric(window, units = "secs"))
  }
  if (inherits(window, c("Period", "Duration"))) {
    return(as.numeric(lubridate::as.duration(window), units = "secs"))
  }
  if (is.numeric(window) && length(window) == 1L) {
    return(as.numeric(window))
  }
  if (is.character(window) && length(window) == 1L) {
    # MM:SS or HH:MM:SS (with optional fractional seconds).
    if (grepl("^\\d+:\\d{1,2}(:\\d{1,2})?(\\.\\d+)?$", window)) {
      parts <- as.numeric(strsplit(window, ":", fixed = TRUE)[[1L]])
      if (length(parts) == 2L) {
        return(parts[1L] * 60 + parts[2L])
      }
      return(parts[1L] * 3600 + parts[2L] * 60 + parts[3L])
    }
    # ISO 8601 duration ("PT7M30S", "PT1H", "P1D", ...) or a natural-
    # language form that `lubridate::duration` recognises.
    parsed <- suppressWarnings(tryCatch(
      lubridate::duration(window),
      error = function(e) NULL
    ))
    if (!is.null(parsed) && !is.na(parsed)) {
      return(as.numeric(parsed, units = "secs"))
    }
  }
  stop(
    "`window` must be a clock-style 'MM:SS'/'HH:MM:SS' string, an ISO ",
    "8601 duration (e.g. 'PT7M30S'), a number of seconds, a difftime, ",
    "or a lubridate Period/Duration; got: ",
    paste(format(window), collapse = " "),
    call. = FALSE
  )
}
