context("read_waterdata_nearest_continuous")

# --- Helpers -----------------------------------------------------------------

# Build a synthetic data frame of continuous observations (one per row).
fake_continuous <- function(
  times,
  values = seq_along(times),
  site = "USGS-02238500"
) {
  data.frame(
    monitoring_location_id = rep(site, length(times)),
    time = as.POSIXct(times, tz = "UTC"),
    value = values,
    stringsAsFactors = FALSE
  )
}

# Capture the args that our stubbed read_waterdata_continuous receives, then
# return a canned data frame.
stub_continuous <- function(return_df, recorder = new.env()) {
  recorder$calls <- list()
  recorder$fn <- function(...) {
    args <- list(...)
    recorder$calls[[length(recorder$calls) + 1L]] <- args
    return_df
  }
  recorder
}

# --- Filter construction -----------------------------------------------------

test_that("builds one AND-bracketed OR-clause per target", {
  targets <- as.POSIXct(
    c("2023-06-15 10:30:31", "2023-06-15 14:07:12"),
    tz = "UTC"
  )
  recorder <- stub_continuous(fake_continuous(targets))
  with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = targets,
        monitoring_location_id = "USGS-02238500",
        parameter_code = "00060",
        window = 450
      )
    }
  )
  expect_length(recorder$calls, 1L)
  args <- recorder$calls[[1L]]
  expect_equal(args$filter_lang, "cql-text")
  expect_match(args$filter, "time >= '2023-06-15T10:23:01Z'", fixed = TRUE)
  expect_match(args$filter, "time <= '2023-06-15T10:38:01Z'", fixed = TRUE)
  expect_match(args$filter, "time >= '2023-06-15T13:59:42Z'", fixed = TRUE)
  expect_match(args$filter, "time <= '2023-06-15T14:14:42Z'", fixed = TRUE)
  # Exactly one OR joins the two bracketed clauses.
  expect_equal(
    lengths(regmatches(args$filter, gregexpr(" OR ", args$filter))),
    1L
  )
})

# --- Nearest reduction -------------------------------------------------------

test_that("picks the observation with minimum absolute time delta", {
  target <- as.POSIXct("2023-06-15 10:30:31", tz = "UTC")
  obs_times <- as.POSIXct(c(
    "2023-06-15 10:25:00",
    "2023-06-15 10:30:00", # closest to target (31s away)
    "2023-06-15 10:35:00"
  ), tz = "UTC")
  recorder <- stub_continuous(fake_continuous(obs_times))
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = target,
        monitoring_location_id = "USGS-02238500",
        parameter_code = "00060",
        window = 600
      )
    }
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$time, as.POSIXct("2023-06-15 10:30:00", tz = "UTC"))
  expect_equal(out$target_time, target)
})

test_that("on_tie = 'first' keeps earlier equidistant observation", {
  target <- as.POSIXct("2023-06-15 10:22:30", tz = "UTC")
  obs_times <- as.POSIXct(c(
    "2023-06-15 10:15:00",
    "2023-06-15 10:30:00"
  ), tz = "UTC")
  recorder <- stub_continuous(fake_continuous(obs_times, values = c(10, 20)))
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = target,
        window = 450,
        on_tie = "first"
      )
    }
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$time, as.POSIXct("2023-06-15 10:15:00", tz = "UTC"))
  expect_equal(out$value, 10)
})

test_that("on_tie = 'last' keeps later equidistant observation", {
  target <- as.POSIXct("2023-06-15 10:22:30", tz = "UTC")
  obs_times <- as.POSIXct(c(
    "2023-06-15 10:15:00",
    "2023-06-15 10:30:00"
  ), tz = "UTC")
  recorder <- stub_continuous(fake_continuous(obs_times, values = c(10, 20)))
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = target,
        window = 450,
        on_tie = "last"
      )
    }
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$time, as.POSIXct("2023-06-15 10:30:00", tz = "UTC"))
  expect_equal(out$value, 20)
})

test_that("on_tie = 'mean' averages numerics and pins time to the target", {
  target <- as.POSIXct("2023-06-15 10:22:30", tz = "UTC")
  obs_times <- as.POSIXct(c(
    "2023-06-15 10:15:00",
    "2023-06-15 10:30:00"
  ), tz = "UTC")
  recorder <- stub_continuous(fake_continuous(obs_times, values = c(10, 20)))
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = target,
        window = 450,
        on_tie = "mean"
      )
    }
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$time, target)
  expect_equal(out$value, 15)
})

# --- Edge cases --------------------------------------------------------------

test_that("targets with no observations in their window are dropped", {
  targets <- as.POSIXct(
    c("2023-06-15 10:30:00", "2030-01-01 00:00:00"),
    tz = "UTC"
  )
  obs_times <- as.POSIXct("2023-06-15 10:30:00", tz = "UTC")
  recorder <- stub_continuous(fake_continuous(obs_times))
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(targets = targets, window = 450)
    }
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$target_time, targets[1L])
})

test_that("multi-site returns one row per (target, site) pair", {
  targets <- as.POSIXct(
    c("2023-06-15 10:30:00", "2023-06-15 10:45:00"),
    tz = "UTC"
  )
  df <- rbind(
    fake_continuous(targets, site = "USGS-02238500"),
    fake_continuous(targets, site = "USGS-01646500")
  )
  recorder <- stub_continuous(df)
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(
        targets = targets,
        monitoring_location_id = c("USGS-02238500", "USGS-01646500"),
        window = 450
      )
    }
  )
  expect_equal(nrow(out), 4L)
  expect_setequal(
    unique(out$monitoring_location_id),
    c("USGS-02238500", "USGS-01646500")
  )
})

test_that("empty targets returns an empty frame without calling the service", {
  recorder <- stub_continuous(
    data.frame(
      monitoring_location_id = character(0),
      time = as.POSIXct(character(0), tz = "UTC"),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  )
  out <- with_mocked_bindings(
    read_waterdata_continuous = recorder$fn,
    {
      read_waterdata_nearest_continuous(targets = character(0))
    }
  )
  expect_length(recorder$calls, 1L)
  # The single call is the "shape probe" — it uses a trivially-empty time
  # range, not a filter.
  expect_null(recorder$calls[[1L]]$filter)
  expect_equal(nrow(out), 0L)
  expect_true("target_time" %in% names(out))
})

# --- Argument validation -----------------------------------------------------

test_that("forbidden kwargs raise an informative error", {
  target <- as.POSIXct("2023-06-15 10:30:00", tz = "UTC")
  for (forbidden in c("time", "filter", "filter_lang")) {
    args <- list(targets = target)
    args[[forbidden]] <- "anything"
    expect_error(
      do.call(read_waterdata_nearest_continuous, args),
      forbidden,
      info = forbidden
    )
  }
})

test_that("on_tie is validated", {
  target <- as.POSIXct("2023-06-15 10:30:00", tz = "UTC")
  expect_error(
    read_waterdata_nearest_continuous(targets = target, on_tie = "bogus"),
    "should be one of"
  )
})

test_that("window accepts HH:MM:SS strings and programmatic forms", {
  expect_equal(dataRetrieval:::as_window_seconds("00:07:30"), 450)
  expect_equal(dataRetrieval:::as_window_seconds("00:15:00"), 900)
  expect_equal(dataRetrieval:::as_window_seconds("01:00:00"), 3600)
  # Fractional seconds are allowed.
  expect_equal(dataRetrieval:::as_window_seconds("00:00:01.5"), 1.5)
  # Programmatic forms.
  expect_equal(dataRetrieval:::as_window_seconds(450), 450)
  expect_equal(
    dataRetrieval:::as_window_seconds(as.difftime(5, units = "mins")),
    300
  )
  expect_equal(
    dataRetrieval:::as_window_seconds(lubridate::minutes(10)),
    600
  )
  # Reject garbage with a clear message.
  expect_error(dataRetrieval:::as_window_seconds("7.5 mins"), "HH:MM:SS")
  expect_error(dataRetrieval:::as_window_seconds("bogus"), "HH:MM:SS")
})
