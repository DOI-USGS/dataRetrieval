# ------------------------------------------------------------------------------
# clean_value_cols()
# ------------------------------------------------------------------------------

test_that("values column is unnested and converted to numeric", {
  DT <- data.table(
    values = list(c("1.2", "3.4")),
    percentiles = list(c("10", "90")),
    computation = "percentile",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "month",
    ts_id = "1",
    sample_count = 10,
    approval_status = "approved",
    computation_id = "abc"
  )
  
  out <- DT[, .j, env = list(.j = clean_value_cols())]
  
  expect_equal(out$value, c(1.2, 3.4))
  expect_equal(out$percentile, c(10, 90))
})

test_that("value column is used when values is absent", {
  DT <- data.table(
    value = "2.5",
    computation = "arithmetic_mean",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "year",
    sample_count = 1,
    ts_id = "1",
    approval_status = "approved",
    computation_id = "xyz"
  )
  
  out <- DT[, .j, env = list(.j = clean_value_cols())]
  
  expect_equal(out$value, 2.5)
  expect_true(is.na(out$percentile))
})

test_that("'nan' values are converted to NA_real_", {
  DT <- data.table(
    values = list("nan"),
    computation = "arithmetic_mean",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "month",
    ts_id = "1",
    sample_count = 1,
    approval_status = "approved",
    computation_id = "nan-test"
  )
  
  out <- DT[, .j, env = list(.j = clean_value_cols())]
  
  expect_true(is.na(out$value))
})

test_that("percentile is inferred from computation type", {
  DT <- data.table(
    value = "5",
    computation = c("minimum", "median", "maximum"),
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "year",
    ts_id = "1",
    sample_count = 1,
    approval_status = "approved",
    computation_id = 1:3
  )
  
  out <- DT[, .j, by = computation, env = list(.j = clean_value_cols())]
  
  expect_equal(
    out[order(computation)]$percentile,
    c(100, 50, 0) # maximum, median, minimum
  )
})

test_that("scalar percentile is recycled to match values length", {
  DT <- data.table(
    values = list(c("1", "2", "3")),
    percentiles = list("50"),
    computation = "percentile",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "month",
    ts_id = "1",
    sample_count = 3,
    approval_status = "approved",
    computation_id = "recycle"
  )
  
  out <- DT[, .j, env = list(.j = clean_value_cols())]
  
  expect_equal(out$percentile, c(50, 50, 50))
})

test_that("missing value and values yields NA value", {
  DT <- data.table(
    computation = "arithmetic_mean",
    start_date = "2000-01-01",
    end_date = "2000-12-31",
    interval_type = "year",
    ts_id = "1",
    sample_count = 1,
    approval_status = "approved",
    computation_id = "missing"
  )
  
  out <- DT[, .j, env = list(.j = clean_value_cols())]
  
  expect_true(is.na(out$value))
})

# ------------------------------------------------------------------------------
# construct_statistics_request()
# ------------------------------------------------------------------------------

test_that("construct_statistics_request builds correct URL path", {
  req <- construct_statistics_request(service = "Normals", version = 0)
  
  expect_true(grepl("statistics/v0/observationNormals", req$url))
})

test_that("Intervals service path is correct", {
  req <- construct_statistics_request(service = "Intervals", version = 0)
  
  expect_true(grepl("statistics/v0/observationIntervals", req$url))
})

# ------------------------------------------------------------------------------
# explode_query integration (no HTTP)
# ------------------------------------------------------------------------------

test_that("arguments are passed as query parameters", {
  args <- list(
    state_code = "WI",
    computation_type = "median",
    page_size = 10
  )
  
  req <- construct_statistics_request("Normals", 0)
  full <- explode_query(req, POST = FALSE, x = args)
  
  qs <- httr2::req_get_url(full)
  
  expect_match(qs, "state_code=WI")
  expect_match(qs, "computation_type=median")
  expect_match(qs, "page_size=10")
})

# ------------------------------------------------------------------------------
# get_statistics_data() (mocked)
# ------------------------------------------------------------------------------

test_that("get_statistics_data returns sf object with attributes", {
  fake_page <-
    sf::st_sf(
      monitoring_location_id = c("USGS-01432160", "USGS-01432110"),
      monitoring_location_name = c(
        "DELAWARE RIVER AT BARRYVILLE NY",
        "Lackawaxen River at Rowland, PA"
      ),
      site_type = c("Stream", "Stream"),
      site_type_code = c("ST", "ST"),
      country_code = c("US", "US"),
      state_code = c("42", "42"),
      county_code = c("103", "103"),
      geometry = sf::st_sfc(
        sf::st_point(c(0, 0)),
        sf::st_point(c(1, 1))
      ),
      data = c(
        # First site
        jsonlite::toJSON(list(
          list(
            parameter_code = "00060",
            unit_of_measure = "ft^3/s",
            parent_time_series_id = "1692e9a29c8c4276add4497c5da872fa",
            values = list(
              list(
                start_date = "2018-10-01",
                end_date = "2018-10-31",
                interval_type = "month",
                value = "8032.903",
                sample_count = 31,
                approval_status = "approved",
                computation_id = "d98ebe80-d476-4144-b7ce-c6ca32f015de",
                computation = "arithmetic_mean"
              ),
              list(
                start_date = "2018-11-01",
                end_date = "2018-11-30",
                interval_type = "month",
                value = "12521.667",
                sample_count = 30,
                approval_status = "approved",
                computation_id = "93f45097-93ad-4b48-83d4-b27b5fc137dc",
                computation = "arithmetic_mean"
              )
            )
          )
        ), auto_unbox = TRUE),
        # Second site
        jsonlite::toJSON(list(
          list(
            parameter_code = "00060",
            unit_of_measure = "ft^3/s",
            parent_time_series_id = "7dda388ea270420dbe7324e56b6f907f",
            values = list(
              list(
                start_date = "2007-07-27",
                end_date = "2007-07-31",
                interval_type = "month",
                values = list("1", "2"),
                percentiles = list("50", "100"),
                sample_count = 5,
                approval_status = "approved",
                computation_id = "4579653d-f26f-4e9f-99ea-c3fa3fc68686",
                computation = "arithmetic_mean"
              )
            )
          )
        ), auto_unbox = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  
  with_mocked_bindings(
    walk_pages = function(...) list(fake_page),
    {
      out <- get_statistics_data(list(), "Normals")
      
      expect_s3_class(out, "sf")
      expect_true("request" %in% names(attributes(out)))
      expect_true("queryTime" %in% names(attributes(out)))
      expect_equal(nrow(out), 4)
    }
  )
})

test_that("get_statistics_data handles empty response", {
  with_mocked_bindings(
    walk_pages = function(...) list(),
    {
      out <- get_statistics_data(list(), "Normals")
      
      expect_s3_class(out, "sf")
      expect_equal(nrow(out), 0)
    }
  )
})

# ------------------------------------------------------------------------------
# High-level API tests (skipped on CRAN)
# ------------------------------------------------------------------------------

test_that("read_waterdata_stats_normal returns data", {
  skip_on_cran()
  skip_if_offline()
  
  out <- read_waterdata_stats_normal(
    monitoring_location_id = "USGS-01646500",
    parameter_code = "00060",
    computation_type = "median",
    page_size = 5
  )
  
  expect_s3_class(out, "sf")
  expect_true(nrow(out) > 0)
})

test_that("read_waterdata_stats_interval returns data", {
  skip_on_cran()
  skip_if_offline()
  
  out <- read_waterdata_stats_interval(
    monitoring_location_id = "USGS-01646500",
    parameter_code = "00060",
    computation_type = "maximum",
    page_size = 5
  )
  
  expect_s3_class(out, "sf")
  expect_true(nrow(out) > 0)
})
