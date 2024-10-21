context("NLDI...")

test_that("NLDI messageing NULL", {
  skip_on_cran()
  xx <- findNLDI(
    wqp = "TCEQMAIN-10016",
    nav = "UM",
    find = "nwissite",
    distance_km = 2,
    warn = FALSE
  )
  
  expect_warning(findNLDI(
    wqp = "TCEQMAIN-10016",
    nav = "UM",
    find = "nwissite",
    distance_km = 2, 
    warn = TRUE
  ))
  
  expect_true(class(xx) == "list")
  expect_true(nrow(xx[[1]]) == 1)
})


test_that("NLDI offerings...", {
  skip_on_cran()
  expect_true(nrow(get_nldi_sources()) > 1)
})





test_that("NLDI starting sources...", {
  skip_on_cran()
  
  # LINESTRING GEOMETERY
  expect_equal(sum(names(findNLDI(comid = 101, warn = FALSE)$origin) %in%
                     c("sourceName", "identifier", "comid", "geometry")), 4)
  # POINT GEOMETERY
  expect_true(all(names(findNLDI(nwis = "11120000", warn = FALSE)$origin) %in%
                    c(
                      "sourceName", "identifier", "comid",
                      "name", "reachcode", "measure",
                      "X", "Y", "geometry"
                    )))
  # COMID
  expect_equal(findNLDI(comid = 101, warn = FALSE)$origin$sourceName, "NHDPlus comid")
  # NWIS
  expect_equal(findNLDI(nwis = "11120000", warn = FALSE)$origin$sourceName, "NWIS Surface Water Sites")
  # WQP
  expect_equal(findNLDI(wqp = "USGS-04024315", warn = FALSE)$origin$sourceName, "Water Quality Portal")
  expect_equal(findNLDI(wqp = "CCU_EQL-2 H9", warn = FALSE)$origin$sourceName, "Water Quality Portal")
  # LOCATION
  expect_equal(findNLDI(location = c(-115, 40), warn = FALSE)$origin$sourceName, "NHDPlus comid")
  # ERROR: LOCATION COORDINATES FLIPPED
  expect_error(findNLDI(location = c(40, -115), warn = FALSE))
  # GENERAL START: STABLE
  expect_equal(findNLDI(origin = list("comid" = 101), warn = FALSE)$origin$sourceName, "NHDPlus comid")
  # GENERAL START: NON-STABLE
  expect_equal(findNLDI(origin = list("nwissite" = "USGS-05427850"), warn = FALSE)$origin$sourceName, "NWIS Surface Water Sites")
  # ERROR: TWO STARTS
  expect_error(findNLDI(nwis = 1000, comid = 101, warn = FALSE))
  # NON EXISTING SITE
  expect_message(findNLDI(comid = 1, warn = FALSE))
})

test_that("NLDI navigation sources...", {
  skip_on_cran()
  
  # UPPER TRIBUTARY
  expect_equal(length(findNLDI(nwis = "11120000", nav = "UT", warn = FALSE)$UT_flowlines), 2)
  # UPPER MAIN
  expect_equal(length(findNLDI(nwis = "11120000", nav = "UM", warn = FALSE)$UM_flowlines), 2)
  # DOWNSTREAM MAIN
  expect_equal(length(findNLDI(nwis = "11120000", nav = "DM", warn = FALSE)$DM_flowlines), 2)
  # MULTI-REQUEST
  expect_equal(length(findNLDI(nwis = "11120000", nav = c("UT", "UM"), warn = FALSE)), 3)
  # ERRORS: Bad NAV REQUEST
  expect_error(findNLDI(nwis = "11120000", nav = c("DT"), warn = FALSE))
  expect_error(findNLDI(nwis = "11120000", nav = c("DT", "UM"), warn = FALSE))
  # WARNING: Data not found
  expect_warning(findNLDI(comid = 101, nav = "UM", find = "nwis", warn = TRUE))
})

test_that("NLDI find sources...", {
  skip_on_cran()
  
  expect_equal(length(findNLDI(nwis = "11120000", nav = "UT", find = "wade", warn = FALSE)), 2)
  expect_equal(length(findNLDI(nwis = "11120000", nav = c("UT", "UM"), find = c("nwis", "wade", "flowlines"), warn = FALSE)), 7)
})

test_that("sf not installed...", {
  skip_on_cran()
  
  expect_true(!"geometry" %in% findNLDI(nwis = "11120000", no_sf = TRUE, warn = FALSE)[[1]])
  expect_equal(class(findNLDI(nwis = "11120000", nav = "UT", find = c("nwis"), no_sf = TRUE, warn = FALSE)[[2]]), "data.frame")
  expect_true(c("X") %in% names(findNLDI(nwis = "11120000", nav = "UT", find = c("nwis"), no_sf = TRUE, warn = FALSE)[[2]]))
})


test_that("Distance...", {
  skip_on_cran()
  
  full <- findNLDI(comid = 101, nav = "UT", find = "nwis", distance_km = 9999, warn = FALSE)
  part <- findNLDI(comid = 101, nav = "UT", find = "nwis", warn = FALSE)
  expect_true(nrow(full$UT_nwissite) > nrow(part$UT_nwissite))
})

test_that("basin", {
  skip_on_cran()
  
  xx <- findNLDI(comid = 101, nav = "UT", find = "basin", warn = FALSE)
  xx2 <- findNLDI(comid = 101, nav = "UT", find = "basin", no_sf = TRUE , warn = FALSE)
  expect_true(sf::st_geometry_type(xx$basin) == "POLYGON")
  expect_equal(ncol(xx2$basin), 0)
})


test_that("ignore flowlines", {
  skip_on_cran()
  
  xx <- findNLDI(comid = 101, nav = "DM", find = c("nwis", "flowlines"), warn = FALSE)
  xx2 <- findNLDI(comid = 101, nav = "DM", find = "nwis", warn = FALSE)
  expect_gt(length(xx), length(xx2))
  expect_true("DM_flowlines" %in% names(xx))
  expect_true(!"DM_flowlines" %in% names(xx2))
})


test_that("sf points", {
  skip_on_cran()
  library(sf)
  p2 <- st_sfc(st_point(c(-119.8458, 34.4146)), crs = 4326)
  expect_equal(findNLDI(location = p2, warn = FALSE), findNLDI(location = sf::st_as_sf(p2)))
  expect_error(findNLDI(location = st_buffer(p2, .01), warn = FALSE))
})


test_that("warn_flag", {
  
  expect_warning(
    findNLDI(wqp = "TCEQMAIN-10016",
             nav = "UM",
             find = "nwissite",
             distance_km = 2,
             no_sf = TRUE,
             warn = FALSE),
    regexp = NA
  )
  
  expect_warning(
    findNLDI(wqp = "TCEQMAIN-10016",
             nav = "UM",
             find = "nwissite",
             distance_km = 2,
             no_sf = TRUE,
             warn = TRUE)
  )
})
