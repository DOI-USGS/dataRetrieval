context("NLDI...")

test_that("NLDI messageing NULL", {
  skip_on_cran()
  xx = findNLDI(wqp = "TCEQMAIN-10016",
                nav = "UM",
                find = "nwissite",
                distance_km = 2)
  
  expect_warning(findNLDI(wqp = "TCEQMAIN-10016",
                               nav = "UM",
                               find = "nwissite",
                               distance_km = 2))
  
  expect_true(nrow(xx) == 1)
})


test_that("NLDI offerings...", {
  skip_on_cran()
  expect_true(nrow(get_nldi_sources()) > 1)
})


xx = findNLDI(wqp = "TCEQMAIN-10016",
              nav = "UM",
              find = "nwissite",
              distance_km = 2,
              no_sf = FALSE)


test_that("NLDI starting sources...", {

  skip_on_cran()

  # LINESTRING GEOMETERY
  expect_equal(sum(names(findNLDI(comid = 101)) %in%
                     c('sourceName', 'identifier', "comid", "geometry")),  4)
  # POINT GEOMETERY
  expect_true(all(names(findNLDI(nwis = '11120000')) %in%
                     c('sourceName', 'identifier', "comid",
                       "name", "reachcode", "measure",
                       "X", "Y", "geometry")))
  # COMID
  expect_equal(findNLDI(comid = 101)$sourceName, "NHDPlus comid")
  # NWIS
  expect_equal(findNLDI(nwis = '11120000')$sourceName, "NWIS Surface Water Sites")
  # WQP
  expect_equal(findNLDI(wqp = 'USGS-04024315')$sourceName, "Water Quality Portal")
  # LOCATION
  expect_equal(findNLDI(location = c(-115,40))$sourceName, "NHDPlus comid")
  # ERROR: LOCATION COORDINATES FLIPPED
  expect_error(findNLDI(location = c(40, -115)))
  # GENERAL START: STABLE
  expect_equal(findNLDI(origin = list("comid" = 101))$sourceName, "NHDPlus comid")
  # GENERAL START: NON-STABLE
  expect_equal(findNLDI(origin = list("wade" = 'NMwr_S148023'))$sourceName, "Water Data Exchange 2.0 Sites")
  # ERROR: TWO STARTS
  expect_error(findNLDI(nwis = 1000, comid = 101))
  # NON EXISTING SITE
  expect_message(findNLDI(comid = 1))
})

test_that("NLDI navigation sources...", {

  skip_on_cran()

  # UPPER TRIBUTARY
  expect_equal(length(findNLDI(nwis = '11120000', nav = "UT")), 2)
  # UPPER MAIN
  expect_equal(length(findNLDI(nwis = '11120000', nav = "UM")), 2)
  # DOWNSTREAM MAIN
  expect_equal(length(findNLDI(nwis = '11120000', nav = "DM")), 2)
  # MULTI-REQUEST
  expect_equal(length(findNLDI(nwis = '11120000', nav = c("UT", "UM"))), 3)
  # ERRORS: Bad NAV REQUEST
  expect_error(findNLDI(nwis = '11120000', nav = c("DT")))
  expect_error(findNLDI(nwis = '11120000', nav = c("DT", "UM")))
  # WARNING: Data not found
  expect_warning(findNLDI(comid = 101, nav = "UM", find = "nwis"))
})

test_that("NLDI find sources...", {

  skip_on_cran()

  expect_equal(length(findNLDI(nwis = '11120000', nav = "UT", find = "wade")), 2)
  expect_equal(length(findNLDI(nwis = '11120000', nav = c("UT", "UM"), find = c("nwis", "wade", "flowlines"))), 6)
})

test_that("sf not installed...", {

  skip_on_cran()

  expect_true(!"geometry" %in% findNLDI(nwis = '11120000', no_sf = TRUE)[[1]])
  expect_equal(class(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis"),  no_sf = TRUE)[[2]]), "data.frame")
  expect_true(c("X") %in% names(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis"), no_sf = TRUE)[[2]]))
})


test_that("Distance...", {

  skip_on_cran()

  full = findNLDI(comid = 101, nav = "UT", find = "nwis", distance_km = 9999)
  part = findNLDI(comid = 101, nav = "UT", find = "nwis")
  expect_true(nrow(full$UT_nwissite) > nrow(part$UT_nwissite))
})

test_that("basin", {

  skip_on_cran()

  xx = findNLDI(comid = 101, nav = "UT", find = "basin")
  xx2 = findNLDI(comid = 101, nav = "UT", find = "basin", no_sf = TRUE)
  expect_true(sf::st_geometry_type(xx$basin) == "POLYGON")
  expect_equal(ncol(xx2$basin),0)
})


test_that("ignore flowlines", {

  skip_on_cran()

  xx = findNLDI(comid = 101, nav = "DM", find = c("nwis", "flowlines"))
  xx2 = findNLDI(comid = 101, nav = "DM", find = "nwis")
  expect_gt(length(xx), length(xx2))
  expect_true("DM_flowlines" %in% names(xx))
  expect_true(!"DM_flowlines" %in% names(xx2))
})


test_that("sf points", {
  skip_on_cran()
  library(sf)
  p2 = st_sfc(st_point(c(-119.8458,34.4146)), crs = 4326)
  expect_equal(findNLDI(location = p2), findNLDI(location = sf::st_as_sf(p2)))
  expect_error(findNLDI(location = st_buffer(p2,.01)))
})

