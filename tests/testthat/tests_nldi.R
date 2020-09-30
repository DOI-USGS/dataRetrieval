context("NLDI...")

test_that("NLDI offerings...", {
  expect_true(nrow(get_nldi_sources()) > 1)
})

test_that("NLDI starting sources...", {

  # LINESTRING GEOMETERY
  expect_equal(sum(names(findNLDI(comid = 101)[[1]]) ==
                     c('sourceName', 'identifier', "geometry")),  3)
  # POINT GEOMETERY
  expect_equal(sum(names(findNLDI(nwis = '11120000')[[1]]) ==
                     c('sourceName', 'identifier', "X", "Y", "geometry")),  5)
  # COMID
  expect_equal(findNLDI(comid = 101)[[1]]$sourceName, "NHDPlus comid")
  # NWIS
  expect_equal(findNLDI(nwis = '11120000')[[1]]$sourceName, "NWIS Sites")
  # WQP
  expect_equal(findNLDI(wqp = 'USGS-04024315')[[1]]$sourceName, "Water Quality Portal")
  # LOCATION
  expect_equal(findNLDI(location = c(-115,40))[[1]]$sourceName, "NHDPlus comid")
  # ERROR: LOCATION COORDINATES FLIPPED
  expect_error(findNLDI(location = c(40, -115)))
  # GENERAL START: STABLE
  expect_equal(findNLDI(origin = list("comid" = 101))[[1]]$sourceName, "NHDPlus comid")
  # GENERAL START: NON-STABLE
  expect_equal(findNLDI(origin = list("wade" = 'CA_45206'))[[1]]$sourceName, "Water Data Exchange 2.0 Sites")
  # ERROR: TWO STARTS
  expect_error(findNLDI(nwis = 1000, comid = 101))
  # NON EXISTING SITE
  expect_message(findNLDI(comid = 1))
})

test_that("NLDI navigation sources...", {
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
  # MESSAGE: Data not found
  expect_message(findNLDI(comid = 101, nav = "UM", find = "nwis"))
})

test_that("NLDI find sources...", {
  expect_equal(length(findNLDI(nwis = '11120000', nav = "UT", find = "wade")),3)
  expect_equal(length(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis", "wqp"))), 4)
  expect_equal(length(findNLDI(nwis = '11120000', nav = c("UT", "UM"), find = c("nwis", "wqp"))), 7)
})

test_that("sf not installed...", {
  expect_true(!"geometry" %in% findNLDI(nwis = '11120000', no_sf = TRUE)[[1]])
  expect_equal(class(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis"),  no_sf = TRUE)[[2]]), "character")
  expect_true(c("X") %in% names(findNLDI(nwis = '11120000', nav = "UT", find = c("nwis"), no_sf = TRUE)[[3]]))
})


test_that("Distance...", {
  full = findNLDI(comid = 101, nav = "UT", find = "nwis", distance_km = 9999)
  part = findNLDI(comid = 101, nav = "UT", find = "nwis")
  expect_true(nrow(full$UT_nwissite) > nrow(part$UT_nwissite))
})

test_that("basin", {
  xx = findNLDI(comid = 101, nav = "UT", find = "basin")
  xx2 = findNLDI(comid = 101, nav = "UT", find = "basin", no_sf = TRUE)
  expect_true(sf::st_geometry_type(sf::st_as_sf(xx$basin)) == "POLYGON")
  expect_true(ncol(xx2$basin) == 0)
})




