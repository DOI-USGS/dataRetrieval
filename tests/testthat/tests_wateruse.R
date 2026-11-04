context("Water Use functions")

test_that("Water Use retrievals working", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  
  vars <-  c("pswdtot", "pswdgw", "pswdsw")
  wu1 <- read_wateruse(model = "wu-public-supply-wd",
                            variable = vars,
                            location = "stateCd:RI",
                            startdate = "2020-01",
                            timeres = "monthly")
  
  expect_all_true(paste0(vars, "_mgd") %in% names(wu1))
  
  expect_error(read_wateruse(model = "wu-public-supply-wd",
                                  variable = c("pswdtot", "pswdgw", "pswdsw"),
                                  location = "stateCd:RI",
                                  startdate = "2020", #wrong type of startdate
                                  timeres = "monthly"))
  
  wu2 <- read_wateruse(model = "wu-thermoelectric",
                            variable = c('tecufgw'),
                            location = "stateCd:RI",
                            startdate = "2020-01",
                            timeres = "monthly")
  
  expect_all_true(paste0("tecufgw", "_mgd") %in% names(wu2))
  
  wu3 <- read_wateruse(model = "wu-irrigation-cu",
                            variable = "irrcutot",
                            location = "stateCd:WI",
                            startdate = "2020-01",
                            timeres = "monthly")
  
  expect_all_true(paste0("irrcutot", "_mgd") %in% names(wu3))
  
  vars <- c("irrwdtot", "irrwdgw", "irrwdsw")
  wu4 <- read_wateruse(model = "wu-irrigation-wd",
                            variable = vars,
                            location = "huc2:04",
                            startdate = "2015",
                            timeres = "annualcy")
  expect_all_true(paste0(vars, "_mgd") %in% names(wu4))
  
  wu5 <- read_wateruse(model = "wu-public-supply-cu",
                            variable = "pscutot",
                            location = "stateCd:WI",
                            startdate = "2020",
                            timeres = "annualwy")
  expect_all_true(paste0("pscutot", "_mgd") %in% names(wu5))
})
