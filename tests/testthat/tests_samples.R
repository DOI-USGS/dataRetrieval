context("General functions")


test_that("General samples-data retrievals work using WQP tests", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHData <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315",
                        characteristic = nameToUse)
  expect_is(pHData$Activity_StartDateTime, "POSIXct")

  # testing lists:
  startDate <- as.Date("2022-01-01")
  secchi_ops <- check_param("observedproperty",
                            text = "secchi")
  
  state_fips <- paste0("US:", stateCdLookup("WI", "id"))
  lakeData <- read_USGS_samples(activityStartDateLower = startDate,
                                activityStartDateUpper  = "2024-01-01",
                                stateFips = "US:55",
                                characteristicUserSupplied = secchi_ops$observedProperty, 
                                dataProfile = "narrow")
  
  expect_true(nrow(lakeData) > 0)
  
  lakeSites <- read_USGS_samples(monitoringLocationIdentifier = unique(lakeData$Location_Identifier),
                                 dataType = "locations", 
                                 dataProfile = "site")
  expect_type(lakeSites, "list")


  rawPcode <- read_USGS_samples(monitoringLocationIdentifier = "USGS-01594440",
                                usgsPCode = "01075")
  
  expect_true(all(c("url", "queryTime", "headerInfo") %in%
                    names(attributes(rawPcode))))

  
  pHData <- read_USGS_samples(monitoringLocationIdentifier = "USGS-04024315",
                              characteristic = "pH",
                              dataProfile = "narrow")
  
  expect_true(all(c("url", "queryTime", "headerInfo") %in%
                    names(attributes(pHData))))

  
})


context("samples-data samples")
test_that("samples-data activities working", {
  testthat::skip_on_cran()

  activityInfo <- read_USGS_samples(monitoringLocationIdentifier = "USGS-01594440",
                                dataType = "activities")
  expect_true(nrow(activityInfo) > 0)
})

context("samples-data project")
test_that("samples-data project working", {
  testthat::skip_on_cran()
  type <- "Stream"
  
  projectInfo <- read_USGS_samples(countyFips = countyCdLookup("WI", "Dane"),
                                siteTypeName = type,
                                dataType = "projects")
  expect_true(ncol(projectInfo) >= 0)
})

context("summary_USGS_samples")
test_that("summary_USGS_samples working", {
  testthat::skip_on_cran()
  
  site1 <- summarize_USGS_samples(monitoringLocationIdentifier = "USGS-01594440")
  expect_is(site1, "data.frame")

})

test_that("profiles", {
  
  testthat::skip_on_cran()
  # Data profiles: "Organization Data"
  org_data <- read_USGS_samples(
    countyFips =  countyCdLookup("WI", "Dane"),
    dataType = "organizations"
  )
  
  # Data profiles: "Site Data Only"
  site_data <- read_USGS_samples(
    countyFips =  countyCdLookup("WI", "Dane"),
    dataType = "locations"
  )
  
  expect_true(all(c("ProviderName", "Location_Identifier") %in% names(site_data)))
  
  # Data profiles: "Project Data"
  project_data <- read_USGS_samples(
    countyFips =  countyCdLookup("WI", "Dane"),
    dataType = "projects"
  )
 
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(project_data)))
  
  # Data profiles: "Project Monitoring Location Weighting Data"
  proj_mlwd <- read_USGS_samples(
    countyFips =  countyCdLookup("WI", "Dane"),
    dataType = "projects",
    dataProfile = "projectmonitoringlocationweight"
  )
  
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(proj_mlwd)))
  
  # Data profiles: "Sample Results (biological metadata)"
  samp_bio <- read_USGS_samples(
    monitoringLocationIdentifier = "USGS-04024315",
    dataProfile = "basicbio",
    dataType = "results"
  )
  
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(samp_bio)))
  
  # Data profiles: "Sample Results (narrow)"
  samp_narrow <- read_USGS_samples(
    monitoringLocationIdentifier = "USGS-04024315",
    dataProfile = "narrow",
    dataType = "results"
  )
  
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(samp_narrow)))
  
  # Data profiles: "Sampling Activity"
  samp_activity <- read_USGS_samples(
    monitoringLocationIdentifier = "USGS-04024315",
    dataProfile = "sampact", # Sampling Activities
    dataType = "activities"
  )
  
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(samp_activity)))
  
  # Data profile: "Result Detection Quantitation Limit Data"
  dl_data <- read_USGS_samples(
    monitoringLocationIdentifier = "USGS-04024315",
    dataType = "results",
    dataProfile = "resultdetectionquantitationlimit"
  )
  
  expect_true(all(c(
    "Org_Identifier",
    "Org_FormalName"
  ) %in% names(dl_data)))
})



