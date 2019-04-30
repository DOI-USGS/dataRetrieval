context("General functions")

test_that("General NWIS retrievals working", {
  testthat::skip_on_cran()
  
  multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", 
                            parameterCd="00060")
  expect_is(multiSite$dateTime, 'POSIXct')
  
  recent_uv <- readNWISdata(siteNumber="04025500",parameterCd="00060",service="uv",
                          startDate=as.Date(Sys.Date()-10),endDate=Sys.Date())
  expect_equal(grep(x = attr(recent_uv, "url"),pattern = "https://waterservices.usgs.gov/nwis/iv/"),1)
  
  older_uv <- readNWISdata(siteNumber="04025500",parameterCd="00060",service="uv",
                            startDate="2016-01-01",endDate="2016-01-02")
  expect_equal(grep(x = attr(older_uv, "url"),pattern = "https://nwis.waterservices.usgs.gov/nwis/iv/"),1)
  
  
  expect_error(readNWISdata(), "No arguments supplied")
  expect_error(readNWISdata(siteNumber = NA), "NA's are not allowed in query")
  
  bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
  expect_true(length(unique(bBoxEx$site_no)) > 1)
  
  startDate <- as.Date("2013-10-01")
  endDate <- as.Date("2014-09-30")
  waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
                   service="dv", startDate=startDate, endDate=endDate)
  expect_is(waterYear$dateTime, 'POSIXct')
  
  siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",hasDataTypeCd="iv", 
                           service="site")
  expect_is(siteInfo$station_nm, "character")
  
  qwData <- readNWISdata(bBox=c(-82.5,41.52,-81,41),startDate=as.Date("2000-01-01"),
                   drain_area_va_min=50, qw_count_nu=50,qw_attributes="expanded",
                    qw_sample_wide="wide",list_of_search_criteria=c("lat_long_bounding_box",
                    "drain_area_va","obs_count_nu"),service="qw")
  expect_is(qwData$startDateTime, "POSIXct")
  
  url <- "https://waterservices.usgs.gov/nwis/dv/?site=09037500&format=rdb&ParameterCd=00060&StatCd=00003&startDT=1985-10-02&endDT=2012-09-06"
  dv <- importRDB1(url, asDateTime = FALSE)
  
  urlEmpty <- "https://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=413437087150601&sort_key=site_no&group_key=NONE&inventory_output=0&begin_date=&end_date=&TZoutput=0&param_group=NUT,INN&qw_attributes=0&format=rdb&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&list_of_search_criteria=multiple_site_no"
  dv <- importRDB1(urlEmpty, asDateTime = FALSE)
  expect_true(nrow(dv) == 0)
  
  dailyStat <- readNWISdata(site=c("03112500","03111520"),service="stat",statReportType="daily",
                           statType=c("p25","p50","p75","min","max"),parameterCd="00065",convertType=FALSE)
  expect_true(length(dailyStat$min_va) > 1)
  expect_is(dailyStat$p25_va,"character")
  
  waterYearStat <- readNWISdata(site=c("03112500"),service="stat",statReportType="annual",
                                statYearType="water", missingData="on")
  expect_is(waterYearStat$mean_va,"numeric")
  expect_is(waterYearStat$parameter_cd,"character")
  
  #2 data descriptors, but some random empty "values" tag:
  urlTest <- "https://nwis.waterservices.usgs.gov/nwis/iv/?site=11447650&format=waterml,1.1&ParameterCd=63680&startDT=2016-12-13&endDT=2016-12-13"
  x <- importWaterML1(urlTest)
  expect_equal(ncol(x), 6)
  
  #Test list:
  args <- list(sites="05114000", service="iv", 
               parameterCd="00060", 
               startDate="2014-05-01T00:00Z",
               endDate="2014-05-01T12:00Z")
  
  instData <- readNWISdata(args)
  
  args <- list(sites="05114000", service="dv", 
               parameterCd="00060", 
               startDate="2014-05-01",
               endDate="2014-05-01")
  
  dailyData <- readNWISdata(args)
  expect_lt(nrow(dailyData), nrow(instData))
  args <- list(stateCd="OH",parameterCd="00665")
  sites <- whatNWISsites(args)
  expect_type(sites, "list")
  
  #Test counties:
  dailyStaffordVA <- readNWISdata(stateCd = "Virginia",
                                  countyCd="Stafford",
                                  parameterCd = "00060",
                                  startDate = "2015-01-01",
                                  endDate = "2015-01-30")
  expect_gt(nrow(dailyStaffordVA),1)
  
  AS <- readNWISdata(stateCd = "AS", service="site")
  expect_gt(nrow(AS),0)
  
  site_id <- '01594440'
  rating_curve <- readNWISdata(service = "rating", site_no = site_id, file_type="base")
  rating_curve2 <- readNWISrating(siteNumber = site_id, type = "base")
  expect_equal(attr(rating_curve,"url"), "https://waterdata.usgs.gov/nwisweb/get_ratings/?site_no=01594440&file_type=base")
  expect_equal(rating_curve$INDEP, rating_curve2$INDEP)
  
  state_rating_list <- readNWISdata(service = "rating", file_type="base", period = 24)
  expect_true(all(names(state_rating_list) %in% c("agency_cd",
                                              "site_no",
                                              "type",
                                              "update_time",
                                              "url")))
  
})

test_that("whatNWISdata",{
  
  #no service specified:
  availableData <- whatNWISdata(siteNumber = '05114000')
  expect_equal(ncol(availableData), 24)
  
  uvData <- whatNWISdata(siteNumber = '05114000',service="uv")
  expect_equal(unique(uvData$data_type_cd), "uv")
  
  #multiple services
  uvDataMulti <- whatNWISdata(siteNumber = c('05114000','09423350'),service=c("uv","dv"))
  expect_true(all(unique(uvDataMulti$data_type_cd) %in% c("uv","dv")))
  
  #state codes:
  flowAndTemp <- whatNWISdata(stateCd = "WI", service = c("uv","dv"),
                              parameterCd = c("00060","00010"),
                              statCd = "00003")
  expect_true(all(unique(flowAndTemp$data_type_cd) %in% c("uv","dv")))
  expect_true(all(unique(flowAndTemp$parm_cd) %in% c("00060","00010")))
  expect_true(all(unique(flowAndTemp$stat_cd) %in% c("00003",NA)))
  
})

test_that("General WQP retrievals working", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHData <- readWQPdata(siteid="USGS-04024315",characteristicName=nameToUse)
  expect_is(pHData$ActivityStartDateTime, 'POSIXct')
  
  #testing lists:
  startDate <- as.Date("2013-01-01")
  secchi.names = c("Depth, Secchi disk depth",
                   "Depth, Secchi disk depth (choice list)",
                   "Secchi Reading Condition (choice list)",
                   "Secchi depth",
                   "Water transparency, Secchi disc")
  args_2 <- list('startDateLo' = startDate,
               'startDateHi' = "2013-12-31",
                statecode="WI",
                characteristicName=secchi.names)

  wqp.summary <- readWQPdata(args_2, querySummary = TRUE)
  expect_true("list" %in% class(wqp.summary))
  
  #pretty sloooow:
  # wqp.data <- readWQPdata(args_2, querySummary = FALSE)
  # expect_false("list" %in% class(wqp.data))
  
  # Testing multiple lists:
  arg_3 <- list('startDateLo' = startDate,
               'startDateHi' = "2013-12-31")
  arg_4 <- list(statecode="WI",
                characteristicName=secchi.names)
  wqp.summary <- readWQPdata(arg_3, arg_4, querySummary=TRUE)
  expect_true("list" %in% class(wqp.summary))
  
  lakeSites <- whatWQPsites(args_2)
  expect_type(lakeSites, "list")
  
  # Test county code:
   dailyLexingtonVA <- readWQPdata(statecode = "Virginia", 
                                   countycode="Lexington", 
                                   parameterCd = "00010")
   
   expect_equal(ncol(dailyLexingtonVA),65)
  
   bioData <- readWQPdata(statecode = "WI",
                          countycode = "Dane",
                          providers = "BIODATA")
   
   expect_equal(attr(bioData, "url"), "https://www.waterqualitydata.us/Result/search?statecode=US%3A55&countycode=US%3A55%3A025&providers=BIODATA&zip=yes&mimeType=tsv")
   expect_gt(nrow(bioData), 1)
   
   site1 <- readWQPsummary(siteid="USGS-07144100",
                           summaryYears=5,
                           dataProfile="periodOfRecord")
   
   expect_type(site1$ActivityCount, "double")
   expect_type(site1$MonitoringLocationIdentifier, "character")

   expect_equal(attr(site1, "url"), "https://www.waterqualitydata.us/data/summary/monitoringLocation/search?siteid=USGS-07144100&summaryYears=5&dataProfile=periodOfRecord&zip=yes&mimeType=csv")
   
})

test_that("WQP head query retrievals working", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHDataQueryResults <- readWQPdata(siteid="USGS-04024315",
                                    characteristicName=nameToUse, 
                                    querySummary=TRUE)
  expect_false(is.null(pHDataQueryResults$date))
  expect_is(pHDataQueryResults$date, 'Date')
  expect_false(is.null(pHDataQueryResults$`total-site-count`))
  expect_is(pHDataQueryResults$`total-site-count`, 'numeric')
  expect_false(is.null(pHDataQueryResults$`total-result-count`))
  expect_is(pHDataQueryResults$`total-result-count`, 'numeric')
  
  pHDataQueryResults <- readWQPqw(siteNumbers="USGS-04024315",
                                  parameterCd=nameToUse, 
                                  querySummary=TRUE)
  expect_false(is.null(pHDataQueryResults$date))
  expect_is(pHDataQueryResults$date, 'Date')
  expect_false(is.null(pHDataQueryResults$`total-site-count`))
  expect_is(pHDataQueryResults$`total-site-count`, 'numeric')
  expect_false(is.null(pHDataQueryResults$`total-result-count`))
  expect_is(pHDataQueryResults$`total-result-count`, 'numeric')
})

test_that("zeroPad handles NAs", {
  toPad <- c(1,5,55,NA)
  padded <- zeroPad(toPad,3)
  expect_true(identical(c("001","005","055",NA),padded))
})

test_that("Dates with no days can be handled", {
  testthat::skip_on_cran()
  empty_df <- readNWISgwl("425957088141001", startDate = "1980-01-01")
  expect_true(nrow(empty_df) > 0) 
 })

context("whatWQPsamples")
test_that("whatWQPsamples working", {
  testthat::skip_on_cran()
  # The warning is caused by a confirmed bug in WQP
  siteInfo <- suppressWarnings(whatWQPsamples(siteid="USGS-01594440"))
  expect_true(nrow(siteInfo) > 0)
  
  })

context("whatWQPmetrics")
test_that("whatWQPmetrics working", {
  testthat::skip_on_cran()
  type <- "Stream"
  siteInfo <- whatWQPmetrics(countycode="US:55:025",siteType=type)
  expect_true(ncol(siteInfo) >= 21)
  
})

context("whatWQPdata")
test_that("whatWQPdata working", {
  testthat::skip_on_cran()
  
  site1 <- whatWQPdata(siteid="USGS-01594440")
  expect_is(site1, "data.frame")
  expect_equal(1, nrow(site1))
  
  type <- "Stream"
  sites <- whatWQPdata(countycode="US:55:025",siteType=type)
  expect_gt(nrow(sites), 1)
  
  lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
  expect_is(lakeSites$activityCount, "numeric")
})

context("whatNWISsites")
test_that("whatNWISsites working", {
  testthat::skip_on_cran()
  siteListPhos <- whatNWISsites(stateCd="OH",parameterCd="00665")
  expect_true(nrow(siteListPhos) > 0)
  expect_true(is.numeric(siteListPhos$dec_lat_va))
  
  bboxSites <- whatNWISsites(bbox = c(-92.5, 45.4, -87, 47), parameterCd="00060")
  expect_true(nrow(bboxSites) > 0)
  expect_true(is.numeric(bboxSites$dec_lat_va))
})

context("readWQPdots")
test_that("readWQPdots working", {
  testthat::skip_on_cran()
  
  # bbox vector turned into single string with coords separated by semicolons
  formArgs_bbox <- dataRetrieval:::readWQPdots(bbox = c(-92.5, 45.4, -87, 47))
  expect_true(length(formArgs_bbox) == 2)
  expect_true(length(gregexpr(";", formArgs_bbox)[[1]]) == 3)
  
  # NWIS names (siteNumber) converted to WQP expected names (siteid)
  formArgs_site <- dataRetrieval:::readWQPdots(siteNumber="04010301")
  expect_true(length(formArgs_site) == 2)
  expect_true("siteid" %in% names(formArgs_site))
  expect_false("siteNumber" %in% names(formArgs_site))
  
  # NWIS names (stateCd) converted to WQP expected names (statecode)
  formArgs <- dataRetrieval:::readWQPdots(stateCd="OH",parameterCd="00665")
  expect_true(length(formArgs) == 3)
  expect_true("statecode" %in% names(formArgs))
  expect_false("stateCd" %in% names(formArgs))
})

context("NGWMN")
test_that("NGWMN functions working", {
  testthat::skip_on_cran()
  noDataSite <- "UTGS.401544112060301"
  noDataSite <- readNGWMNlevels(siteNumbers = noDataSite)
  expect_true(is.data.frame(noDataSite))

  #bounding box and multi-site getFeatureOfInterest request
  bboxSites <- readNGWMNdata(service = "featureOfInterest", bbox = c(30, -99, 31, 102))
  expect_gt(nrow(bboxSites), 0)
  # siteInfo <- readNGWMNsites(bboxSites$site[1:3])
  # expect_equal(nrow(siteInfo), 3)

  #one site
  site <- "USGS.430427089284901"
  oneSite <- readNGWMNlevels(siteNumbers = site)
  siteInfo <- readNGWMNsites(site)
  expect_true(is.numeric(oneSite$value))
  expect_true(is.character(oneSite$site))
  expect_true(is.data.frame(siteInfo))
  expect_true(nrow(siteInfo) > 0)
  expect_true(nrow(oneSite) > 0)

  #non-USGS site
  data <- readNGWMNlevels(siteNumbers = "MBMG.103306")
  expect_true(nrow(data) > 1)
  expect_true(is.numeric(oneSite$value))

  #sites with colons and NAs work

  na_colons <- c(NA, bboxSites$site[200:212], NA, NA)
  returnDF <- readNGWMNdata(service = "observation",
                            siteNumbers = na_colons, asDateTime = FALSE)
  expect_is(returnDF, "data.frame")
  expect_true(nrow(returnDF) > 1)
  #expect_true(!is.null(attributes(returnDF)$siteInfo))

  sites <- c("USGS:424427089494701", NA)
  siteInfo <- readNGWMNsites(sites)
  expect_is(siteInfo, "data.frame")
  expect_true(nrow(siteInfo) == 1)

  #time zones
  tzSite <- "USGS.385111104214403"
  tzDataUTC <- readNGWMNlevels(tzSite, asDateTime = TRUE)
  tzDataMT <- readNGWMNlevels(tzSite, asDateTime = TRUE,
                              tz = "US/Mountain")
  expect_gt(nrow(tzDataMT), 1)
  expect_gt(nrow(tzDataUTC), 1)
  expect_is(tzDataUTC$dateTime, "POSIXct")
  expect_is(tzDataMT$dateTime, "POSIXct")
  expect_equal(attr(tzDataMT$dateTime, 'tzone'), "US/Mountain")
  expect_equal(attr(tzDataUTC$dateTime, 'tzone'), "UTC")
})

context("getWebServiceData")
test_that("long urls use POST", {
  testthat::skip_on_cran()
  url <- paste0(rep("reallylongurl", 200), collapse = '')
  with_mock(
    RETRY = function(method, ...) {
      return(method == "POST")
    },
    status_code = function(resp) 200,
    headers = function(resp) list(`content-type` = "logical"),
    content = function(resp, encoding) resp,
    expect_true(getWebServiceData(url)),
    .env = "httr"
  )
})

test_that("ngwmn urls don't use post", {
  testthat::skip_on_cran()
  url <- paste0(rep("urlwithngwmn", 200), collapse = '')
  with_mock(
    RETRY = function(method, ...) {
      return(method == "POST")
    },
    status_code = function(resp) 200,
    headers = function(resp) list(`content-type` = "logical"),
    content = function(resp, encoding) resp,
    expect_false(getWebServiceData(url)),
    .env = "httr"
  )
})

test_that("400 errors return a verbose error", {
  testthat::skip_on_cran()
  
  url <- "https://waterservices.usgs.gov/nwis/site/?stateCd=IA&bBox=-92.821445,42.303044,-92.167168,42.646524&format=mapper"

  expect_error(getWebServiceData(url))
  expect_equal(tryCatch(getWebServiceData(url), error = function(e) e$message), "HTTP Status 400 - syntactic error: Only one Major filter can be supplied. Found [bbox] and [stateCd]. Please remove the extra major filter[s].")
  
})

test_that("internal functions",{
  
  # get empty_col type
  expect_equal(dataRetrieval:::empty_col("numeric"), numeric())
  expect_equal(dataRetrieval:::empty_col("character"), character())
  expect_equal(dataRetrieval:::empty_col("Date"), as.Date(numeric(),origin = "1970-01-01"))
  
  # add empty columns
  df1 <- data.frame(a = 1:2,
                    b = c("a","b"),
                    c = as.Date(c("2010-01-01","2010-01-02")),
                    stringsAsFactors = FALSE)
  df2 <- data.frame(d = 1:2,
                    b = c("a","b"),
                    e = factor(c("c","d")),
                    stringsAsFactors = FALSE)
  
  df2_ret <- dataRetrieval:::add_empty_col(df2, df1, c("a","c"))
  df1_ret <- dataRetrieval:::add_empty_col(df1, df2, c("d","e"))
  
  expect_true(ncol(df2_ret) == 5)
  expect_true(ncol(df1_ret) == 5)
  expect_true(all(is.na(df2_ret[,c("a","c")])))
  expect_true(all(is.na(df1_ret[,c("d","e")])))
  
  bound_df <- dataRetrieval:::r_bind_dr(df1, df2)
  
  expect_true(nrow(bound_df) == 4)
  expect_true(class(bound_df$c) == "Date")
  expect_true(class(bound_df$a) == "integer")
  expect_true(class(bound_df$b) == "character")

})


