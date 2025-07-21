## ----setup, include=FALSE, message=FALSE------------------
library(knitr)
library(dataRetrieval)

options(continue = " ")
options(width = 60)
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.height = 7,
  fig.width = 7
)

## ----workflow, echo=TRUE,eval=FALSE-----------------------
# library(dataRetrieval)
# # Choptank River near Greensboro, MD
# siteNumber <- "USGS-01491000"
# ChoptankInfo <- read_waterdata_monitoring_location(siteNumber)
# parameterCd <- "00060"
# 
# # Raw daily data:
# rawDailyData <- read_waterdata_daily(monitoring_location_id = siteNumber,
#                                      parameter_code = parameterCd,
#                                      time = c("1980-01-01", "2010-01-01"))
# 
# 
# pCode <- read_waterdata_parameter_codes(parameter_code = parameterCd)

## ----echo=FALSE-------------------------------------------
Functions <- c(
  "read_waterdata",
  "read_waterdata_daily",
  "readNWISuv",
  "readNWISrating",
  "read_waterdata_field_measurements",
  "readNWISpeak",
  "read_waterdata_field_measurements",
  "readNWISuse",
  "readNWISstat",
  "read_waterdata_parameter_codes",
  "read_waterdata_monitoring_location",
  "read_waterdata_samples",
  "summarize_waterdata_samples",
  "whatNWISsites",
  "read_waterdata_ts_meta",
  "readWQPdata",
  "readWQPqw",
  "whatWQPsites",
  "whatWQPdata",
  "readWQPsummary",
  "whatWQPmetrics",
  "whatWQPsamples"
)

Description <- c(
  "Time series data using user-specified queries", # readNWISdata
  "Daily values", # readNWISdv
  "Instantaneous values", # readNWISuv
  "Rating table for active streamgage", # readNWISrating
  "Surface-water measurements", # readNWISmeas
  "Peak flow", # readNWISpeak
  "Groundwater levels", # readNWISgwl
  "Water use", # readNWISuse
  "Statistical service", # readNWISstat
  "Parameter code information", # read_waterdata_parameter_codes
  "Site information", # read_waterdata_monitoring_location
  "Discrete UGSS water quality data", # read_waterdata_samples
  "Discrete USGS water quality summary",
  "Site search using user-specified queries",
  "Data availability",
  "User-specified queries",
  "Water quality data",
  "Site search",
  "Data availability",
  "Summary data",
  "Metric availability",
  "Sample availability"
)
Source <- c("USGS Water Data API",
            "USGS Water Data API",
            rep("NWIS",2),
            "USGS Water Data API",
            "NWIS",
            "USGS Water Data API",
            rep("NWIS",2),
            "USGS Water Data API",
            "USGS Water Data API",
            "USGS Samples Data",
            "USGS Samples Data",
            "NWIS",
            "USGS Water Data API",
            rep("WQP", 7))


data.df <- data.frame(
  Name = Functions,
  `Data Returned` = Description,
  Source, stringsAsFactors = FALSE
)

kable(data.df,
  caption = "Table 1: dataRetrieval functions"
)

## ----tableParameterCodes, echo=FALSE----------------------


pCode <- c("00060", "00065", "00010", "00045", "00400")
shortName <- c(
  "Discharge [ft<sup>3</sup>/s]",
  "Gage height [ft]",
  "Temperature [C]",
  "Precipitation [in]",
  "pH"
)

data.df <- data.frame(pCode, shortName, stringsAsFactors = FALSE)

kable(data.df,
  caption = "Table 2: Common USGS Parameter Codes"
)

## ----tableStatCodes, echo=FALSE---------------------------
StatCode <- c("00001", "00002", "00003", "00008")
shortName <- c("Maximum", "Minimum", "Mean", "Median")

data.df <- data.frame(StatCode, shortName, stringsAsFactors = FALSE)

kable(data.df,
  caption = "Table 3: Commonly used USGS Stat Codes"
)

## ----getSite, echo=TRUE, eval=FALSE-----------------------
# siteNumbers <- c("USGS-01491000", "USGS-01645000")
# siteINFO <- read_waterdata_monitoring_location(siteNumbers)

## ----getSiteExtended, echo=TRUE, eval=FALSE---------------
# # Continuing from the previous example:
# # This pulls out just the daily, mean data:
# 
# dailyDataAvailable <- read_waterdata_ts_meta(
#   monitoring_location_id = siteNumbers,
#   computation_period_identifier = "Daily",
#   statistic_id = "00003"
# )

## ----echo=FALSE, eval=FALSE-------------------------------
# 
# tableData <- dailyDataAvailable[c("monitoring_location_id",
#                          "parameter_description",
#                          "unit_of_measure",
#                          "begin", "end")]
# 
# tableData$begin <- as.Date(tableData$begin)
# tableData$end <- as.Date(tableData$end)
# tableData <- sf::st_drop_geometry(tableData)
# 
# 
# knitr::kable(tableData,
#              caption = "Table 4: Reformatted version of output from the whatNWISdata function for the Choptank River near Greensboro, MD, and from Seneca Creek at Dawsonville, MD from the daily values service [Some columns deleted for space considerations]")
# 
# 
# # nolint end

## ----label=getPCodeInfo, echo=TRUE, eval=FALSE------------
# # Using defaults:
# parameterCd <- "00618"
# parameterINFO <- read_waterdata_parameter_codes(parameter_code = parameterCd)

## ----label=getNWISDaily, echo=TRUE, eval=FALSE------------
# 
# # Choptank River near Greensboro, MD:
# siteNumber <- "USSG-01491000"
# parameterCd <- "00060" # Discharge
# startDate <- "2009-10-01"
# endDate <- "2012-09-30"
# 
# discharge <- read_waterdata_daily(monitoring_location_id = siteNumber,
#                                   parameter_code = parameterCd,
#                                   time = c(startDate, endDate))

## ----label=getNWIStemperature, echo=TRUE, eval=FALSE------
# siteNumber <- "USGS-01491000"
# parameterCd <- c("00010", "00060") # Temperature and discharge
# statCd <- c("00001", "00003") # Mean and maximum
# startDate <- "2012-01-01"
# endDate <- "2012-05-01"
# 
# temperatureAndFlow <- read_waterdata_daily(monitoring_location_id = siteNumber,
#                                   parameter_code = parameterCd,
#                                   statistic_id = statCd,
#                                   time = c(startDate, endDate))

## ----label=getNWIStemperature2, echo=FALSE, eval=TRUE-----
filePath <- system.file("extdata", package = "dataRetrieval")
fileName <- "temperatureAndFlow.RData"
fullPath <- file.path(filePath, fileName)
load(fullPath)

## ---------------------------------------------------------

temperature <- temperatureAndFlow[temperatureAndFlow$parameter_code == "00010",]
temperature <- temperature[temperature$statistic_id == "00001",]

flow <- temperatureAndFlow[temperatureAndFlow$parameter_code == "00060",]

par(mar = c(5, 5, 5, 5)) # sets the size of the plot window

plot(temperature$time, temperature$value,
  ylab = "Maximum Temperture [C]",
  xlab = ""
)
par(new = TRUE)
plot(flow$time,
  flow$value,
  col = "red", type = "l",
  xaxt = "n", yaxt = "n",
  xlab = "", ylab = "",
  axes = FALSE
)
axis(4, col = "red", col.axis = "red")
mtext("Discharge [ft3/s]", side = 4, line = 3, col = "red")
title("CHOPTANK RIVER NEAR GREENSBORO, MD")
legend("topleft", unique(temperatureAndFlow$unit_of_measure),
  col = c("black", "red"), lty = c(NA, 1),
  pch = c(1, NA)
)

## ----label=readNWISuv, eval=FALSE-------------------------
# 
# parameterCd <- "00060" # Discharge
# startDate <- "2012-05-12"
# endDate <- "2012-05-13"
# dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
# dischargeUnit <- renameNWISColumns(dischargeUnit)

## ----gwlexample, echo=TRUE, eval=FALSE--------------------
# siteNumber <- "USGS-434400121275801"
# groundWater <- read_waterdata_field_measurements(monitoring_location_id = siteNumber)

## ----peakexample, echo=TRUE, eval=FALSE-------------------
# siteNumber <- "01594440"
# peakData <- readNWISpeak(siteNumber)

## ----ratingexample, echo=TRUE, eval=FALSE-----------------
# ratingData <- readNWISrating(siteNumber, "base")
# attr(ratingData, "RATING")

## ----surfexample, echo=TRUE, eval=FALSE-------------------
# surfaceData <- read_waterdata_field_measurements(monitoring_location_id = "USGS-01594440")

## ----eval=FALSE-------------------------------------------
# allegheny <- readNWISuse(
#   stateCd = "Pennsylvania",
#   countyCd = "Allegheny"
# )
# 
# 
# national <- readNWISuse(
#   stateCd = NULL,
#   countyCd = NULL,
#   transform = TRUE
# )

## ----eval=FALSE-------------------------------------------
# discharge_stats <- readNWISstat(
#   siteNumbers = c("02319394"),
#   parameterCd = c("00060"),
#   statReportType = "annual"
# )

## ----label=getQWData, echo=TRUE, eval=FALSE---------------
# specificCond <- readWQPqw(
#   "WIDNR_WQX-10032762",
#   "Specific conductance",
#   "2011-05-01", "2011-09-30"
# )

## ----siteSearch, eval=FALSE-------------------------------
# sites <- whatNWISsites(
#   bBox = c(-83.0, 36.5, -81.0, 38.5),
#   parameterCd = c("00010", "00060"),
#   hasDataTypeCd = "dv"
# )

## ----echo=FALSE-------------------------------------------
# nolint start
Service <- c("dv", "iv", "gwlevels", "measurements", "peak", "stat")
Description <- c("Daily", "Instantaneous", "Groundwater Levels", "Surface Water Measurements", "Peak Flow", "Statistics Service")
URL <- c(
  "<a href='https://waterservices.usgs.gov/docs/dv-service/' target='_blank'>https://waterservices.usgs.gov/docs/dv-service/<a>",
  "<a href='https://waterservices.usgs.gov/docs/instantaneous-values/' target='_blank'>https://waterservices.usgs.gov/docs/instantaneous-values/<a>",
  "<a href='https://waterservices.usgs.gov/docs/groundwater-levels/' target='_blank'>https://waterservices.usgs.gov/docs/groundwater-levels/<a>",
  "<a href='https://waterdata.usgs.gov/nwis/measurements/' target='_blank'>https://waterdata.usgs.gov/nwis/measurements/<a>",
  "<a href='https://nwis.waterdata.usgs.gov/usa/nwis/peak/' target='_blank'>https://nwis.waterdata.usgs.gov/usa/nwis/peak/<a>",
  "<a href='https://waterservices.usgs.gov/docs/statistics/' target='_blank'>https://waterservices.usgs.gov/docs/statistics/<a>"
)

tableData <- data.frame(Service,
  Description,
  URL,
  stringsAsFactors = FALSE
)


kable(tableData,
  caption = "Table 5: NWIS general data calls"
)
# nolint end

## ----dataExample, eval=FALSE------------------------------
# dischargeWI <- readNWISdata(
#   service = "dv",
#   stateCd = "WI",
#   parameterCd = "00060",
#   drainAreaMin = "50",
#   statCd = "00003"
# )
# 
# siteInfo <- attr(dischargeWI, "siteInfo")

## ----NJChloride, eval=FALSE-------------------------------
# 
# sitesNJ <- whatWQPsites(
#   statecode = "US:34",
#   characteristicName = "Chloride"
# )

## ----phData, eval=FALSE-----------------------------------
# dataPH <- readWQPdata(
#   statecode = "US:55",
#   characteristicName = "pH"
# )

## ----eval=FALSE-------------------------------------------
# type <- "Stream"
# sites <- whatWQPdata(countycode = "US:55:025", siteType = type)

## ----eval=FALSE-------------------------------------------
# site <- whatWQPsamples(siteid = "USGS-01594440")

## ----eval=FALSE-------------------------------------------
# type <- "Stream"
# sites <- whatWQPmetrics(countycode = "US:55:025", siteType = type)

## ----seeVignette,eval = FALSE-----------------------------
# # to see all available vignettes:
# vignette(package="dataRetrieval")
# 
# #to open a specific vignette:
# vignette(topic = "qwdata_changes", package = "dataRetrieval")

## ----cite, eval=TRUE--------------------------------------
citation(package = "dataRetrieval")

## ----nwisCite, eval=FALSE---------------------------------
# 
# dv <- readNWISdv("09010500", "00060")
# 
# NWIScitation <- create_NWIS_bib(dv)
# NWIScitation

## ----show1, eval=FALSE------------------------------------
# print(NWIScitation, style = "Bibtex")

## ----show2, eval=FALSE------------------------------------
# print(NWIScitation, style = "citation")

## ----WQPcite, eval=FALSE----------------------------------
# SC <- readWQPqw(siteNumbers = "USGS-05288705",
#                 parameterCd = "00300")
# 
# WQPcitation <- create_WQP_bib(SC)
# WQPcitation

## ----show3, eval=FALSE------------------------------------
# print(WQPcitation, style = "Bibtex")

## ----show4, eval=FALSE------------------------------------
# print(WQPcitation, style = "citation")

