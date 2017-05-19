## ----setup, include=FALSE, message=FALSE------------------
library(knitr)
library(dataRetrieval)

options(continue=" ")
options(width=60)
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.height = 7,
                      fig.width = 7)

opts_chunk$set(highlight=TRUE, 
               tidy=TRUE, 
               keep.space=TRUE,
               keep.blank.space=FALSE,
               keep.comment=TRUE)
knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})

## ----workflow, echo=TRUE,eval=FALSE-----------------------
#  library(dataRetrieval)
#  # Choptank River near Greensboro, MD
#  siteNumber <- "01491000"
#  ChoptankInfo <- readNWISsite(siteNumber)
#  parameterCd <- "00060"
#  
#  #Raw daily data:
#  rawDailyData <- readNWISdv(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  # Sample data Nitrate:
#  parameterCd <- "00618"
#  qwData <- readNWISqw(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  pCode <- readNWISpCode(parameterCd)
#  

## ----echo=FALSE-------------------------------------------

library(htmlTable)

Functions <- c("readNWISdata","",
               "readNWISdv","","","","",
               "readNWISqw","","","","",
               "readNWISuv","","","",
               "readNWISrating","",
               "readNWISmeas","","",
               "readNWISpeak","","",
               "readNWISgwl","","",
               "readNWISuse","","","",
               "readNWISstat","","","","","",
               "readNWISpCode",
               "readNWISsite",
               "whatNWISsites",
               "whatNWISdata","",
               "readWQPdata",
               "readWQPqw","","","",
               "whatWQPsites")
Arguments <- c("...","service", #readNWISdata
               "siteNumber","parameterCd","startDate","endDate","statCd", #readNWISdv
               "siteNumber","parameterCd","startDate","endDate","expanded", #readNWISqw
               "siteNumber","parameterCd","startDate","endDate", #readNWISuv
               "siteNumber","type", #readNWISrating
               "siteNumber","startDate","endDate", #readNWISmeas
               "siteNumber","startDate","endDate", #readNWISpeak
               "siteNumber","startDate","endDate", #readNWISgwl
               "stateCd","countyCd","years","categories", #readNWISuse
               "siteNumbers","parameterCd","startDate","endDate","statReportType","statType", #readNWISstat
               "parameterCd", #readNWISpCode
               "siteNumber", #readNWISsite
               "...", #whatNWISsites
               "siteNumber","service", #whatNWISdata
               "...", #readWQPdata
               "siteNumber","parameterCd","startDate","endDate", #readWQPqw
               "...") #whatWQPsites
Description <- c("NWIS data using user-specified queries","", #readNWISdata
                 "NWIS daily data","","","","", #readNWISdv
                 "NWIS water quality data","","","","", #readNWISqw
                 "NWIS instantaneous value data","","","", #readNWISuv
                 "NWIS rating table for active streamgage","", #readNWISrating
                 "NWIS surface-water measurements","","", #readNWISmeas
                 "NWIS peak flow data","","", #readNWISpeak
                 "NWIS groundwater level measurements","","", #readNWISgwl
                 "NWIS water use","","","", #readNWISuse
                 "NWIS statistical service","","","","","", #readNWISstat
                 "NWIS parameter code information", #readNWISpCode
                 "NWIS site information", #readNWISsite
                 "NWIS site search using user-specified queries",
                 "NWIS data availability, including period of record and count","",
                 "WQP data using user-specified queries",
                 "WQP data","","","",
                 "WQP site search using user-specified queries")

data.df <- data.frame(`Function Name` = Functions, Arguments, Description, stringsAsFactors=FALSE)

htmlTable(data.df, 
          caption="Table 1: dataRetrieval functions",
          rnames=FALSE, align=c("l","l","l","l"),
          col.rgroup = c("none", "#F7F7F7"), 
          css.cell="padding-bottom: 0.0em; padding-right: 0.5em; padding-top: 0.0em;")


## ----tableParameterCodes, echo=FALSE----------------------


pCode <- c('00060', '00065', '00010','00045','00400')
shortName <- c("Discharge [ft<sup>3</sup>/s]","Gage height [ft]","Temperature [C]", "Precipitation [in]", "pH")

data.df <- data.frame(pCode, shortName, stringsAsFactors=FALSE)

htmlTable(data.df, 
          caption="Table 2: Common USGS Parameter Codes",
          rnames=FALSE, align=c("c","c"), col.rgroup = c("none", "#F7F7F7"), 
          css.cell="padding-bottom: 0.0em; padding-right: 0.5em; padding-top: 0.0em;")



## ----tableStatCodes, echo=FALSE---------------------------
StatCode <- c('00001', '00002', '00003','00008')
shortName <- c("Maximum","Minimum","Mean", "Median")

data.df <- data.frame(StatCode, shortName, stringsAsFactors=FALSE)

htmlTable(data.df, 
          caption="Table 3: Commonly used USGS Stat Codes",
          rnames=FALSE, align=c("c","c"), col.rgroup = c("none", "#F7F7F7"), 
          css.cell="padding-bottom: 0.0em; padding-right: 0.5em; padding-top: 0.0em;")




## ----getSite, echo=TRUE, eval=FALSE-----------------------
#  siteNumbers <- c("01491000","01645000")
#  siteINFO <- readNWISsite(siteNumbers)

## ----siteNames3, echo=TRUE, eval=FALSE--------------------
#  comment(siteINFO)

## ----getSiteExtended, echo=TRUE, eval=FALSE---------------
#  # Continuing from the previous example:
#  # This pulls out just the daily, mean data:
#  
#  dailyDataAvailable <- whatNWISdata(siteNumbers,
#                      service="dv", statCd="00003")
#  
#  

## ----echo=FALSE-------------------------------------------

tableData <- data.frame(
  siteNumber = c("01491000","01491000","01645000","01491000","01491000","01491000"),
  srsname = c("Temperature, water","Stream flow, mean daily",
              "Stream flow, mean daily",
              "Specific conductance",
              "Suspended sediment concentration (SSC)",
              "Suspended sediment discharge" ),
  startDate = c("2010-10-01","1948-01-01","1930-09-26","2010-10-01","1980-10-01","1980-10-01"),
  endDate = c("2012-05-09","2017-05-17","2017-05-17","2012-05-09","1991-09-30","1991-09-30"),
  count = c("529","25340","31646","527","4017","4017"),
  units = c("deg C","ft<sup>3</sup>/s","ft<sup>3</sup>/s","uS/cm @25C","mg/l","tons/day"),
  stringsAsFactors = FALSE)


htmlTable(tableData, 
          caption="Table 4: Reformatted version of output from the whatNWISdata function for the Choptank River near Greensboro, MD, and from Seneca Creek at Dawsonville, MD from the daily values service [Some columns deleted for space considerations]",
          rnames=FALSE, 
          col.rgroup = c("none", "#F7F7F7"), 
          css.cell="padding-bottom: 0.0em; padding-right: 0.5em; padding-top: 0.0em;")


## ----label=getPCodeInfo, echo=TRUE, eval=FALSE------------
#  # Using defaults:
#  parameterCd <- "00618"
#  parameterINFO <- readNWISpCode(parameterCd)

## ----label=getNWISDaily, echo=TRUE, eval=FALSE------------
#  
#  # Choptank River near Greensboro, MD:
#  siteNumber <- "01491000"
#  parameterCd <- "00060"  # Discharge
#  startDate <- "2009-10-01"
#  endDate <- "2012-09-30"
#  
#  discharge <- readNWISdv(siteNumber,
#                      parameterCd, startDate, endDate)

## ----label=getNWIStemperature, echo=TRUE, eval=FALSE------
#  siteNumber <- "01491000"
#  parameterCd <- c("00010","00060")  # Temperature and discharge
#  statCd <- c("00001","00003")  # Mean and maximum
#  startDate <- "2012-01-01"
#  endDate <- "2012-05-01"
#  
#  temperatureAndFlow <- readNWISdv(siteNumber, parameterCd,
#          startDate, endDate, statCd=statCd)
#  

## ----label=getNWIStemperature2, echo=FALSE, eval=TRUE-----
filePath <- system.file("extdata", package="dataRetrieval")
fileName <- "temperatureAndFlow.RData"
fullPath <- file.path(filePath, fileName)
load(fullPath)


## ----label=renameColumns, echo=TRUE-----------------------
names(temperatureAndFlow)

temperatureAndFlow <- renameNWISColumns(temperatureAndFlow)
names(temperatureAndFlow)


## ----label=attr1, echo=TRUE-------------------------------
#Information about the data frame attributes:
names(attributes(temperatureAndFlow))

statInfo <- attr(temperatureAndFlow, "statisticInfo")
variableInfo <- attr(temperatureAndFlow, "variableInfo")
siteInfo <- attr(temperatureAndFlow, "siteInfo")


## ---------------------------------------------------------
variableInfo <- attr(temperatureAndFlow, "variableInfo")
siteInfo <- attr(temperatureAndFlow, "siteInfo")

par(mar=c(5,5,5,5)) #sets the size of the plot window

plot(temperatureAndFlow$Date, temperatureAndFlow$Wtemp_Max,
  ylab=variableInfo$parameter_desc[1],xlab="" )
par(new=TRUE)
plot(temperatureAndFlow$Date, temperatureAndFlow$Flow,
  col="red",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE
  )
axis(4,col="red",col.axis="red")
mtext(variableInfo$parameter_desc[2],side=4,line=3,col="red")
title(paste(siteInfo$station_nm,"2012"))
legend("topleft", variableInfo$param_units, 
       col=c("black","red"),lty=c(NA,1),pch=c(1,NA))

## ----label=readNWISuv, eval=FALSE-------------------------
#  
#  parameterCd <- "00060"  # Discharge
#  startDate <- "2012-05-12"
#  endDate <- "2012-05-13"
#  dischargeUnit <- readNWISuv(siteNumber, parameterCd,
#          startDate, endDate)
#  dischargeUnit <- renameNWISColumns(dischargeUnit)

## ----label=getQW, echo=TRUE, eval=FALSE-------------------
#  
#  # Dissolved Nitrate parameter codes:
#  parameterCd <- c("00618","71851")
#  startDate <- "1985-10-01"
#  endDate <- "2012-09-30"
#  
#  dfLong <- readNWISqw(siteNumber, parameterCd,
#        startDate, endDate)
#  
#  # Or the wide return:
#  dfWide <- readNWISqw(siteNumber, parameterCd,
#        startDate, endDate, reshape=TRUE)
#  

## ----qwmeta, echo=TRUE, eval=FALSE------------------------
#  
#  comment(dfLong)
#  

## ----gwlexample, echo=TRUE, eval=FALSE--------------------
#  siteNumber <- "434400121275801"
#  groundWater <- readNWISgwl(siteNumber)

## ----peakexample, echo=TRUE, eval=FALSE-------------------
#  siteNumber <- '01594440'
#  peakData <- readNWISpeak(siteNumber)
#  

## ----ratingexample, echo=TRUE, eval=FALSE-----------------
#  ratingData <- readNWISrating(siteNumber, "base")
#  attr(ratingData, "RATING")
#  

## ----surfexample, echo=TRUE, eval=FALSE-------------------
#  surfaceData <- readNWISmeas(siteNumber)
#  

## ----eval=FALSE-------------------------------------------
#  allegheny <- readNWISuse(stateCd = "Pennsylvania",
#                           countyCd = "Allegheny")
#  
#  
#  national <- readNWISuse(stateCd = NULL,
#                          countyCd = NULL,
#                          transform = TRUE)
#  

## ----eval=FALSE-------------------------------------------
#  discharge_stats <- readNWISstat(siteNumbers=c("02319394"),
#                     parameterCd=c("00060"),
#                     statReportType="annual")
#  

## ----label=getQWData, echo=TRUE, eval=FALSE---------------
#  specificCond <- readWQPqw('WIDNR_WQX-10032762',
#                            'Specific conductance',
#                            '2011-05-01','2011-09-30')

## ----siteSearch, eval=FALSE-------------------------------
#  sites <- whatNWISsites(bBox=c(-83.0,36.5,-81.0,38.5),
#                        parameterCd=c("00010","00060"),
#                        hasDataTypeCd="dv")

## ----echo=FALSE-------------------------------------------

Service <- c("dv","iv","gwlevels","qwdata","measurements","peak","stat")
Description <- c("Daily","Instantaneous","Groundwater Levels","Water Quality","Surface Water Measurements","Peak Flow","Statistics Service")
URL <- c("<a href='https://waterservices.usgs.gov/rest/DV-Test-Tool.html' target='_blank'>https://waterservices.usgs.gov/rest/DV-Test-Tool.html<a>",
         "<a href='https://waterservices.usgs.gov/rest/IV-Test-Tool.html' target='_blank'>https://waterservices.usgs.gov/rest/IV-Test-Tool.html<a>",
         "<a href='https://waterservices.usgs.gov/rest/GW-Levels-Test-Tool.html' target='_blank'>https://waterservices.usgs.gov/rest/GW-Levels-Test-Tool.html<a>",
         "<a href='https://nwis.waterdata.usgs.gov/nwis/qwdata' target='_blank'>https://nwis.waterdata.usgs.gov/nwis/qwdata<a>",
         "<a href='https://waterdata.usgs.gov/nwis/measurements/' target='_blank'>https://waterdata.usgs.gov/nwis/measurements/<a>",
         "<a href='https://nwis.waterdata.usgs.gov/usa/nwis/peak/' target='_blank'>https://nwis.waterdata.usgs.gov/usa/nwis/peak/<a>",
         "<a href='https://waterservices.usgs.gov/rest/Statistics-Service-Test-Tool.html' target='_blank'>https://waterservices.usgs.gov/rest/Statistics-Service-Test-Tool.html<a>")

tableData <- data.frame(Service,
                        Description,
                        URL,
  stringsAsFactors = FALSE)


htmlTable(tableData, 
          caption="Table 5: NWIS general data calls",
          rnames=FALSE, align=c("l","l","l"),
          col.rgroup = c("none", "#F7F7F7"), 
          css.cell="padding-bottom: 0.0em; padding-right: 0.5em; padding-top: 0.0em;")


## ----dataExample, eval=FALSE------------------------------
#  dischargeWI <- readNWISdata(service="dv",
#                             stateCd="WI",
#                             parameterCd="00060",
#                             drainAreaMin="50",
#                             statCd="00003")
#  
#  siteInfo <- attr(dischargeWI, "siteInfo")
#  

## ----NJChloride, eval=FALSE-------------------------------
#  
#  sitesNJ <- whatWQPsites(statecode="US:34",
#                         characteristicName="Chloride")
#  

## ----phData, eval=FALSE-----------------------------------
#  dataPH <- readWQPdata(statecode="US:55",
#                   characteristicName="pH")

## ----eval=FALSE-------------------------------------------
#  type <- "Stream"
#  sites <- whatWQPdata(countycode="US:55:025",siteType=type)

## ----eval=FALSE-------------------------------------------
#  site <- whatWQPsamples(siteid="USGS-01594440")

## ----eval=FALSE-------------------------------------------
#  type <- "Stream"
#  sites <- whatWQPmetrics(countycode="US:55:025",siteType=type)

## ----meta1, eval=FALSE------------------------------------
#  
#  attr(dischargeWI, "url")
#  
#  attr(dischargeWI, "queryTime")
#  
#  siteInfo <- attr(dischargeWI, "siteInfo")
#  

## ----meta2, eval=FALSE------------------------------------
#  
#  names(attributes(dischargeWI))
#  

## ----meta3, eval=FALSE------------------------------------
#  
#  siteInfo <- attr(dischargeWI, "siteInfo")
#  
#  variableInfo <- attr(dischargeWI, "variableInfo")
#  
#  

## ----meta5, eval=FALSE------------------------------------
#  comment(peakData)
#  
#  #Which is equivalent to:
#  attr(peakData, "comment")

## ----seeVignette,eval = FALSE-----------------------------
#  vignette(dataRetrieval)

## ----installFromCran,eval = FALSE-------------------------
#  install.packages("dataRetrieval")

## ----openLibraryTest, eval=FALSE--------------------------
#  library(dataRetrieval)

## ----label=getSiteApp, echo=TRUE, eval=FALSE--------------
#  availableData <- whatNWISdata(siteNumber, "dv")
#  dailyData <- availableData["00003" == availableData$stat_cd,]
#  
#  tableData <- with(dailyData,
#        data.frame(
#          shortName=srsname,
#          Start=begin_date,
#          End=end_date,
#          Count=count_nu,
#          Units=parameter_units)
#        )

## ----label=saveData, echo=TRUE, eval=FALSE----------------
#  write.table(tableData, file="tableData.tsv",sep="\t",
#              row.names = FALSE,quote=FALSE)

