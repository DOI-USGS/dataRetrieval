## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, tidy=FALSE,comment="")
knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

bold.colHeaders <- function(x) {
  x <- gsub("\\^(\\d)","$\\^\\1$",x)
  x <- gsub("\\%","\\\\%",x)
  x <- gsub("\\_"," ",x)
  returnX <- paste("\\multicolumn{1}{c}{\\textbf{\\textsf{", x, "}}}", sep = "")
}
addSpace <- function(x) ifelse(x != "1", "[5pt]","")

## ----workflow, echo=TRUE,eval=FALSE-----------------------
#  library(dataRetrieval)
#  # Choptank River near Greensboro, MD
#  siteNumber <- "01491000"
#  ChoptankInfo <- getNWISSiteInfo(siteNumber)
#  parameterCd <- "00060"
#  
#  #Raw daily data:
#  rawDailyData <- getNWISdvData(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  # Data compiled for EGRET analysis
#  Daily <- getNWISDaily(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  # Sample data Nitrate:
#  parameterCd <- "00618"
#  Sample <- getNWISSample(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  # Metadata on site and nitrate:
#  INFO <- getNWISInfo(siteNumber,parameterCd)
#  
#  # Merge discharge and nitrate data to one dataframe:
#  Sample <- mergeReport()
#  

## ----tableParameterCodes, echo=FALSE,results='asis'-------
pCode <- c('00060', '00065', '00010','00045','00400')
shortName <- c("Discharge [ft$^3$/s]","Gage height [ft]","Temperature [C]", "Precipitation [in]", "pH")

data.df <- data.frame(pCode, shortName, stringsAsFactors=FALSE)

print(xtable(data.df,
       label="tab:params",
       caption="Common USGS Parameter Codes"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----tableParameterCodesDataRetrieval---------------------
library(dataRetrieval)
parameterCdFile <-  parameterCdFile
names(parameterCdFile)

## ----tableStatCodes, echo=FALSE,results='asis'------------
StatCode <- c('00001', '00002', '00003','00008')
shortName <- c("Maximum","Minimum","Mean", "Median")

data.df <- data.frame(StatCode, shortName, stringsAsFactors=FALSE)

print(xtable(data.df,label="tab:stat",
           caption="Commonly used USGS Stat Codes"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.colnames.function = bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----getSite, echo=TRUE-----------------------------------
siteNumbers <- c("01491000","01645000") 
siteINFO <- getNWISSiteInfo(siteNumbers)

## ----siteNames2, echo=TRUE--------------------------------
siteINFO$station.nm

## ----getSiteExtended, echo=TRUE---------------------------
# Continuing from the previous example:
# This pulls out just the daily data:

dailyDataAvailable <- getNWISDataAvailability(siteNumbers,
                    type="dv")


## ----tablegda, echo=FALSE,results='asis'------------------
tableData <- with(dailyDataAvailable, 
      data.frame( 
      siteNumber= site_no,
      srsname=srsname, 
      startDate=as.character(startDate), 
      endDate=as.character(endDate), 
      count=as.character(count),
      units=parameter_units,
      statCd = statCd,
      stringsAsFactors=FALSE)
      )

tableData$units[which(tableData$units == "ft3/s")] <- "ft$^3$/s"
tableData$units[which(tableData$units == "uS/cm @25C")] <- "$\\mu$S/cm @25C"


print(xtable(tableData,label="tab:gda",
    caption="Daily mean data availabile at the Choptank River near Greensboro, MD. [Some columns deleted for space considerations]"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----label=getPCodeInfo, echo=TRUE------------------------
# Using defaults:
parameterCd <- "00618" 
parameterINFO <- getNWISPcodeInfo(parameterCd)
colnames(parameterINFO)

## ----siteNames, echo=TRUE---------------------------------
parameterINFO$parameter_nm

## ----label=getNWISDaily, echo=TRUE, eval=TRUE-------------

# Continuing with our Choptank River example
siteNumber <- "01491000"
parameterCd <- "00060"  # Discharge
startDate <- ""  # Will request earliest date
endDate <- "" # Will request latest date

discharge <- getNWISdvData(siteNumber, 
                    parameterCd, startDate, endDate)
names(discharge)

## ----label=getNWIStemperature, echo=TRUE------------------

parameterCd <- c("00010","00060")  # Temperature and discharge
statCd <- c("00001","00003")  # Mean and maximum
startDate <- "2012-01-01"
endDate <- "2012-05-01"

temperatureAndFlow <- getNWISdvData(siteNumber, parameterCd, 
        startDate, endDate, statCd=statCd)


## ----label=renameColumns, echo=TRUE-----------------------
names(temperatureAndFlow)

temperatureAndFlow <- renameColumns(temperatureAndFlow)
names(temperatureAndFlow)

## ----getNWIStemperaturePlot, echo=TRUE, fig.cap="Temperature and discharge plot of Choptank River in 2012.",out.width='1\\linewidth',out.height='1\\linewidth',fig.show='hold'----
par(mar=c(5,5,5,5)) #sets the size of the plot window

with(temperatureAndFlow, plot(
  datetime, Temperature_water_degrees_Celsius_Max_01,
  xlab="Date",ylab="Max Temperature [C]"
  ))
par(new=TRUE)
with(temperatureAndFlow, plot(
  datetime, Discharge_cubic_feet_per_second,
  col="red",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE
  ))
axis(4,col="red",col.axis="red")
mtext(expression(paste("Mean Discharge [ft"^"3","/s]",
                       sep="")),side=4,line=3,col="red")
title(paste(siteINFO$station.nm[1],"2012",sep=" "))
legend("topleft", c("Max Temperature", "Mean Discharge"), 
       col=c("black","red"),lty=c(NA,1),pch=c(1,NA))

## ----label=getNWISUnit, echo=TRUE-------------------------

parameterCd <- "00060"  # Discharge
startDate <- "2012-05-12" 
endDate <- "2012-05-13" 
dischargeToday <- getNWISunitData(siteNumber, parameterCd, 
        startDate, endDate)

## ----dischargeData, echo=FALSE----------------------------
head(dischargeToday)

## ----label=getQW, echo=TRUE-------------------------------
 
# Dissolved Nitrate parameter codes:
parameterCd <- c("00618","71851")
startDate <- "1985-10-01"
endDate <- "2012-09-30"

dissolvedNitrate <- getNWISqwData(siteNumber, parameterCd, 
      startDate, endDate, expanded=TRUE)
names(dissolvedNitrate)



## ----getQWtemperaturePlot, echo=TRUE, fig.cap=paste(parameterINFO$parameter_nm, "at", siteINFO$station.nm[1])----
with(dissolvedNitrate, plot(
  startDateTime, result_va_00618,
  xlab="Date",ylab = paste(parameterINFO$srsname,
      "[",parameterINFO$parameter_units,"]")
  ))
title(siteINFO$station.nm[1])

## ----label=geturl, echo=TRUE, eval=FALSE------------------
#  # Dissolved Nitrate parameter codes:
#  pCode <- c("00618","71851")
#  startDate <- "1964-06-11"
#  endDate <- "2012-12-18"
#  url_qw <- constructNWISURL(siteNumber,pCode,startDate,endDate,'qw')
#  url_dv <- constructNWISURL(siteNumber,"00060",startDate,endDate,
#                             'dv',statCd="00003")
#  url_uv <- constructNWISURL(siteNumber,"00060",startDate,endDate,'uv')

## ----label=getQWData, echo=TRUE, eval=FALSE---------------
#  specificCond <- getWQPqwData('WIDNR_WQX-10032762',
#                  'Specific conductance','2011-05-01','2011-09-30')

## ----siteSearch-------------------------------------------
sites <- getNWISSites(bBox="-83.0,36.5,-81.0,38.5", 
                      parameterCd="00010,00060",
                      hasDataTypeCd="dv")

names(sites)
nrow(sites)

## ----dataExample------------------------------------------
dischargeWI <- getNWISData(stateCd="WI",
                           parameterCd="00060",
                           drainAreaMin="50",
                           statCd="00003")
names(dischargeWI)
nrow(dischargeWI)

## ----NJChloride, eval=FALSE-------------------------------
#  
#  sitesNJ <- getWQPSites(statecode="US:34",
#                         characteristicName="Chloride")
#  

## ----phData, eval=FALSE-----------------------------------
#  
#  dataPH <- getWQPData(statecode="US:55",
#                   characteristicName="pH")
#  

## ----helpFunc,eval = FALSE--------------------------------
#  ?getNWISPcodeInfo

## ----rawFunc,eval = TRUE----------------------------------
getNWISPcodeInfo

## ----seeVignette,eval = FALSE-----------------------------
#  vignette(dataRetrieval)

## ----installFromCran,eval = FALSE-------------------------
#  install.packages("dataRetrieval",
#  repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
#  dependencies=TRUE,
#  type="both")

## ----openLibraryTest, eval=FALSE--------------------------
#  library(dataRetrieval)

## ----label=getSiteApp, echo=TRUE--------------------------
availableData <- getNWISDataAvailability(siteNumber)
dailyData <- availableData["dv" == availableData$service,]
dailyData <- dailyData["00003" == dailyData$statCd,]

tableData <- with(dailyData, 
      data.frame(
        shortName=srsname, 
        Start=startDate, 
        End=endDate, 
        Count=count,
        Units=parameter_units)
      )
tableData

## ----label=saveData, echo=TRUE, eval=FALSE----------------
#  write.table(tableData, file="tableData.tsv",sep="\t",
#              row.names = FALSE,quote=FALSE)

