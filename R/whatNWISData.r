#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count.
#'
#' @param siteNumbers string vector of USGS site number or multiple sites.
#' @param service vector string. Options are "all", or one or many of "dv"(daily values),
#'      "uv","rt", or "iv"(unit values), "qw"(water-quality),"sv"(sites visits),"pk"(peak measurements),
#'      "gw"(groundwater levels), "ad" (sites included in USGS Annual Water Data Reports External Link), 
#'      "aw" (sites monitored by the USGS Active Groundwater Level Network External Link), "id" (historical 
#'      instantaneous values), "
#' @param pCode string vector of valid parameter codes to return. Defaults to "all" which will not perform a filter.
#' @param statCd string vector of all statistic codes to return. Defaults to "all" which will not perform a filter.
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @import RCurl
#' @import lubridate
#' @examples
#' availableData <- whatNWISdata('05114000')
#' # To find just unit value ('instantaneous') data:
#' uvData <- whatNWISdata('05114000',service="uv")
#' uvDataMulti <- whatNWISdata(c('05114000','09423350'),service=c("uv","dv"))
#' siteNumbers <- c("01491000","01645000")
#' flowAndTemp <- whatNWISdata(siteNumbers, pCode=c("00060","00010"))
whatNWISdata <- function(siteNumbers,service="all",pCode="all",statCd="all"){
  
  siteNumber <- paste(siteNumbers,collapse=",")
  
  if(!("all" %in% service)){
    service <- match.arg(service, c("dv","uv","qw","ad","id","pk","sv","gw","aw","all","ad","iv","rt"), several.ok = TRUE)
  }
  
  if(!("all" %in% pCode)){
    pcodeCheck <- all(nchar(pCode) == 5) & all(!is.na(suppressWarnings(as.numeric(pCode))))
    
    if(!pcodeCheck){
      goodIndex <- which(pCode %in% parameterCdFile$parameter_cd)
      if(length(goodIndex) > 0){
        badPcode <- pCode[-goodIndex]
      } else {
        badPcode <- pCode
      }
      message("The following pCodes seem mistyped:",paste(badPcode,collapse=","), "and will be ignored.")
      pCode <- pCode[goodIndex]
    }
  }
  
  
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=",siteNumber,sep = "")
 
  doc = tryCatch({
    h <- basicHeaderGatherer()
    doc <- getURL(urlSitefile, headerfunction = h$update)
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", urlSitefile))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", urlSitefile))
    message(e)
    return(NA)
  }) 
  
  if(h$value()["Content-Type"] == "text/plain;charset=UTF-8"){
    SiteFile <- read.delim(
      textConnection(doc),
      header = TRUE,
      quote="\"",
      dec=".",
      sep='\t',
      colClasses=c('character'),
      fill = TRUE,
      comment.char="#")
    
    SiteFile <- SiteFile[-1,]

    numberColumns <- grep("_va",names(SiteFile))    
    SiteFile[,numberColumns] <- sapply(SiteFile[,numberColumns],as.numeric)
    
    intColumns <- grep("_nu",names(SiteFile))
    SiteFile[,intColumns] <- sapply(SiteFile[,intColumns],as.integer)
    
    pCodes <- unique(SiteFile$parm_cd)
    
    parameterCdFile <- parameterCdFile
    
    pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes,]
    SiteFile <- merge(SiteFile,pcodeINFO,by.x="parm_cd" ,by.y="parameter_cd",all=TRUE)
    
    
    if(!("all" %in% service)){
      SiteFile <- SiteFile[SiteFile$data_type_cd %in% service,]
    }
    if(!("all" %in% statCd)){
      SiteFile <- SiteFile[SiteFile$stat_cd %in% statCd,]
    }
    if(!("all" %in% pCode)){
      SiteFile <- SiteFile[SiteFile$parm_cd %in% pCode,]
    }
    
    
    SiteFile$begin_date <- as.Date(parse_date_time(SiteFile$begin_date, c("Ymd", "mdY", "Y!")))
    SiteFile$end_date <- as.Date(parse_date_time(SiteFile$end_date, c("Ymd", "mdY", "Y!")))
    
    return(SiteFile)
  } else {
    message(paste("URL caused an error:", urlSitefile))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
}
