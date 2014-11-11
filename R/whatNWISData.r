#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count. See \url{http://waterservices.usgs.gov/rest/Site-Service.html}
#' for more information.
#'
#' @param siteNumbers string vector of USGS site number or multiple sites.
#' @param service vector string. Options are "all", or one or many of "dv"(daily values),
#'      "uv","rt", or "iv"(unit values), "qw"(water-quality),"sv"(sites visits),"pk"(peak measurements),
#'      "gw"(groundwater levels), "ad" (sites included in USGS Annual Water Data Reports External Link), 
#'      "aw" (sites monitored by the USGS Active Groundwater Level Network External Link), "id" (historical 
#'      instantaneous values)
#' @param parameterCd string vector of valid parameter codes to return. Defaults to "all" which will not perform a filter.
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
#' flowAndTemp <- whatNWISdata(siteNumbers, parameterCd=c("00060","00010"))
whatNWISdata <- function(siteNumbers,service="all",parameterCd="all",statCd="all"){
  
  siteNumber <- paste(siteNumbers,collapse=",")
  
  if(!("all" %in% service)){
    service <- match.arg(service, c("dv","uv","qw","ad","id","pk","sv","gw","aw","all","ad","iv","rt"), several.ok = TRUE)
  }
  
  if(!("all" %in% parameterCd)){
    parameterCdCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    
    if(!parameterCdCheck){
      goodIndex <- which(parameterCd %in% parameterCdFile$parameter_cd)
      if(length(goodIndex) > 0){
        badparameterCd <- parameterCd[-goodIndex]
      } else {
        badparameterCd <- parameterCd
      }
      message("The following parameterCds seem mistyped:",paste(badparameterCd,collapse=","), "and will be ignored.")
      parameterCd <- parameterCd[goodIndex]
    }
  }
  
  
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=",siteNumber,sep = "")
 
  SiteFile <- importRDB1(urlSitefile, asDateTime = FALSE)
  
  headerInfo <- comment(SiteFile)
  
  parameterCds <- unique(SiteFile$parm_cd)
  
  parameterCdFile <- parameterCdFile
  
  parameterCdINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% parameterCds,]
  SiteFile <- merge(SiteFile,parameterCdINFO,by.x="parm_cd" ,by.y="parameter_cd",all=TRUE)
  
  
  if(!("all" %in% service)){
    SiteFile <- SiteFile[SiteFile$data_type_cd %in% service,]
  }
  if(!("all" %in% statCd)){
    SiteFile <- SiteFile[SiteFile$stat_cd %in% statCd,]
  }
  if(!("all" %in% parameterCd)){
    SiteFile <- SiteFile[SiteFile$parm_cd %in% parameterCd,]
  }
  
  
  SiteFile$begin_date <- as.Date(parse_date_time(SiteFile$begin_date, c("Ymd", "mdY", "Y!")))
  SiteFile$end_date <- as.Date(parse_date_time(SiteFile$end_date, c("Ymd", "mdY", "Y!")))
  
  comment(SiteFile) <- headerInfo
  attr(SiteFile, "url") <- urlSitefile
  
  return(SiteFile)

}
