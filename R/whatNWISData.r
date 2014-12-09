#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count. See \url{http://waterservices.usgs.gov/rest/Site-Service.html}
#' for more information.
#'
#' @param siteNumbers character USGS site number or multiple sites.
#' @param service character. Options are "all", or one or many of "dv"(daily values),
#'      "uv","rt", or "iv"(unit values), "qw"(water-quality),"sv"(sites visits),"pk"(peak measurements),
#'      "gw"(groundwater levels), "ad" (sites included in USGS Annual Water Data Reports External Link), 
#'      "aw" (sites monitored by the USGS Active Groundwater Level Network External Link), "id" (historical 
#'      instantaneous values)
#' @param parameterCd character vector of valid parameter codes to return. Defaults to "all" which will not perform a filter.
#' @param statCd character vector of all statistic codes to return. Defaults to "all" which will not perform a filter.
#' @keywords data import USGS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' station_nm \tab character \tab Site name \cr 
#' site_tp_cd \tab character \tab Site type \cr
#' dec_lat_va \tab numeric \tab Decimal latitude\cr
#' dec_long_va \tab numeric \tab Decimal longitude \cr
#' coord_acy_cd \tab character \tab Latitude-longitude accuracy \cr
#' dec_coord_datum_cd \tab character \tab Decimal Latitude-longitude datum \cr
#' alt_va \tab character \tab Altitude of Gage or land surface \cr
#' alt_acy_va \tab character \tab Altitude accuracy \cr
#' alt_datum_cd \tab character \tab Altitude datum \cr
#' huc_cd \tab character \tab Hydrologic unit code \cr
#' data_type_cd \tab character \tab Data type \cr
#' parm_cd \tab character \tab Parameter code \cr
#' stat_cd \tab character \tab Statistical code \cr
#' dd_nu \tab character \tab Internal database key \cr
#' loc_web_ds \tab character \tab Additional measurement description \cr
#' medium_grp_cd \tab character \tab Medium group code \cr
#' parm_grp_cd \tab character \tab Parameter group code \cr
#' srs_id \tab character \tab SRS ID \cr
#' access_cd \tab character \tab Access code \cr
#' begin_date \tab Date \tab Begin date \cr
#' end_date \tab Date \tab End date \cr
#' count_nu \tab integer \tab Record count\cr
#' parameter_group_nm \tab character \tab Parameter group name \cr
#' parameter_nm \tab character \tab Parameter name \cr
#' casrn \tab character \tab Chemical Abstracts Service (CAS) Registry Number \cr
#' srsname \tab character \tab Substance Registry Services \cr
#' parameter_units \tab character \tab Parameter units \cr
#' }
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' @export
#' @import lubridate
#' @examples
#' \dontrun{
#' availableData <- whatNWISdata('05114000')
#' # To find just unit value ('instantaneous') data:
#' uvData <- whatNWISdata('05114000',service="uv")
#' uvDataMulti <- whatNWISdata(c('05114000','09423350'),service=c("uv","dv"))
#' siteNumbers <- c("01491000","01645000")
#' flowAndTemp <- whatNWISdata(siteNumbers, parameterCd=c("00060","00010"))
#' }
whatNWISdata <- function(siteNumbers,service="all",parameterCd="all",statCd="all"){
  
  siteNumber <- paste(siteNumbers,collapse=",")
  
  if(!("all" %in% service)){
    service <- match.arg(service, c("dv","uv","qw","ad","id","pk","sv","gw","aw","all","ad","iv","rt"), several.ok = TRUE)
  }

  
  data("parameterCdFile")
  
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
  attr(SiteFile, "queryTime") <- Sys.time()
  
  return(SiteFile)

}
