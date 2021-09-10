#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count. See \url{https://waterservices.usgs.gov/rest/Site-Service.html}
#' for more information.
#'
#' @param \dots see \url{https://waterservices.usgs.gov/rest/Site-Service.html} for a complete list of options.  A list of arguments can also be supplied. 
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
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
#' @examples
#' \donttest{
#' 
#' availableData <- whatNWISdata(siteNumber = '05114000')
#' # To find just unit value ('instantaneous') data:
#' uvData <- whatNWISdata(siteNumber = '05114000',service="uv")
#' uvDataMulti <- whatNWISdata(siteNumber = c('05114000','09423350'),service=c("uv","dv"))
#' flowAndTemp <- whatNWISdata(stateCd = "WI", service = "uv", 
#'                              parameterCd = c("00060","00010"),
#'                              statCd = "00003")
#' 
#' }
whatNWISdata <- function(..., convertType=TRUE){
  
  matchReturn <- convertLists(...)
  
  if("service" %in% names(matchReturn)){
    service <- matchReturn$service
    if(any(service %in% c("qw", "qwdata"))){
      .Deprecated(old = "whatNWISdata", package = "dataRetrieval", 
                  new = "whatWQPdata",
                  msg = "NWIS qw web services are being retired. Please see the vignette 
'Changes to NWIS QW services' for more information.")
    }
  } else {
    service <- "all"
  }
  
  if("statCd" %in% names(matchReturn)){
    statCd <- matchReturn$statCd
    matchReturn <- matchReturn[names(matchReturn) != "statCd"]
  } else {
    statCd <- "all"
  }
  
  if("parameterCd" %in% names(matchReturn)){
    parameterCd <- matchReturn$parameterCd
    matchReturn[["parameterCd"]] <- NULL
  } else {
    parameterCd <- "all"
  }
  
  matchReturn$service <- "site"
  
  valuesList <- readNWISdots(matchReturn)
  
  values <- sapply(valuesList$values, function(x) URLencode(x))

  if(any(service == "iv")){
    service[service == "iv"] <- "uv"
  }
  
  urlSitefile <- drURL('site', Access=pkg.env$access, seriesCatalogOutput='true',arg.list=values)
 
  SiteFile <- importRDB1(urlSitefile, asDateTime = FALSE, convertType = convertType)

  if(!("all" %in% service)){
    SiteFile <- SiteFile[SiteFile$data_type_cd %in% service,]
  }
  if(!("all" %in% statCd)){
    SiteFile <- SiteFile[SiteFile$stat_cd %in% c(statCd,NA),]
  }
  if(!("all" %in% parameterCd)){
    SiteFile <- SiteFile[SiteFile$parm_cd %in% parameterCd,]
  }
  
  if(nrow(SiteFile) > 0 & convertType){
    SiteFile$begin_date <- as.Date(lubridate::parse_date_time(SiteFile$begin_date, c("Ymd", "mdY", "Y!")))
    SiteFile$end_date <- as.Date(lubridate::parse_date_time(SiteFile$end_date, c("Ymd", "mdY", "Y!")))
  }
  
  return(SiteFile)

}
