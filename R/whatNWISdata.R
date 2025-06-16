#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count. See
#' <https://waterservices.usgs.gov/docs/site-service/>
#' for more information.
#'
#' @param \dots see <https://waterservices.usgs.gov/docs/site-service/>
#' for a complete list of options.  A list of arguments can also be supplied.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function will
#' convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @keywords data import USGS web service
#' 
#' @details This function requires users to create their own arguments
#' based on the NWIS web services. It is a more complicated function to use
#' compared to other NWIS functions such as [readNWISdv()], [readNWISuv()],
#' etc. However, this function adds a lot of
#' flexibility to the possible queries. If the "service" argument is included,
#' the results will be filtered to the proper data_type_cd. This is a great
#' function to use before a large data set, by filtering down the number
#' of sites that have useful data.
#' 
#' 
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
#' @seealso [read_waterdata_ts_meta()]
#' @examples
#' 
#' # see ?read_waterdata_ts_meta
#' 
#' #site1 <- whatWQPsamples(siteid = "USGS-01594440")
#'
#' #type <- "Stream"
#'
#' #sites <- whatWQPsamples(countycode = "US:55:025", siteType = type)
#'
#' #lakeSites_samples <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment",
#' #                                    countycode = "US:55:025")
#' 
#' 
whatNWISdata <- function(..., convertType = TRUE) {
  matchReturn <- convertLists(...)

  prewarned <- FALSE
  
  .Deprecated(new = "read_waterdata_ts_meta",
              package = "dataRetrieval", 
              msg = "NWIS servers are slated for decommission. Please begin to migrate to read_waterdata_ts_meta")
  
  if ("service" %in% names(matchReturn)) {
    service <- matchReturn$service

    if (any(service %in% c("qw", "qwdata"))) {
      .Deprecated(
        old = "whatNWISdata", package = "dataRetrieval",
        new = "whatWQPdata",
        msg = nwis_message()
      )
      prewarned <- TRUE
    } 
  } else {
    service <- "all"
  }
  
  if (any(service == "site")) {
    service <- "all"
  } else if (any(service == "iv")) {
    service[service == "iv"] <- "uv"
  } else if (any(service == "peak")) {
    service[service == "peak"] <- "pk"
  } else if (any(service == "measurements")) {
    service[service == "measurements"] <- "sv"
  } else if(any(service == "gwlevels")) {
    service[service == "gwlevels"] <- "gw"
  }
  
  if ("statCd" %in% names(matchReturn)) {
    statCd <- matchReturn$statCd
    matchReturn <- matchReturn[names(matchReturn) != "statCd"]
  } else {
    statCd <- "all"
  }

  if ("parameterCd" %in% names(matchReturn)) {
    parameterCd <- matchReturn$parameterCd
    matchReturn[["parameterCd"]] <- NULL
  } else {
    parameterCd <- "all"
  }
  
  if("startDate" %in% names(matchReturn) &&
     !"endDate" %in% names(matchReturn)){
    matchReturn[["endDate"]] <- matchReturn[["startDate"]]
  }

  matchReturn$service <- "site"

  valuesList <- readNWISdots(matchReturn)

  values <- valuesList[["values"]]
  values <- values[names(values) != "format"]
  
  POST <- nchar(paste0(unlist(values), collapse = "")) > 2048
  
  urlSitefile <- httr2::request(pkg.env[["site"]])
  urlSitefile <- get_or_post(urlSitefile,
                             POST = POST,
                             seriesCatalogOutput = "true")
  urlSitefile <- get_or_post(urlSitefile,
                             POST = POST,
                             !!!values,
                             .multi = "comma")
  
  SiteFile <- importRDB1(urlSitefile, asDateTime = FALSE, convertType = convertType)

  if (!("all" %in% service)) {
    SiteFile <- SiteFile[SiteFile$data_type_cd %in% service, ]
  }
  if (!("all" %in% statCd)) {
    SiteFile <- SiteFile[SiteFile$stat_cd %in% c(statCd, NA), ]
  }
  if (!("all" %in% parameterCd)) {
    SiteFile <- SiteFile[SiteFile$parm_cd %in% parameterCd, ]
  }

  if (nrow(SiteFile) > 0 && convertType) {
    SiteFile$begin_date <- as.Date(suppressWarnings(lubridate::parse_date_time(SiteFile$begin_date, c("Ymd", "mdY", "Y!"))))
    SiteFile$end_date <- as.Date(suppressWarnings(lubridate::parse_date_time(SiteFile$end_date, c("Ymd", "mdY", "Y!"))))
  }

  if(any(SiteFile$data_type_cd == "qw")){
    if(!prewarned){
      message(nwis_message())
    }
  }
  return(SiteFile)
}

