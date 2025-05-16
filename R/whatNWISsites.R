#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from:
#' <https://waterservices.usgs.gov/docs/site-service/>.
#' Mapper format is used
#'
#' @param \dots see <https://waterservices.usgs.gov/docs/site-service/>
#' for a complete list of options. A list (or lists) can also be supplied.
#' 
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' station_nm \tab character \tab Station name \cr
#' site_tp_cd \tab character \tab Site type code \cr
#' dec_lat_va \tab numeric \tab Decimal latitude \cr
#' dec_long_va \tab numeric \tab Decimal longitude \cr
#' queryTime \tab POSIXct \tab Query time \cr
#' }
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' @export
#'
#' @examples
#' \donttest{
#'
#' siteListPhos <- whatNWISsites(stateCd = "OH", parameterCd = "00665")
#' oneSite <- whatNWISsites(sites = "05114000")
#' }
whatNWISsites <- function(...) {

  matchReturn <- convertLists(...)
  if ("service" %in% names(matchReturn)) {
    service <- matchReturn$service
    matchReturn$service <- NULL
  } else {
    service <- NULL
  }
  
  valuesList <- readNWISdots(matchReturn)

  values <- valuesList[["values"]]
  values <- values[names(values) != "format"]

  #################
  # temporary gwlevels fixes
  values <- values[!names(values) %in% c("date_format",
                                        "TZoutput",
                                        "rdb_inventory_output",
                                        "list_of_search_criteria")]
  

  names(values)[names(values) == "state_cd"] <- "stateCd"
  ##################

  if(!is.null(service)){
    service[service == "gwlevels"] <- "gw"
    service[service == "meas"] <- "sv"
    service[service == "peak"] <- "pk"
    service[service == "uv"] <- "id"
    
    values[["hasDataTypeCd"]] <- service
  }
  
  POST <- nchar(paste0(unlist(values), collapse = "")) > 2048
  
  urlCall <- httr2::request(pkg.env[["site"]])
 
  urlCall <- get_or_post(urlCall,
                         POST = POST,
                         !!!values,
                         .multi = "comma")
  
  urlCall <- get_or_post(urlCall,
                         POST = POST, 
                         format = "mapper")
  
  rawData <- getWebServiceData(urlCall, encoding = "gzip")
  if (is.null(rawData)) {
    return(invisible(NULL))
  }
  doc <- xml2::xml_root(rawData)
  siteCategories <- xml2::xml_children(doc)
  retVal <- NULL
  for (sc in siteCategories) {
    sites <- xml2::xml_children(sc)
    site_no <- xml2::xml_attr(sites, "sno")
    station_nm <- xml2::xml_attr(sites, "sna")
    site_tp_cd <- xml2::xml_attr(sites, "cat")
    dec_lat_va <- as.numeric(xml2::xml_attr(sites, "lat"))
    dec_long_va <- as.numeric(xml2::xml_attr(sites, "lng"))
    agency_cd <- xml2::xml_attr(sites, "agc")

    colocated <- isTRUE(xml2::xml_name(sc) == "colocated_sites")

    df <- data.frame(agency_cd, site_no, station_nm, site_tp_cd,
      dec_lat_va, dec_long_va, colocated,
      stringsAsFactors = FALSE
    )

    if (is.null(retVal)) {
      retVal <- df
    } else {
      retVal <- r_bind_dr(retVal, df)
    }
  }

  retVal <- retVal[!duplicated(retVal), ]

  attr(retVal, "url") <- urlCall$url

  timenow <- Sys.time()

  attr(retVal, "queryTime") <- timenow
  # Backwards compatible, might remove later:
  retVal$queryTime <- timenow

  return(retVal)
}
