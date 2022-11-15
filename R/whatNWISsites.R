#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from:
#' \url{https://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Mapper format is used
#'
#' @param \dots see \url{https://waterservices.usgs.gov/rest/Site-Service.html}
#' for a complete list of options. A list (or lists) can also be supplied.
#' One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' \url{https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET}
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, zip, and providers is optional (these arguments are picked automatically).
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
  valuesList <- readNWISdots(...)

  values <- sapply(valuesList$values, function(x) utils::URLencode(x))
  values["format"] <- "mapper"

  urlCall <- drURL("site", Access = pkg.env$access, arg.list = values)

  rawData <- getWebServiceData(urlCall, encoding = "gzip")
  if (is.null(rawData)) {
    return(invisible(NULL))
  }
  doc <- xml_root(rawData)
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

  attr(retVal, "url") <- urlCall

  timenow <- Sys.time()

  attr(retVal, "queryTime") <- timenow
  # Backwards compatible, might remove later:
  retVal$queryTime <- timenow

  return(retVal)
}
