#' USGS Site File Data Retrieval
#'
#' Imports data from USGS site file site. This function gets data from here: <https://waterservices.usgs.gov/>
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @keywords data import USGS web service
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#'  station_nm \tab character \tab Site name \cr
#'  site_tp_cd \tab character \tab Site type \cr
#'  lat_va \tab numeric \tab DMS latitude \cr
#'  long_va \tab numeric \tab DMS longitude \cr
#'  dec_lat_va \tab numeric \tab Decimal latitude \cr
#'  dec_long_va \tab numeric \tab Decimal longitude \cr
#'  coord_meth_cd  \tab character \tab Latitude-longitude method \cr
#'  coord_acy_cd \tab character \tab Latitude-longitude accuracy \cr
#'  coord_datum_cd \tab character \tab Latitude-longitude datum \cr
#'  dec_coord_datum_cd \tab character \tab Decimal Latitude-longitude datum \cr
#'  district_cd \tab character \tab District code \cr
#'  state_cd \tab character \tab State code \cr
#'  county_cd \tab character \tab County code \cr
#'  country_cd \tab character \tab Country code \cr
#'  land_net_ds \tab character \tab Land net location description \cr
#'  map_nm \tab character \tab Name of location map \cr
#'  map_scale_fc \tab character \tab Scale of location map \cr
#'  alt_va \tab numeric \tab Altitude of Gage/land surface \cr
#'  alt_meth_cd \tab character \tab Method altitude determined \cr
#'  alt_acy_va  \tab numeric \tab Altitude accuracy \cr
#'  alt_datum_cd \tab character \tab Altitude datum \cr
#'  huc_cd \tab character \tab Hydrologic unit code \cr
#'  basin_cd \tab character \tab Drainage basin code \cr
#'  topo_cd \tab character \tab Topographic setting code \cr
#'  instruments_cd \tab character \tab Flags for instruments at site \cr
#'  construction_dt \tab character \tab Date of first construction \cr
#'  inventory_dt \tab character \tab Date site established or inventoried \cr
#'  drain_area_va \tab numeric \tab Drainage area \cr
#'  contrib_drain_area_va \tab numeric \tab Contributing drainage area \cr
#'  tz_cd  \tab character \tab Time Zone abbreviation \cr
#'  local_time_fg \tab character \tab Site honors Daylight Savings Time \cr
#'  reliability_cd \tab character \tab Data reliability code \cr
#'  gw_file_cd \tab character \tab Data-other GW files \cr
#'  nat_aqfr_cd \tab character \tab National aquifer code \cr
#'  aqfr_cd \tab character \tab Local aquifer code \cr
#'  aqfr_type_cd \tab character \tab Local aquifer type code \cr
#'  well_depth_va \tab numeric \tab Well depth \cr
#'  hole_depth_va \tab numeric \tab Hole depth \cr
#'  depth_src_cd \tab character \tab Source of depth data \cr
#'  project_no \tab character \tab Project number \cr
#' }
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' }
#' @export
#' @seealso [read_waterdata_monitoring_location()]
#' @examples
#' 
#' # see ?read_waterdata_monitoring_location
#' # siteINFOMulti <- readNWISsite(c("05114000", "09423350"))
#' 
readNWISsite <- function(siteNumbers) {

  .Deprecated(new = "read_waterdata_monitoring_location",
              package = "dataRetrieval", 
              msg = "NWIS servers are slated for decommission. Please begin to migrate to read_waterdata_monitoring_location")
  
  
  baseURL <- httr2::request(pkg.env[["site"]])
  urlSitefile <- httr2::req_url_query(baseURL,
                                      siteOutput = "Expanded", 
                                      format = "rdb")

  POST <- nchar(paste0(siteNumbers, collapse = "")) > 2048

  urlSitefile <- get_or_post(urlSitefile, POST = POST,
              site = siteNumbers, 
              .multi = "comma")

  data <- importRDB1(urlSitefile, asDateTime = FALSE)
  # readr needs multiple lines to convert to anything but characters:
  data[grep("_va", names(data))][data[grep("_va", names(data))] == "."] <- NA

  data[, grep("_va", names(data))] <- sapply(data[, grep("_va", names(data))], as.numeric)

  return(data)
}
