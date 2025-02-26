#' Set data endpoint
#'
#' access Indicate which dataRetrieval access code
#' you want to use options: \code{c('public','internal')}
#'
#' @param access code for data access. Options are: "public","internal","cooperator", or "USGS".
#' \itemize{
#' \item{"internal" represents Access=3 ...for a single water science center}
#' \item{"USGS" represents Access=2 ...for all water science centers}
#' \item{"cooperator" represents Access=1}
#' \item{"public" represents Access=0, public access}
#' }
#'
#' @author Luke Winslow, Jordan S Read
#'
#' @examples
#' \donttest{
#' setAccess("internal")
#'
#' setAccess("public")
#' }
#'
#' @export
setAccess <- function(access = "public") {
  access <- match.arg(access, c("public", "internal", "cooperator", "USGS"))

  if (access == "internal") {
    pkg.env$access <- "3"
    message("setting access to internal")
  } else if (access == "cooperator") {
    pkg.env$access <- "1"
    message("setting access to cooperator")
  } else if (access == "USGS") {
    pkg.env$access <- "2"
    message("setting access to all USGS Water Science Centers")
  } else {
    pkg.env$access <- NULL
    message("setting access to public")
  }
  # nolint start: line_length_linter
  pkg.env$site <- "https://waterservices.usgs.gov/nwis/site/"
  pkg.env$iv <- "https://nwis.waterservices.usgs.gov/nwis/iv/"
  pkg.env$iv_recent <- "https://waterservices.usgs.gov/nwis/iv/"
  pkg.env$dv <- "https://waterservices.usgs.gov/nwis/dv/"
  pkg.env$gwlevels <- "https://nwis.waterdata.usgs.gov/nwis/gwlevels"
  pkg.env$measurements <- "https://waterdata.usgs.gov/nwis/measurements/"
  pkg.env$peak <- "https://nwis.waterdata.usgs.gov/usa/nwis/peak/"
  pkg.env$rating <- "https://waterdata.usgs.gov/nwisweb/get_ratings/"
  pkg.env$stat <- "https://waterservices.usgs.gov/nwis/stat/"
  pkg.env$useNat <- "https://waterdata.usgs.gov/nwis/water_use"
  pkg.env$pCode <- "https://help.waterdata.usgs.gov/code/parameter_cd_query"
  pkg.env$pCodeSingle <- "https://help.waterdata.usgs.gov/code/parameter_cd_nm_query"
  # NOTE: state water use are still in: constructUseURL

  pkg.env$Result <- "https://www.waterqualitydata.us/data/Result/search"
  pkg.env$Station <- "https://www.waterqualitydata.us/data/Station/search"
  pkg.env$Activity <- "https://www.waterqualitydata.us/data/Activity/search"
  pkg.env$ActivityMetric <- "https://www.waterqualitydata.us/data/ActivityMetric/search"
  pkg.env$SiteSummary <- "https://www.waterqualitydata.us/data/summary/monitoringLocation/search"
  pkg.env$Project <- "https://www.waterqualitydata.us/data/Project/search"
  pkg.env$ProjectMonitoringLocationWeighting <- "https://www.waterqualitydata.us/data/ProjectMonitoringLocationWeighting/search"
  pkg.env$ResultDetectionQuantitationLimit <- "https://www.waterqualitydata.us/data/ResultDetectionQuantitationLimit/search"
  pkg.env$BiologicalMetric <- "https://www.waterqualitydata.us/data/BiologicalMetric/search"
  pkg.env$Organization <- "https://www.waterqualitydata.us/data/Organization/search"
  pkg.env$ResultWQX3 <- "https://www.waterqualitydata.us/wqx3/Result/search"
  pkg.env$StationWQX3 <- "https://www.waterqualitydata.us/wqx3/Station/search"
  pkg.env$ActivityWQX3 <- "https://www.waterqualitydata.us/wqx3/Activity/search"
  pkg.env$samplesData <- "https://api.waterdata.usgs.gov/samples-data/summary"
  pkg.env$status <- "https://www.waterqualitydata.us/wqx3/status/"
  
  pkg.env$NGWMN <- "https://cida.usgs.gov/ngwmn_cache/sos"
  # nolint end
}

