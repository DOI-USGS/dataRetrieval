#' Create NWIS data citation
#'
#' Uses attributes from the NWIS functions to create data citations.
#' 
#' See \code{?bibentry} for more information.
#'
#' @param x Any data returned from an NWIS function, must
#' include "queryTime" and "url" attributes, which should
#' come with the data by default.  
#' @return bibentry object to use for citing the data.
#' 
#' @export
#'
#' @examples
#' \donttest{
#' nwisData <- readNWISdv("04085427", "00060", "2012-01-01", "2012-06-30")
#' nwis_citation <- create_NWIS_bib(nwisData)
#' nwis_citation
#' 
#' print(nwis_citation, style = "Bibtex")
#' print(nwis_citation, style = "citation")
#' }
create_NWIS_bib <- function(x){
  
  textVersion <- paste0("U.S. Geological Survey, ",
                        format(attr(x, "queryTime"), "%Y"),
                        ", National Water Information System data available on the World Wide Web (USGS Water Data for the Nation), accessed ",
                        format(attr(x, "queryTime"), "%b %d, %Y"),
                        ", at ",
                        attr(x, "url"),
                        ", http://dx.doi.org/10.5066/F7P55KJN")
  
  ref <- utils::bibentry(
    bibtype = "Manual",
    textVersion = textVersion,
    title = "National Water Information System data available on the World Wide Web (USGS Water Data for the Nation)",
    author = utils::person("U.S. Geological Survey"),
    doi = "10.5066/F7P55KJN",
    note = paste("Accessed", format(attr(x, "queryTime"), "%b %d, %Y")),
    year = format(attr(x, "queryTime"), "%Y"),
    url = attr(x, "url"))
  
  return(ref)
}


#' Create WQP data citation
#'
#' Uses attributes from the WQP functions to create data citations.
#' 
#' See \code{?bibentry} for more information.
#'
#' @param x Any data returned from an NWIS function, must
#' include "queryTime" and "url" attributes, which should
#' come with the data by default.  
#' @return bibentry object to use for citing the data.
#' 
#' @export
#'
#' @examples
#' \donttest{
#' WQPData <- readWQPqw("USGS-05288705",
#'                      parameterCd = "00300")
#' wqp_citation <- create_WQP_bib(WQPData)
#' wqp_citation
#' 
#' print(wqp_citation, style = "Bibtex")
#' print(wqp_citation, style = "citation")
#' }
create_WQP_bib <- function(x){
  
  textVersion <- paste0("National Water Quality Monitoring Council, ",
                        format(attr(x, "queryTime"), "%Y"),
                        ", Water Quality Portal, accessed ",
                        format(attr(x, "queryTime"), "%m, %d, %Y"),
                        ", ",
                        attr(x, "url"), 
                        ", https://doi.org/10.5066/P9QRKUVJ.")
  
  ref <- utils::bibentry(
    bibtype = "Manual",
    textVersion = textVersion,
    title = " Water Quality Portal",
    author = utils::person("National Water Quality Monitoring Council"),
    doi = "10.5066/P9QRKUVJ",
    note = paste("Accessed", format(attr(x, "queryTime"), "%b %d, %Y")),
    year = format(attr(x, "queryTime"), "%Y"),
    url = attr(x, "url"))
  
  return(ref)
}
